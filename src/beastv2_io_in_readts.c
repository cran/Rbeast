#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "abc_000_warning.h"
#include "abc_001_config.h"
#include "abc_datatype.h"
#include "abc_ide_util.h"
#include "abc_ts_func.h"
#include "abc_common.h" 
#include "abc_mat.h"
#include "abc_blas_lapack_lib.h"
#include "beastv2_io.h"
static I32 __GetRawTimeDimension(A(IO_PTR) io) {
	return io->dims[io->meta.whichDimIsTime - 1];
}
static void GetInputOffsetStride(A(IO_PTR) io,I64 idx,I64 *pStride,I64 *pOffset,I64 *pN)
{
	I64 N=__GetRawTimeDimension(io);  
	I64 stride,offset;	
	if (io->ndim==1)       
		stride=1L,
		offset=0;	
	if (io->ndim==2)       
	{
		if (io->meta.whichDimIsTime==1) {
			stride=1L,
			offset=(idx - 1) * N;
		} 	else {
			I32 ROW=io->dims[0];
			stride=ROW,
			offset=(idx - 1);
		}
	}
	else if (io->ndim==3L)  
	{
			I64  ROW,COL;
			switch (io->meta.whichDimIsTime) {
			case 1:
				ROW=io->dims[1];
				COL=io->dims[2];
				stride=1L;
				offset=(idx - 1)*N;
				break;
			case 2: {
				ROW=io->dims[0];
				COL=io->dims[2];
				I64 r,c;
				c=(idx-1)/ROW;
				r=idx - c*ROW;
				c=c+1;
				stride=ROW;
				offset=(c - 1)*(N*ROW)+r - 1;
				break;
			}
			case 3: {
				ROW=io->dims[0],
				COL=io->dims[1];
				I64 r,c;
				c=(idx - 1)/ROW;
				r=idx - c*ROW;
				c=c+1;
				stride=ROW*COL;
				offset=(c - 1)*ROW+r - 1;
				break;
			}
		} 
	} 
	*pStride=stride;
	*pOffset=offset;
	*pN=N;
}
static void fetch_next_timeSeries_MEM_reglular(A(YINFO_PTR)  yInfo,int idx,A(IO_PTR) io)
{   
	I64  stride,offset,N;
	GetInputOffsetStride(io,idx,&stride,&offset,&N);
	I32    q=io->q; 
	F32PTR Y=yInfo->Y;
	for (I32 i=0; i < q; i++) {
		CopyStrideMEMToF32Arr(Y+i * N,io->pdata[i],N,stride,offset,io->dataType);
		f32_set_nan_by_value(Y+i * N,N,io->meta.missingValue);
	}
}
static void fetch_next_timeSeries_MEM_irregular(A(YINFO_PTR)  yInfo,int idx,F32PTR GlobalMEMBuf,A(IO_PTR)  io)
{   
	I64 stride,offset,Nraw; 	
	GetInputOffsetStride(io,idx,&stride,&offset,&Nraw);
    I32    Nnew=io->N;
	I32    q=io->q; 
	F32PTR Y=yInfo->Y;	
	for (I32 i=0; i < q; i++) {
		CopyStrideMEMToF32Arr(GlobalMEMBuf,io->pdata[i],Nraw,stride,offset,io->dataType);
		f32_set_nan_by_value(GlobalMEMBuf,Nraw,io->meta.missingValue);
		tsAggegrationPerform(Y+Nnew*i,Nnew,GlobalMEMBuf,Nraw,io->T.numPtsPerInterval,io->T.sortedTimeIdx+io->T.startIdxOfFirsInterval);
	}			
}
void BEAST2_fetch_next_timeSeries(A(YINFO_PTR)  yInfo,int pixelIndex,F32PTR GlobalMEMBuf,A(IO_PTR)  io)  {
		if (io->meta.isRegularOrdered)	 fetch_next_timeSeries_MEM_reglular( yInfo,pixelIndex,io );
		else                 			 fetch_next_timeSeries_MEM_irregular(yInfo,pixelIndex,GlobalMEMBuf,io);	 
}
static I08 _timeseries_deseasonalize_detrend(A(YINFO_PTR)  yInfo,BEAST2_BASIS_PTR basis,F32PTR Xtmp,BEAST2_OPTIONS_PTR opt) {
	int    N=opt->io.N;
	int    q=opt->io.q;
	int    period=opt->io.meta.period; 
	int    Ktrend=opt->prior.trendMaxOrder+1;
	int    Kseason=period - 1;
	F32PTR X=Xtmp;
	int    K=0;
	if (yInfo->Yseason||yInfo->Ytrend) {
		TREND_CONST* bConst=basis[0].type==TRENDID ? &basis[0].bConst.trend : &basis[1].bConst.trend;		
		SCPY(Ktrend * N,bConst->TERMS,X);
		X+=Ktrend * N;
		K+=Ktrend;
	}
	if (yInfo->Yseason){
		F32PTR TERMS=NULL;
		if      (basis[0].type==SEASONID){ SEASON_CONST* bConst=&basis[0].bConst.season; TERMS=bConst->TERMS;} 
		else if (basis[0].type==DUMMYID) { DUMMY_CONST* bConst=&basis[0].bConst.dummy;   TERMS=bConst->TERMS;}
		else if (basis[0].type==SVDID)  { 	SVD_CONST* bConst=&basis[0].bConst.dummy;	 TERMS=bConst->TERMS;	}	
		SCPY(Kseason * N,TERMS,X);
		X+=Kseason * N;
		K+=Kseason;		
	}
	X=Xtmp;
	F32PTR Y=X+N * K;
	F32PTR Yfit=Y+N;
	F32PTR XtX=Yfit+N;
	F32PTR B=XtX+K*K;	
	I32PTR badRowIdx=B+K;
	for (int i=0; i < q;++i) {
		SCPY(N,yInfo->Y+i * N,Y);
		int  nMissing=f32_find_nans(Y,N,badRowIdx);
		U08  skipCurrentPixel=nMissing > (N * opt->io.meta.maxMissingRate) ? 1 : 0;
		if (skipCurrentPixel) { return skipCurrentPixel; }
		F32PTR  Xcopy=badRowIdx+nMissing;
		f32_mat_multirows_extract_set_by_scalar(Xtmp,N,K+1,Xcopy,badRowIdx,nMissing,0);
		linear_regression(Y,X,N,N,K,B,Yfit,NULL,XtX);			
		f32_mat_multirows_set_by_submat(Xtmp,N,K+1,Xcopy,badRowIdx,nMissing);
		if (yInfo->Ytrend) {
			r_cblas_sgemv(CblasColMajor,CblasNoTrans,N,Ktrend,1.f,X,N,B,1L,0.f,yInfo->Ytrend+N * i,1L);
			r_ippsSub_32f_I(yInfo->Ytrend+N * i,yInfo->Y+i * N,N);
		}		
		if (yInfo->Yseason){
			r_cblas_sgemv(CblasColMajor,CblasNoTrans,N,Kseason,1.f,X+N*Ktrend,N,B+Ktrend,1L,0.f,yInfo->Yseason+N*i,1L);
			r_ippsSub_32f_I(yInfo->Yseason+N * i,yInfo->Y+i * N,N);
		}
	}
	return 0;
}
I08 BEAST2_preprocess_timeSeries(A(YINFO_PTR)  yInfo,BEAST2_BASIS_PTR basis,F32PTR Xtmp,BEAST2_OPTIONS_PTR opt) {
	U08 skipCurrentPixel=0;
	if (yInfo->Yseason !=NULL||yInfo->Ytrend !=NULL) {
		skipCurrentPixel=_timeseries_deseasonalize_detrend(yInfo,basis,Xtmp,opt);
		if (skipCurrentPixel) return skipCurrentPixel;
	}
	F32PTR Y=yInfo->Y;
	int    N=opt->io.N;
	int    q=opt->io.q;
	yInfo->nMissing=f32_normalize_multicols_zeroout_nans(Y,yInfo->rowsMissing,N,N,q,yInfo->mean,yInfo->sd);
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,N,1.0,Y,N,Y,N,0.f,yInfo->YtY_plus_alpha2Q,q);
	yInfo->n=N - yInfo->nMissing;
	skipCurrentPixel=yInfo->nMissing > (N * opt->io.meta.maxMissingRate) ? 1L : 0L;
	return skipCurrentPixel;
}
#include "abc_000_warning.h"
