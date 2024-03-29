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
static void __convert_index_to_datasubs3(BEAST2_IO* io,int index,int subs3[]) {
	int subs2[2];
	ind2sub(io->imgdims,2L,index,subs2);
	subs3[io->rowdim - 1]=subs2[0];
	subs3[io->coldim - 1]=subs2[1];
	subs3[io->timedim - 1]=1;     
}
void BEAST2_fetch_timeSeries(A(YINFO_PTR)  yInfo,int pixelIndex,F32PTR GlobalMEMBuf,A(IO_PTR)  io)  {
	int subs3[3];
	I64 stride,offset;
	__convert_index_to_datasubs3(io,pixelIndex,subs3);
	ndarray_get1d_stride_offset(io->dims,3L,subs3,io->timedim,&stride,&offset);
	int    Nraw=io->dims[io->timedim - 1L];
	I32    q=io->q; 
	F32PTR Y=yInfo->Y;
	if ( !io->T.out.needAggregate && !io->T.out.needReOrder)  {
		for (I32 i=0; i < q; i++) {
			f32_from_strided_mem(Y+i * Nraw,io->pdata[i],Nraw,stride,offset,io->dtype[i]);
		}
		f32_set_value_to_nan(Y,Nraw*q,io->meta.missingValue); 
	} else {
		for (I32 i=0; i < q; i++) {
			f32_from_strided_mem(GlobalMEMBuf,io->pdata[i],Nraw,stride,offset,io->dtype[i]);
			f32_set_value_to_nan(GlobalMEMBuf,Nraw,io->meta.missingValue);
			I32    Nnew=io->N;
			tsAggegrationPerform(Y+Nnew * i,Nnew,GlobalMEMBuf,Nraw,io->T.out.numPtsPerInterval,io->T.sorted_time_indices+io->T.out.startIdxOfFirsInterval);
		}
	}
}
static int  __timeseries_deseasonalize_detrend(A(YINFO_PTR)  yInfo,BEAST2_BASIS_PTR basis,F32PTR Xtmp,BEAST2_OPTIONS_PTR opt) {
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
		else if (basis[0].type==DUMMYID) { DUMMY_CONST * bConst=&basis[0].bConst.dummy;  TERMS=bConst->TERMS;}
		else if (basis[0].type==SVDID)  {  SVD_CONST   * bConst=&basis[0].bConst.svd;	 TERMS=bConst->TERMS;	}	
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
		F32PTR Ycopy=Yfit; 
		f32_mat_multirows_extract_set_by_scalar(Y,N,1L,Ycopy,badRowIdx,nMissing,0);
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
		skipCurrentPixel=__timeseries_deseasonalize_detrend(yInfo,basis,Xtmp,opt);
		if (skipCurrentPixel) return skipCurrentPixel;
	}
	F32PTR Y=yInfo->Y;
	int    N=opt->io.N;
	int    q=opt->io.q;
	yInfo->nMissing=f32_normalize_multicols_zeroout_nans(Y,yInfo->rowsMissing,N,N,q,yInfo->mean,yInfo->sd);
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,N,1.0,Y,N,Y,N,0.f,yInfo->YtY_plus_alpha2Q,q);
	yInfo->n=N - yInfo->nMissing;
	skipCurrentPixel=yInfo->nMissing > (N * opt->io.meta.maxMissingRate) ? 1L : 0L;
	if (skipCurrentPixel) {
		return skipCurrentPixel;
	}
	if ('V'==opt->io.meta.seasonForm && opt->io.meta.svdTerms_Object==NULL) {
		void compute_seasonal_svdbasis_from_originalY(F32PTR y,int N,int P,F32PTR Yout,int  Kmax,VOID_PTR BUF);
		void compute_seasonal_svdbasis_from_seasonalY(F32PTR y,int N,int P,F32PTR Yout,int  Kmax,VOID_PTR BUF);
		int Kmax=opt->prior.seasonMaxOrder;
		int P=opt->io.meta.period;
		SVD_CONST* bConst=basis[0].type==SVDID ? &basis[0].bConst.svd : &basis[1].bConst.svd;
		F32PTR   Yout=bConst->TERMS;
		if (opt->io.meta.svdYseason_Object) {
			F32PTR y=bConst->TERMS; 
			CopyNumericObjToF32Arr(y,opt->io.meta.svdYseason_Object,N);		
			compute_seasonal_svdbasis_from_seasonalY(y,N,P,Yout,Kmax,Xtmp);
		} else{
			compute_seasonal_svdbasis_from_originalY(Y,N,P,Yout,Kmax,Xtmp);
		}
		F32PTR ptr=Yout;
		F32PTR ptr1=bConst->SQR_CSUM;
		for (I32 order=1; order <=Kmax; order++) {
			*ptr1=0.f;
			f32_copy(ptr,ptr1+1,N);         f32_cumsumsqr_inplace(ptr1+1,N);
			ptr+=N;
			ptr1+=N+1;
		} 
	}
	return skipCurrentPixel;
}
#include "abc_000_warning.h"
