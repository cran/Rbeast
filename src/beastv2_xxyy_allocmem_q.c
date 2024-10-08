#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include "abc_001_config.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "abc_rand.h"
#include "abc_mat.h"
#include "beastv2_func.h"
#include "abc_ide_util.h" 
static I32 __GetNumElem_of_XnewTerm(BEAST2_MODEL_PTR model,BEAST2_OPTIONS_PTR opt,I32 * MAX_COL) {
	#define MODEL (*model)
     #define MAX_NUM_NEW_SEG 2
	I32 MXNCOL_PERSEG1=MODEL.sid >=0 ? (MODEL.b[MODEL.sid].prior.maxOrder)*2L : -9999;  	
	I32 MXNCOL_PERSEG2=MODEL.tid >=0 ? (MODEL.b[MODEL.tid].prior.maxOrder+1L) : -9999;    
	I32 MXNCOL_PERSEG3=MODEL.did >=0 ? opt->io.meta.period : -9999;                       
	I32 MXNCOL_PERSEG4=MODEL.vid >=0 ? MODEL.b[MODEL.tid].prior.maxOrder : -9999;    
	I32 MAXNUMCOL_Xnewterm=max3(MXNCOL_PERSEG1,MXNCOL_PERSEG2,MXNCOL_PERSEG3) * MAX_NUM_NEW_SEG;
	I32 MAX_TOTAL_SEGNUM=0;
	for (int i=0; i < MODEL.NUMBASIS; i++) { 
		MAX_TOTAL_SEGNUM+=(MODEL.b[i].prior.maxKnotNum+1);
	}
	I32 MAX_NUMELEM_SEGINFO=(MAX_TOTAL_SEGNUM * sizeof(BEAST2_BASESEG)+3)/4;
	I32 MAX_MEM_FOR_CHANGEPOINTS=6 * opt->io.N;
	I32 Nraw=opt->io.dims[ opt->io.meta.whichDimIsTime-1L];
	I32 MAX_COLS_YPRED=opt->io.q * MODEL.NUMBASIS;
	I32 Npad=RoundTo8(opt->io.N);
	Npad=opt->io.N;
	I32 TOTAL_NUM=max4( MAXNUMCOL_Xnewterm* Npad+MAX_NUMELEM_SEGINFO, 
		                  MAX_MEM_FOR_CHANGEPOINTS,
		                  MAX_COLS_YPRED*Npad,
		                  Nraw);
	*MAX_COL=MAXNUMCOL_Xnewterm;
	return TOTAL_NUM;
    #undef MODEL
}
void AllocateXXXMEM( F32PTR * Xt_mars,F32PTR*  Xnewterm,F32PTR*  Xt_zeroBackup,
	                 BEAST2_MODEL_PTR model,BEAST2_OPTIONS_PTR opt,MemPointers * MEM) 
{	
	I32 N=opt->io.N;
	I32 Npad=RoundTo8(N);
	I32 K_MAX=opt->prior.K_MAX;
	I64 SIZE_Xtmars=Npad * K_MAX;
	I32 XNEW_MAX_NUMCOL;
	I32 XNEW_TOTAL_NUM=__GetNumElem_of_XnewTerm(model,opt,&XNEW_MAX_NUMCOL);
	I32 MAXNUM_MISSINGROW=N * opt->io.meta.maxMissingRate+1;
	I64 SIZE_Xnewterm=XNEW_TOTAL_NUM;
	I64 SIZE_XtzeroBackup=MAXNUM_MISSINGROW * XNEW_MAX_NUMCOL;
	I32 SIZE_Total=RoundTo64(SIZE_Xtmars)+RoundTo64(SIZE_Xnewterm)+RoundTo64(SIZE_XtzeroBackup);
	typedef struct SVDBasisMEM SVDBasisMEM;
	I64 Get_Alloc_SVDBasisMEM(int N,int P,SVDBasisMEM * s,VOID_PTR bufBase); 
	I32 SIZE_svdASISMEM=0;  
	if (opt->io.meta.seasonForm=='V') {
		int P=opt->io.meta.period;
		SIZE_svdASISMEM=Get_Alloc_SVDBasisMEM(N,P,NULL,NULL);
	}
	SIZE_Total=max(SIZE_Total,SIZE_svdASISMEM);
	*Xt_mars=MyALLOC(*MEM,SIZE_Total,F32,64);
	*Xnewterm=*Xt_mars+RoundTo64(SIZE_Xtmars) ;
	*Xt_zeroBackup=*Xnewterm+RoundTo64(SIZE_Xnewterm);
}
void AllocateYinfoMEM(BEAST2_YINFO_PTR yInfo,BEAST2_OPTIONS_PTR opt,MemPointers* MEM) {
	I32  N=opt->io.N;	
	I32  q=opt->io.q;  
	I32  qxq=q*q;
	I32  MAXNUM_MISSINGROW=N;  
	MemNode nodes[10];
	int     nNodes=0;
	nodes[nNodes++]=(MemNode){ .addr=&yInfo->Y,.size=sizeof(F32)*N*q,.align=64,.offset_from_origin=0};
	nodes[nNodes++]=(MemNode){ .addr=&yInfo->YtY_plus_alpha2Q,.size=sizeof(F32)*q*q,.align=4,.offset_from_origin=0 };
	nodes[nNodes++]=(MemNode){ .addr=&yInfo->mean,.size=sizeof(F32)*q,.align=4,.offset_from_origin=0 };
	nodes[nNodes++]=(MemNode){ .addr=&yInfo->sd,.size=sizeof(F32)*q,.align=4,.offset_from_origin=0 };
	nodes[nNodes++]=(MemNode){ .addr=&yInfo->rowsMissing,.size=sizeof(U32) * MAXNUM_MISSINGROW,.align=64,.offset_from_origin=0 };
	yInfo->q=q;
	if (opt->io.meta.deseasonalize) {
		nodes[nNodes++]=(MemNode){ .addr=&yInfo->Yseason,.size=sizeof(F32) * N * q,.align=4,.offset_from_origin=0 };
	} else {
		yInfo->Yseason=NULL;
	}
	if (opt->io.meta.detrend) {
		nodes[nNodes++]=(MemNode){ .addr=&yInfo->Ytrend,.size=sizeof(F32) * N * q,.align=4,.offset_from_origin=0 }; 
	} else {
		yInfo->Ytrend=NULL;
	}
	nodes[nNodes++]=(MemNode){ .addr=NULL };
	MEM->alloclist(MEM,nodes,AggregatedMemAlloc,NULL);
}
#include "abc_000_warning.h"
