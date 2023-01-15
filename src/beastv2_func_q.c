#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "abc_001_config.h"
#include "abc_rand.h"
#include "abc_mat.h"
#include "abc_math.h"  
#include "abc_vec.h"  
#include "beastv2_func.h"
F32  GetPercentileNcp(F32PTR prob,I32 N,F32 pctile) {
	F32 cumProb=0.f;
	for (int i=0; i < N; i++) {
		cumProb+=prob[i];
		if (cumProb > pctile) {
			return i;		
		}	 
	}
	return N - 1; 
}
void SetupPointersForCoreResults(CORESULT* coreResults,BEAST2_BASIS_PTR b,I32 NumBasis,BEAST2_RESULT* resultChain) {
	for (I32 i=0; i < NumBasis; i++) {
		if (b[i].type==SEASONID||b[i].type==DUMMYID||b[i].type==SVDID) {
			coreResults[i].xNProb=resultChain->sncpPr,
			coreResults[i].xProb=resultChain->scpOccPr,
			coreResults[i].xorder=resultChain->sorder, 
			coreResults[i].x=resultChain->sY,
			coreResults[i].xSD=resultChain->sSD;
		}
		else if (b[i].type==TRENDID) {
			coreResults[i].xNProb=resultChain->tncpPr,
			coreResults[i].xProb=resultChain->tcpOccPr,
			coreResults[i].xorder=resultChain->torder, 
			coreResults[i].x=resultChain->tY,
			coreResults[i].xSD=resultChain->tSD;
		}
		else if (b[i].type==OUTLIERID) {
			coreResults[i].xNProb=resultChain->oncpPr,
			coreResults[i].xProb=resultChain->ocpOccPr,
			coreResults[i].xorder=NULL,				
			coreResults[i].x=resultChain->oY,
			coreResults[i].xSD=resultChain->oSD;
		}
	}
}
void BEAST2_EvaluateModel(
	BEAST2_MODELDATA *curmodel,BEAST2_BASIS_PTR b,F32PTR Xt_mars,I32 N,I32 NUMBASIS,
	BEAST2_YINFO_PTR  yInfo,BEAST2_HyperPar *hyperPar,F32PTR precVec,VOID_PTR stream )
{
	I32 Npad=(I32)ceil((F32)N/8.0f) * 8; Npad=N;
	I32 K=0;	 
	for (I32 basisID=0; basisID < NUMBASIS; basisID++) {
		BEAST2_BASIS_PTR basis=b+basisID;
		if (basis->type !=OUTLIERID) {
			int         NUM_SEG=basis->nKnot+1;
			TKNOT_PTR   KNOT=basis->KNOT;
			TORDER_PTR  ORDER=basis->ORDER;
			BEAST2_BASESEG seg;
			seg.ORDER1=basis->type==TRENDID ? 0 : 1;
			for (int i=1; i <=NUM_SEG; i++) {
				seg.R1=KNOT[(i - 1) - 1L];
				seg.R2=KNOT[i - 1L] - 1L;
				seg.ORDER2=basis->type==DUMMYID? 0 :  ORDER[i - 1L];
				I32 k=basis->GenTerms(Xt_mars+Npad * K,N,&seg,&(basis->bConst));
				K+=k;
			}
		} 	else	{
			int         numOfSeg=basis->nKnot;
			TKNOT_PTR   knotList=basis->KNOT;
			BEAST2_BASESEG seg;
			seg.ORDER1=seg.ORDER2=0; 
			for (int i=1; i <=numOfSeg; i++) {
				seg.R1=knotList[(i)-1L];
				seg.R2=knotList[(i)-1L];
				I32 k=basis->GenTerms(Xt_mars+Npad * K,N,&seg,&(basis->bConst));
				K+=k;
			}
		}
	}
	curmodel->K=K;
	F32PTR	GlobalMEMBuf=Xt_mars+K * Npad;
	F32PTR	Xt_zeroBackup=GlobalMEMBuf;
	if (yInfo->nMissing > 0) {
		F32 fillvalue=0.f;
		f32_mat_multirows_extract_set_by_scalar(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing,fillvalue);
	}
	F32PTR XtX=curmodel->XtX;
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,K,N,1.f,Xt_mars,Npad,Xt_mars,Npad,0.f,XtX,K);
	F32PTR XtY=curmodel->XtY;
	r_cblas_sgemv(CblasColMajor,CblasTrans,Npad,K,1,Xt_mars,Npad,yInfo->Y,1,0,XtY,1);
	if (yInfo->nMissing > 0) {
		f32_mat_multirows_set_by_submat(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing);
	}
	F32PTR cholXtX=curmodel->cholXtX;
	F32PTR beta_mean=curmodel->beta_mean;	
	chol_addCol_skipleadingzeros_prec_invdiag(XtX,cholXtX,precVec,K,1,K);
	solve_U_as_LU_invdiag_sqrmat(cholXtX,XtY,beta_mean,K);
	F32 alpha2_star=(yInfo->YtY_plus_alpha2Q[0] - DOT(K,XtY,beta_mean)) * 0.5;
	F32 half_log_det_post=sum_log_diagv2(cholXtX,K);
	F32 half_log_det_prior=-.5f * K*logf(precVec[0]);
	F32 marg_lik=half_log_det_post - half_log_det_prior - yInfo->alpha1_star * logf(alpha2_star);
	curmodel->alpha2Q_star[0]=alpha2_star;
	curmodel->marg_lik=marg_lik;
	return;
}
void MatxVec(BEAST2_BASESEG* SEG,I32 numSeg,F32PTR X,F32PTR Y,F32PTR XtY,I32 N)
{
	I32 Npad=(N+7)/8 * 8; Npad=N;
	for (int i=1; i<=numSeg; i++) {  		
		I32 r1=SEG[i -1].R1;
		I32 r2=SEG[i -1].R2;
		I32 Nseg=r2 - r1+1;
		I32 Kseg=SEG[i - 1].K;
		r_cblas_sgemv(CblasColMajor,CblasTrans,
			Nseg,Kseg,1.f,
			X+r1 - 1,Npad,
			Y+r1 - 1,1L,0.f,
			XtY,1L);	
		X+=Kseg*Npad;
		XtY+=Kseg;
	}
}
I32  GetInfoBandList(BEAST2_BASESEG* info,BEAST2_MODEL_PTR model,I32 Klastcol) {
	I32 numBands=0;
	I32 QUITFLAG=0;
	for (int basisID=0; basisID < model->NUMBASIS; basisID++) {
		BEAST2_BASIS_PTR b=model->b+basisID;
		if (b->type !=OUTLIERID)
		{
			I32 numSeg=b->nKnot+1;
			for (int j=0; j < numSeg; j++) {
				I32 Kbase=b->Kbase;
				if (Klastcol >=(Kbase+b->ks[j])) {
					info->R1=b->KNOT[j - 1];
					info->R2=b->KNOT[j] - 1L;
					info->K=min(Kbase+b->ke[j],Klastcol) - (Kbase+b->ks[j])+1;
					info++;
					numBands++;
				}
				else {
					QUITFLAG=1;	break;
				}
			}
		}
		else
		{
			I32 numSeg=b->nKnot;
			for (int j=0; j < numSeg; j++) {
				I32 Kbase=b->Kbase;
				if (Klastcol >=(Kbase+b->ks[j])) {
					info->R1=b->KNOT[j];
					info->R2=b->KNOT[j] ;
					info->K=min(Kbase+b->ke[j],Klastcol) - (Kbase+b->ks[j])+1;
					info++;
					numBands++;
				}
				else {
					QUITFLAG=1;	break;
				}
			}
		}
		if (QUITFLAG==1) { break; }
	}
	return numBands;
}
I32  GetInfoBandList_post(BEAST2_BASESEG* info,BEAST2_MODEL_PTR model,I32 Kstartcol) {
	I32 numBands=0;
	for (int basisID=0; basisID < model->NUMBASIS; basisID++) {
		BEAST2_BASIS_PTR b=model->b+basisID;
		if (b->type !=OUTLIERID)
		{
			for (int j=0; j < b->nKnot+1; j++) {
				I32  Kbase=b->Kbase;
				if (Kstartcol <=(Kbase+b->ke[j])) {
					info->R1=b->KNOT[j - 1];
					info->R2=b->KNOT[j] - 1L;
					info->K=(Kbase+b->ke[j]) - max(Kbase+b->ks[j],Kstartcol)+1;
					info++;
					numBands++;
				}
			}
		}
		else
		{
			for (int j=0; j < b->nKnot; j++) {
				I32  Kbase=b->Kbase;
				if (Kstartcol <=(Kbase+b->ke[j])) {
					info->R1=b->KNOT[j];
					info->R2=b->KNOT[j];
					info->K=(Kbase+b->ke[j]) - max(Kbase+b->ks[j],Kstartcol)+1;
					info++;
					numBands++;
				}
			}
		}
	}
	return numBands;
}
void MatxMat(BEAST2_BASESEG*infoX,I32 numBandsX,F32PTR X,BEAST2_BASESEG*infoY,I32 numBandsY,F32PTR Y,F32PTR XtY,I32 N,I32 Knew)
{
	I32 Npad=(N+7)/8 * 8; Npad=N;
	I32 Ksegcsum=0;
	for (int i=0; i < numBandsY; i++) {
		I32 new_r1=infoY[i].R1;
		I32 new_r2=infoY[i].R2;
		I32 Kseg=infoY[i].K;
		I32 Kbandcsum=0;	
		for (int j=0; j < numBandsX; j++) {
			I32 old_r1=infoX[j].R1;
			I32 old_r2=infoX[j].R2;
			I32 r1=max(new_r1,old_r1);
			I32 r2=min(new_r2,old_r2);
			I32 Kband=infoX[j].K;
			if (r2>=r1) {
				I32 Nseg=r2 - r1+1;				
				r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,\
					Kband,Kseg,Nseg,1.0f,\
					X+r1 - 1L,Npad,
					Y+r1 - 1L,Npad,0.f,\
					XtY+Kbandcsum,Knew); 
			}
			Kbandcsum+=Kband;
			X+=Kband * Npad;
		}
		Ksegcsum+=Kseg;
		Y+=Kseg * Npad;
		XtY+=Kseg *Knew;
		X -=Kbandcsum * Npad;
	}
}
void XtX_ByGroup(BEAST2_BASESEG* SEG,I32 numSeg,F32PTR X,F32PTR XtX,I32 N,I32 Knew) {
	I32 Npad=(N+7)/8 * 8; Npad=N;
	I32 Ksegcolcsum=0;
	for (int i=1; i <=numSeg; i++) {
		I32 Ksegcol=SEG[i- 1].K;
		I32 col_r1=SEG[i - 1].R1;
		I32 col_r2=SEG[i - 1].R2;
		I32 Ksegrowcsum=0;
		for (int j=1; j <=i; j++) {
			I32 Ksegrow=SEG[j - 1].K;
			I32 row_r1=SEG[j - 1].R1;
			I32 row_r2=SEG[j - 1].R2;
			I32 r1=max(col_r1,row_r1);
			I32 r2=max(col_r2,row_r2);
			I32 Nseg=r2 - r1+1;
			if (r2 >=r1) {
				r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,
					Ksegrow,Ksegcol,Nseg,1.0,
					X+Ksegrowcsum * Npad+r1 - 1,Npad,
					X+Ksegcolcsum * Npad+r1 - 1,Npad,0.,
					XtX+Ksegrowcsum,Knew);
			}
			Ksegrowcsum+=Ksegrow;			
		}
		Ksegcolcsum+=Ksegcol;
		XtX+=Ksegcol*Knew;
	}
}
void MoveCOLsWithinMatrix(F32PTR X,I32 N,I32 Kstart,I32 Kend,I32 Knewstart) {
	rI32 j=Knewstart - Kstart;
	if (j < 0||Knewstart > Kend)
		r_cblas_scopy((Kend-Kstart+1)*N,X+(Kstart-1)*N,1,X+(Knewstart-1)*N,1);
	else
	{
		rI32 segStartIdx=Kend+1;
		while (_True_) {
			segStartIdx=segStartIdx - j;
			if (segStartIdx > Kstart) {
				r_cblas_scopy(j * N,X+(segStartIdx-1) * N,1L,X+((segStartIdx+j)- 1) * N,1);
			}
			else {
				j=(segStartIdx+j) - Kstart;
				r_cblas_scopy(j *N,X+(Kstart - 1)*N,1L,X+(Knewstart-1) * N,1);
				break;
			}
		}
	}
}
void MR_EvaluateModel(
	BEAST2_MODELDATA *curmodel,BEAST2_BASIS_PTR b,F32PTR Xt_mars,I32 N,I32 NUMBASIS,
	BEAST2_YINFO_PTR yInfo,BEAST2_HyperPar *hyperPar,F32PTR precVec,VOID_PTR stream )
{
	I32 Npad=(I32)ceil((F32)N/8.0f) * 8; Npad=N;
	I32 K=0;	 
	for (I32 basisID=0; basisID < NUMBASIS; basisID++) {
		BEAST2_BASIS_PTR basis=b+basisID;
		if (basis->type !=OUTLIERID) {
			int         NUM_SEG=basis->nKnot+1;
			TKNOT_PTR   KNOT=basis->KNOT;
			TORDER_PTR  ORDER=basis->ORDER;
			BEAST2_BASESEG seg;
			seg.ORDER1=basis->type==TRENDID ? 0 : 1;
			for (int i=1; i <=NUM_SEG; i++) {
				seg.R1=KNOT[(i - 1) - 1L];
				seg.R2=KNOT[i - 1L] - 1L;
				seg.ORDER2=basis->type==DUMMYID? 0 :  ORDER[i - 1L];
				I32 k=basis->GenTerms(Xt_mars+Npad * K,N,&seg,&(basis->bConst));
				K+=k;
			}
		} 	else	{
			int         numOfSeg=basis->nKnot;
			TKNOT_PTR   knotList=basis->KNOT;
			BEAST2_BASESEG seg;
			seg.ORDER1=seg.ORDER2=0; 
			for (int i=1; i <=numOfSeg; i++) {
				seg.R1=knotList[(i)-1L];
				seg.R2=knotList[(i)-1L];
				I32 k=basis->GenTerms(Xt_mars+Npad * K,N,&seg,&(basis->bConst));
				K+=k;
			}
		}
	}
	curmodel->K=K;
	F32PTR	GlobalMEMBuf=Xt_mars+K * Npad;
	F32PTR	Xt_zeroBackup=GlobalMEMBuf;
	if (yInfo->nMissing > 0) {
		F32 fillvalue=0.f;
		f32_mat_multirows_extract_set_by_scalar(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing,fillvalue);
	}
	F32PTR XtX=curmodel->XtX;
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,K,N,1.f,Xt_mars,Npad,Xt_mars,Npad,0.f,XtX,K);
    I32 q=yInfo->q;
	F32PTR XtY=curmodel->XtY;
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,q,N,1.f,Xt_mars,Npad,yInfo->Y,N,0.f,XtY,K);
	if (yInfo->nMissing > 0) {
		f32_mat_multirows_set_by_submat(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing);
	}
	F32PTR cholXtX=curmodel->cholXtX;
	F32PTR beta_mean=curmodel->beta_mean;	
	chol_addCol_skipleadingzeros_prec_invdiag(XtX,cholXtX,precVec,K,1,K);
	solve_U_as_LU_invdiag_sqrmat_multicols(cholXtX,XtY,beta_mean,K,q);
    r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,K,1.f,beta_mean,K,XtY,K,0.f,curmodel->alpha2Q_star,q);
	r_ippsSub_32f(curmodel->alpha2Q_star,yInfo->YtY_plus_alpha2Q,curmodel->alpha2Q_star,q * q);
	r_LAPACKE_spotrf(LAPACK_COL_MAJOR,'U',q,curmodel->alpha2Q_star,q); 
	F32 log_det_alphaQ=sum_log_diagv2(curmodel->alpha2Q_star,q);
	F32 half_log_det_post=sum_log_diagv2(cholXtX,K);
	F32 half_log_det_prior=-.5f * K*logf(precVec[0]);
	F32 marg_lik=q*(half_log_det_post - half_log_det_prior) - yInfo->alpha1_star * log_det_alphaQ*2.0f;
    curmodel->marg_lik=marg_lik;
 	return;
}
#include "abc_000_warning.h"
