#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include <math.h>
#include <string.h>
#include "abc_001_config.h"
#include "abc_rand.h"
#include "abc_mat.h"
#include "abc_math.h"  
#include "abc_vec.h"  
#include "beastv2_func.h"
#include "beastv2_prior_precfunc.h"
#include <stdio.h>	   
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
int BEAST2_Basis_To_XmarsXtX_XtY(BEAST2_BASIS_PTR b,I32 NUMBASIS,F32PTR Xt_mars,I32 N,F32PTR XtX,F32PTR XtY,BEAST2_YINFO_PTR  yInfo) {
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
	F32PTR	GlobalMEMBuf=Xt_mars+K * Npad;
	F32PTR	Xt_zeroBackup=GlobalMEMBuf;
	if (yInfo->nMissing > 0) {
		F32 fillvalue=0.f;
		f32_mat_multirows_extract_set_by_scalar(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing,fillvalue);
	}
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,K,N,1.f,Xt_mars,Npad,Xt_mars,Npad,0.f,XtX,K);
	if (yInfo->q==1) {	
		r_cblas_sgemv(CblasColMajor,CblasTrans,Npad,K,1,Xt_mars,Npad,yInfo->Y,1,0,XtY,1);
	}	else {
		r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,yInfo->q,N,1.f,Xt_mars,Npad,yInfo->Y,N,0.f,XtY,K);
	}
	if (yInfo->nMissing > 0) {
		f32_mat_multirows_set_by_submat(Xt_mars,Npad,K,Xt_zeroBackup,yInfo->rowsMissing,yInfo->nMissing);
	}
	return K;
}
void BEAST2_EvaluateModel(	BEAST2_MODELDATA *curmodel,BEAST2_BASIS_PTR b,F32PTR Xt_mars,I32 N,I32 NUMBASIS,
	      BEAST2_YINFO_PTR  yInfo,BEAST2_HyperPar *hyperPar,PRECSTATE_PTR precState,PREC_FUNCS * precFunc )
{
	curmodel->K=BEAST2_Basis_To_XmarsXtX_XtY(b,NUMBASIS,Xt_mars,N,curmodel->XtX,curmodel->XtY,yInfo);
	I32 Npad=N;
	I32 K=curmodel->K;
	F32PTR XtX=curmodel->XtX;
	F32PTR XtY=curmodel->XtY;
	F32PTR cholXtX=curmodel->cholXtX;
	F32PTR beta_mean=curmodel->beta_mean;	
	precFunc->SetPrecXtXDiag(curmodel->precXtXDiag,b,NUMBASIS,precState);    
	precFunc->chol_addCol(XtX,cholXtX,curmodel->precXtXDiag,K,1,K);
	precFunc->SetNtermsPerPrecGrp(curmodel->nTermsPerPrecGrp,b,NUMBASIS,precState);
	precFunc->ComputeMargLik(curmodel,precState,yInfo,hyperPar);
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
		if (b->type !=OUTLIERID) {
			I32 numSeg=b->nKnot+1;
			for (int j=0; j < numSeg; j++) {
				I32 Kbase=b->Kbase;
				if (Klastcol >=(Kbase+b->ks[j])) {
					info->R1=b->KNOT[j - 1];
					info->R2=b->KNOT[j] - 1L;
					info->K=min(Kbase+b->ke[j],Klastcol) - (Kbase+b->ks[j])+1;
					info++;
					numBands++;
				} else {
					QUITFLAG=1;	
					break;
				}
			}
		} 
		else {
			I32 numSeg=b->nKnot;
			for (int j=0; j < numSeg; j++) {
				I32 Kbase=b->Kbase;
				if (Klastcol >=(Kbase+b->ks[j])) {
					info->R1=b->KNOT[j];
					info->R2=b->KNOT[j] ;
					info->K=min(Kbase+b->ke[j],Klastcol) - (Kbase+b->ks[j])+1;
					info++;
					numBands++;
				}	else {
					QUITFLAG=1;	
					break;
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
		if (b->type !=OUTLIERID) {
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
		else {
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
void Update_XtX_from_Xnewterm_ByGroup(F32PTR X,F32PTR Xnewterm,F32PTR XtX,F32PTR XtXnew,NEWTERM* NEW,BEAST2_MODEL_PTR model) {
    const I32 k1=NEW->newcols.k1;
	const I32 k2_old=NEW->newcols.k2_old;
	const I32 k2_new=NEW->newcols.k2_new;
	const I32 N=NEW->newcols.N;
	const I32 Nlda=NEW->newcols.Nlda;
	const I32 Knewterm=NEW->newcols.Knewterm;
	const I32 KOLD=NEW->newcols.KOLD;
	const I32 KNEW=NEW->newcols.KNEW;
	const I32  Npad=N;
	for (I32 i=1; i < k1; i++) SCPY(i,XtX+(i - 1L) * KOLD,XtXnew+(i - 1L) * KNEW);
	if (Knewterm !=0) {
		FILL0(XtXnew+(k1 - 1) * KNEW,(KNEW - k1+1) * KNEW); 
		if (k1 > 1) {
			BEAST2_BASESEG*  OLD_SEG=(BEAST2_BASESEG*)(Xnewterm+Knewterm * Npad);
			I32              OLD_numSeg=GetInfoBandList(OLD_SEG,model,k1 - 1);
			MatxMat( OLD_SEG,OLD_numSeg,X,
				     NEW->SEG,NEW->numSeg,Xnewterm,
				     XtXnew+(k1 - 1L) * KNEW,N,KNEW);
		}
		XtX_ByGroup(NEW->SEG,NEW->numSeg,Xnewterm,XtXnew+(k1 - 1) * KNEW+k1 - 1,N,KNEW);
	}
	if (k2_old !=KOLD) {
		for (I32 kold=k2_old+1,knew=k2_new+1; kold <=KOLD; kold++,knew++) {
			F32PTR ColStart_old=XtX+(kold - 1) * KOLD;
			F32PTR ColStart_new=XtXnew+(knew - 1) * KNEW;
			SCPY(k1 - 1,ColStart_old,ColStart_new                   ); 
			SCPY(kold - k2_old,ColStart_old+(k2_old+1) - 1,ColStart_new+(k2_new+1) - 1); 
		}
		if (Knewterm !=0) {
			BEAST2_BASESEG* OLD_SEG=(BEAST2_BASESEG*)(Xnewterm+Knewterm * Npad);
			I32             OLD_numSeg=GetInfoBandList_post(OLD_SEG,model,k2_old+1);
			MatxMat(NEW->SEG,NEW->numSeg,Xnewterm,
				    OLD_SEG,OLD_numSeg,X+k2_old * Npad,
				    XtXnew+(k2_new+1 - 1) * KNEW+k1 - 1,N,KNEW);
		}
	}
}
void Update_XtY_from_Xnewterm_ByGroup(F32PTR Y,F32PTR Xnewterm,F32PTR XtY,F32PTR XtYnew,NEWTERM* NEW,int q) {
    const I32 k1=NEW->newcols.k1;
	const I32 k2_old=NEW->newcols.k2_old;
	const I32 k2_new=NEW->newcols.k2_new;
	const I32 N=NEW->newcols.N;
	const I32 Nlda=NEW->newcols.Nlda;
	const I32 Knewterm=NEW->newcols.Knewterm;
	const I32 KOLD=NEW->newcols.KOLD;
	const I32 KNEW=NEW->newcols.KNEW;
	const I32  Npad=N;
	if (q==1) {
		if (k1 > 1)          SCPY(k1 - 1,XtY,XtYnew);
		if (Knewterm > 0)  	 MatxVec(NEW->SEG,NEW->numSeg,Xnewterm,Y,XtYnew+k1 - 1,N);
		if (k2_old !=KOLD)  SCPY(KNEW - k2_new,XtY+(k2_old+1L) - 1L,XtYnew+(k2_new+1) - 1);
	} else {
		if (k1 > 1) {
			for (I32 c=0; c < q;++c) {
				SCPY(k1 - 1,XtY+KOLD * c,XtYnew+KNEW * c);
			}
		}
		if (Knewterm > 0) {
			r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,Knewterm,q,N,1.f,Xnewterm,Npad,Y,N,0.f,XtYnew+k1 - 1,KNEW);
		}
		if (k2_old !=KOLD) {
			for (I32 c=0; c < q;++c) {
				SCPY(KNEW -k2_new,XtY+(k2_old+1L) - 1L+KOLD * c,XtYnew+(k2_new+1) - 1+KNEW * c);
			}
		}
	} 
}
void Update_XtX_from_Xnewterm_NoGroup(F32PTR X,F32PTR Xnewterm,F32PTR XtX,F32PTR XtXnew,NEWTERM* NEW,BEAST2_MODEL* MODEL_not_used) {
    const I32 k1=NEW->newcols.k1;
	const I32 k2_old=NEW->newcols.k2_old;
	const I32 k2_new=NEW->newcols.k2_new;
	const I32 N=NEW->newcols.N;
	const I32 Nlda=NEW->newcols.Nlda;
	const I32 Knewterm=NEW->newcols.Knewterm; 
	const I32 KOLD=NEW->newcols.KOLD;
	const I32 KNEW=NEW->newcols.KNEW;
	for (I32 i=1; i < k1; i++) SCPY(i,XtX+(i - 1L) * KOLD,XtXnew+(i - 1L) * KNEW);
	if (Knewterm !=0) {
		FILL0(XtXnew+(k1 - 1) * KNEW,(KNEW - k1+1) * KNEW); 
		if (k1 > 1) {
			r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,k1 - 1,Knewterm,N,1.0f,
				X,Nlda,
				Xnewterm,Nlda,0.f,
				XtXnew+(k1 - 1L) * KNEW,KNEW);
		}
		r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,
			Knewterm,Knewterm,N,1.0,
			Xnewterm,Nlda,
			Xnewterm,Nlda,0.f,
			XtXnew+(k1 - 1) * KNEW+k1 - 1,KNEW);
	}
	if (k2_old !=KOLD) {
		for (I32 kold=k2_old+1,knew=k2_new+1; kold <=KOLD; kold++,knew++) {
			F32PTR ColStart_old=XtX+(kold - 1) * KOLD;
			F32PTR ColStart_new=XtXnew+(knew - 1) * KNEW;
			SCPY(k1 - 1,ColStart_old,ColStart_new); 
			SCPY(kold - k2_old,ColStart_old+(k2_old+1) - 1,ColStart_new+(k2_new+1) - 1); 
		}
		if (Knewterm !=0) {
			r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,
				Knewterm,(KOLD - k2_old),N,1.0,
				Xnewterm,Nlda,
				X+(k2_old+1 - 1) * Nlda,Nlda,0.0,
				XtXnew+(k2_new+1 - 1) * KNEW+k1 - 1,KNEW);
		}
	}
}
void Update_XtY_from_Xnewterm_NoGroup(F32PTR Y,F32PTR Xnewterm,F32PTR XtY,F32PTR XtYnew,NEWTERM* NEW,I32 q) {
    const I32 k1=NEW->newcols.k1;
	const I32 k2_old=NEW->newcols.k2_old;
	const I32 k2_new=NEW->newcols.k2_new;
	const I32 N=NEW->newcols.N;
	const I32 Nlda=NEW->newcols.Nlda;
	const I32 Knewterm=NEW->newcols.Knewterm; 
	const I32 KOLD=NEW->newcols.KOLD;
	const I32 KNEW=NEW->newcols.KNEW;
	if (q==1) {
		if (k1 > 1)       SCPY(k1 - 1,XtY,XtYnew);
		if (Knewterm > 0) { 
				r_cblas_sgemv(CblasColMajor,CblasTrans,N,Knewterm,1.f,
						Xnewterm,Nlda,
						Y,1L,0.f,
					    XtYnew+k1 - 1,1L);
		}
		if (k2_old !=KOLD) SCPY(KNEW - k2_new,XtY+(k2_old+1L) - 1L,XtYnew+(k2_new+1) - 1);
	}
	else {
		if (k1 > 1) {
			for (I32 c=0; c < q;++c) {
				SCPY(k1 - 1,XtY+KOLD * c,XtYnew+KNEW * c);
			}
		}
		if (Knewterm > 0) {
			r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans, 
				Knewterm,q,N,1.f, 
				Xnewterm,Nlda,
				Y,N,0.f,
				XtYnew+k1 -1,KNEW);
		}
		if (k2_old !=KOLD) {
			for (I32 c=0; c < q;++c) {
				SCPY(KNEW - k2_new,XtY+(k2_old+1L) - 1L+KOLD * c,XtYnew+(k2_new+1) - 1+KNEW * c);
			}
		}
	}
}
#include "abc_000_warning.h"
