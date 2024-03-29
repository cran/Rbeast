#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include <math.h>
#include <string.h>
#include "abc_mcmc.h"
#include "abc_ts_func.h"
#include "abc_mat.h"
#include "abc_math.h"    
#include "abc_rand.h"
#include "abc_mem.h"
#include "abc_blas_lapack_lib.h"
#include "globalvars.h"
#include "beastv2_header.h"
#include "beastv2_prior_precfunc.h" 
#define MODEL (*model)
void GetNumTermsPerPrecGrp_prec01(BEAST2_MODEL_PTR model) {
	return;
}
void GetNumTermsPerPrecGrp_prec2(BEAST2_MODEL_PTR model) {
	for (int i=0; i < model->NUMBASIS; i++) {
		model->curr.nTermsPerPrecGrp[i]=model->b[i].K;
	}
	return;
}
void GetNumTermsPerPrecGrp_prec3(BEAST2_MODEL_PTR  model) { 
	memset(MODEL.curr.nTermsPerPrecGrp,0,MODEL.nPrec *sizeof(I16));
	for (int id=0; id < MODEL.NUMBASIS;++id) {
		I16PTR   nTermsPerGrp=MODEL.curr.nTermsPerPrecGrp+MODEL.b[id].offsetPrec;
		U08PTR   termType=MODEL.b[id].termType;
		for (int k=0; k < MODEL.b[id].K;++k) {	 
			nTermsPerGrp[ termType[k]-1 ]++;
		}
	}
}
void GetXtXPrecDiag_prec01(BEAST2_MODEL_PTR model) {return;}
void GetXtXPrecDiag_prec2(BEAST2_MODEL_PTR model) {
	F32PTR precXtXDiag=MODEL.curr.precXtXDiag;
	for (int id=0; id < MODEL.NUMBASIS;++id) {
		F32		  prec=MODEL.precVec[id];  
		for (int k=0; k < MODEL.b[id].K;++k) {
			*precXtXDiag++=prec;
		}
	}
}
void GetXtXPrecDiag_prec3( BEAST2_MODEL_PTR model) {
	F32PTR precXtXDiag=MODEL.curr.precXtXDiag;
	for (int id=0; id < MODEL.NUMBASIS;++id) {
		U08PTR    termType=MODEL.b[id].termType;
		F32PTR    prec=MODEL.precVec+MODEL.b[id].offsetPrec;
		for (int k=0; k < MODEL.b[id].K;++k) {
			*precXtXDiag++=prec[termType[k] - 1];
		}
	} 
}
void UpdateXtXPrec_nTermsPerGrp_prec01(BEAST2_MODEL_PTR model,BEAST2_BASIS_PTR basis,NEWTERM_PTR new,NEWCOLINFO_PTR newcol){
	return; 
}
void UpdateXtXPrec_nTermsPerGrp_prec2(BEAST2_MODEL_PTR model,BEAST2_BASIS_PTR basis,NEWTERM_PTR new,NEWCOLINFO_PTR newcol) {
	I32 Kold=MODEL.curr.K;
	SCPY(newcol->k1 - 1,MODEL.curr.precXtXDiag,MODEL.prop.precXtXDiag);
	SCPY(Kold- newcol->k2_old,MODEL.curr.precXtXDiag+newcol->k2_old,MODEL.prop.precXtXDiag+newcol->k2_new);
	I32 id=basis - model->b;
	F32 prec=MODEL.precVec[id];
	F32PTR  precXtXDiag=MODEL.prop.precXtXDiag;
	for (int k=newcol->k1 ; k <=newcol->k2_new;++k) {
		precXtXDiag[k - 1]=prec;		
	}
	memcpy(MODEL.prop.nTermsPerPrecGrp,MODEL.curr.nTermsPerPrecGrp,sizeof(I16) * model->NUMBASIS);
	MODEL.prop.nTermsPerPrecGrp[id]=MODEL.curr.nTermsPerPrecGrp[id]+newcol->k2_new - newcol->k2_old;
}
void UpdateXtXPrec_nTermsPerGrp_prec3(BEAST2_MODEL_PTR model,BEAST2_BASIS_PTR basis,NEWTERM_PTR new,NEWCOLINFO_PTR newcol)
{
	I32 Kold=MODEL.curr.K;
	SCPY(newcol->k1 - 1,MODEL.curr.precXtXDiag,MODEL.prop.precXtXDiag);
	SCPY(Kold - newcol->k2_old,MODEL.curr.precXtXDiag+newcol->k2_old,MODEL.prop.precXtXDiag+newcol->k2_new);
	F32PTR  precXtXDiag=MODEL.prop.precXtXDiag+newcol->k1 - 1;
	F32PTR  prec=MODEL.precVec+basis->offsetPrec;
	if (basis->type==SEASONID){
		for (int i=0; i < new->numSeg; i++) {
			for (int order=new->SEG[i].ORDER1; order <=new->SEG[i].ORDER2; order++) {
				*precXtXDiag++=prec[order - 1];
				*precXtXDiag++=prec[order - 1];
			}
		}
	}
	else if (basis->type==TRENDID) {
		for (int i=0; i < new->numSeg; i++) {
			for (int order=new->SEG[i].ORDER1; order <=new->SEG[i].ORDER2; order++) {
				*precXtXDiag++=prec[order];
			}
		}
	}
	else if (basis->type==OUTLIERID) {
		for (int i=0; i < new->numSeg; i++)
			precXtXDiag[i]=prec[0];
	}
	#undef  NEW   
	#define NEW (*new)
	I16PTR nTermsPerPrecGrp=MODEL.prop.nTermsPerPrecGrp+basis->offsetPrec;
	memcpy(MODEL.prop.nTermsPerPrecGrp,MODEL.curr.nTermsPerPrecGrp,sizeof(I16) * MODEL.nPrec); 	
	memset(nTermsPerPrecGrp,0,sizeof(I16) * basis->nPrec);
	if (basis->type==SEASONID||basis->type==TRENDID) {
		I32 k1=newcol->k1     - basis->Kbase;
		I32 k2old=newcol->k2_old - basis->Kbase;
		I32 k2new=newcol->k2_new - basis->Kbase;
		U08PTR termType=basis->termType;
		for (int i=1; i <=k1 - 1; i++) {
			nTermsPerPrecGrp[termType[i - 1] - 1]++;
		}
		if (basis->type==SEASONID) {			
			for (int i=0; i < NEW.numSeg; i++) {
				for (int order=new->SEG[i].ORDER1; order <=new->SEG[i].ORDER2; order++) {
					nTermsPerPrecGrp[order - 1]+=2;
				}
			}
		} else		{
			for (int i=0; i < NEW.numSeg; i++) {
				for (int order=new->SEG[i].ORDER1; order <=new->SEG[i].ORDER2; order++) {
					nTermsPerPrecGrp[order ]+=1;
				}
			}
		}
		I32 Kbasis_old=basis->K;
		for (int i=k2old+1; i <=Kbasis_old; i++) {
			nTermsPerPrecGrp[termType[i - 1] - 1]++;
		}
	}
	else if (basis->type==OUTLIERID) {
		nTermsPerPrecGrp[0]=NEW.nKnot_new;
	}
#undef  NEW
}
void ResamplePrecValues_prec0(BEAST2_MODEL_PTR model,BEAST2_HyperPar*hyperPar,VOID_PTR stream) {
	return;
}
void ResamplePrecValues_prec1(BEAST2_MODEL_PTR model,BEAST2_HyperPar *hyperPar,VOID_PTR stream) {
	I32 K=MODEL.curr.K;
	F32 sumq=DOT(K,MODEL.beta,MODEL.beta);
	r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,(*(VSLStreamStatePtr*)stream),1L,model->precVec,(hyperPar->del_1+K * 0.5f),0,1.f);
	F32 newPrecVal=model->precVec[0]/(hyperPar->del_2+0.5f * sumq/model->sig2[0]);
	model->precVec[0]=newPrecVal > MIN_PREC_VALUE? newPrecVal: MIN_PREC_VALUE;
	model->logPrecVec[0]=logf(model->precVec[0]);
}
void ResamplePrecValues_prec2(BEAST2_MODEL_PTR model,BEAST2_HyperPar *hyperPar,VOID_PTR stream) {
		for (int id=0; id < MODEL.NUMBASIS; id++) {
			BEAST2_BASIS_PTR	basis=&MODEL.b[id];
			F32PTR				beta=MODEL.beta+basis->Kbase;
			I32                 K=basis->K;
			if (K <=0) continue;
			F32		sumq=DOT(K,beta,beta);
			r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,(*(VSLStreamStatePtr*)stream),1,&(MODEL.precVec[id]),(hyperPar->del_1+K * 0.5f),0,1.f);
			F32 newPrecVal=MODEL.precVec[id]/(hyperPar->del_2+0.5f * sumq/MODEL.sig2[0]);
			MODEL.precVec[id]=newPrecVal > MIN_PREC_VALUE ? newPrecVal : MIN_PREC_VALUE;
			MODEL.logPrecVec[id]=logf(MODEL.precVec[id]);	
		}		
}
void ResamplePrecValues_prec3(BEAST2_MODEL_PTR model,BEAST2_HyperPar *hyperPar,VOID_PTR stream) {
	for (int id=0; id < MODEL.NUMBASIS; id++) {
		BEAST2_BASIS_PTR basis=&MODEL.b[id];
		F32PTR prec=MODEL.precVec+basis->offsetPrec;;
		F32PTR logPrec=MODEL.logPrecVec+basis->offsetPrec;;
		U08PTR termType=basis->termType;
		F32PTR beta=MODEL.beta+basis->Kbase;
		for (int i=1; i <=basis->nPrec; i++) {
			F32 sumq=0;
			I32 K=0;
			for (int j=0; j < basis->K; j++) {
				if (termType[j]==i) {
					sumq+=beta[j]*beta[j]; K++;
				}
			}
			if (K > 0) {
				r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,(*(VSLStreamStatePtr*)stream),1,&prec[i - 1],(hyperPar->del_1+K * 0.5f),0,1.f);
				F32 newPrecVal=prec[i - 1]/(hyperPar->del_2+0.5f * sumq/MODEL.sig2[0]);
				prec[i - 1]=newPrecVal > MIN_PREC_VALUE ? newPrecVal : MIN_PREC_VALUE;
				logPrec[i - 1]=logf(prec[i - 1]);
			}
		}
	}
}
void IncreasePrecValues_prec0(BEAST2_MODEL_PTR model) {
	return;
}
void IncreasePrecValues_prec1(BEAST2_MODEL_PTR model) {
	model->precVec[0]=model->precVec[0]+model->precVec[0];
	model->logPrecVec[0]=logf(model->precVec[0]);
}
void IncreasePrecValues_prec2(BEAST2_MODEL_PTR model) {
	for (int id=0; id < MODEL.NUMBASIS;++id) {
		MODEL.precVec[id]=MODEL.precVec[id]+MODEL.precVec[id];
		MODEL.logPrecVec[id]=logf(MODEL.precVec[id]);
	}
}
void IncreasePrecValues_prec3(BEAST2_MODEL_PTR model) {
	for (int id=0; id < MODEL.NUMBASIS;++id) {
		BEAST2_BASIS_PTR basis=&MODEL.b[id];
		F32PTR prec=MODEL.precVec+basis->offsetPrec;;
		F32PTR logPrec=MODEL.logPrecVec+basis->offsetPrec;;
		U08PTR termType=basis->termType;
		for (int i=1; i <=basis->nPrec; i++) {
			I32 K=0;
			for (int j=0; j < basis->K; j++) {
				if (termType[j]==i) { K++; }
			}
			if (K > 0) {								
				prec[i - 1]=prec[i - 1]+prec[i - 1];
				logPrec[i - 1]=logf(prec[i - 1]);
			}
		}
	}
}
void ComputeMargLik_prec01(BEAST2_MODELDATA_PTR data,BEAST2_MODEL_PTR model,BEAST2_YINFO_PTR yInfo, 
								 BEAST2_HyperPar_PTR hyperPar)
{
	 I32 K=data->K;
	 solve_U_as_LU_invdiag_sqrmat(data->cholXtX,data->XtY,data->beta_mean,K);
	F32 alpha2_star=(yInfo->YtY_plus_alpha2Q[0] - DOT(K,data->XtY,data->beta_mean))*0.5;
	F32 half_log_det_post=sum_log_diagv2(data->cholXtX,K);
	F32 half_log_det_prior=-0.5f*model->logPrecVec[0] * K;
	F32 marg_lik=half_log_det_post  -half_log_det_prior	- yInfo->alpha1_star * fastlog(alpha2_star);
	data->alpha2Q_star[0]=alpha2_star;
	data->marg_lik=marg_lik;
}
void ComputeMargLik_prec23(BEAST2_MODELDATA_PTR data,BEAST2_MODEL_PTR model,BEAST2_YINFO_PTR yInfo, 
	BEAST2_HyperPar_PTR hyperPar) {
	I32 K=data->K;
	solve_U_as_LU_invdiag_sqrmat(data->cholXtX,data->XtY,data->beta_mean,K);
	F32 alpha2_star=(yInfo->YtY_plus_alpha2Q[0] - DOT(K,data->XtY,data->beta_mean)) * 0.5;
	F32 half_log_det_post=sum_log_diagv2(data->cholXtX,K);
	F32  half_log_det_prior=0;
	for (I32 i=0; i < model->nPrec; i++)
		half_log_det_prior+=model->logPrecVec[i] * data->nTermsPerPrecGrp[i];
	half_log_det_prior *=-0.5;
	F32 marg_lik=half_log_det_post - half_log_det_prior -yInfo->alpha1_star * fastlog(alpha2_star);
	data->alpha2Q_star[0]=alpha2_star;
	data->marg_lik=marg_lik;
}
void MR_ComputeMargLik_prec01(BEAST2_MODELDATA_PTR data,BEAST2_MODEL_PTR model,BEAST2_YINFO_PTR yInfo,
								BEAST2_HyperPar_PTR hyperPard)
{
	I32 q=yInfo->q;
	I32 K=data->K;
	solve_U_as_LU_invdiag_sqrmat_multicols(data->cholXtX,data->XtY,data->beta_mean,K,q);
	F32PTR beta_mean=data->beta_mean;
	F32PTR XtY=data->XtY;
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,K,1.f,beta_mean,K,XtY,K,0.f,data->alpha2Q_star,q);
	r_ippsSub_32f(data->alpha2Q_star,yInfo->YtY_plus_alpha2Q,data->alpha2Q_star,q * q);
	F32 half_log_det_post=sum_log_diagv2(data->cholXtX,K);
	F32 half_log_det_prior=-0.5f * K * model->logPrecVec[0];
	r_LAPACKE_spotrf(LAPACK_COL_MAJOR,'U',q,data->alpha2Q_star,q); 
	F32 sumLn_alphaQ_det=sum_log_diagv2(data->alpha2Q_star,q);
	data->marg_lik=q*(half_log_det_post - half_log_det_prior) - yInfo->alpha1_star * sumLn_alphaQ_det*2.f; 	 
}
void SetUpPrecFunctions(I08 precPriorType,I32 q,PREC_FUNCS * funcs) {
	if (q==1) {
			if (  precPriorType==ConstPrec||precPriorType==UniformPrec) {
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec01;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec01;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec01;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_prec_invdiag;
			}
			else if (precPriorType==ComponentWise)
			{
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec2;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec2;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec2;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_precVec_invdiag;
			}
			else if (precPriorType==OrderWise )
			{
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec3;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec3;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec3;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_precVec_invdiag;
			}
			if      (precPriorType==0)	funcs->IncreasePrecValues=IncreasePrecValues_prec0;
			else if (precPriorType==1) 	funcs->IncreasePrecValues=IncreasePrecValues_prec1;
			else if (precPriorType==2)	funcs->IncreasePrecValues=IncreasePrecValues_prec2;
			else if (precPriorType==3)	funcs->IncreasePrecValues=IncreasePrecValues_prec3;
			if (precPriorType==0)			funcs->ResamplePrecValues=ResamplePrecValues_prec0;
			else if (precPriorType==1) 	funcs->ResamplePrecValues=ResamplePrecValues_prec1;
			else if (precPriorType==2)	funcs->ResamplePrecValues=ResamplePrecValues_prec2;
			else if (precPriorType==3)	funcs->ResamplePrecValues=ResamplePrecValues_prec3;
			if (precPriorType==0)	        funcs->ComputeMargLik=ComputeMargLik_prec01;
			else if (precPriorType==1) 	funcs->ComputeMargLik=ComputeMargLik_prec01;
			else if (precPriorType==2)	funcs->ComputeMargLik=ComputeMargLik_prec23;
			else if (precPriorType==3)	funcs->ComputeMargLik=ComputeMargLik_prec23;
	}
	if (q> 1) {
			if (  precPriorType==ConstPrec||precPriorType==UniformPrec) {
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec01;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec01;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec01;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_prec_invdiag;
			}
			else if (precPriorType==ComponentWise)
			{
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec2;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec2;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec2;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_precVec_invdiag;
			}
			else if (precPriorType==OrderWise )
			{
				funcs->GetXtXPrecDiag=GetXtXPrecDiag_prec3;
				funcs->GetNumTermsPerPrecGrp=GetNumTermsPerPrecGrp_prec3;
				funcs->UpdateXtXPrec_nTermsPerGrp=UpdateXtXPrec_nTermsPerGrp_prec3;
				funcs->chol_addCol=chol_addCol_skipleadingzeros_precVec_invdiag;
			}
			if      (precPriorType==0)	funcs->IncreasePrecValues=IncreasePrecValues_prec0;
			else if (precPriorType==1) 	funcs->IncreasePrecValues=IncreasePrecValues_prec1;
			else if (precPriorType==2)	funcs->IncreasePrecValues=IncreasePrecValues_prec2;
			else if (precPriorType==3)	funcs->IncreasePrecValues=IncreasePrecValues_prec3;
			if (precPriorType==0)			funcs->ResamplePrecValues=ResamplePrecValues_prec0;
			else if (precPriorType==1) 	funcs->ResamplePrecValues=ResamplePrecValues_prec1;
			else if (precPriorType==2)	funcs->ResamplePrecValues=ResamplePrecValues_prec2;
			else if (precPriorType==3)	funcs->ResamplePrecValues=ResamplePrecValues_prec3;
			if (precPriorType==0)	        funcs->ComputeMargLik=MR_ComputeMargLik_prec01;  
			else if (precPriorType==1) 	funcs->ComputeMargLik=MR_ComputeMargLik_prec01;
			else if (precPriorType==2)	funcs->ComputeMargLik=MR_ComputeMargLik_prec01; 
			else if (precPriorType==3)	funcs->ComputeMargLik=MR_ComputeMargLik_prec01; 
	}
}
#include "abc_000_warning.h"
