#include <string.h>
#include <math.h>
#include "abc_000_warning.h"
#include "abc_mcmc.h"  
#include "beastv2_header.h"
#include "abc_vec.h"   
static INLINE void  __CalcAbsDeviation(F32PTR  deviation,F32PTR avgDeviation,PROP_DATA_PTR info,I32 NumBasis) {
	F32 invsample=1.f/info->samples[0];
	I32 N=info->N;
	I32 q=info->yInfo->q; 
	F32PTR  Y=info->yInfo->Y;
	I32PTR  rowsMissing=info->yInfo->rowsMissing;
	I32     nMissing=info->yInfo->nMissing;		
	F32     nan=getNaN();
	if (NumBasis==1) {		
		F32PTR  Ypred1=info->keyresult[0].x;	
		for (int col=0; col < q; col++) {
				I32     idxMissing=0;
				F32     sumError=0;
				for (int i=0; i < N; i++) {
					if (idxMissing < nMissing && i==rowsMissing[idxMissing]) {  
						deviation[i]=nan;
						idxMissing++;
					}	else {
						F32 error=fabsf(Y[i] - Ypred1[i] * invsample);
						deviation[i]=error;
						sumError+=error;
					}
				}
				avgDeviation[col]=sumError/info->yInfo->n;
				Y+=N;  
				Ypred1+=N;
		}
;
	} 
	else if (NumBasis==2) {
		F32PTR  Ypred1=info->keyresult[0].x;
		F32PTR  Ypred2=info->keyresult[1].x;
		for (int col=0; col < q; col++) {
			I32     idxMissing=0;
			F32     sumError=0;
			for (int i=0; i < N; i++) {
				if (idxMissing < nMissing && (i)==rowsMissing[idxMissing])  
					deviation[i]=nan,idxMissing++;
				else {
					F32 error=fabsf(Y[i] - (Ypred1[i]+Ypred2[i]) * invsample);
					deviation[i]=error;
					sumError+=error;
				}
			}
			avgDeviation[col]=sumError/info->yInfo->n;
			Y+=N;  
			Ypred1+=N;
			Ypred2+=N;
		}
	} 
	else  {
		F32PTR  Ypred1=info->keyresult[0].x;
		F32PTR  Ypred2=info->keyresult[1].x;
		F32PTR  Ypred3=info->keyresult[2].x;
		for (int col=0; col < q; col++) {
			I32     idxMissing=0;
			F32     sumError=0;
			for (int i=0; i < N; i++) {
				if (idxMissing<nMissing && (i)==rowsMissing[idxMissing])  
					deviation[i]=nan,idxMissing++;
				else {
					F32 error=fabsf(  Y[i] - (Ypred1[i]+Ypred2[i]+Ypred2[i]) * invsample );
					deviation[i]=error;
					sumError+=error;
				}
			}	
			avgDeviation[col]=sumError/info->yInfo->n;
			Y+=N;  
			Ypred1+=N;
			Ypred2+=N;
			Ypred3+=N;
		}
	}
}
static INLINE void __CalcExtremKnotPos_ST_BirthOnly(I08PTR extremePosVec,F32PTR deviation,I32 N,F32 threshold) {
	int i=0;
	for (; i < N - 3; i+=4) {
		extremePosVec[i]=deviation[i]   > threshold;
		extremePosVec[i+1]=deviation[i+1] > threshold;
		extremePosVec[i+2]=deviation[i+2] > threshold;
		extremePosVec[i+3]=deviation[i+3] > threshold;
	}	
	for (; i < N ;++i) 
		extremePosVec[i]=deviation[i] > threshold;
}
static INLINE void _CalcDevExtremPos(PROP_DATA_PTR info ) {
	BEAST2_MODEL_PTR model=info->model;
	I32              NumBasis=model->NUMBASIS;
	__CalcAbsDeviation( model->deviation,model->avgDeviation,info,NumBasis);
	__CalcExtremKnotPos_ST_BirthOnly(model->extremePosVec,model->deviation,info->N,(model->avgDeviation[0]* info->sigFactor));
}
static void DSVT_Propose( BEAST2_BASIS_PTR basis,NEWTERM_PTR new,PROP_DATA_PTR info)
{	
	I32					goodNum=basis->goodNum;
	I16					nKnot=basis->nKnot;
	BEAST2_RANDSEEDPTR	PRND=info->pRND;
	MOVETYPE flag;
	{
		I32  Ktotal=info->model->curr.K;
		I32  MINKNOTNUM=basis->prior.minKnotNum;
		I32  MAXKNOTNUM=basis->prior.maxKnotNum;
		I32  MAX_K_StopAddingNewTerms=basis->mcmc_Kstopping;
		U08  rnd=*(PRND->rnd08)++;
		if (MINKNOTNUM !=MAXKNOTNUM) {
				if (rnd < basis->propprob.birth) { 
					flag=BIRTH;
					if (nKnot >=MAXKNOTNUM||goodNum==0)	flag=MOVE;
					if (Ktotal  >=MAX_K_StopAddingNewTerms  )  flag=(nKnot==0) ? BIRTH : MOVE;
				}
				else if (rnd < basis->propprob.move)   
					flag=nKnot==0        ? BIRTH : MOVE;
				else if (rnd < basis->propprob.death)  
					flag=nKnot==MINKNOTNUM ? BIRTH : DEATH;
				else if (rnd <=basis->propprob.merge) { 
					if (nKnot==MINKNOTNUM)
						flag=BIRTH;
					else {
						if (nKnot >=2) flag=MERGE;
						else  			  flag=nKnot==0 ? BIRTH : DEATH;
					}				
				}
				else {                                 
					if (Ktotal < MAX_K_StopAddingNewTerms)	flag=ChORDER;
					else                                    flag=(nKnot==MINKNOTNUM) ? BIRTH : MOVE;
				}
		}
		else
		{ 	
			if (MAXKNOTNUM==0) { 
				flag=ChORDER; 
			} else {
				if (basis->propprob.merge==255) 				
					flag=MOVE;				
				else
					flag=rnd > 128 ? MOVE : ChORDER;
			}
		}
	} 
	TKNOT_PTR  knotList=basis->KNOT;
	TORDER_PTR orderList=basis->ORDER; 
	I32 newIdx,endIdx;
	switch (flag)
	{
	case BIRTH:
	{
		I32    Npad16=info->Npad16;		
		U64PTR goodVec=basis->goodvec; 
		U64PTR tmpGoodVec;
		I32    tmpGoodNum;
		if ( *(PRND->rnd08)++< 255* PROB_SAMPLE_EXTREME_VECTOR ) {				             
			I32 samples=info->samples[0];
			if ( samples >=info->nSample_ExtremVecNeedUpdate) {				
				_CalcDevExtremPos(info);  
				info->nSample_ExtremVecNeedUpdate=samples+100;
			}
			U64PTR extremeVec=info->model->extremePosVec;
			tmpGoodVec=info->mem;   
			for (I32 i=0; i < (Npad16/8)-1; i+=2) {
				tmpGoodVec[i]=extremeVec[i]   & goodVec[i];
				tmpGoodVec[i+1]=extremeVec[i+1] & goodVec[i+1];
			}
			tmpGoodNum=i08_sum_binvec(tmpGoodVec,Npad16);
			if (tmpGoodNum==0) { tmpGoodVec=goodVec,tmpGoodNum=goodNum; }
		}	else {
			tmpGoodVec=basis->goodvec;
			tmpGoodNum=goodNum;
		}
		I32  randLoc=RANDINT(1,(I32)tmpGoodNum,*(PRND->rnd32)++);
		new->newKnot=i08_find_nth_onebyte_binvec(tmpGoodVec,(I32)Npad16,randLoc);
		newIdx=1;	for (TKNOT_PTR tmp=knotList; *tmp++< new->newKnot; newIdx++);
		new->numSeg=2;
		new->SEG[0].R1=knotList[(newIdx - 1) - 1];
		new->SEG[0].R2=new->newKnot - 1;
		new->SEG[1].R1=new->newKnot;
		new->SEG[1].R2=knotList[(newIdx)-1] - 1;
		new->SEG[1].ORDER2=new->SEG[0].ORDER2=orderList[(newIdx)-1];
		endIdx=newIdx;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot+1;		
		break;
	}
	case DEATH:
	{
		newIdx=RANDINT(1,(U16)nKnot,*(PRND->rnd16)++);       
		new->numSeg=1;
		new->SEG[0].R1=knotList[(newIdx - 1) - 1];
		new->SEG[0].R2=knotList[(newIdx+1) - 1] - 1L;
		new->SEG[0].ORDER2=orderList[(newIdx+1L) - 1]; 
		endIdx=newIdx+1L;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot - 1;
		break;
	}
	case MERGE:
	{
		newIdx=RANDINT(1,(U16)nKnot - 1,*(PRND->rnd16)++);  
		I32  r1=knotList[(newIdx)-1];
		I32  r2=knotList[(newIdx+1) - 1];
		I32  count=(r2 - r1)+1L - 2L;
		if (count==0L) {
			new->newKnot=*(*(I08**)&(PRND->rnd08))++> 0 ? r1 : r2;
		}
		else {
			new->newKnot=RANDINT(r1+1,r2 - 1,*(PRND->rnd32)++);  
		}
		new->numSeg=2;
		new->SEG[0].R1=knotList[newIdx - 1L - 1L];
		new->SEG[0].R2=new->newKnot - 1L;
		new->SEG[1].R1=new->newKnot;
		new->SEG[1].R2=knotList[newIdx+2L - 1L] - 1L;
		new->SEG[0].ORDER2=orderList[(newIdx)-1L];
		new->SEG[1].ORDER2=orderList[newIdx+2L - 1L];
		endIdx=newIdx+2L;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot - 1;	 		
		break;
	}
	case MOVE: 
	{
		newIdx=RANDINT(1,(U16)nKnot,*(PRND->rnd16)++);  
		I32 oldKnot=knotList[newIdx - 1];
		I32 r1=knotList[(newIdx - 1) - 1];
		I32 r2=knotList[(newIdx+1) - 1];
		I32 minSepDist=basis->prior.minSepDist;
		I32 MCMC_maxMoveStepSize=basis->mcmc_MoveStep;
		r1=max(r1+minSepDist+1,oldKnot - MCMC_maxMoveStepSize);
		r2=min(r2 - minSepDist - 1,oldKnot+MCMC_maxMoveStepSize);
		if (r2==r1) {
			new->newKnot=oldKnot;
		} else if (r2 > r1) {
			RANDINT_SKIPONE(new->newKnot,r1,oldKnot,r2,*(PRND->rnd32)++);
		} else {
			r_error("ERROR: r1 < r2; there must be something wrong!\n");
			return ;
		}
		new->numSeg=2;
		new->SEG[0].R1=knotList[newIdx - 1L - 1L];
		new->SEG[0].R2=new->newKnot - 1L;
		new->SEG[1].R1=new->newKnot;
		new->SEG[1].R2=knotList[newIdx+1L - 1L] - 1L;
		new->SEG[0].ORDER2=orderList[newIdx - 1L];
		new->SEG[1].ORDER2=orderList[newIdx+1L - 1L];
		endIdx=newIdx+1L;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot;
		break;
	}
	case ChORDER:
	{
		newIdx=RANDINT(1,(U16)nKnot+1,*(PRND->rnd16)++);  
		I32 newOrder;
		I32 oldOrder=orderList[newIdx - 1];
		{
			I32 minOrder=basis->prior.minOrder;
			I32 maxOrder=basis->prior.maxOrder;
			if (oldOrder==minOrder)		newOrder=oldOrder+1;
			else if (oldOrder==maxOrder)	newOrder=oldOrder - 1;
			else           			        newOrder=*(*(I08**)&(PRND->rnd08))++> 0 ? oldOrder - 1 : oldOrder+1;
		}
		new->newOrder=newOrder;
		new->oldOrder=oldOrder;
		new->SEG[0].R1=knotList[(newIdx - 1) - 1];
		new->SEG[0].R2=knotList[(newIdx)-1] - 1L;
		new->SEG[0].ORDER2=newOrder;
		new->SEG[0].ORDER1=newOrder; 
		new->numSeg=newOrder > oldOrder ? 1 : 0;
		endIdx=newIdx;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot;
		break;
	}
	}
	if (flag !=ChORDER) {
		TORDER startOrder=(basis->type==TRENDID) ? 0 : 1;
		new->SEG[1].ORDER1=new->SEG[0].ORDER1=startOrder;
	}
	I16PTR  KS_old=basis->ks;
	I16PTR  KE_old=basis->ke;
	if (flag !=ChORDER) {
		new->k1_old=KS_old[(newIdx)-1];
		new->k2_old=KE_old[(endIdx)-1];
	}
	else { 
		if (new->newOrder <=new->oldOrder) { 
			new->k2_old=KE_old[newIdx - 1];
			new->k1_old=basis->type==SEASONID ? (new->k2_old - 1) : new->k2_old;
		}
		else {
			new->k2_old=KE_old[newIdx - 1];    
			new->k1_old=new->k2_old+1;       
		} 
	} 
	new->jumpType=flag;
}
static int __OO_NewKnot_BirthMove(BEAST2_BASIS_PTR basis,PROP_DATA_PTR info) {
	I32 N=info->N;
	I32 Npad16=info->Npad16;
	BEAST2_MODEL_PTR model=info->model;
	I08PTR goodvec=(I08PTR) basis->goodvec; memset(goodvec,1,N);
	for (int J=0; J < model->NUMBASIS; J++) {
		TKNOT_PTR KNOT=model->b[J].KNOT;
		I32       nKnot=model->b[J].nKnot;
		if (model->b[J].type==OUTLIERID) {
			for (int i=0; i < nKnot;++i) goodvec[KNOT[i] - 1]=0;		
		} 
		else {
			for (int i=0; i < nKnot; i++) {
				I32 idx=KNOT[i] - 1;
				goodvec[idx]=0;
				goodvec[max(idx - 1,0)]=0;
				goodvec[min(idx+1,N - 1)]=0;
			}
		}		
	} 
	F32PTR deviation=model->deviation;
	F32 maxValue=0;
	I32 maxIdx=-1;
	for (I32 i=0; i < N; i++) {
		F32 value=deviation[i];
		if (goodvec[i]==0||value !=value) {
			continue;
		}
		value=fabsf(value);
		if (maxValue < value) {
			maxValue=value;
			maxIdx=i;
		}
	}
	if (maxIdx < 0) {
		r_printf("maxIdx=-1: there must be something wrong!");
	}
	return maxIdx+1;
}
static int __OO_NewIdx_MoveDeath(BEAST2_BASIS_PTR basis,PROP_DATA_PTR info) {
	I32 N=info->N;
	I32 Npad16=info->Npad16;
	BEAST2_MODEL_PTR model=info->model;
	F32PTR deviation=model->deviation;
	F32    minValue=1e30;
	I32    minIdx=-1;
	I32       nKnot=basis->nKnot;
	TKNOT_PTR KNOT=basis->KNOT;
	for (int i=0; i < nKnot; i++) {
		I32 idx=KNOT[i] - 1;
		F32 value=fabsf(deviation[idx]);
		if (minValue > value) {
			minValue=value;
			minIdx=i;
		}
	}
	if (minIdx < 0) {
		r_printf("minIdx=-1: there must be something wrong!");
	}
	return minIdx+1;
}
static void OO_Propose_0(	BEAST2_BASIS_PTR basis,NEWTERM_PTR new,PROP_DATA_PTR info)
{	
	I32  goodNum=basis->goodNum; 
	I16  nKnot=basis->nKnot;
	BEAST2_RANDSEEDPTR PRND=info->pRND;
	MOVETYPE flag;
	{
		I32  Ktotal=info->model->curr.K;
		I32  MAXKNOTNUM=basis->prior.maxKnotNum;
		I32  MAX_K_StopAddingNewTerms=basis->mcmc_Kstopping;
		U08  rnd=*(PRND->rnd08)++;
		if (rnd < basis->propprob.birth) { 
			flag=BIRTH;
			if (nKnot >=MAXKNOTNUM )	           flag=MOVE;			
			if (Ktotal > MAX_K_StopAddingNewTerms) flag=(nKnot==0) ? BIRTH : MOVE;			
		} else if (rnd < basis->propprob.move)   
			flag=nKnot==0 ? BIRTH : MOVE;		
		else 
			flag=nKnot==0 ? BIRTH : DEATH;
	}  
	I32              samples=info->samples[0];	
	if (samples > 0) { 
		_CalcDevExtremPos(info);
		info->nSample_ExtremVecNeedUpdate=samples+40L;
	}
	TKNOT_PTR  knotList=basis->KNOT;
	I32 newIdx;      
	switch (flag)
	{
	case BIRTH:
	{
		new->newKnot=__OO_NewKnot_BirthMove(basis,info);;
		new->numSeg=1;
		new->SEG[0].R1=new->newKnot;
		new->SEG[0].R2=new->newKnot;
		new->SEG[0].outlierKnot=new->newKnot; 
		new->newIdx=-9999;            
		new->nKnot_new=nKnot+1;
		break;
	}
	case DEATH:
	{
		newIdx=__OO_NewIdx_MoveDeath(basis,info);
		new->newKnot=knotList[newIdx - 1];
		new->numSeg=0;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot - 1;
		break;
	}
	case MOVE: 
	{
		newIdx=__OO_NewIdx_MoveDeath(basis,info);
		new->newKnot=__OO_NewKnot_BirthMove(basis,info);;
		new->numSeg=1;
		new->SEG[0].R1=new->newKnot;
		new->SEG[0].R2=new->newKnot;
		new->SEG[0].outlierKnot=new->newKnot; 
		new->newIdx=newIdx;
		new->nKnot_new=nKnot;
		break;
	}
	}
	I16PTR  KS_old=basis->ks;
	I16PTR  KE_old=basis->ke;
	if (flag==BIRTH) {
		I32 nKnot=basis->nKnot;
		new->k2_old=KE_old[nKnot - 1];
		new->k1_old=new->k2_old+1;
	}
	else if (flag==DEATH) {
		new->k2_old=KE_old[newIdx - 1];
		new->k1_old=KS_old[newIdx - 1];
	}
	else if (flag==MOVE) {
		new->k2_old=KE_old[newIdx - 1];
		new->k1_old=KS_old[newIdx - 1];
	}
	new->jumpType=flag;
}
static void OO_Propose_1(	BEAST2_BASIS_PTR basis,NEWTERM_PTR new,PROP_DATA_PTR info)
{	
	I32  goodNum=basis->goodNum; 
	I16  nKnot=basis->nKnot;
	BEAST2_RANDSEEDPTR PRND=info->pRND;
	MOVETYPE flag;
	{
		I32  Ktotal=info->model->curr.K;
		I32  MAXKNOTNUM=basis->prior.maxKnotNum;
		I32  MAX_K_StopAddingNewTerms=basis->mcmc_Kstopping;
		U08  rnd=*(PRND->rnd08)++;
		if (rnd < basis->propprob.birth) { 
			flag=BIRTH;
			if (nKnot >=MAXKNOTNUM )	           flag=MOVE;			
			if (Ktotal > MAX_K_StopAddingNewTerms) flag=(nKnot==0) ? BIRTH : MOVE;			
		} else if (rnd < basis->propprob.move)   
			flag=nKnot==0 ? BIRTH : MOVE;		
		else 
			flag=nKnot==0 ? BIRTH : DEATH;
	}  
	I32              samples=info->samples[0];	
	if (samples > 0) { 
		_CalcDevExtremPos(info);
		info->nSample_ExtremVecNeedUpdate=samples+40L;
	}
	TKNOT_PTR  knotList=basis->KNOT;
	I16 newIdx;      
	switch (flag)
	{
	case BIRTH:
	{
		newIdx=-9999;
		new->newKnot=__OO_NewKnot_BirthMove(basis,info);
		new->numSeg=1;
		new->SEG[0].R1=1;       
		new->SEG[0].R2=info->N; 
		new->SEG[0].outlierKnot=new->newKnot; 
		new->newIdx=newIdx;
		new->nKnot_new=nKnot+1;
		break;
	}
	case DEATH:
	{
		newIdx=__OO_NewIdx_MoveDeath(basis,info);		
		new->newKnot=knotList[newIdx - 1];
		new->numSeg=0;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot - 1;
		break;
	}
	case MOVE: 
	{
		newIdx=__OO_NewIdx_MoveDeath(basis,info);
		new->newKnot=__OO_NewKnot_BirthMove(basis,info);;
		new->numSeg=1;
		new->SEG[0].R1=1;       
		new->SEG[0].R2=info->N; 
		new->SEG[0].outlierKnot=new->newKnot; 
		new->newIdx=newIdx;
		new->nKnot_new=nKnot;
		break;
	}
	}
	I16PTR  KS_old=basis->ks;
	I16PTR  KE_old=basis->ke;
	if (flag==BIRTH) {
		I32 nKnot=basis->nKnot;
		new->k2_old=KE_old[nKnot - 1];
		new->k1_old=new->k2_old+1;
	}
	else if (flag==DEATH) {
		new->k2_old=KE_old[newIdx - 1];
		new->k1_old=KS_old[newIdx - 1];
	}
	else if (flag==MOVE) {
		new->k2_old=KE_old[newIdx - 1];
		new->k1_old=KS_old[newIdx - 1];
	}
	new->jumpType=flag;
}
void * Get_Propose(I08 id,BEAST2_OPTIONS_PTR opt) {
	switch (id) {
	case SVDID:
	case DUMMYID:
	case SEASONID: 
	case TRENDID: 
		return DSVT_Propose;
	case OUTLIERID: {
		if      (opt->prior.outlierBasisFuncType==0) 			return OO_Propose_0;
		else if (opt->prior.outlierBasisFuncType==1)			return OO_Propose_1;
	}
	}
	return NULL;
}
#include "abc_000_warning.h"