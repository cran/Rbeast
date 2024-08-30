#include <string.h>
#include <math.h>
#include "abc_000_warning.h"
#include "abc_mcmc.h"  
#include "abc_ide_util.h"  
#include "beastv2_header.h"
#include "abc_vec.h"   
static  void  __CalcAbsDeviation(F32PTR  deviation,F32PTR avgDeviation,PROP_DATA_PTR info,I32 NumBasis) {
	F32     invsample=1.f/info->samples[0];
	I32     N=info->N;
	I32     q=info->yInfo->q; 
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
				deviation+=N;			
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
			deviation+=N;
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
					F32 error=fabsf(  Y[i] - (Ypred1[i]+Ypred2[i]+Ypred3[i]*0) * invsample );
					deviation[i]=error;
					sumError+=error;
				}
			}	
			avgDeviation[col]=sumError/info->yInfo->n;
			Y+=N;  
			Ypred1+=N;
			Ypred2+=N;
			Ypred3+=N;
			deviation+=N;
		}
	}
	if (q > 1) {
		deviation=deviation - N * q;
		for (int i=0; i < q;++i) {
			f32_mul_val_inplace(1./avgDeviation[i],(deviation+i * N),N); 	
		}
		for (int i=1; i < q;++i) {
			F32PTR relDeviation2=deviation+i * N;
			for (int j=0; j < N;++j) {
				deviation[j]=max(deviation[j],relDeviation2[j]);
			}
		}
	}
}
static INLINE void __CalcExtremKnotPos(I08PTR extremePosVec,F32PTR deviation,I32 N,F32 threshold) {
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
					if (nKnot   >=MAXKNOTNUM||goodNum==0)	flag=MOVE;
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
						else  			flag=nKnot==0 ? BIRTH : DEATH;
					}				
				}
				else {                                 
					if (Ktotal < MAX_K_StopAddingNewTerms)	flag=ChORDER;
					else                                    flag=(nKnot==MINKNOTNUM) ? BIRTH : MOVE;
				}
		}
		else { 	
			if (MINKNOTNUM==0) {
				if (basis->propprob.merge==255)   
				{
					flag=NoChangeFixGlobal; 
				}
				else
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
		I32    Npad16=info ->Npad16;		
		U64PTR goodVec=basis->goodvec; 
		U64PTR tmpGoodVec;
		I32    tmpGoodNum;
		if ( *(PRND->rnd08)++< 255 * PROB_SAMPLE_EXTREME_VECTOR ) {				             
			I32 samples=info->samples[0];
			if ( samples >=info->nSample_DeviationNeedUpdate) {
				BEAST2_MODEL_PTR model=info->model;		
				__CalcAbsDeviation(model->deviation,model->avgDeviation,info,info->numBasisWithoutOutlier);
				I32  extraSamples=min( (10+samples/8),200);
				info->nSample_DeviationNeedUpdate=samples+extraSamples;
				info->shallUpdateExtremVec=1L;
			}
			if (info->shallUpdateExtremVec) {
				BEAST2_MODEL_PTR model=info->model;
				F32 threshold=(info->yInfo->q==1) ? (model->avgDeviation[0] * info->sigFactor) : info->sigFactor;
				__CalcExtremKnotPos(model->extremePosVec,model->deviation,info->N,threshold);
				info->shallUpdateExtremVec=0L;
			}
			U64PTR extremeVec=info->model->extremePosVec;
			tmpGoodVec=info->mem;   
			for (I32 i=0; i < (Npad16/8)-1; i+=2) {
				tmpGoodVec[i]=extremeVec[i]   & goodVec[i];
				tmpGoodVec[i+1]=extremeVec[i+1] & goodVec[i+1];
			}
			tmpGoodNum=i08_sum_binvec(tmpGoodVec,Npad16);
			if (tmpGoodNum==0) { 
				tmpGoodVec=goodVec,tmpGoodNum=goodNum; 
			}
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
		newIdx=RANDINT(1,nKnot,*(PRND->rnd16)++);       
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
		newIdx=RANDINT(1,nKnot - 1,*(PRND->rnd16)++);  
		I32  r1=knotList[(newIdx)-1];
		I32  r2=knotList[(newIdx+1) - 1];
		I32  count=(r2 - r1)+1L - 2L;
		if (count==0L) {
			new->newKnot=*(*(I08**)&(PRND->rnd08))++> 0 ? r1 : r2;
		}	else {
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
		newIdx=RANDINT(1,nKnot,*(PRND->rnd16)++);  
		I32 oldKnot=knotList[newIdx - 1];
		I32 r1=newIdx==1     ?  knotList[INDEX_FakeStart]: knotList[(newIdx - 1) - 1];
		I32 r2=newIdx==nKnot ?  knotList[INDEX_FakeEnd]  : knotList[(newIdx+1) - 1];
		I32 minSepDist=basis->prior.minSepDist;
		I32 MCMC_maxMoveStepSize=basis->mcmc_MoveStep;
		r1=max(r1+minSepDist+1,oldKnot - MCMC_maxMoveStepSize);
		r2=min(r2 - minSepDist - 1,oldKnot+MCMC_maxMoveStepSize);
		if (r2==r1) {
			new->newKnot=oldKnot;
		} else if (r2 > r1) {
			RANDINT_SKIPONE(new->newKnot,r1,oldKnot,r2,*(PRND->rnd32)++);
		} else {
			r_error("ERROR: r1 < r2; this should never happen and there must be something wrong!\n");			
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
		newIdx=RANDINT(1,nKnot+1,*(PRND->rnd16)++);  
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
	case NoChangeFixGlobal: {
		new->numSeg=0;
		new->newKnot=-9999;
		new->SEG[0].R1=0x0fffffff;
		new->SEG[0].R2=0x0fffffff;
		new->SEG[1].R1=0x0fffffff;
		new->SEG[1].R2=0x0fffffff;
		new->SEG[0].ORDER2=0x0fff;
		new->SEG[1].ORDER2=0x0fff;
		endIdx=newIdx=1;
		new->newIdx=0x0fff;
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
		if (flag==NoChangeFixGlobal) {
			new->newcols.k1=basis->K+1;
			new->newcols.k2_old=basis->K;
		}	else {
			new->newcols.k1=KS_old[(newIdx)-1];
			new->newcols.k2_old=KE_old[(endIdx)-1];
		}
	}
	else { 
		if (new->newOrder <=new->oldOrder) { 
			new->newcols.k2_old=KE_old[newIdx - 1];
			new->newcols.k1=basis->type==SEASONID ? (new->newcols.k2_old - 1) :  new->newcols.k2_old;
		}
		else {
			new->newcols.k2_old=KE_old[newIdx - 1];          
			new->newcols.k1=new->newcols.k2_old+1;     
		} 
	} 
	new->jumpType=flag;
}
static int __OO_NewKnot_BirthMove(BEAST2_BASIS_PTR basis,PROP_DATA_PTR info,I32PTR maxIndex) {
	I32 N=info->N;
	I32 Npad16=info->Npad16;
	BEAST2_MODEL_PTR model=info->model;
	BEAST2_RANDSEEDPTR PRND=info->pRND;
	I08PTR goodvec=(I08PTR) basis->goodvec; 
	memset(goodvec,1,N);
	TKNOT_PTR KNOT=basis->KNOT;
	I32       nKnot=basis->nKnot; 
	for (int i=0; i < nKnot;++i) goodvec[KNOT[i] - 1]=0;
	I32PTR IndicesLargeDeviation=info->mem;   
	I32    numLargeDev=0;
	F32PTR deviation=model->deviation;
	F32    threshold=info->yInfo->q==1
							   ? model->avgDeviation[0]* info->outlierSigFactor
							   : info->outlierSigFactor;
	F32 maxValue=0;
	I32 maxIdx=-1;	
	for (I32 i=0; i < N; i++) {
		F32 value=deviation[i];  
		if ( !goodvec[i]||IsNaN (value) ) {
			continue;
		}
		if ( value > maxValue) {						
			maxValue=value;
			maxIdx=i;
		}
		IndicesLargeDeviation[numLargeDev]=i;
		numLargeDev+=(value > threshold);
	}
	int newKnot=-1L;
	if (numLargeDev > 1) {	
		I32 rndIdx=RANDINT(1,(U16)numLargeDev,*(PRND->rnd16)++);
		newKnot=IndicesLargeDeviation[rndIdx - 1];
	} else if (numLargeDev==1) {
		newKnot=IndicesLargeDeviation[0];
	}
	if (maxIdx < 0) {
		r_printf("ERROR: __OO_NewKnot_BirthMove: maxIdx=-1,and there must be something wrong!");
	}
	*maxIndex=maxIdx+1L;
	return newKnot+1L;
}
static int __OO_NewIdx_MoveDeath(BEAST2_BASIS_PTR basis,PROP_DATA_PTR info) {
	F32PTR deviation=info->model->deviation; 
	F32    minValue=1e34;
	I32    minIdx=-1;
	I32       nKnot=basis->nKnot;
	TKNOT_PTR KNOT=basis->KNOT;
	for (int i=0; i < nKnot; i++) {
		F32 value=deviation[KNOT[i] - 1]; 	 
		if (value < minValue ) {
			minValue=value;
			minIdx=i;
		}
	}
	if (minIdx < 0) {
		r_printf("__OO_NewIdx_MoveDeath: maxIdx=-1,and there must be something wrong!");
	}
	return minIdx+1;
}
static void OO_Propose_01(	BEAST2_BASIS_PTR basis,NEWTERM_PTR new,PROP_DATA_PTR info)
{	
	I32  goodNum=basis->goodNum; 
	I16  nKnot=basis->nKnot;
	BEAST2_RANDSEEDPTR PRND=info->pRND;
	MOVETYPE flag;
	{
		I32  Ktotal=info->model->curr.K;
		I32  MINKNOTNUM=basis->prior.minKnotNum;
		I32  MAXKNOTNUM=basis->prior.maxKnotNum;
		I32  MAX_K_StopAddingNewTerms=basis->mcmc_Kstopping;
		if (MINKNOTNUM !=MAXKNOTNUM) {
			U08  rnd=*(PRND->rnd08)++;
			if (rnd < basis->propprob.birth) { 
				flag=BIRTH;
				if (nKnot >=MAXKNOTNUM )	           flag=MOVE;			
				if (Ktotal > MAX_K_StopAddingNewTerms) flag=(nKnot==0) ? BIRTH : MOVE;			
			} else if (rnd < basis->propprob.move)   
				flag=nKnot==0 ? BIRTH : MOVE;		
			else 
				flag=nKnot==0 ? BIRTH : DEATH;
		} else {
			flag=MOVE;
		}
	}  
	I32  samples=info->samples[0];	
	if (samples >=info->nSample_DeviationNeedUpdate) { 
	 	BEAST2_MODEL_PTR model=info->model;
		__CalcAbsDeviation(model->deviation,model->avgDeviation,info,info->numBasisWithoutOutlier);
		I32  extraSamples=min((10+samples/8),200);
		info->nSample_DeviationNeedUpdate=samples+extraSamples;
		info->shallUpdateExtremVec=1L;
	}
	TKNOT_PTR  knotList=basis->KNOT;
	switch (flag)
	{
	case BIRTH:
	{		
		I32 maxIdx;
		new->newKnot=__OO_NewKnot_BirthMove(basis,info,&maxIdx);
		if (new->newKnot==0 && nKnot==0) {
			new->newKnot=maxIdx;
		}
		if (new->newKnot > 0) {
			new->numSeg=1;
			new->SEG[0].R1=new->newKnot;
			new->SEG[0].R2=new->newKnot;
			new->SEG[0].outlierKnot=new->newKnot; 
			new->newIdx=-9999;                       
			new->nKnot_new=nKnot+1;
		}	else {
			flag=DEATH; 			
		}
		break;
	}
	case MOVE: 
	{
		I32 maxIdx;
		new->newKnot=__OO_NewKnot_BirthMove(basis,info,&maxIdx);
		if (new->newKnot > 0) {
			new->numSeg=1;
			new->SEG[0].R1=new->newKnot;
			new->SEG[0].R2=new->newKnot;
			new->SEG[0].outlierKnot=new->newKnot; 
			I32 newIdx=__OO_NewIdx_MoveDeath(basis,info);
			new->newIdx=newIdx;
			new->nKnot_new=nKnot;
		}	else {
			flag=DEATH;
		}
		break;
	}
	}
 	if (flag==DEATH)
	{
		I32 newIdx=__OO_NewIdx_MoveDeath(basis,info);
		new->newKnot=knotList[newIdx - 1];
		new->numSeg=0;
		new->newIdx=newIdx;
		new->nKnot_new=nKnot - 1;
	}
	I16PTR  KS_old=basis->ks;
	I16PTR  KE_old=basis->ke;
	if (flag==BIRTH) {
		I32 nKnot=basis->nKnot;
		new->newcols.k2_old=KE_old[nKnot - 1];
		new->newcols.k1=new->newcols.k2_old+1;
	} else if (flag==DEATH) {
		new->newcols.k2_old=KE_old[new->newIdx - 1];
		new->newcols.k1=KS_old[new->newIdx - 1];
	} else if (flag==MOVE) {
		new->newcols.k2_old=KE_old[new->newIdx - 1];
		new->newcols.k1=KS_old[new->newIdx - 1];
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
		if      (opt->prior.outlierBasisFuncType==0) 			return OO_Propose_01;
		else if (opt->prior.outlierBasisFuncType==1)			return OO_Propose_01;
	}
	}
	return NULL;
}
#include "abc_000_warning.h"
