#include "math.h"
#include "abc_000_warning.h"
#include "abc_mcmc.h"
#include "abc_vec.h"
#include "beastv2_header.h"
static void DSVT(BEAST2_BASIS_PTR basis,I32 N,BEAST2_RNDSTREAM* PRND)
{
	I32 rndOrder=ceil((basis->prior.maxOrder+basis->prior.minOrder)/2.0); 
	I32 maxKnotNum=basis->prior.maxKnotNum;
	I32 minKnotNum=basis->prior.minKnotNum;
	basis->nKnot=minKnotNum;  
	I32 leftMargin=basis->prior.leftMargin;
	I32 rightargin=basis->prior.rightMargin;
	I32 minSepDist=basis->prior.minSepDist;
	I32 Nvalid=(N - rightargin) - (1+leftMargin+1)+1;
	I32 SEP=Nvalid/max(basis->nKnot,1);
	I32 initKnot=1+leftMargin+1;
	for (I32 i=1; i <=basis->nKnot;++i) {
		basis->ORDER[i - 1]=rndOrder;       
		basis->KNOT[i - 1]=initKnot;
		initKnot+=SEP;
	}
	basis->ORDER[basis->nKnot]=rndOrder;
	I32 fakeStart=1L+(leftMargin - minSepDist);
	I32 fakeEnd=(N+1) -  rightargin+minSepDist;
	basis->KNOT[INDEX_FakeStart]=fakeStart;
	basis->KNOT[INDEX_FakeEnd]=fakeEnd;	 
	basis->KNOT[-1]=1L;	    
	basis->KNOT[basis->nKnot]=N+1L;
	basis->CalcBasisKsKeK_TermType(basis); 
}
 static  void OO(BEAST2_BASIS_PTR basis,I32 N,BEAST2_RNDSTREAM* PRND,BEAST2_YINFO_PTR yInfo)
{
	basis->nKnot=basis->prior.minKnotNum;    
	if (basis->nKnot > 0) {	
		I32    nMissing=yInfo->nMissing;
		I32PTR rowsMissing=yInfo->rowsMissing;
		memset(basis->goodvec,1,N);
		for (int i=0; i < nMissing; i++) {
			basis->goodvec[rowsMissing[i]]=0;  
		}
		I32  goodNum=yInfo->n;
		for (I32 i=1; i <=basis->nKnot;++i) {		
			int   randLoc=RANDINT(1,(I32)goodNum,*(PRND->rnd32)++);
			int   newKnot=i08_find_nth_onebyte_binvec(basis->goodvec,N,randLoc);
			basis->KNOT[i - 1]=newKnot;
			basis->goodvec[newKnot - 1]=0;
			goodNum--;
		}	
	}
	int fakeStart=1;
	int fakeEnd=N+1L;
	basis->KNOT[INDEX_FakeStart]=fakeStart;
	basis->KNOT[INDEX_FakeEnd]=fakeEnd;
	basis->KNOT[-1]=1L;
	basis->KNOT[basis->nKnot]=N+1L;
	basis->CalcBasisKsKeK_TermType(basis);
}
void GenarateRandomBasis(BEAST2_BASIS_PTR basis,I32 NUMBASIS,I32 N,BEAST2_RNDSTREAM* PRAND,BEAST2_YINFO_PTR yInfo)
{
	for (I32 i=0; i < NUMBASIS; i++) {
		I32 id=basis[i].type;
		switch (id) {
		case DUMMYID:    
		case SEASONID:    
		case SVDID:
		case TRENDID:   
			DSVT(basis+i,N,PRAND); break;
		case OUTLIERID: 
			OO(basis+i,N,PRAND,yInfo); break;
		}
	}
}
#include "abc_000_warning.h"
