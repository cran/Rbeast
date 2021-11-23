#include "math.h"
#include "abc_000_warning.h"
#include "abc_mcmc.h"
#include "beastv2_header.h"
static void DSVT(BEAST2_BASIS_PTR basis,I32 N,BEAST2_RNDSTREAM* PRAND)
{
	I32 maxKnotNum=basis->prior.maxKnotNum;
	I32 minKnotNum=basis->prior.minKnotNum;
	I32 rndOrder=ceil((basis->prior.maxOrder+basis->prior.minOrder)/2.0);
	basis->nKnot=ceil((minKnotNum+maxKnotNum)/2.0);   
	basis->nKnot=0;                                       
	I32 SEP=N/(basis->nKnot+1);
	I32 initKnot=1+SEP;
	for (I32 i=1; i <=basis->nKnot; i++) {
		basis->ORDER[i - 1]=rndOrder;                     
		basis->KNOT[i - 1]=initKnot;
		initKnot+=SEP;
	}
	basis->ORDER[basis->nKnot]=rndOrder;
	basis->KNOT[basis->nKnot]=N+1L;
	basis->CalcBasisKsKeK_TermType(basis); 
}
static void OO(BEAST2_BASIS_PTR basis,I32 N_not_used,BEAST2_RNDSTREAM* PRAND_not_used)
{
	basis->nKnot=0;     
	basis->CalcBasisKsKeK_TermType(basis);
}
void GenarateRandomBasis(BEAST2_BASIS_PTR basis,I32 NUMBASIS,I32 N,BEAST2_RNDSTREAM* PRAND) 
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
			OO(basis+i,N,PRAND); break;
		}
	}
}
#include "abc_000_warning.h"
