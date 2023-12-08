#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include "beastv2_header.h"
static void DD_CalcBasisKsKeK_prec0123(BEAST2_BASIS_PTR  basis) {
	I32		   period=basis->bConst.dummy.period; 
	TKNOT_PTR  KNOT=basis->KNOT;
	I16PTR     KS=basis->ks;
	I16PTR     KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                  
	for (I32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 Nseg=(KNOT[(i)-1] - 1) - KNOT[(i-1)-1];
		I32 Kterms=Nseg >=period ? period : Nseg;
		kCounter+=Kterms;
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void VV_CalcBasisKsKeK_prec0123(BEAST2_BASIS_PTR  basis)
{
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR     KS=basis->ks;
	I16PTR     KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                  
	for (I32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 order=ORDER[i - 1];
		for (I32 j=1; j <=order; j++) {
			kCounter++;
		}
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void SS_CalcBasisKsKeK_prec012(BEAST2_BASIS_PTR  basis)
{
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR     KS=basis->ks;
	I16PTR     KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                  
	for (I32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 order=ORDER[i - 1];
		for (I32 j=1; j <=order; j++) {
			kCounter++;
			kCounter++;
		}
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void TT_CalcBasisKsKeK_prec012(BEAST2_BASIS_PTR  basis)
{
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR KS=basis->ks;
	I16PTR KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                            
	for (I32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 order=ORDER[i - 1];		
		for (I32 j=1; j <=order+1; j++) {
			kCounter++;
		}
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void OO_CalcBasisKsKeK_prec012(BEAST2_BASIS_PTR  basis)
{
	U08PTR TERM_TYPE=basis->termType;
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR KS=basis->ks;
	I16PTR KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot; 
	int kCounter=1L;                            
	for (rI32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		*KE++=kCounter;
		kCounter++;
	}
	basis->K=kCounter - 1;
}
static void SS_CalcBasisKsKeK_prec3(BEAST2_BASIS_PTR  basis)
{
	U08PTR     TERM_TYPE=basis->termType;
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR     KS=basis->ks;
	I16PTR     KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                  
	for (rI32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 order=ORDER[i - 1];
		for (rI32 j=1; j <=order; j++) {
			*TERM_TYPE++=j; 
			kCounter++;
			*TERM_TYPE++=j; 
			kCounter++;
		}
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void TT_CalcBasisKsKeK_prec3(BEAST2_BASIS_PTR  basis)
{
	U08PTR     TERM_TYPE=basis->termType;
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR KS=basis->ks;
	I16PTR KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot+1L; 
	int kCounter=1L;                            
	for (I32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		I32 order=ORDER[i - 1];		
		for (I32 j=1; j <=order+1; j++) {
			*TERM_TYPE++=j; 
			kCounter++;
		}
		*KE++=kCounter - 1L;
	}
	basis->K=kCounter - 1;
}
static void OO_CalcBasisKsKeK_prec3(BEAST2_BASIS_PTR  basis)
{
	U08PTR TERM_TYPE=basis->termType;
	TORDER_PTR ORDER=basis->ORDER;
	I16PTR KS=basis->ks;
	I16PTR KE=basis->ke;
	int NUM_OF_SEG=basis->nKnot; 
	int kCounter=1L;                            
	for (rI32 i=1; i <=NUM_OF_SEG; i++) {
		*KS++=kCounter;
		*KE++=kCounter;
		*TERM_TYPE++=1;
		kCounter++;
	}
	basis->K=kCounter - 1;
}
void* Get_CalcBasisKsKeK(I08 id,I08 precPriorType) {
	if (precPriorType==0||precPriorType==1||precPriorType==2)
	{
		switch (id) {
		case DUMMYID:   return DD_CalcBasisKsKeK_prec0123;
		case SVDID:	    return VV_CalcBasisKsKeK_prec0123;
		case SEASONID:  return SS_CalcBasisKsKeK_prec012;
		case TRENDID:   return TT_CalcBasisKsKeK_prec012;
		case OUTLIERID: return OO_CalcBasisKsKeK_prec012;
		}
	}
	else if (precPriorType==3) {
		switch (id) {
		case SEASONID:  return SS_CalcBasisKsKeK_prec3;
		case TRENDID:   return TT_CalcBasisKsKeK_prec3;
		case OUTLIERID: return OO_CalcBasisKsKeK_prec3;
		}
	}
	return NULL;
}
#include "abc_000_warning.h"
