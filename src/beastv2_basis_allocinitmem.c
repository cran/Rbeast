#include <string.h>
#include "abc_000_warning.h"
#include "beastv2_header.h"
#include "abc_mem.h"
static void DSVT_AllocInitBasis(BEAST2_BASIS_PTR basis,I32 N,I32 K_MAX,MemPointers* MEM)
{
	I32 MAX_KNOTNUM=basis->prior.maxKnotNum;
	I32 MAX_NUM_SEG=(MAX_KNOTNUM+1L);
	I32 nBytes=sizeof(TKNOT) * (1+MAX_KNOTNUM+1)
					+sizeof(TORDER) * MAX_NUM_SEG		   ;
	basis->KNOT=(TKNOT_PTR) MyALLOC(*MEM,nBytes,char,64);
	*basis->KNOT++=1L;                              
	basis->ORDER=(TORDER_PTR)(basis->KNOT+(MAX_KNOTNUM+1)); 
	I32 nElem=MAX_NUM_SEG * 2;
	basis->ks=MyALLOC(*MEM,nElem,I16,64);
	basis->ke=basis->ks+MAX_NUM_SEG;
	I32 Npad16=16 * ((N+15)/16);
	basis->goodvec=MyALLOC(*MEM,Npad16,U08,8);
	memset(basis->goodvec+N,0L,Npad16 - N);    
	basis->termType=MyALLOC(*MEM,K_MAX,U08,64);
}
static void OO_AllocInitBasis(BEAST2_BASIS_PTR basis,I32 N,I32 K_MAX,MemPointers* MEM)
{
	I32 MAX_KNOTNUM=basis->prior.maxKnotNum;
	I32 MAX_NUM_SEG=(MAX_KNOTNUM); 
	I32 nBytes;
	nBytes=sizeof(TKNOT) * MAX_KNOTNUM; 
	basis->KNOT=(TKNOT_PTR) MyALLOC(*MEM,nBytes,char,64);  
	nBytes=sizeof(I16) * (1L+MAX_NUM_SEG) * 2;
	basis->ks=(I16PTR) MyALLOC(*MEM,nBytes,U08,64);
	*basis->ks++=1;
	basis->ke=basis->ks+MAX_NUM_SEG;
	*basis->ke++=0; 
	basis->termType=MyALLOC(*MEM,K_MAX,U08,64);
	I32 Npad16=16 * ((N+15)/16);
	basis->goodvec=MyALLOC(*MEM,Npad16,U08,64);
	memset(basis->goodvec+N,0L,Npad16 - N);
}
void* Get_AllocInitBasis(I08 id) {
	switch (id) {
	case SVDID:
	case DUMMYID:	
	case SEASONID:  
	case TRENDID:  
		return DSVT_AllocInitBasis;
		break;
	case OUTLIERID:
		return OO_AllocInitBasis;
	}
	return NULL;
}
#include "abc_000_warning.h"
