#include <string.h>
#include "abc_000_warning.h"
#include "beastv2_header.h"
#include "abc_mem.h"
static void DSVT_AllocInitBasis(BEAST2_BASIS_PTR basis,I32 N,I32 K_MAX,MemPointers* MEM)
{
	I32 MAX_KNOTNUM=basis->prior.maxKnotNum;
	I32 MAX_NUM_SEG=(MAX_KNOTNUM+1L);
	I32 Npad16=16 * ((N+15)/16);
	I32 leftMargin=basis->prior.leftMargin;
	I32 rightMargin=basis->prior.rightMargin;
	I32 minSepDist=basis->prior.minSepDist;
	I32 extraElemLeft=max(minSepDist - leftMargin,0);
	I32 extraElemRight=max(minSepDist - rightMargin,0);
	I32 extraLeft_pad8=(extraElemLeft+7)/8 * 8;
	I32 extraRight_pad16=max(Npad16 - N,extraElemRight);
	I32 NgoodVec=extraLeft_pad8+N+extraRight_pad16;
	MemNode nodes[]={
		{&basis->KNOT,sizeof(TKNOT) * (1+1+1+MAX_KNOTNUM+1),.align=64 },
		{&basis->ORDER,sizeof(TORDER) * MAX_NUM_SEG,.align=2  },
		{&basis->ks,sizeof(I16) * MAX_NUM_SEG,.align=2  },
		{&basis->ke,sizeof(I16) * MAX_NUM_SEG,.align=2  },
		{&basis->goodvec,sizeof(U08)* NgoodVec,.align=8  }, 
		{&basis->termType,sizeof(U08) * K_MAX,.align=1  },
		{NULL,}
	};	 
	MEM->alloclist(MEM,nodes,AggregatedMemAlloc,NULL);
	basis->KNOT+=3L;  
	 memset(basis->goodvec,0L,extraLeft_pad8);  
	 memset(basis->goodvec+extraLeft_pad8+N,0L,extraRight_pad16);    
	 basis->goodvec+=extraLeft_pad8; 
}
static void OO_AllocInitBasis(BEAST2_BASIS_PTR basis,I32 N,I32 K_MAX,MemPointers* MEM)
{
	I32 MAX_KNOTNUM=basis->prior.maxKnotNum;
	I32 MAX_NUM_SEG=(MAX_KNOTNUM); 
	I32 Npad16=16 * ((N+15)/16);
	MemNode nodes[]={
		{&basis->KNOT,sizeof(TKNOT)  * (1+1+1+MAX_KNOTNUM+1),.align=64 },
		{&basis->ORDER,sizeof(TORDER) * MAX_NUM_SEG*0,.align=2 },  
		{&basis->ks,sizeof(I16)    * (1+MAX_NUM_SEG),.align=2 },
		{&basis->ke,sizeof(I16)    * (1+MAX_NUM_SEG),.align=2 },
		{&basis->goodvec,sizeof(U08)    * Npad16,.align=8 }, 
		{&basis->termType,sizeof(U08)   * K_MAX*0,. align=1 },
		{NULL,}
	};
	MEM->alloclist(MEM,nodes,AggregatedMemAlloc,NULL);
	basis->KNOT+=3L;  
	*basis->ks++=1;	
	*basis->ke++=0; 
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
