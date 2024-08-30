#include "abc_000_warning.h"  
#include "abc_mcmc.h"
#include "abc_vec.h"               
#include "abc_blas_lapack_lib.h"   
#include "beastv2_header.h"
static void DSVT_UpdateGoodVecForNewTerm(BEAST2_BASIS_PTR basis,NEWTERM_PTR new,I32 Npad16)
{
	I32     newKnot=new->newKnot;
	I32     newIdx=new->newIdx;
	U08PTR    goodVec=basis->goodvec;
	I32       MINSEP=basis->prior.minSepDist;
	TKNOT_PTR knotList=basis->KNOT;
	MOVETYPE flag=new->jumpType;
	if (flag==BIRTH) 
		r_ippsSet_8u(0,goodVec+(newKnot - MINSEP) - 1,2 * MINSEP+1);	
	else if (flag==DEATH) {
		I32 oldKnot=knotList[newIdx - 1];         
		I32 r1=knotList[(newIdx - 1) - 1];
		I32 r2=knotList[(newIdx+1) - 1] - 1;
		r_ippsSet_8u(1,goodVec+(oldKnot - MINSEP) - 1,2 * MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r1)-1,MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r2 - MINSEP+1) - 1,MINSEP);
	}
	else if (flag==MOVE) {
		I32 oldKnot=knotList[newIdx - 1];
		I32 r1=knotList[(newIdx - 1) - 1];
		I32 r2=knotList[(newIdx+1) - 1] - 1;
		r_ippsSet_8u(1,goodVec+(oldKnot - MINSEP) - 1,2 * MINSEP+1);
		r_ippsSet_8u(0,goodVec+(newKnot - MINSEP) - 1,2 * MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r1)-1,MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r2 - MINSEP+1) - 1,MINSEP);
	}
	else if (flag==MERGE) {
		I32 r1=knotList[(newIdx - 1) - 1];
		I32 r2=knotList[(newIdx+2) - 1] - 1;
		I32 oldKnot=knotList[newIdx - 1]; 
		r_ippsSet_8u(1,goodVec+(oldKnot - MINSEP) - 1,2 * MINSEP+1);
		oldKnot=knotList[(newIdx+1) - 1];
		r_ippsSet_8u(1,goodVec+(oldKnot - MINSEP) - 1,2 * MINSEP+1);
		r_ippsSet_8u(0,goodVec+(newKnot - MINSEP) - 1,2 * MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r1)-1,MINSEP+1);
		r_ippsSet_8u(0,goodVec+(r2 - MINSEP+1) - 1,MINSEP);
	}
	if (flag !=ChORDER) {
		basis->goodNum=i08_sum_binvec(goodVec,Npad16);
	}
#define InsertNewElem(dst,n,newIdx,newValue,T)            for(I32 i=(n); i>=(newIdx);i--) *((T*)dst+i)=*((T*)dst+i-1); \
                                                                 *((T*)dst+newIdx-1)=newValue;
#define RepeatElem(dst,n,newIdx,T)                        for(I32 i=(n); i>=(newIdx); i--) *((T*)dst+i)=*((T*)dst+i-1); 
#define ReplaceElem(dst,n,newIdx,newValue,T)              *((T*)dst+newIdx-1)=newValue;
#define DeleteElem(dst,n,newIdx,T)                          memmove((T*)dst+newIdx-1,(T*)dst+newIdx+1-1,sizeof(T)*(n-(newIdx)));
#define MergeTwoElemWithNewValue(dst,n,newIdx,newValue,T)   *((T*)dst+newIdx-1)=newValue;\
			                                                      memmove((T*)dst+(newIdx+1) - 1,(T*)dst+(newIdx+2) - 1,sizeof(T)* (n - (newIdx+2L)+1L));
#define DoNothing(dst,n,T) ;
	TORDER_PTR orderList=basis->ORDER; 	
    I32        nKnot=basis->nKnot;
	switch (flag)
	{
	case BIRTH: {
		InsertNewElem(knotList,nKnot+1L,newIdx,newKnot,TKNOT);
		RepeatElem(orderList,nKnot+1,newIdx,TORDER);  
		break;
	}
	case DEATH: {
		DeleteElem(knotList,nKnot+1,newIdx,TKNOT);
		DeleteElem(orderList,nKnot+1,newIdx,TORDER);   
		break;
	}
	case MERGE: {
		MergeTwoElemWithNewValue(knotList,nKnot+1L,newIdx,newKnot,TKNOT);
		DeleteElem(orderList,nKnot+1,newIdx+1,TORDER);  
		break;
	}
	case MOVE: 
	{
		ReplaceElem(knotList,nKnot+1,newIdx,newKnot,TKNOT)
		DoNothing(orderList,nKnot+1,TORDER);        
		break;
	}
	case ChORDER:
	{
		DoNothing(knotList,nKnot+1,TKNOT);
		ReplaceElem(orderList,nKnot+1,newIdx,new->newOrder,TORDER);  
		break;
	}
	}
	basis->nKnot=new->nKnot_new;
}
static void OO_UpdateGoodVecForNewTerm(BEAST2_BASIS_PTR basis,NEWTERM_PTR new,I32 Npad16_not_used)
{
	I32     newKnot=new->newKnot;
	U08PTR  goodVec=basis->goodvec;
	MOVETYPE flag=new->jumpType;
	if (flag==BIRTH) {
		goodVec[newKnot - 1]=0;
		basis->goodNum--;
	}
	else if (flag==DEATH) {
		goodVec[newKnot - 1]=1;
		basis->goodNum++;
	}
	else if (flag==MOVE) {
		I32 newIdx=new->newIdx;
		I32 oldKnot=basis->KNOT[newIdx - 1];
		goodVec[oldKnot - 1]=1;
		goodVec[newKnot - 1]=0;
	}
	TKNOT_PTR knotList=basis->KNOT;
	I32       nKnot=basis->nKnot;
	switch (flag)
	{
	case BIRTH: {
		knotList[nKnot+1 - 1]=newKnot;
		break;
	}
	case DEATH: {
		I32 newIdx=new->newIdx;
		DeleteElem(knotList,nKnot,newIdx,TKNOT);
		break;
	}
	case MOVE: 
	{
		I32 newIdx=new->newIdx;
		ReplaceElem(knotList,nKnot,newIdx,newKnot,TKNOT)
			break;
	}
	}
	basis->nKnot=new->nKnot_new;
}
void* Get_UpdateGoodVec_KnotList(I08 id) {
	if      (id==SEASONID)    return DSVT_UpdateGoodVecForNewTerm;
	else if (id==SVDID)       return DSVT_UpdateGoodVecForNewTerm;
	else if (id==DUMMYID)     return DSVT_UpdateGoodVecForNewTerm;
	else if (id==TRENDID)     return DSVT_UpdateGoodVecForNewTerm;
	else if (id==DUMMYID)     return DSVT_UpdateGoodVecForNewTerm;
	else if (id==OUTLIERID)   return OO_UpdateGoodVecForNewTerm;
	return NULL;
}
#include "abc_000_warning.h"
