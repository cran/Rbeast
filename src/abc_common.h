#pragma once
#include <stdio.h>
#include "abc_000_macro.h"
#include "abc_mem.h"
#include "abc_datatype.h"       
extern void    ToUpper(char* s);
extern void    QuickSortD(F32PTR arr,I32PTR  index,I32 low,I32 high);
extern void    QuickSortA(F32PTR arr,I32PTR  index,I32 low,I32 high);
extern I32	   strcicmp(char const * _restrict a,char const * _restrict b);
extern F32     DeterminePeriod(F32PTR Y,I32 N);
extern I32     FindChangepoint(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber);
extern void WriteF32ArraryToStrideMEM(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 offset,DATA_TYPE dataType);
void CopyStrideMEMToF32Arr(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset,DATA_TYPE srcDataType);
#define    CopyInsertNewElement(SRC,NSRC,DST,newPos,newValue,T)                                     \
                         memcpy(DST,SRC,(newPos-1)*sizeof(T));        \
	                     *((T*)DST+newPos-1L)=newValue;                                             \
                         memcpy((T*)DST+newPos,(T*)SRC+newPos-1,(NSRC-(newPos-1))*sizeof(T));   
#define   CopyRepeatChosenElement(SRC,NSRC,DST,newIdx,T)                                                  \
                         memcpy( (T*)DST,(T*)SRC,(newIdx)*sizeof(T)          );  \
                         memcpy( (T*)DST+(newIdx+1) -1,(T*)SRC+newIdx-1,(NSRC-(newIdx-1))*sizeof(T) );
#define   CopyDeleteChosenElement(SRC,NSRC,DST,newIdx,T)                                     \
              memcpy( (T*)DST,(T*)SRC,sizeof(T)*(newIdx -  1      )    );  \
              memcpy( (T*)DST+newIdx-1,(T*)SRC+(newIdx+1)-1,sizeof(T)*(NSRC   - (newIdx) )   );  
#define   CopyMergeTwoValuesWithNewValue(SRC,NSRC,DST,newIdx,newKnot,T)                            \
              memcpy( DST,SRC,sizeof(T)*(newIdx-1)      );                                      \
              *((T*)DST+newIdx-1)=newKnot;                                                            \
              memcpy( (T*)DST+(newIdx+1)-1,(T*)SRC+(newIdx+2)-1,sizeof(T)*(NSRC-(newIdx+1))   );  
#define   CopyReplaceChosenElemWithNewValue( SRC,NSRC,DST,newIdx,newKnot,T )                          \
              memcpy( DST,SRC,(NSRC)*sizeof(T) );  \
              *((T*)DST+newIdx-1)=newKnot; 
#define   CopyNoChange( SRC,NSRC,DST,T )       memcpy(DST,SRC,(NSRC)*sizeof(T) ); 
