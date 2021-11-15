#pragma once
#include <inttypes.h>
#include "abc_000_macro.h"
#include "abc_datatype.h"
typedef struct MemPointers MemPointers;
struct MemPointers
{
	I64     bytesAllocated;
	VOID_PTR * memPointer;
	I08PTR     mem64Aligned;
	I16  npts;
	I16  nptsMax;
	void       (*init)(     MemPointers *  self);
	VOID_PTR   (*alloc)(    MemPointers *  self,I64 size,U08 alignment);
	VOID_PTR   (*alloc0)(   MemPointers *  self,I64 size,U08 alignment);
	void       (*free_all)( MemPointers *  self);
};
 extern void  mem_init(MemPointers* _restrict self);
#define MyALLOC(MEM,numElem,type,alignment) (type *)(MEM).alloc(&(MEM),(I64) sizeof(type)* (I64)(numElem),alignment)
#define MyALLOC0(MEM,numElem,type,alignment) (type *)(MEM).alloc0(&(MEM),(I64) sizeof(type)* (I64)(numElem),alignment)
