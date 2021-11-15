#include <stdlib.h>
#include "abc_000_warning.h"
#include "abc_001_config.h"
#include "abc_mem.h"
#include "abc_ide_util.h"
 static VOID_PTR  malloc_64(size_t N)	{ 
	VOID_PTR  mem=malloc(N+64);
	VOID_PTR  ptr=(VOID_PTR )(((uintptr_t)mem+64) & ~(uintptr_t)0x3F);
	*((char *)((char*)ptr - 1))=(char)((char *)ptr - (char *)mem);
	return ptr;
}
 static void  free_64(VOID_PTR  p)	{
	char * porig=(char*)p - *((char*)p - 1);
	free(porig);
}
 static void  ExpandInternelBuf(MemPointers* self ) {
	 if (self->npts==self->nptsMax) {
		 int       oldMax=self->nptsMax;
		 VOID_PTR* oldPointers=self->memPointer;
		 I08PTR    oldAlign=self->mem64Aligned;
		 self->nptsMax=oldMax+200;
		 self->memPointer=(VOID_PTR*)malloc(sizeof(VOID_PTR) * self->nptsMax);
		 self->mem64Aligned=(I08PTR)   malloc(sizeof(I08) *      self->nptsMax);
		 if (oldPointers) {
			 memcpy((const void* )self->memPointer,(const void*)oldPointers,sizeof(VOID_PTR) * oldMax);
			 memcpy((const void*)self->mem64Aligned,(const void*)oldAlign,sizeof(I08) * oldMax);
			 free(( void*)oldPointers);
			 free(( void*)oldAlign);
		 } 
	 }
 }
static VOID_PTR  MemAlloc(MemPointers * self,I64 sizeInByte,U08 alignment)
{
	VOID_PTR  newPointer;
	if (alignment==0){
		newPointer=malloc(sizeInByte);
		self->bytesAllocated+=sizeInByte;
	} else {
		newPointer=malloc_64(sizeInByte);
		self->bytesAllocated+=sizeInByte+64;
	}
	ExpandInternelBuf(self);
	self->memPointer[  self->npts]=newPointer;
	self->mem64Aligned[self->npts]=alignment;
	self->npts++;
	return newPointer;
}
static VOID_PTR  MemAlloc0(MemPointers* _restrict self,I64 sizeInByte,U08 alignment)
{
	VOID_PTR  newPointer=MemAlloc(self,sizeInByte,alignment);
	memset(newPointer,0,sizeInByte);
	return newPointer;
}
static void mem_free_all(MemPointers * _restrict self)
{
	for (int i=0; i < self->npts; i++) 	{
		if (self->mem64Aligned[i]==0)	free(   self->memPointer[i]);
		else                 			free_64(self->memPointer[i]);
	}
	if (self->memPointer !=NULL) 	{
		free(self->memPointer);
		self->memPointer=NULL;
	}
	if (self->mem64Aligned !=NULL) 	{
		free(self->mem64Aligned);
		self->mem64Aligned=NULL;
	}
	self->bytesAllocated+=0;
}
void mem_init(MemPointers* self)
{
	*self=(MemPointers) {
			.alloc=MemAlloc,
			.alloc0=MemAlloc0,
			.init=mem_init,
			.free_all=mem_free_all,
			.nptsMax=0,
			.npts=0,
			.bytesAllocated=0,
			};	
}
#include "abc_000_warning.h"
