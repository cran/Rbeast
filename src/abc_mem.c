#include <stdlib.h>
#include "abc_000_warning.h"
#include "abc_ide_util.h"
#include "abc_001_config.h"
#include "abc_mem.h"
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
	 if (self->npts >=self->nptsMax) {
		 int       oldMax=self->nptsMax;
		 VOID_PTR* oldPointers=self->memPointer;
		 I08PTR    oldAlign=self->memAlignOffset;
		 self->nptsMax=oldMax+200;
		 self->memPointer=(VOID_PTR*)malloc(sizeof(VOID_PTR) * self->nptsMax);
		 self->memAlignOffset=(I08PTR)  malloc(sizeof(I08) *      self->nptsMax);
		 if (oldPointers) {
			 memcpy((const void* )self->memPointer,(const void*)oldPointers,sizeof(VOID_PTR) * oldMax);
			 memcpy((const void*)self->memAlignOffset,(const void*)oldAlign,sizeof(I08) * oldMax);
			 free(( void*)oldPointers);
			 free(( void*)oldAlign);
		 } 
		 if (self->checkHeader) {	 
			 U64PTR   oldHeader=self->memHeaderBackup; 
			 self->memHeaderBackup=(U64PTR)malloc(sizeof(64) * self->nptsMax);
			 if (oldHeader) {
				 memcpy((const void*)self->memHeaderBackup,(const void*)oldHeader,sizeof(64) * oldMax);				 
				 free((void*)oldHeader);
			 }
		 }
	 }
 }
static VOID_PTR  MemAlloc(MemPointers * self,I64 N,U08 alignment)
{
	ExpandInternelBuf(self);
	alignment=alignment==0 ? 1 : alignment;
	int       isSuccess=0;
	VOID_PTR  ptr=NULL;
	VOID_PTR  ptrAligned;
	if (alignment <=8) {
		ptr=malloc(N);
		ptrAligned=(VOID_PTR)((uintptr_t)ptr & ~(uintptr_t) (alignment-1) );  
		isSuccess=(ptr==ptrAligned);
		self->bytesAllocated+=isSuccess?N:0;
	}
	if (!isSuccess) { 
		if (ptr) free(ptr);
		ptr=malloc(N+(alignment-1)  );
		ptrAligned=(VOID_PTR)( ( (uintptr_t)ptr+alignment-1) &  ~(uintptr_t)(alignment - 1) );		
		self->bytesAllocated+=N+(alignment - 1);
	}
	self->memPointer[   self->npts]=ptrAligned;
	self->memAlignOffset[self->npts]=(uintptr_t)ptrAligned- (uintptr_t)ptr;
	if (self->checkHeader) {
		self->memHeaderBackup[self->npts]=*(U64PTR) ((uintptr_t)ptr - 8); 
	}
    self->npts++;
	return ptrAligned;
}
static VOID_PTR  MemAlloc0(MemPointers* _restrict self,I64 sizeInByte,U08 alignment){
	VOID_PTR  ptr=MemAlloc(self,sizeInByte,alignment);
	memset(ptr,0,sizeInByte);
	return ptr;
}
static void mem_free_all(MemPointers * _restrict self)
{
	for (int i=0; i < self->npts; i++) 	{
		free(  (char*) self->memPointer[i] - self->memAlignOffset[i]);	
	}
	if (self->memPointer) 	{
		free(self->memPointer);
		self->memPointer=NULL;
	}
	if (self->memAlignOffset) 	{
		free(self->memAlignOffset);
		self->memAlignOffset=NULL;
	}
	if (self->memHeaderBackup) {
		free(self->memHeaderBackup);
		self->memHeaderBackup=NULL;
	}
	self->bytesAllocated=0;
	self->npts=0;
	self->nptsMax=0;
}
static I32  verify_header(MemPointers* _restrict self) {
	if (!self->checkHeader||self->npts==0) {
		return 0;
	}
	int badHeaderNum=0;
	for (int i=0; i < self->npts;++i) {
		U64 curheader=*(U64PTR)((uintptr_t)self->memPointer[i] - self->memAlignOffset[i] - 8);
		if (curheader !=self->memHeaderBackup[i]) {
			badHeaderNum++;
		}
	}
	return badHeaderNum;
}
void mem_init(MemPointers* self) {	 
	*self=(MemPointers) {
			.alloc=MemAlloc,
			.alloc0=MemAlloc0,
			.init=mem_init,
			.free_all=mem_free_all,
			.nptsMax=0,
			.npts=0,
			.bytesAllocated=0,
			.memAlignOffset=NULL,
			.memPointer=NULL,
			.memHeaderBackup=NULL,
			.checkHeader=0,
			.verify_header=verify_header,
			};			
}
#include "abc_000_warning.h"
