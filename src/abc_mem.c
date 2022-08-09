#include <stdlib.h>  
#include <string.h>  
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
			 self->memHeaderBackup=(U64PTR)malloc(sizeof(U64) * self->nptsMax);
			 if (oldHeader) {
				 memcpy((const void*)self->memHeaderBackup,(const void*)oldHeader,sizeof(U64) * oldMax);
				 free((void*)oldHeader);
			 }
		 }
	 }
 }
static VOID_PTR  MemAlloc(MemPointers * self,I64 N,U08 alignment)
{
	if (N <=0) {
		return NULL;
	}
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
		if ((curheader & 0xffff0000ffffffff) !=(self->memHeaderBackup[i] & 0xffff0000ffffffff)) {
			badHeaderNum++;
		}
	}
	return badHeaderNum;
}
static void  __NullifyNodes(MemNode* list,VOIDPTR * nodesRemove) {
	int i=0;
	while (nodesRemove[i]) {	        
		int j=0;
		while (list[j].addr) {
			if (list[j].addr==nodesRemove[i]) {
				list[j].size=0;
				break;
			}
			j++;
		}
		i++;
	}
}
static void  MemAllocList(MemPointers* self,MemNode* list,int aggregatedAllocation,VOIDPTR * nodesRemove) {
	if (nodesRemove) {
		__NullifyNodes(list,nodesRemove);
	}
	if (!aggregatedAllocation) {
		int i=0;
		while (list[i].addr ) {									 
			*list[i].addr=MemAlloc(self,list[i].size,list[i].align);	 
			i++;
		}    
		return;
	}
	#define MAX_LEN 255
	I64 newoffsets[MAX_LEN]; 
	I64 prevoffset=0;	
	I64 currNodeSize=0;  
	int i=0;
	while (list[i].addr) {
		int curalign=max(2,list[i].align);
		int prevsize=(i==0)? 0  : list[i - 1].size;
		currNodeSize=list[i].size;
		I64 curr_offset;
		if (currNodeSize==0) {
			curr_offset=prevoffset+prevsize;			
		}	else {
			 curr_offset=(prevoffset+prevsize+curalign - 1) & ~(uintptr_t)(curalign - 1);
		}	
		newoffsets[i]=curr_offset;
		prevoffset=curr_offset;
		i++;
	 }
	I64 totalSize=prevoffset+currNodeSize;
	I08 *p=NULL;
	if (totalSize > 0) {
		p=MemAlloc(self,totalSize,64);
	}
	i=0;
	while (list[i].addr) {
		if (list[i].size==0) {
			*list[i].addr=NULL;
		} else {
			*list[i].addr=p+newoffsets[i];
		} 
		i++;
	}
}
void mem_init(MemPointers* self) {	 
	*self=(MemPointers) {
			.alloc=MemAlloc,
			.alloc0=MemAlloc0,
			.alloclist=MemAllocList,
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
