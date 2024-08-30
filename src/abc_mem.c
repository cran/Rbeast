#include <stdlib.h>  
#include <string.h>  
#include <stdarg.h>
#include "abc_000_warning.h"
#include "abc_ide_util.h"
#include "abc_001_config.h"
#include "abc_mem.h"
#define AlignedPointer(ptr,Alignment)  (VOID_PTR)  ( ((uintptr_t) (ptr)+Alignment-1) &  ~(uintptr_t)(Alignment-1) )
#define FreeNullify(ptr) {if (ptr!=NULL){ free(ptr); ptr=NULL;} }
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
 typedef struct {
	 VOIDPTR  ptr;
	 VOIDPTR  alignedptr;
	 int      byte_allocated;
 } MemAlignedPtr;
 static MemAlignedPtr malloc_aligned(size_t N,int alignment) {
	 alignment=max(1,alignment);
	 int       isSuccess=0;
	 size_t    bytesAllocated=0;
	 VOID_PTR  ptr=NULL;
	 VOID_PTR  ptrAligned;
	 if (alignment <=8) {
		 ptr=malloc(N);
		 ptrAligned=AlignedPointer(ptr,alignment);
		 isSuccess=(ptr==ptrAligned);
		 bytesAllocated=isSuccess ? N : 0;
	 }
	 if (!isSuccess) {
		 if (ptr) free(ptr);
		 ptr=malloc(N+(alignment - 1));
		 ptrAligned=AlignedPointer(ptr,alignment);
		 bytesAllocated=N+(alignment - 1);
	 }
	 MemAlignedPtr result;
	 result.ptr=ptr;
	 result.alignedptr=ptrAligned;
	 result.byte_allocated=(int) bytesAllocated;
	 return result;
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
			 memcpy((  void* )self->memPointer,(const void*)oldPointers,sizeof(VOID_PTR) * oldMax);
			 memcpy(( void*)self->memAlignOffset,(const void*)oldAlign,sizeof(I08) * oldMax);
			 free(( void*)oldPointers);
			 free(( void*)oldAlign);
		 } 
		 if (self->checkHeader) {	 
			 U64PTR   oldHeader=self->memHeaderBackup; 
			 self->memHeaderBackup=(U64PTR)malloc(sizeof(U64) * self->nptsMax);
			 if (oldHeader) {
				 memcpy((  void*)self->memHeaderBackup,(const void*)oldHeader,sizeof(U64) * oldMax);
				 free((void*)oldHeader);
			 }
		 }
	 }
 }
static VOID_PTR  mem_alloc(MemPointers * self,I64 N,U08 alignment) {
	if (N <=0) {
		return NULL;
	}
	ExpandInternelBuf(self);
	alignment=max(1,alignment);
	MemAlignedPtr resultPtr=malloc_aligned(N,alignment);	
	self->memPointer[   self->npts]=resultPtr.alignedptr;
	self->memAlignOffset[self->npts]=(int8_t) ((uintptr_t)resultPtr.alignedptr - (uintptr_t)resultPtr.ptr);
	self->bytesAllocated+=resultPtr.byte_allocated;
	if (self->checkHeader) {
		self->memHeaderBackup[self->npts]=*(U64PTR) ((uintptr_t)resultPtr.ptr - 8);
	}
    self->npts++;
	return resultPtr.alignedptr;
}
static VOID_PTR  mem_alloc0(MemPointers* _restrict self,I64 sizeInByte,U08 alignment){
	VOID_PTR  ptr=mem_alloc(self,sizeInByte,alignment);
	if (ptr) memset(ptr,0,sizeInByte);
	return ptr;
}
static void mem_free_all(MemPointers * _restrict self) {
	for (int i=0; i < self->npts; i++) 	{
		free( (char*) self->memPointer[i] - self->memAlignOffset[i]);	
	}
	FreeNullify(self->memPointer);
	FreeNullify(self->memAlignOffset);
	FreeNullify(self->memHeaderBackup);
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
static void  memnodes_nullify_badnodes(MemNode* list,VOIDPTR * nodesRemove) {
	if (nodesRemove==NULL) {
		return;
	}
	int i=0;
	while (nodesRemove[i]) {	        
		int j=0;
		while (list[j].addr) {
			if ( list[j].addr==nodesRemove[i] ) {
				list[j].size=0;
				break;
			}
			j++;
		}
		i++;
	}
}
 I64  memnodes_calc_offsets(MemNode* list,int * maxAlignment) {
    int maxAlign=1;
	I64 curr_offset=0; 
	int i=0;
	while (list[i].addr) {
		int curalign=( list[i].size==0||list[i].align <=0) ? 1 : list[i].align; 
		list[i].offset_from_origin=(list[i].size==0)   ? curr_offset : (int) (intptr_t) AlignedPointer(curr_offset,curalign);
		curr_offset=list[i].offset_from_origin+list[i].size;
		i++;
		maxAlign=max(maxAlign,curalign);
	}
	if (maxAlignment) maxAlignment[0]=maxAlign;
	I64 totalSize=curr_offset;
	list[0].offset_from_origin=i;  
	list[i].size=(int) totalSize;
	list[i].align=maxAlign;
	return totalSize;
}
 void   memnodes_assign_from_alignedbase(MemNode* list,VOIDPTR pbase) {
	 int nNodes=(int) list[0].offset_from_origin;     
	 int maxAlign=nNodes>0? list[nNodes].align:1L;      
	 if (nNodes <=0) {
		 memnodes_calc_offsets(list,NULL); 
		 nNodes=(int) list[0].offset_from_origin;
		 maxAlign=(int) list[nNodes].align;
	 }
	 if (AlignedPointer(pbase,maxAlign) !=pbase) {
		 r_printf("Error: the input base pointer is not aligned!\n");
		 return;
	 }
	 int j=0;
	 list[0].offset_from_origin=0;  
	 while (list[j].addr) {
		 list[j].addr[0]=(list[j].size==0) ? NULL : (char*)pbase+list[j].offset_from_origin;
		 j++;
	 }
	 list[0].offset_from_origin=nNodes; 
 }
 void   memnodes_assign_from_unalignedbase(MemNode* list,VOIDPTR pbase,int bufsize) {
	 int nNodes=(int)  list[0].offset_from_origin;    
	 int totalsize=(nNodes > 0) ? list[nNodes].size : 0; 
	 int maxAlign=(nNodes > 0) ? list[nNodes].align : 1L; 
	 if (nNodes==0) {
		 memnodes_calc_offsets(list,NULL); 
		 nNodes=(int) list[0].offset_from_origin;
		 totalsize=(int) list[nNodes].size;
		 maxAlign=(int) list[nNodes].align;
	 }
	 char* palgiend=AlignedPointer(pbase,maxAlign);
	 int offset=(int) (palgiend-(char*)pbase);
	if (offset+totalsize > bufsize) {
		 r_printf("Error: the buf has no enough space!\n");
		 return;
	 }
	 int j=0;
	 list[0].offset_from_origin=0;  
	 while (list[j].addr) {
		 list[j].addr[0]=(list[j].size==0) ? NULL : palgiend+list[j].offset_from_origin;
		 j++;
	 }
	 list[0].offset_from_origin=nNodes; 
 }
 I64    memnodes_find_max_common_size(MemNode* list,...) {
	 va_list arguments;
	 va_start(arguments,list); 	 
	 int nlist=0;
	 MemNode * listVec[100];	 
	 listVec[nlist++]=list;
	  MemNode* nextList;
	 do {
		 nextList=va_arg(arguments,MemNode*);
		 listVec[nlist++]=nextList;
	 } while (nextList !=NULL);
	 va_end(arguments);             
	 nlist--;
	 I64 maxTotalSize=0;
	 for (int i=0; i < nlist; i++) {
		 MemNode* LIST=listVec[i];
		 int nNodes=(int) LIST[0].offset_from_origin;
		 if (nNodes <=0) {
			 memnodes_calc_offsets(LIST,NULL);
			 nNodes=(int) LIST[0].offset_from_origin;
		 }
		 int totalSize=LIST[nNodes].size;
		 int maxAlgin=LIST[nNodes].align;
		 maxTotalSize=max(maxTotalSize,totalSize+maxAlgin - 1);
	 }
	 return maxTotalSize;
 }
static void  mem_alloc_list(MemPointers* self,MemNode* list,int aggregatedAllocation,VOIDPTR * nodesRemove) {
	memnodes_nullify_badnodes(list,nodesRemove);
	if (!aggregatedAllocation) {
		int i=0;
		while (list[i].addr ) {									 
			*list[i].addr=mem_alloc(self,list[i].size,list[i].align);
			i++;
		}    
		return;
	}
	int maxAlign;
	I64 totalSize=memnodes_calc_offsets(list,&maxAlign);  
	I08 *paligned=mem_alloc(self,totalSize,maxAlign);   
	memnodes_assign_from_alignedbase(list,paligned);
}
void mem_init(MemPointers* self) {	 
	*self=(MemPointers) {
			.alloc=mem_alloc,
			.alloc0=mem_alloc0,
			.alloclist=mem_alloc_list,
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
void dynbuf_init(DynMemBufPtr buf,int init_max_len) {
	buf->cur_len=0;
	if ( (size_t) init_max_len > buf->max_len) {		
		if (buf->raw) {
			free(buf->raw);			
			buf->raw=NULL;
		}
		buf->max_len=init_max_len;		
	}
	if (buf->raw==NULL) {		
		buf->raw=malloc(buf->max_len);
		return;
	}
}
void dynbuf_kill(DynMemBufPtr buf) {
	if (buf->raw) {
		free(buf->raw);
	}
	memset(buf,0,sizeof(DynMemBuf));
}
void dynbuf_requestmore(DynMemBufPtr buf,int moreBytes) {
	size_t newLength=(size_t) (buf->cur_len+moreBytes) ;
	if (newLength <=buf->max_len) {
		if (buf->raw==NULL) {
			buf->raw=malloc(buf->max_len);
			buf->cur_len=0;
		}
		return;
	}
	else {
		newLength=(int) max(newLength,buf->max_len+buf->max_len/2);
		int8_t* newptr;
		if (buf->cur_len==0) {
			FreeNullify(buf->raw);
			newptr=malloc(newLength);
		}	else {
			newptr=realloc(buf->raw,newLength);
		}
		if (newptr) {
			buf->max_len=newLength;
			buf->raw=newptr;
		} else {
			buf->max_len=0;
			buf->raw=NULL;
		}
	}
}
void dynbuf_insert_bytes(DynMemBufPtr buf,char * eewData,int nbytes) {	 
	 dynbuf_requestmore(buf,nbytes);                  
	 memcpy(buf->raw+buf->cur_len,eewData,nbytes); 
	 buf->cur_len+=nbytes;
}
void dynbuf_insert_str(DynMemBufPtr buf,char* newstr) {
	int newstr_len=(int) strlen(newstr)+1;    
	dynbuf_requestmore(buf,newstr_len);
	memcpy(buf->raw+buf->cur_len,newstr,newstr_len); 
	buf->cur_len+=newstr_len;
}
void dynbuf_alloc_list(DynMemBufPtr buf,MemNode* list) {
	buf->cur_len=0;
	buf->max_len=buf->raw==NULL ? 0 : buf->max_len; 
	int maxALign;
	I64 totalSize=memnodes_calc_offsets(list,&maxALign);  
	VOIDPTR paligned=AlignedPointer(buf->raw,maxALign);
	int     offset=(int) ((char*)paligned - (char*)buf->raw);
	int     bytesAvailble=(int) (buf->max_len - offset);
	if (bytesAvailble< totalSize) {
		FreeNullify(buf->raw);
		MemAlignedPtr resultPtr=malloc_aligned(totalSize,maxALign);
		buf->raw=resultPtr.ptr;
		buf->max_len=resultPtr.byte_allocated;
		paligned=resultPtr.alignedptr;		
	}  
	buf->cur_len=totalSize+((char*)paligned - (char*)buf->raw);
	memnodes_assign_from_alignedbase(list,paligned);
}
 void adynbuf_init(DynAlignedBufPtr buf,int init_max_len) {
	 buf->cur_len=0;
	 if (buf->elem_size==0||buf->align==0) {
		 r_printf("ERROR: elem_size and algin should not be zeros (in abynbuf_nit).\n");
		 return;
	 }
	 if ((size_t)init_max_len > buf->max_len) {
		 buf->max_len=init_max_len;
		 if (buf->p.raw) {
			 free(buf->p.raw - buf->offset);
			 buf->p.raw=NULL;
		 }	 
	 }
	 if (buf->max_len>0 && buf->p.raw==NULL) {
		 int bufsize=buf->max_len * buf->elem_size+(buf->align);
		 char* ptr=malloc(bufsize);
		 char* palign=AlignedPointer(ptr,buf->align);
		 buf->p.raw=palign;
		 buf->offset=(int) (palign - ptr);	 
	 }
 }
 void adynbuf_kill(DynAlignedBufPtr buf) {
	 if ((*buf).p.raw) {
		 free(buf->p.raw - buf->offset);
	 }
	 memset(buf,0,sizeof(DynAlignedBuf));
 }
 void adynbuf_requestmore(DynAlignedBufPtr buf,int moreElements) {
	 size_t newLength=moreElements+buf->cur_len;
	 if (newLength <=buf->max_len) {
		 return;
	 }
	 newLength=max(newLength,buf->max_len+buf->max_len/2);
	 int newbufsize=newLength* buf->elem_size+(buf->align);
	 int8_t* newptr=realloc(buf->p.raw - buf->offset,newbufsize);
	 int8_t* newpalign=(VOID_PTR)(((uintptr_t)newptr+buf->align - 1) & ~(uintptr_t)(buf->align - 1));	 
	 if (newptr) {
		 int  newoffset=(int) (newpalign - newptr);
		 if (newoffset==buf->offset) {			 
			 buf->p.raw=newpalign;
			 buf->max_len=newLength;
		 }
		 else if (newoffset < buf->offset) {			 
			 memcpy(newpalign,newptr+buf->offset,buf->elem_size * buf->cur_len);
			 buf->p.raw=newpalign;
			 buf->offset=newoffset;
			 buf->max_len=newLength;
		 }  else if (newoffset > buf->offset) {		 
			 int8_t* newptr1=malloc( newbufsize);
			 int8_t* newpalign1=(VOID_PTR)(((uintptr_t)newptr1+buf->align - 1) & ~(uintptr_t)(buf->align - 1));
			 memcpy(newpalign1,newptr+buf->offset,buf->elem_size * buf->cur_len);
			 buf->p.raw=newpalign1;
			 buf->offset=(int) (newpalign1 - newptr1);
			 buf->max_len=newLength;
			 free(newptr); 
		 }
	 }
 }
#include "abc_000_warning.h"
