#pragma once
#include "abc_001_config.h"
#include "abc_datatype.h"
extern I08 IDE_USER_INTERRUPT;
#if M_INTERFACE==1
	#define  r_printf(...)  mexPrintf(__VA_ARGS__)
	#define  r_error(...)   mexPrintf(__VA_ARGS__)
    #define  r_warning(...) mexPrintf(__VA_ARGS__)
	#define  r_malloc(x)    mxMalloc(x) 
	#define  r_free(x)      mxFree(x)
    #define  IDE_NULL       mxCreateNumericMatrix(0,0,mxSINGLE_CLASS,mxREAL)
#elif R_INTERFACE==1
	#define  r_printf(...)   Rprintf(__VA_ARGS__)
	#define  r_error(...)    error(__VA_ARGS__)
    #define  r_warning(...)  Rf_warning(__VA_ARGS__)
	#define  r_malloc(x)     Calloc(x,char)  
	#define  r_free(x)       Free(x) 
   #define IDE_NULL          R_NilValue
#endif
typedef enum   IO_TYPE { MEM_IO,DISK_IO } IO_TYPE;
typedef struct FIELD_ITEM {
	char      name[64 - 1];
	DATA_TYPE type;
	int       ndim;
	int       dims[5];
	void **   ptr;
	int       extra; 
} FIELD_ITEM;
VOID_PTR GetFieldByIdx(VOID_PTR strucVar,I32 ind);
void * CreateStructVar(FIELD_ITEM *fieldList,int nfields);
void   DestoryStructVar(VOID_PTR strutVar);
void   RemoveField(FIELD_ITEM *fieldList,int nfields,char * fieldName);
void AddStringAttribute(VOID_PTR listVar,const char* field,const char* value);
void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value);
void RemoveAttribute(VOID_PTR listVar,const char* field);
extern  I32   GetConsoleWidth();
extern  void  printProgress(F32 pct,I32 width,char * buf,I32 firstTimeRun);
extern  void  printProgress2(F32 pct,F64 time,I32 width,char * buf,I32 firstTimeRun);
I32 GetCharArray(void *ptr,char * dst,int n);
I32 GetCharVecElem(void* ptr,int idx,char* dst,int n);
void * GetField(const void * structVar,char *fname);
F64   GetScalar(const void * ptr);
F64   GetNumericElement(const void* Y,I32 idx);
void * GetData(const void * ptr);
int  GetDataType(VOID_PTR Y);
int  GetDim1(const void * ptr);
int  GetDim2(const void * ptr);
int  GetNumOfDim(const void * ptr);
void GetDimensions(const void * ptr,int dims[],int ndims);
int  GetNumberOfElements(const void* ptr);
I32  GetNumberOfFields(const void* structVar);
int IsCell(void* ptr);
int IsChar(void* ptr);
int IsStruct(void* ptr);
int IsNumeric(void* ptr);
int IsDouble(void* ptr);
int IsSingle(void* ptr);
int IsInt32(void* ptr);
int IsInt16(void* ptr);
int IsInt64(void* ptr);
int IsLogical(void* ptr);
int HaveEqualDimesions(const void* p1,const void* p2);
int CopyNumericArrToF32Arr(F32PTR outmem,VOID_PTR infield,int N);
extern I32  CheckInterrupt();
extern void ConsumeInterruptSignal();
static INLINE int IsRinterface() {
	#if    R_INTERFACE==1
		return 1;
	#elif  M_INTERFACE==1
		return 0;
	#endif
}
#if R_INTERFACE==1
extern  SEXP	getListElement(SEXP list,const char *str);
extern  SEXP    getListElement_CaseIn(SEXP list,const char *str);
#elif M_INTERFACE==1
	#define PROTECT(XXXX)   XXXX
	#define UNPROTECT(XXXX) XXXX
#endif
