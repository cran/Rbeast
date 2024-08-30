#pragma once
#include "abc_001_config.h"
#include "abc_datatype.h"
#include "abc_ts_func.h"
#ifdef __cplusplus
extern "C" {
#endif
	extern void StdouFlush(void);
	extern I08 IDE_USER_INTERRUPT;
	typedef enum   IO_TYPE { MEM_IO,DISK_IO } IO_TYPE;
	typedef struct FIELD_ITEM {
		char      name[64];
		DATA_TYPE type;
		int       ndim;
		int       dims[5];
		void**   ptr;
		int       extra; 
	} FIELD_ITEM;
	VOID_PTR GetFieldByIdx(VOID_PTR strucVar,I32 ind);
	void  GetFieldNameByIdx(VOID_PTR strucVar,I32 ind0,char* str,int buflen);
	void* CreateNumVar(DATA_TYPE dtype,int* dims,int ndims,VOIDPTR* data_ptr);
	void* CreateNumVector(DATA_TYPE dtype,int length,VOIDPTR* data_ptr);
	void* CreateNumMatrix(DATA_TYPE dtype,int Nrow,int Ncol,VOIDPTR* data_ptr);
	void* CreateF32NumVector(int length,VOIDPTR* data_ptr);
	void* CreateF32NumMatrix(int Nrow,int Ncol,VOIDPTR* data_ptr);
	void* CreateF64NumVector(int length,VOIDPTR* data_ptr);
	void* CreateF64NumMatrix(int Nrow,int Ncol,VOIDPTR* data_ptr);
	void* CreateI32NumVector(int length,VOIDPTR* data_ptr);
	void* CreateI32NumMatrix(int Nrow,int Ncol,VOIDPTR* data_ptr);
	void   ReplaceStructField(VOIDPTR s,char* fname,VOIDPTR newvalue);
	void*  CreateStructVar(FIELD_ITEM* fieldList,int nfields);
	void   DestoryStructVar(VOID_PTR strutVar);
	void   RemoveSingltonDims(FIELD_ITEM* flist,I32 nlist);
	void   RemoveField(FIELD_ITEM* fieldList,int nfields,char* fieldName);
	void AddStringAttribute(VOID_PTR listVar,const char* field,const char* value);
	void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value);
	void RemoveAttribute(VOID_PTR listVar,const char* field);
	extern  I32   GetConsoleWidth(void);
	extern  void  printProgress1(F32 pct,I32 width,char* buf,I32 firstTimeRun);
	extern  void  printProgress2(F32 pct,F64 time,I32 width,char* buf,I32 firstTimeRun);
	I32 GetCharArray(void* ptr,char* dst,int n);
	I32 GetCharVecElem(void* ptr,int idx,char* dst,int n);
	void* GetField123(const void* structVar,char* fname,int nPartial);
	void* GetField(const void* structVar,char* fname);
	void* GetField123Check(const void* structVar,char* fname,int nPartial);
	void* GetFieldCheck(const void* structVar,char* fname);
	F64   GetScalar(const void* ptr);
	F64   GetNumericElement(const void* Y,I32 idx);
	void* GetData(const void* ptr);
	int  GetDataType(VOID_PTR Y);
	int  GetDim1(const void* ptr);
	int  GetDim2(const void* ptr);
	int  GetNumOfDim(const void* ptr);
	void GetDimensions(const void* ptr,int dims[],int ndims);
	void *SetDimensions(const void* ptr,int dims[],int ndims);
	int  GetNumberOfElements(const void* ptr);
	I32  GetNumberOfFields(const void* structVar);
	int IsClass(void* ptr,char* classstr);
	int IsCell(void* ptr);
	int IsChar(void* ptr);
	int IsEmpty(void* ptr);
	int IsStruct(void* ptr);
	int IsNumeric(void* ptr);
	int IsDouble(void* ptr);
	int IsSingle(void* ptr);
	int IsInt32(void* ptr);
	int IsInt16(void* ptr);
	int IsInt64(void* ptr);
	int IsLogical(void* ptr);
	int HaveEqualDimesions(const void* p1,const void* p2);
	int CopyNumericObjToF32Arr(F32PTR outmem,VOID_PTR infield,int N);
	int CopyNumericObjToI32Arr(I32PTR outmem,VOID_PTR infield,int N);
	int CopyNumericObjToF64Arr(F64PTR outmem,VOID_PTR infield,int N);
	extern I32  CheckInterrupt(void);
	extern void ConsumeInterruptSignal(void);
	static INLINE int IsRinterface(void) { return R_INTERFACE; }
	static INLINE int IsMinterface(void) { return M_INTERFACE; }
	static INLINE int IsPinterface(void) { return P_INTERFACE; }
	int     GetNumElemTimeObject(VOID_PTR timeObj);
	typedef struct {
		F64 fyear;
		F64 value;
		I08 unit;
	} TimeScalarInfo;
	F64 Parse_TimeIntervalObject(VOIDPTR* obj,TimeScalarInfo* tint);
	F64 Parse_SingelDateObject(VOIDPTR* obj,TimeScalarInfo* tint);
	extern char* dateNumOriginStr;
	int  JDN_to_DateNum(int jdn);
	void TimeVec_init(TimeVecInfo* tv);
	void TimeVec_kill(TimeVecInfo* tv);
	void TimeVec_kill_fyearArray(TimeVecInfo* tv);
	int  TimeVec_from_TimeObject(VOID_PTR timeObj,TimeVecInfo* tv);
	void TimeVec_from_StartDeltaTime(TimeVecInfo* tv,F32 start,F32 dt,int N,int isDate);
	void TimeVec_SortCheckRegularOrder(TimeVecInfo* tv);
	void TimeVec_UpdateUserStartDt(TimeVecInfo* tv);
	void* CvtToPyArray_NewRef(VOIDPTR Y);
#include "abc_mem.h"
	void CharObj2CharArr(VOID_PTR o,DynMemBuf* str,DynAlignedBuf* charstart,DynAlignedBuf* nchars);
	void obj_to_str(VOID_PTR o,DynMemBufPtr s,int leftMargin);
	int  IDEPrintObject(VOID_PTR o);
#if M_INTERFACE==1
#define  r_printf(...)  mexPrintf(__VA_ARGS__)
#define  r_error(...)    do {      \
        char sss[1000]; \
        snprintf(sss,1000-1,__VA_ARGS__);\
        word_wrap_indented(sss,110,5); \
        mexPrintf("%s",sss); \
     }while(0)
#define   r_warning(...) r_error(__VA_ARGS__)
#define  r_malloc(x)    mxMalloc(x) 
#define  r_free(x)      mxFree(x)
#define  IDE_NULL       mxCreateNumericMatrix(0,0,mxSINGLE_CLASS,mxREAL)
#elif R_INTERFACE==1
#define  r_printf(...)   Rprintf(__VA_ARGS__)
#define  r_error(...)    error(__VA_ARGS__)
#define  r_warning(...)  Rf_warning(__VA_ARGS__)
#define  r_malloc(x)     Calloc(x,char)  
#define  r_free(x)       Free(x) 
#define  IDE_NULL        R_NilValue
#elif P_INTERFACE==1
#define  r_printf(...)   PySys_WriteStdout(__VA_ARGS__)
#define  r_error(...)    PySys_WriteStderr(__VA_ARGS__)    
#define  r_warning(...)  printf(__VA_ARGS__)
#define  r_malloc(x)     PyMem_RawMalloc(x,char)  
#define  r_free(x)       PyMem_RawFree(x) 
#define  IDE_NULL        Py_None
#endif
extern char GLOBAL_PRNT_WARNING;
#define q_warning(...)  { if (GLOBAL_PRNT_WARNING) {r_warning(__VA_ARGS__);} }
#define q_printf(...)   { if (GLOBAL_PRNT_WARNING) {r_printf(__VA_ARGS__); } }
#if R_INTERFACE==1
	extern  SEXP	getListElement(SEXP list,const char* str);
	extern  SEXP    getListElement_CaseIn(SEXP list,const char* str);
#elif M_INTERFACE==1
#define PROTECT(XXXX)   XXXX
#define UNPROTECT(XXXX) XXXX
#elif P_INTERFACE==1 
	#define PROTECT(XXXX)   XXXX
	#define UNPROTECT(XXXX) XXXX
	extern PyTypeObject BarObject_Type;  
	extern PyObject* currentModule;
	extern PyObject* classOutout;
	extern PyObject* setClassObjects(PyObject* self,PyObject* args);
#endif
#ifdef __cplusplus
}
#endif
