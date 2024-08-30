#include <math.h>
#include <string.h>
#include "assert.h"
#include "abc_000_warning.h"
#include "abc_ide_util.h"
#include "abc_common.h"
#include "abc_date.h"
#include "inttypes.h"  
#if M_INTERFACE==1  
int  JDN_to_DateNum(int jdn) {
	return jdn - 1721059;
}
void StdouFlush(void) {
#if defined(OS_WIN32)||defined(OS_WIN64)
 #if defined(COMPILER_MSVC)
    #pragma comment(linker,"/alternatename:ioFlush=?ioFlush@@YA_NXZ")
 #else
    extern Bool ioFlush(void)  asm("?ioFlush@@YA_NXZ");
 #endif
#elif defined(OS_MAC)
   extern Bool ioFlush(void)  asm("__Z7ioFlushv");
#elif defined(OS_LINUX)   
   extern Bool ioFlush(void)  asm("_Z7ioFlushv");
#else
#endif
 #ifndef O_INTERFACE
	ioFlush();
 #endif
}
I32 GetConsoleWidth() {
	mxArray *pOut[1],*pIn[2];
	pIn[0]=mxCreateDoubleScalar(0);
	pIn[1]=mxCreateString("CommandWindowSize");
	mexCallMATLAB(1,pOut,2,pIn,"get");
	F64 *ptr=mxGetData(pOut[0]);
	I32 screenWidth=ptr[0];
	mxDestroyArray(pIn[0]);
	mxDestroyArray(pIn[1]);
	mxDestroyArray(pOut[0]);
	return screenWidth;
}
int IsClass(void* ptr,char* class) { return 0; }
int IsCell(void* ptr) { return mxIsCell(ptr); }
int IsChar(void* ptr)    { 
	if (ptr==NULL) return 0;
	if (IsCell(ptr)) {
		int n=GetNumberOfElements(ptr);
		for (int i=0; i < n;++i) {
			void *tmp=mxGetCell(ptr,i);
			if ( !mxIsChar(tmp) && !mxIsClass(tmp,"string")) return 0; 
		}
		return 1L;		
	}
	else {
		return mxIsChar(ptr)||mxIsClass(ptr,"string");
	}
}
int IsEmpty(void* ptr) { return mxIsEmpty(ptr); }
int IsStruct(void* ptr)  { return mxIsStruct(ptr)||mxIsEmpty(ptr); } 
int IsNumeric(void* ptr) { return mxIsNumeric(ptr) && !mxIsEmpty(ptr); } 
int IsDouble(void* ptr)  { return mxIsDouble(ptr); }
int IsSingle(void* ptr)  { return mxIsSingle(ptr); }
int IsInt32(void* ptr)   { return mxIsInt32(ptr); }
int IsInt16(void* ptr) { return mxIsInt16(ptr); }
int IsInt64(void* ptr) { return mxIsInt64(ptr); }
int IsLogical(void* ptr) { return mxIsLogical(ptr); }
VOID_PTR GetFieldByIdx(VOID_PTR ptr,I32 ind) {
	if (IsCell(ptr)) {
		return  mxGetCell(ptr,ind);
	}
	if (IsStruct(ptr)) {
		return mxGetFieldByNumber(ptr,0,ind);
	}
	return NULL;
}
void GetFieldNameByIdx(VOID_PTR strucVar,I32 ind0,char *str,int buflen) {
	I32 numFlds=mxGetNumberOfFields(strucVar); 
	if (ind0 < numFlds) {
		char* name=mxGetFieldNameByNumber(strucVar,ind0);
		strncpy(str,name,buflen);
		str[buflen - 1]=0;
	}	else {
		str[0]=0;
	}
}
I32 GetCharArray(void* ptr,char* dst,int n) {
	dst[0]=0;
	if (ptr==NULL) {
		return 0;
	}
	int len=0;
	if (mxIsChar(ptr)){		
		char* tmp=mxArrayToString(ptr);
		strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
		r_free(tmp);
		return len;
	}
	else if (mxIsClass(ptr,"string")) {
		mxArray *tmpMxChar=mxGetProperty(ptr,0,"data");
		if (tmpMxChar !=NULL) {
			char* tmp=mxArrayToString(tmpMxChar);
			strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
			r_free(tmp);
			return len;
		}
		else {
			mxArray* string_class[1]={ptr},*char_array[1];			
			mexCallMATLAB(1,char_array,1,string_class,"char");
			char* tmp=mxArrayToString(char_array[0]);
			mxDestroyArray(char_array[0]);
			strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
			r_free(tmp);
			return len;			
		}
	}
	else {
		dst[0]=0;
		return 0L;
	}
}
I32 GetCharVecElem(void* ptr,int idx,char* dst,int n) {
	int len=0;
	if (IsCell(ptr) ) {
		int  nelem=mxGetNumberOfElements(ptr);
		if (idx >=nelem) {
			dst[0]=0;
			return 0;
		}
		void* elem=mxGetCell(ptr,idx);
		if (mxIsChar(elem)) {
			char* tmp=mxArrayToString(elem);
			strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
			r_free(tmp);
			return len;
		}
		else if (mxIsClass(elem,"string")) {
			mxArray* tmpMxChar=mxGetProperty(elem,0,"data");
			if (tmpMxChar !=NULL) {
				char* tmp=mxArrayToString(tmpMxChar);
				strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
				r_free(tmp);
				return len;
			}
			else {
				mxArray* string_class[1]={ elem },* char_array[1];
				mexCallMATLAB(1L,char_array,1L,string_class,"char");
				char* tmp=mxArrayToString(char_array[0]);
				mxDestroyArray(char_array[0]);
				strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
				r_free(tmp);
				return len;
			}
		}
		else {
			dst[0]=0;
			return 0L;
		}
	}
	else if (mxIsChar(ptr)) {
		int            ndims=mxGetNumberOfDimensions(ptr);
		const mwSize*  dims=mxGetDimensions(ptr);
		if (idx >=dims[0]||ndims > 2) {
			dst[0]=0;
			return 0;
		}
		else {
			int    cols=dims[1];
			int    rows=dims[0];
			mwSize   newdims[2]={1,cols };
			mxArray* newrow=mxCreateCharArray(2,newdims);
			mxChar* newpos=mxGetChars(newrow);
			mxChar* oldpos=mxGetChars(ptr)+idx;
			for (int i=0; i < cols; i++) {
				newpos[i]=*oldpos;
				oldpos+=rows;
			}			
			char* tmp=mxArrayToString(newrow);
			strncpy(dst,tmp,n); dst[n]=0;
			len=strlen(dst);
			r_free(tmp);
			mxDestroyArray(newrow);
			return len;
		}
	}
	else if (mxIsClass(ptr,"string") && !IsCell(ptr)) {
		void* elem=(void**)mxGetData(ptr)+idx;
		mxArray* tmpMxChar=mxGetProperty(elem,0,"data");
		if (tmpMxChar !=NULL) {
			char* tmp=mxArrayToString(tmpMxChar);
			strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
			r_free(tmp);
			return len;
		}
		else {
			mxArray* string_class[1]={ elem },* char_array[1];
			mexCallMATLAB(1L,char_array,1L,string_class,"char");
			char* tmp=mxArrayToString(char_array[0]);
			mxDestroyArray(char_array[0]);
			strncpy(dst,tmp,n); dst[n]=0;	len=strlen(dst);
			r_free(tmp);
			return len;
		}
	}
	else {
		dst[0]=0;
		return 0L;
	}
}
I32  GetNumberOfFields(const void* structVar) {
	return mxGetNumberOfFields(structVar); 
}
void * GetField(const void * structVar,char *fname) {	
	if (structVar==NULL||mxIsEmpty(structVar)) {
		return NULL;
	}
	VOIDPTR ptr=(VOIDPTR) mxGetField(structVar,0,fname);
	if (ptr !=NULL) {
		return ptr;
	}
	I32 numFlds=mxGetNumberOfFields(structVar);
	for (I32 idx=0; idx < numFlds; idx++) {
		char* tmpName=mxGetFieldNameByNumber(structVar,idx);
		if (strcicmp(fname,tmpName)==0) {
			return mxGetFieldByNumber(structVar,0,idx);
		}
	}
	return NULL;	
}
void* GetField123(const void* structVar,char* fname,int nPartial) {
	if (structVar==NULL||mxIsEmpty(structVar)) {
		return NULL;
	}
	VOIDPTR ptr=(VOIDPTR)mxGetField(structVar,0,fname);
	if (ptr !=NULL) {
		return ptr;
	}
	I32 numFlds=mxGetNumberOfFields(structVar);
	for (I32 idx=0; idx < numFlds; idx++) {
		char* tmpName=mxGetFieldNameByNumber(structVar,idx);
		if (strcicmp_nfirst(fname,tmpName,nPartial)==0) {
			return mxGetFieldByNumber(structVar,0,idx);
		}
	}
	return NULL;
}
F64    GetScalar(const void * ptr) { return mxGetScalar((mxArray *)ptr); }
void * GetData(const void * ptr) {   return mxGetData((mxArray *)ptr); }
int    GetDim1(const void * ptr) {   return mxGetM((mxArray *)ptr); }
int    GetDim2(const void * ptr) {   return mxGetN((mxArray *)ptr); }
int    GetNumOfDim(const void * ptr) { return mxGetNumberOfDimensions((mxArray *)ptr); }
void   GetDimensions(const void * ptr,int dims[],int ndims) {
	int   N=min(ndims,GetNumOfDim(ptr) );
	const mwSize *mxdims=mxGetDimensions((mxArray *)ptr);
	for (int i=0; i < N; i++) 	{
		dims[i]=mxdims[i];
	}	
}
void  * SetDimensions(const void* ptr,int dims[],int ndims) {
	if (!ptr) return NULL;
	mwSize mwdims[20];
	for (int i=0; i < ndims; i++) mwdims[i]=dims[i];
	mwSize ndimension=ndims;
	int res=mxSetDimensions(ptr,mwdims,ndimension);
	mwSize* p=mxGetDimensions(ptr);
	return ptr;
}
int    GetNumberOfElements(const void * ptr)
{ 
	if (mxIsChar(ptr)) {
		const mwSize* dims=mxGetDimensions((mxArray*)ptr);
		return dims[0]; 		
	} else {
		return mxGetNumberOfElements(ptr);
	}	
}
void* CreateNumVar(DATA_TYPE dtype,int* dims,int ndims,VOIDPTR* data_ptr) {
		mxClassID fieldDataType;
		switch (dtype)	{
			case DATA_FLOAT: 	fieldDataType=mxSINGLE_CLASS;		break;
			case DATA_DOUBLE:   fieldDataType=mxDOUBLE_CLASS;		break;
			case DATA_INT32:    fieldDataType=mxINT32_CLASS;		break;
			default:			fieldDataType=mxSINGLE_CLASS;			
		}
		mxArray * _restrict mxptr=NULL;
		if (ndims==1) {
			mxptr=mxCreateNumericMatrix(dims[0],1L,fieldDataType,mxREAL);
		}else if (ndims==2) {
			mxptr=mxCreateNumericMatrix(dims[0],dims[1],fieldDataType,mxREAL);
		} else if (ndims >=3)	{
			mwSize DIMS[4]={dims[0],dims[1],dims[2],dims[3] };
			mxptr=mxCreateNumericArray(ndims,DIMS,fieldDataType,mxREAL);
		}
		if (data_ptr  && mxptr ) {
			*data_ptr=mxGetData(mxptr);
		}
		return mxptr;
}
void * CreateStructVar(FIELD_ITEM *fieldList,int nfields)
{ 
	int nfields_actual=0;
	for (int i=0; i < nfields;++i) {
		nfields_actual++;
		if (fieldList[i].name[0]==0) {
			nfields_actual--;
			break;
		}
	}
	nfields=nfields_actual;
	mxArray * _restrict out;
	{
		char * fldNames[100];		
		for (int i=0; i < nfields; i++) { fldNames[i]=fieldList[i].name; }					
		mwSize dims_2d[2]={ 1,1 };
		out=mxCreateStructArray(2,dims_2d,nfields,fldNames);
	}
	for (int i=0; i < nfields; i++) {
		if (fieldList[i].ptr==NULL) {
			continue;
		}
		mxArray* _restrict newFldPtr;
		if (fieldList[i].type==DATA_STRUCT){			
			newFldPtr=(mxArray*) fieldList[i].ptr;
		} else {
			newFldPtr=CreateNumVar(fieldList[i].type,fieldList[i].dims,fieldList[i].ndim,fieldList[i].ptr);
		} 
		mxSetField(out,0L,fieldList[i].name,newFldPtr);		
	}
	return (void*)out;
}
void ReplaceStructField(VOIDPTR s,char* fname,VOIDPTR newvalue){
	mxArray *fld=mxGetField(s,0,fname);
	if (fld !=NULL) {
		mxDestroyArray(fld);
	}
	mxSetField(s,0,fname,newvalue);
}
void  DestoryStructVar(VOID_PTR strutVar) {
		mxDestroyArray(strutVar);
}
void AddStringAttribute(VOID_PTR listVar,const char* field,const char* value) {
	mxArray* tmp=mxCreateString(value);
	mxAddField(listVar,field);	
	mxSetField(listVar,0,field,tmp);
}
void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value) {
	mxArray* tmp=mxCreateDoubleScalar(value);
	mxAddField(listVar,field);
	mxSetField(listVar,0,field,tmp);
}
void RemoveAttribute(VOID_PTR listVar,const char* field) {
}
Bool utIsInterruptPending();
void utSetInterruptPending(Bool);
#ifndef O_INTERFACE
I32  CheckInterrupt()         { return utIsInterruptPending(); }
void ConsumeInterruptSignal() { utSetInterruptPending(_False_); }
#else
I32  CheckInterrupt() { return 0; }
void ConsumeInterruptSignal() { ; }
#endif
#else
const static char achar='a';
#endif
#include "abc_000_warning.h"
