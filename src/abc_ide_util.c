#include <math.h>
#include <string.h>
#include "abc_000_warning.h"
#include "abc_ide_util.h"
#include "abc_common.h"
#include "abc_date.h"
extern void matlab_IOflush(void);
I08 IDE_USER_INTERRUPT;
void printProgress(F32 pct,I32 width,char * buf,I32 firstTimeRun)
{
 	static char spinnerChar[]="|/-\\";
	static I32  cnt=1;
	cnt++;
	cnt=cnt==4 ? 0 : cnt;
	memset(buf,'*',width); 
	I32  len=0;
	buf[len++]=spinnerChar[cnt];
	
	
	char prefix[]="Progress:";
	I32 strLen=sizeof(prefix)-1L; 
	memcpy(buf+len,prefix,strLen);
	len+=strLen;
	sprintf(buf+len,"%5.1f%% done",pct * 100);
	len+=5+1+5;
	buf[len++]='[';


	I32 finishedLen = round((width - len - 1)*pct);
	memset(buf + len, '=', finishedLen);
	len += finishedLen;
	buf[len++] = '>';
	//memset(buf + len, ' ', width-len);
	buf[width - 1] = ']';
	buf[width] = 0;

#if R_INTERFACE==1
	Rprintf("\r%s", buf);

	//R doesnto allow io operationrs from external libraries
	//fflush(stdout);
#elif M_INTERFACE==1
	if (firstTimeRun == 1)
	{
		r_printf("\r\n");
		r_printf("%s", buf);
		matlab_IOflush();
		//mexEvalString("drawnow");
		//mexCallMATLAB(0, NULL, 0, NULL, "drawnow");
	}
	else
	{
		char * back = buf + width + 5;
		memset(back, '\b', width + 2);
		back[width + 2] = 0;

		r_printf(back);
		r_printf("%s\r\n", buf);
		matlab_IOflush();
		//mexEvalString("drawnow");
		//mexCallMATLAB(0, NULL, 0, NULL, "drawnow");

	}


#endif		
}
 void printProgress2(F32 pct, F64 time, I32 width, char * buf, I32 firstTimeRun)
{//https:// stackoverflow.com/questions/2685435/cooler-ascii-spinners
	
	static char spinnerChar[] = "|/-\\";
	static int  count         = 1;
	count = (++count) == 4 ? 0 : count;

	memset(buf, '*', width); // space = 20

	I32  len = 0;
	buf[len++] = (pct<1.0) ? spinnerChar[count]: ' ';

	sprintf(buf + len, "%5.1f%%", pct * 100);
	len += 5 + 1;

	char prefix[] = "done";
	I32 strLen = sizeof(prefix)-1L; // the last byte is a Zero
	memcpy(buf + len, prefix, strLen);
	len += strLen;

	F64 SecsPerDay = 3600 * 24;
	I32 days = time / SecsPerDay;
	F64 tmp = time - days *SecsPerDay;
	I32 hrs = tmp / 3600;
	tmp = (tmp - hrs * 3600);
	I32 mins = tmp / 60;
	tmp = tmp - mins * 60;
	I32 secs = tmp;
	days = days >= 99 ? 99 : days;

	if (time > SecsPerDay)
		sprintf(buf + len, "<Remaining%02dday%02dhrs%02dmin>", days, hrs, mins);
	else
		sprintf(buf + len, "<Remaining%02dhrs%02dmin%02dsec>", hrs, mins, secs);
	len += 26;

	buf[len++] = '[';

	I32 finishedLen = round((width - len - 1)*pct);
	memset(buf + len, '=', finishedLen);
	len += finishedLen;
	buf[len++] = '>';
	//memset(buf + len, ' ', width-len);
	buf[width - 1] = ']';
	buf[width] = 0;

#if R_INTERFACE==1
	r_printf("\r%s", buf);
	//R doesnto allow io operationrs from external libraries
	//fflush(stdout);
#elif M_INTERFACE==1

	if (firstTimeRun == 1)
	{
		r_printf("\r\n");
		r_printf("%s", buf);
		//mexEvalString("drawnow");
		//mexCallMATLAB(0, NULL, 0, NULL, "drawnow");
		matlab_IOflush();
	}
	else {
		char * back = buf + width + 5;
		memset(back, '\b', width + 2);
		back[width + 2] = 0;

		r_printf(back);
		r_printf("%s\r\n", buf);
		//mexEvalString("drawnow");
		//mexCallMATLAB(0, NULL, 0, NULL, "drawnow");
		matlab_IOflush();
	}
#endif	
}

void RemoveField(FIELD_ITEM *fieldList,int nfields,char * fieldName)
{
	for (I64 i=0; i < nfields; i++){
		if (strcmp(fieldList[i].name,fieldName)==0)	{
			fieldList[i].ptr=NULL;
			break;
		}
	}
}
int CopyNumericArrToF32Arr(F32PTR outmem,VOID_PTR infield,int N) {
	VOID_PTR data=GetData(infield);
	if (IsSingle(infield))     		memcpy(outmem,data,sizeof(F32) * N);
	else if (IsDouble(infield))		for (I32 i=0; i < N; i++) outmem[i]=*((double*)data+i);
	else if (IsInt32(infield))		for (I32 i=0; i < N; i++) outmem[i]=*((int*)data+i);
	else if (IsInt64(infield))		for (I32 i=0; i < N; i++) outmem[i]=*((I64*)data+i);
	else if (IsChar(infield))		 return 0;
	else {	
		return 0;
	}
	return 1L;
}
#if R_INTERFACE==1
#include <string.h>
#include <stdio.h>
SEXP getListElement(SEXP list,const char *str)
{
	SEXP elmt=NULL; 
	SEXP names=getAttrib(list,R_NamesSymbol);
	for (int i=0; i < length(list); i++)
	if (strcmp(CHAR(STRING_ELT(names,i)),str)==0) {
		elmt=VECTOR_ELT(list,i);
		break;
	}
	return elmt;
}
SEXP getListElement_CaseIn(SEXP list,const char *str)
{
	SEXP elmt=NULL; 
	SEXP names=getAttrib(list,R_NamesSymbol);
	for (int i=0; i < length(list); i++)
	if (strcicmp(CHAR(STRING_ELT(names,i)),str)==0) {
		elmt=VECTOR_ELT(list,i);
		break;
	}
	return elmt;
}
VOID_PTR GetFieldByIdx(VOID_PTR strucVar,I32 ind) { return VECTOR_ELT(strucVar,ind); }
I32      GetConsoleWidth()  { return (I32) GetOptionWidth(); }
I32 GetCharArray(void *ptr,char * dst,int n) {
	if (TYPEOF((SEXP)ptr)!=STRSXP) return 0;
	char *tmp=CHAR(STRING_ELT(ptr,0));	
	strncpy(dst,tmp,n);	
	dst[n]=0;
	return (I32) strlen(dst);
}
I32 GetCharVecElem(void* ptr,int idx,char* dst,int n) {
	if (TYPEOF((SEXP)ptr) !=STRSXP) return 0;
	char* tmp=CHAR(STRING_ELT((SEXP)ptr,idx));
	strncpy(dst,tmp,n);
	dst[n]=0;
	return (I32)strlen(dst);
}
I32  GetNumberOfFields(const void* structVar) { return Rf_length (structVar); }
void * GetField(const void * structVar,char *fname) {
	if (!structVar) return NULL;
	void * elem=(void*)getListElement(structVar,fname);
	if (elem==NULL) {
		elem=(void*)getListElement_CaseIn(structVar,fname);
	}
	return elem;
}
#define IS_SCALAR(x,type) (TYPEOF((SEXP)x)==(type) && XLENGTH((SEXP)x)==1)
F64    GetScalar(const void * ptr) { 
	if (TYPEOF((SEXP)ptr)==INTSXP)
		return (F64)asInteger((SEXP)ptr);
	else if (TYPEOF((SEXP)ptr)==REALSXP)
		return (F64)asReal((SEXP)ptr);
	else if (TYPEOF((SEXP)ptr)==LGLSXP)
		return asLogical((SEXP)ptr);
	return getNaN();
}
void * GetData(const void * ptr) { 
	if (TYPEOF((SEXP)ptr)==INTSXP)
		return INTEGER((SEXP)ptr);
	else if (TYPEOF((SEXP)ptr)==REALSXP)
		return REAL((SEXP)ptr);
	else if (TYPEOF((SEXP)ptr)==LGLSXP)
		return LOGICAL((SEXP)ptr);
	return NULL;
}
int   GetDim1(const void * ptr) {
	SEXP dims=PROTECT( getAttrib((SEXP)ptr,R_DimSymbol) );
	int dim1=INTEGER(dims)[0];	
	UNPROTECT(1);
	return dim1;
}
int   GetDim2(const void * ptr) {
	SEXP dims=PROTECT(getAttrib((SEXP)ptr,R_DimSymbol));
	int  dim2=INTEGER(dims)[1];
	UNPROTECT(1);
	return dim2;
}
int   GetNumOfDim(const void * ptr) {
	SEXP dims=PROTECT(getAttrib(ptr,R_DimSymbol));
	int  ndims=Rf_length(dims);
	UNPROTECT(1);
	return ndims;
}
void GetDimensions(const void * ptr,int dims[],int ndims) {
	int  N=min(ndims,GetNumOfDim(ptr));
	SEXP DIMS=PROTECT(getAttrib((SEXP)ptr,R_DimSymbol));
	for (int i=0; i < N; i++)
	{
		dims[i]=INTEGER(DIMS)[i];
	}
	UNPROTECT(1);
}
int GetNumberOfElements(const void * ptr) {	return Rf_length((SEXP)ptr);}
void * GetCellElement(const void * ptr,I32 idx) {		return NULL; }
int IsClass(void* ptr,char* class) {
	SEXP klass;	
	if (OBJECT(ptr)) {
		klass=getAttrib(ptr,R_ClassSymbol);
		for (int i=0; i < length(klass); i++) {
			if (strcmp(CHAR(STRING_ELT(klass,i)),class)==0)
				return 1;
		}		
	}
	return 0;
}
int IsCell(void* ptr)    { return 0L; }
int IsChar(void* ptr)    { return TYPEOF((SEXP)ptr)==STRSXP; }
int IsStruct(void* ptr)  { return isNewList((SEXP)ptr);       }
int IsNumeric(void* ptr) { return isNumeric((SEXP)ptr); }
int IsDouble(void* ptr)  { return TYPEOF((SEXP)ptr)==REALSXP; }
int IsSingle(void* ptr)  { return 0; }
int IsInt32(void* ptr)   { return TYPEOF((SEXP)ptr)==INTSXP;  }
int IsInt16(void* ptr) { return 0; }
int IsInt64(void* ptr)   { return 0; }
int IsLogical(void* ptr) { return TYPEOF((SEXP)ptr)==LGLSXP; };
void * CreateStructVar(FIELD_ITEM *fieldList,int nfields)
{ 
	SEXP LIST;
	SEXP NAMES;
	int  nprt=0L;
	PROTECT(LIST=allocVector(VECSXP,nfields));++nprt;
	PROTECT(NAMES=allocVector(STRSXP,nfields));++nprt;
	for (I64 i=0; i < nfields; i++)
		SET_STRING_ELT(NAMES,i,mkChar(fieldList[i].name));
	SEXP dims4d;
	PROTECT(dims4d=allocVector(INTSXP,4));++nprt;
	SEXP tmpSEXP;
	for (I32 i=0; i < nfields; i++) 	{	 
		if (fieldList[i].ptr==NULL) {
			SET_VECTOR_ELT(LIST,i,R_NilValue);
			continue;
		} 
		if (fieldList[i].type==DATA_STRUCT) {			
			tmpSEXP=fieldList[i].ptr;
			SET_VECTOR_ELT(LIST,i,tmpSEXP);
		}
		else if (fieldList[i].type==DATA_INT32||fieldList[i].type==DATA_INT64)
		{		
			switch (fieldList[i].ndim) {
			case 1:
				PROTECT( tmpSEXP=allocVector(INTSXP,fieldList[i].dims[0]) );   
				*(fieldList[i].ptr)=INTEGER(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			case 2:
				PROTECT(tmpSEXP=allocMatrix(INTSXP,fieldList[i].dims[0],fieldList[i].dims[1]) );				
				*(fieldList[i].ptr)=INTEGER(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1); 
				break;
			case 3:
				PROTECT(tmpSEXP=alloc3DArray(INTSXP,fieldList[i].dims[0],fieldList[i].dims[1],fieldList[i].dims[2]));				
				*(fieldList[i].ptr)=INTEGER(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			case 4:
				INTEGER(dims4d)[0]=fieldList[i].dims[0];
				INTEGER(dims4d)[1]=fieldList[i].dims[1];
				INTEGER(dims4d)[2]=fieldList[i].dims[2];
				INTEGER(dims4d)[3]=fieldList[i].dims[4];
				PROTECT( tmpSEXP=allocArray(INTSXP,dims4d) );	 
				*(fieldList[i].ptr)=INTEGER(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			}
		} 
		else {
			switch (fieldList[i].ndim) {
			case 1:
				PROTECT(tmpSEXP=allocVector(REALSXP,fieldList[i].dims[0]));   
				*(fieldList[i].ptr)=REAL(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			case 2:
				PROTECT(tmpSEXP=allocMatrix(REALSXP,fieldList[i].dims[0],fieldList[i].dims[1]));				
				*(fieldList[i].ptr)=REAL(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			case 3:
				PROTECT(tmpSEXP=alloc3DArray(REALSXP,fieldList[i].dims[0],fieldList[i].dims[1],fieldList[i].dims[2]));
				*(fieldList[i].ptr)=REAL(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			case 4:
				INTEGER(dims4d)[0]=fieldList[i].dims[0];
				INTEGER(dims4d)[1]=fieldList[i].dims[1];
				INTEGER(dims4d)[2]=fieldList[i].dims[2];
				INTEGER(dims4d)[3]=fieldList[i].dims[3];
				PROTECT(tmpSEXP=allocArray(REALSXP,dims4d));  
				*(fieldList[i].ptr)=REAL(tmpSEXP);
				SET_VECTOR_ELT(LIST,i,tmpSEXP);
				UNPROTECT(1);
				break;
			} 
		} 
	}
	setAttrib(LIST,R_NamesSymbol,NAMES);
	UNPROTECT(nprt);
	return (void*)LIST;
}
void  DestoryStructVar(VOID_PTR strutVar) {
}
void AddStringAttribute(VOID_PTR listVar,const char * field,const char *value) {
	SEXP  tmp=PROTECT(mkString(value)); 
	if (strcmp(field,"class")==0) {
		setAttrib(listVar,R_ClassSymbol,tmp);
	}else{
		setAttrib(listVar,install(field),tmp);
	}
	UNPROTECT(1);
}
void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value) {
	SEXP  tmp=PROTECT(ScalarInteger(value));
	setAttrib(listVar,install(field),tmp);	
	UNPROTECT(1);
}
void RemoveAttribute(VOID_PTR listVar,const char* field) {
	SEXP  sym=PROTECT( install(field)  );
	SEXP  attr=getAttrib(listVar,sym);
	if (!Rf_isNull(attr)) 
		setAttrib(listVar,sym,R_NilValue);
	UNPROTECT(1);
}
static void __chkIntFn(void *dummy) {	R_CheckUserInterrupt();}
I32  CheckInterrupt()        {	return (R_ToplevelExec(__chkIntFn,NULL)==FALSE);}
void ConsumeInterruptSignal() { return ; }
#if defined(WIN64_OS)  && 0
	#define WIN32_LEAN_AND_MEAN
	#include "windows.h"
	#include "Rembedded.h" 
	static void * GetReadConsole(void) 	{
		int(*R_ReadConsole)(const char* prompt,unsigned char* buf,int len,int addtohistory);
		R_ReadConsole=NULL;
		char  dll[1000];
		char *RHOME=get_R_HOME();		
		strncpy(dll,RHOME,1000);
		strcat(dll,"/bin/x64/R.dll");
		HINSTANCE hinstLib=LoadLibrary(dll);
		if (hinstLib !=NULL) {			
			R_ReadConsole=GetProcAddress(hinstLib,"R_ReadConsole");
			FreeLibrary(hinstLib);
		}
		return R_ReadConsole;
	}
	 void* GetR_interrupts_pending(void) {
		char* RHOME=get_R_HOME();
		char  dll[1000];
		strncpy(dll,RHOME,1000);
		strcat(dll,"/bin/x64/R.dll");
		HINSTANCE hinstLib;
		hinstLib=LoadLibrary(dll);
		int(*R_ReadConsole)(const char* prompt,unsigned char* buf,int len,int addtohistory);
		void * R_interrupts_pending=GetProcAddress(hinstLib,"R_interrupts_pending");
		FreeLibrary(hinstLib);
		return R_interrupts_pending;
	}
	bool GetUserInput(void * nullPtr)
	{
		int (*R_ReadConsole)(const char *prompt,unsigned char *buf,int len,int addtohistory);
		R_ReadConsole=GetReadConsole();
		char str[100];
		str[0]=0;
		while (stricmp(str,"exit") !=0) {
			R_ReadConsole("",str,10,0);
			str[4]=0;
		}
		r_printf("Program is forcely terminated ...");
		IDE_USER_INTERRUPT=1L;
	}
#endif
#elif M_INTERFACE==1
bool utIsInterruptPending();
void utSetInterruptPending(bool);
I32 CheckInterrupt() { return utIsInterruptPending(); }
void ConsumeInterruptSignal() { utSetInterruptPending(false); }
I32  GetNumberOfFields(const void* structVar) { return mxGetNumberOfFields(structVar); }
VOID_PTR GetFieldByIdx(VOID_PTR ptr,I32 ind) {
	if (IsCell(ptr)) {
		return  mxGetCell(ptr,ind);
	}
	if (IsStruct(ptr)) {
		mxGetFieldByNumber(ptr,0,ind);
	}
	return NULL;
}
I32 GetCharArray(void* ptr,char* dst,int n) {
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
		return 0L;
	}
}
I32 GetCharVecElem(void* ptr,int idx,char* dst,int n) {
	int len=0;
	if (IsCell(ptr) ) {
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
			return 0L;
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
		return 0L;
	}
}
void * GetField(const void * structVar,char *fname) {	
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
F64    GetScalar(const void * ptr) { 	return mxGetScalar((mxArray *)ptr); }
void * GetData(const void * ptr) {   return mxGetData((mxArray *)ptr); }
int    GetDim1(const void * ptr) { return mxGetM((mxArray *)ptr); }
int    GetDim2(const void * ptr) { return mxGetN((mxArray *)ptr); }
int    GetNumOfDim(const void * ptr) { return mxGetNumberOfDimensions((mxArray *)ptr); }
void   GetDimensions(const void * ptr,int dims[],int ndims) {
	int N=min(ndims,GetNumOfDim(ptr) );
	const mwSize *dimArr=mxGetDimensions((mxArray *)ptr);
	for (int i=0; i < N; i++)
	{
		dims[i]=dimArr[i];
	}	
}
int    GetNumberOfElements(const void * ptr)
{ 
	return mxGetNumberOfElements(ptr);
}
int IsChar(void* ptr)    { 
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
int IsClass(void* ptr,char* class) { return 0 }
int IsStruct(void* ptr)  { return mxIsStruct(ptr); }
int IsCell(void* ptr)    { return mxIsCell(ptr); }
int IsNumeric(void* ptr) { return mxIsNumeric(ptr); }
int IsDouble(void* ptr)  { return mxIsDouble(ptr); }
int IsSingle(void* ptr)  { return mxIsSingle(ptr); }
int IsInt32(void* ptr)   { return mxIsInt32(ptr); }
int IsInt16(void* ptr) { return mxIsInt16(ptr); }
int IsInt64(void* ptr) { return mxIsInt64(ptr); }
int IsLogical(void* ptr) { return mxIsLogical(ptr); }
void * CreateStructVar(FIELD_ITEM *fieldList,int nfields)
{ 
	mxArray * _restrict out;
	{
		char *fildNames[100];
		for (I64 i=0; i < nfields; i++)	
			fildNames[i]=fieldList[i].name;		
		mwSize dims_2d[2]={ 1,1 };
		out=mxCreateStructArray(2,dims_2d,nfields,fildNames);
	}
	for (rI64 i=0; i < nfields; i++)
	{	 
		if (fieldList[i].ptr==NULL) continue;
		if (fieldList[i].type==DATA_STRUCT){
			mxSetField(out,0L,fieldList[i].name,fieldList[i].ptr);
			continue;
		}
		mxClassID fieldDataType;
		switch (fieldList[i].type)
		{
		case DATA_FLOAT: 
			fieldDataType=mxSINGLE_CLASS;		break;
		case DATA_DOUBLE:
			fieldDataType=mxDOUBLE_CLASS;		break;
		case DATA_INT32:
			fieldDataType=mxINT32_CLASS;		break;
		default:
			fieldDataType=mxSINGLE_CLASS;			
		}
		mxArray * _restrict mxPointer;
		if ( fieldList[i].ndim==2)
		{
			mxPointer=mxCreateNumericMatrix(fieldList[i].dims[0],fieldList[i].dims[1],fieldDataType,mxREAL);
			mxSetField(out,0L,fieldList[i].name,mxPointer);
			*(fieldList[i].ptr)=mxGetData(mxPointer);			
		}
		else if (fieldList[i].ndim >=3)
		{
			mwSize DIMS[4]={ fieldList[i].dims[0],fieldList[i].dims[1],fieldList[i].dims[2],fieldList[i].dims[3] };
			mxPointer=mxCreateNumericArray(fieldList[i].ndim,DIMS,fieldDataType,mxREAL);
			mxSetField(out,0,fieldList[i].name,mxPointer);	
			*(fieldList[i].ptr)=mxGetData(mxPointer);
		}
	}
	return (void*)out;
}
void  DestoryStructVar(VOID_PTR strutVar) {
		mxDestroyArray(strutVar);
}
void AddStringAttribute(VOID_PTR listVar,const char* field,const char* value) {}
void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value) {}
void RemoveAttribute(VOID_PTR listVar,const char* field) {
}
I32 GetConsoleWidth()
{
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
#endif
int  HaveEqualDimesions(const void* p1,const void* p2) {
	int dim1=GetNumOfDim(p1);
	int dim2=GetNumOfDim(p2);
	if (dim1 !=dim2) return 0;
	I32 dims1[5],dims2[5];
	GetDimensions(p1,dims1,dim1);
	GetDimensions(p2,dims2,dim2);
	I32 equal=1;
	for (int i=0; i < dim1;++i) {
		equal=equal & (dims1[i]==dims2[i]);
	}
	return equal;
}
int GetDataType(VOID_PTR Y) {
	if      (IsInt32(Y)) 						return DATA_INT32;
	else if (IsInt16(Y)) 						return DATA_INT16;
	else if (IsDouble(Y))  		return DATA_DOUBLE;
	else if (IsSingle(Y))   		return DATA_FLOAT;
	else                                        return DATA_UNKNOWN;
}
F64  GetNumericElement(const void* Y,I32 idx)
{
	if (!IsNumeric(Y)) {
		return getNaN();
	}
	I32 n=GetNumberOfElements(Y);
	if (n==1) {
		if (idx==0)		return GetScalar(Y);
		else       		return getNaN();
	} else {
		if (idx < n) {
			VOID_PTR y=GetData(Y);
			if (IsInt32(Y)) 						    return *((I32PTR)y+idx);
			else if (IsInt16(Y)) 						return *((I16PTR)y+idx);
			else if (IsDouble(Y))  		return *((F64PTR)y+idx);
			else if (IsSingle(Y))   		return *((F32PTR)y+idx);
			else                                        return getNaN();
		}		else {
			return getNaN();
		}
	}
}
#include "abc_000_warning.h"
