#include <math.h>
#include <string.h>
#include "assert.h"
#include "abc_000_warning.h"
#include "abc_ide_util.h"
#include "abc_common.h"
#include "abc_date.h"
#if P_INTERFACE==1  
#include "limits.h"
void** PyArray_API=NULL;
typedef unsigned char npy_bool;
#define NPY_SIZEOF_SHORT SIZEOF_SHORT
#define NPY_SIZEOF_INT   SIZEOF_INT
#define NPY_SIZEOF_LONG SIZEOF_LONG
#define NPY_SIZEOF_FLOAT 4
#define NPY_SIZEOF_COMPLEX_FLOAT 8
#define NPY_SIZEOF_DOUBLE 8
#define NPY_SIZEOF_COMPLEX_DOUBLE 16
#define NPY_SIZEOF_LONGDOUBLE 8
#define NPY_SIZEOF_COMPLEX_LONGDOUBLE 16
#define NPY_SIZEOF_PY_INTPTR_T 8
#define NPY_SIZEOF_OFF_T 4
#define NPY_SIZEOF_PY_LONG_LONG 8
#define NPY_SIZEOF_LONGLONG 8
#define NPY_SIZEOF_CHAR 1
#define NPY_SIZEOF_BYTE 1
#define NPY_SIZEOF_DATETIME 8
#define NPY_SIZEOF_TIMEDELTA 8
#define NPY_SIZEOF_INTP  NPY_SIZEOF_PY_INTPTR_T
#define NPY_SIZEOF_UINTP NPY_SIZEOF_PY_INTPTR_T
#define NPY_SIZEOF_HALF 2
#define NPY_BITSOF_BOOL (sizeof(npy_bool) * CHAR_BIT)
#define NPY_BITSOF_CHAR CHAR_BIT
#define NPY_BITSOF_BYTE (NPY_SIZEOF_BYTE * CHAR_BIT)
#define NPY_BITSOF_SHORT (NPY_SIZEOF_SHORT * CHAR_BIT)
#define NPY_BITSOF_INT (NPY_SIZEOF_INT * CHAR_BIT)
#define NPY_BITSOF_LONG (NPY_SIZEOF_LONG * CHAR_BIT)
#define NPY_BITSOF_LONGLONG (NPY_SIZEOF_LONGLONG * CHAR_BIT)
#define NPY_BITSOF_INTP (NPY_SIZEOF_INTP * CHAR_BIT)
#define NPY_BITSOF_HALF (NPY_SIZEOF_HALF * CHAR_BIT)
#define NPY_BITSOF_FLOAT (NPY_SIZEOF_FLOAT * CHAR_BIT)
#define NPY_BITSOF_DOUBLE (NPY_SIZEOF_DOUBLE * CHAR_BIT)
#define NPY_BITSOF_LONGDOUBLE (NPY_SIZEOF_LONGDOUBLE * CHAR_BIT)
#if NPY_BITSOF_LONG==8
#define NPY_INT8 NPY_LONG
#define NPY_UINT8 NPY_ULONG
#elif NPY_BITSOF_LONG==16
#define NPY_INT16 NPY_LONG
#define NPY_UINT16 NPY_ULONG
#elif NPY_BITSOF_LONG==32
#define NPY_INT32 NPY_LONG
#define NPY_UINT32 NPY_ULONG 
#elif NPY_BITSOF_LONG==64
#define NPY_INT64 NPY_LONG
#define NPY_UINT64 NPY_ULONG
#elif NPY_BITSOF_LONG==128
#define NPY_INT128 NPY_LONG
#define NPY_UINT128 NPY_ULONG
#endif
#if NPY_BITSOF_LONGLONG==8
#  ifndef NPY_INT8
#    define NPY_INT8 NPY_LONGLONG
#    define NPY_UINT8 NPY_ULONGLONG
#  endif
#elif NPY_BITSOF_LONGLONG==16
#  ifndef NPY_INT16
#    define NPY_INT16 NPY_LONGLONG
#    define NPY_UINT16 NPY_ULONGLONG
#  endif
#elif NPY_BITSOF_LONGLONG==32
#  ifndef NPY_INT32
#    define NPY_INT32 NPY_LONGLONG
#    define NPY_UINT32 NPY_ULONGLONG
#  endif
#elif NPY_BITSOF_LONGLONG==64
#  ifndef NPY_INT64
#    define NPY_INT64 NPY_LONGLONG
#    define NPY_UINT64 NPY_ULONGLONG
#  endif
#elif NPY_BITSOF_LONGLONG==128
#  ifndef NPY_INT128
#    define NPY_INT128 NPY_LONGLONG
#    define NPY_UINT128 NPY_ULONGLONG
#  endif
#elif NPY_BITSOF_LONGLONG==256
#  define NPY_INT256 NPY_LONGLONG
#  define NPY_UINT256 NPY_ULONGLONG
#endif
#if NPY_BITSOF_INT==8
#ifndef NPY_INT8
#define NPY_INT8 NPY_INT
#define NPY_UINT8 NPY_UINT
#endif
#elif NPY_BITSOF_INT==16
#ifndef NPY_INT16
#define NPY_INT16 NPY_INT
#define NPY_UINT16 NPY_UINT
#endif
#elif NPY_BITSOF_INT==32
#ifndef NPY_INT32
#define NPY_INT32 NPY_INT
#define NPY_UINT32 NPY_UINT 
#endif
#elif NPY_BITSOF_INT==64
#ifndef NPY_INT64
#define NPY_INT64 NPY_INT
#define NPY_UINT64 NPY_UINT
#endif
#elif NPY_BITSOF_INT==128
#ifndef NPY_INT128
#define NPY_INT128 NPY_INT
#define NPY_UINT128 NPY_UINT
#endif
#endif
#if NPY_BITSOF_SHORT==8
#ifndef NPY_INT8
#define NPY_INT8 NPY_SHORT
#define NPY_UINT8 NPY_USHORT
#endif
#elif NPY_BITSOF_SHORT==16
#ifndef NPY_INT16
#define NPY_INT16 NPY_SHORT
#define NPY_UINT16 NPY_USHORT
#endif
#elif NPY_BITSOF_SHORT==32
#ifndef NPY_INT32
#define NPY_INT32 NPY_SHORT
#define NPY_UINT32 NPY_USHORT
#endif
#elif NPY_BITSOF_SHORT==64
#ifndef NPY_INT64
#define NPY_INT64 NPY_SHORT
#define NPY_UINT64 NPY_USHORT
#endif
#elif NPY_BITSOF_SHORT==128
#ifndef NPY_INT128
#define NPY_INT128 NPY_SHORT
#define NPY_UINT128 NPY_USHORT
#endif
#endif
#if NPY_BITSOF_CHAR==8
#ifndef NPY_INT8
#define NPY_INT8 NPY_BYTE
#define NPY_UINT8 NPY_UBYTE 
#endif
#elif NPY_BITSOF_CHAR==16
#ifndef NPY_INT16
#define NPY_INT16 NPY_BYTE
#define NPY_UINT16 NPY_UBYTE
#endif
#elif NPY_BITSOF_CHAR==32
#ifndef NPY_INT32
#define NPY_INT32 NPY_BYTE
#define NPY_UINT32 NPY_UBYTE
#endif
#elif NPY_BITSOF_CHAR==64
#ifndef NPY_INT64
#define NPY_INT64 NPY_BYTE
#define NPY_UINT64 NPY_UBYTE
#endif
#elif NPY_BITSOF_CHAR==128
#ifndef NPY_INT128
#define NPY_INT128 NPY_BYTE
#define NPY_UINT128 NPY_UBYTE
#endif
#endif
#if NPY_BITSOF_DOUBLE==32
#ifndef NPY_FLOAT32
#define NPY_FLOAT32 NPY_DOUBLE
#define NPY_COMPLEX64 NPY_CDOUBLE
#endif
#elif NPY_BITSOF_DOUBLE==64
#ifndef NPY_FLOAT64
#define NPY_FLOAT64 NPY_DOUBLE
#define NPY_COMPLEX128 NPY_CDOUBLE
#endif
#elif NPY_BITSOF_DOUBLE==80
#ifndef NPY_FLOAT80
#define NPY_FLOAT80 NPY_DOUBLE
#define NPY_COMPLEX160 NPY_CDOUBLE
#endif
#elif NPY_BITSOF_DOUBLE==96
#ifndef NPY_FLOAT96
#define NPY_FLOAT96 NPY_DOUBLE
#define NPY_COMPLEX192 NPY_CDOUBLE
#endif
#elif NPY_BITSOF_DOUBLE==128
#ifndef NPY_FLOAT128
#define NPY_FLOAT128 NPY_DOUBLE
#define NPY_COMPLEX256 NPY_CDOUBLE
#endif
#endif
#if NPY_BITSOF_FLOAT==32
#ifndef NPY_FLOAT32
#define NPY_FLOAT32 NPY_FLOAT
#define NPY_COMPLEX64 NPY_CFLOAT
#endif
#elif NPY_BITSOF_FLOAT==64
#ifndef NPY_FLOAT64
#define NPY_FLOAT64 NPY_FLOAT
#define NPY_COMPLEX128 NPY_CFLOAT
#endif
#elif NPY_BITSOF_FLOAT==80
#ifndef NPY_FLOAT80
#define NPY_FLOAT80 NPY_FLOAT
#define NPY_COMPLEX160 NPY_CFLOAT
#endif
#elif NPY_BITSOF_FLOAT==96
#ifndef NPY_FLOAT96
#define NPY_FLOAT96 NPY_FLOAT
#define NPY_COMPLEX192 NPY_CFLOAT
#endif
#elif NPY_BITSOF_FLOAT==128
#ifndef NPY_FLOAT128
#define NPY_FLOAT128 NPY_FLOAT
#define NPY_COMPLEX256 NPY_CFLOAT
#endif
#endif
typedef Py_intptr_t  npy_intp;
typedef Py_uintptr_t npy_uintp;
typedef enum {
    NPY_ANYORDER=-1,  
    NPY_CORDER=0,     
    NPY_FORTRANORDER=1,        
    NPY_KEEPORDER=2            
} NPY_ORDER;
typedef struct {
    npy_intp* ptr;
    int len;
} PyArray_Dims;
#define NPY_ARRAY_F_CONTIGUOUS    0x0002
#define NPY_ARRAY_ALIGNED         0x0100
#define NPY_ARRAY_WRITEABLE       0x0400
#define NPY_ARRAY_BEHAVED         (NPY_ARRAY_ALIGNED|NPY_ARRAY_WRITEABLE)
#define NPY_ARRAY_FARRAY          (NPY_ARRAY_F_CONTIGUOUS|NPY_ARRAY_BEHAVED)
#define PyArray_Type           (*(PyTypeObject *)             PyArray_API[2])
#define PyArray_DescrFromType  (*(PyArray_Descr * (*)(int))   PyArray_API[45])
#define PyArray_Size           (*(npy_intp (*)(PyObject *))   PyArray_API[59])
#define PyArray_NewFromDescr   (*(PyObject * (*)(PyTypeObject *,PyArray_Descr *,int,npy_intp const *,npy_intp const *,void *,int,PyObject *)) \
                              PyArray_API[94])		 
#define PyArray_FromArray      (*(PyObject * (*)(PyArrayObject *,PyArray_Descr *,int))                   PyArray_API[109])		 
#define PyArray_CheckFromAny   (*(PyObject * (*)(PyObject *,PyArray_Descr *,int,int,int,PyObject *))  PyArray_API[108])	
#define PyArray_Newshape       (*(PyObject * (*)(PyArrayObject *,PyArray_Dims *,NPY_ORDER))              PyArray_API[135])		 
#define PyArray_GetPtr         (*(void * (*)(PyArrayObject *,npy_intp const*))                            PyArray_API[160])
#define PyArray_MultiplyList   (*(npy_intp (*)(npy_intp const *,int))                                     PyArray_API[158])
#define PyArray_DIMS(obj)       (((PyArrayObject_fields *)(obj))->dimensions)
#define PyArray_FROM_OF(m,flags) PyArray_CheckFromAny(m,NULL,0,0,flags,NULL)		 
#define PyArray_Check(op)        PyObject_TypeCheck(op,&PyArray_Type)		 
#define PyArray_SIZE(m)          PyArray_MultiplyList(PyArray_DIMS(m),PyArray_NDIM(m))
#define NPY_ATTR_DEPRECATE(text)
enum NPY_TYPES {
    NPY_BOOL=0,NPY_BYTE,NPY_UBYTE,NPY_SHORT,NPY_USHORT,
    NPY_INT,NPY_UINT,NPY_LONG,NPY_ULONG,NPY_LONGLONG,NPY_ULONGLONG,
    NPY_FLOAT,NPY_DOUBLE,NPY_LONGDOUBLE,NPY_CFLOAT,NPY_CDOUBLE,NPY_CLONGDOUBLE,
    NPY_OBJECT=17,NPY_STRING,NPY_UNICODE,NPY_VOID,
    NPY_DATETIME,NPY_TIMEDELTA,NPY_HALF,NPY_NTYPES,NPY_NOTYPE,
    NPY_CHAR NPY_ATTR_DEPRECATE("Use NPY_STRING"),
    NPY_USERDEF=256,  
    NPY_NTYPES_ABI_COMPATIBLE=21 
};
typedef Py_hash_t npy_hash_t;
typedef struct  PyArrayObject PyArrayObject;
typedef struct  PyArray_ArrFuncs PyArray_ArrFuncs;
typedef struct  NpyAuxData  NpyAuxData;
typedef struct  NpyAuxData  NpyAuxData;
typedef struct _PyArray_Descr {
    PyObject_HEAD
        PyTypeObject* typeobj;
    char kind;  
    char type; 
    char byteorder; 
    char flags;    
    int type_num;  
    int elsize;    
    int alignment; 
    struct _arr_descr* subarray;   
    PyObject* fields; 
    PyObject* names; 
    PyArray_ArrFuncs* f;  
    PyObject* metadata;          
    NpyAuxData* c_metadata; 
    npy_hash_t hash; 
} PyArray_Descr;
typedef struct tagPyArrayObject_fields {
    PyObject_HEAD
        char* data;  
    int nd;      
    npy_intp* dimensions; 
    npy_intp* strides;     
    PyObject* base;
    PyArray_Descr* descr;     
    int flags;                
    PyObject* weakreflist;    
    void* _buffer_info;       
} PyArrayObject_fields;
static int       PyArray_TYPE(const PyArrayObject* arr) { return ((PyArrayObject_fields*)arr)->descr->type_num; }
static void* PyArray_DATA(PyArrayObject* arr) { return ((PyArrayObject_fields*)arr)->data; }
static npy_intp  PyArray_ITEMSIZE(const PyArrayObject* arr) { return ((PyArrayObject_fields*)arr)->descr->elsize; }
static int       PyArray_NDIM(const PyArrayObject* arr) { return ((PyArrayObject_fields*)arr)->nd; }
int  JDN_to_DateNum(int jdn) {
    return jdn - JulianDayNum_from_civil_ag1(1,1,1);
}
void StdouFlush(void) {
    PyObject* sysout=PySys_GetObject("__stdout__");           
    PyObject* pfflush=PyObject_GetAttrString(sysout,"flush");  
    int       ok=PyCallable_Check(pfflush); 
    PyObject* out=PyObject_CallObject(pfflush,NULL);		 
    Py_XDECREF(pfflush);
    Py_XDECREF(out);
}
PyObject* currentModule;
PyObject* classOutput=NULL;
PyObject* setClassObjects(PyObject* self,PyObject* args){
    if (PyTuple_Size(args)==1) {
        classOutput=PyTuple_GetItem(args,0);
    }
    return Py_None;
}
#define  Pob PyObject *
void* CvtToPyArray_NewRef(VOIDPTR Y) {
    if (PyArray_Check(Y)) {
        PyArray_Descr* reqDescr=PyArray_DescrFromType(PyArray_TYPE(Y));
        Pob            out=PyArray_FromArray(Y,reqDescr,NPY_ARRAY_FARRAY);
        return out;
    }   else {
        Pob            out=PyArray_FROM_OF(Y,NPY_ARRAY_FARRAY);
        return out;
    }
    return NULL;
}
typedef PyObject* (*PyGetItem)(PyObject*,Py_ssize_t);
static void* PyGetDict(void *ptr) {
    if (PyDict_Check(ptr)) {
        return ptr;
    }
    if (PyList_Check(ptr)||PyTuple_Check(ptr)) {
        return NULL;
    }
    if (PyLong_Check(ptr)||PyFloat_Check(ptr)) {
        return NULL;
    }
    if (PyObject_IsInstance(ptr,(PyObject *) & PyBaseObject_Type)) {
        PyObject** dictpr=_PyObject_GetDictPtr(ptr);
        if (dictpr !=NULL) {
            PyObject* Dict=*dictpr;
            return Dict;
        }
        if (!PyArray_Check(ptr) && Py_TYPE(ptr)->tp_dict !=NULL) {
            PyObject* Dict=Py_TYPE(ptr)->tp_dict;
            return Dict;
        }
    }
    return NULL;
}
static void* PyGetDictItem(void *ptr,int idx) {
    PyObject* values=PyDict_Values(ptr);              
    Pob       item=PyList_GetItem(values,idx);
    Py_DECREF(values);
    return item;
}
static void* PyGetDictItemString(void *ptr,char *fldname) {
    PyObject* item=PyDict_GetItemString(ptr,fldname); 
    if (item) { 
        return item; 
    }
    char tmpName[100+1];
    PyObject* keys=PyDict_Keys(ptr);              
    int       n=PyList_Size(keys);
    item=NULL;
    for (int i=0; i < n; i++) {
        PyObject* tmpkey=PyList_GetItem(keys,i);  
        int       len=GetCharArray(tmpkey,tmpName,100);
        if (len > 0 && strcicmp(tmpName,fldname)==0) {
            item=PyDict_GetItem(ptr,tmpkey); 
            break;
        }
    }
    Py_DECREF(keys);
    return item;
}
static void* PyGetDictItemString123(void *ptr,char *fldname,int nPartial) {
    PyObject* item=PyDict_GetItemString(ptr,fldname); 
    if (item) { 
        return item; 
    }
    char tmpName[100+1];
    PyObject* keys=PyDict_Keys(ptr);              
    int       n=PyList_Size(keys);
    item=NULL;
    for (int i=0; i < n; i++) {
        PyObject* tmpkey=PyList_GetItem(keys,i);  
        int       len=GetCharArray(tmpkey,tmpName,100);
        if (len > 0 && strcicmp_nfirst(tmpName,fldname,nPartial)==0) {
            item=PyDict_GetItem(ptr,tmpkey); 
            break;
        }
    }
    Py_DECREF(keys);
    return item;
}
int IsClass(void* ptr,char* class) { return 0; }
int IsCell(void* ptr) { return 0; }
int IsChar(void* ptr) {   
    if (ptr==NULL) return 0;
    if (PyUnicode_Check(ptr)) {
        return 1;
    } 
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_STRING) {
        return 1;
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_UNICODE) {
        return 1;
    }
    PyGetItem pyGetItem=NULL;
    if      (PyList_Check(ptr))        pyGetItem=PyList_GetItem;
    else if (PyTuple_Check(ptr))       pyGetItem=PyDict_GetItem;
    if(pyGetItem) {      
        int sz=(int) PyObject_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if ( !PyUnicode_Check(pyGetItem(ptr,i) ) ) {
                ok=0; 
                break;
            }
        }
        if (ok) { return 1; }
    }
    if (PyArray_Check(ptr) && (PyArray_TYPE(ptr)==NPY_STRING||PyArray_TYPE(ptr)==NPY_UNICODE)  ){
        return 1;
    }
    if (PyArray_Check(ptr)  && PyArray_TYPE(ptr)==NPY_OBJECT) {
        void** base=(void**) PyArray_DATA(ptr);
        int sz=(int)    PyArray_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyUnicode_Check(base[i])) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    return 0;
}
int IsStruct(void* ptr) {
    if (ptr==NULL) return 0;
    if (PyList_Check(ptr)||PyDict_Check(ptr)||PyTuple_Check(ptr) ) {
        return 1L;
    }
    Pob dict=PyGetDict(ptr);
    if (dict) {
        if (PyUnicode_Check(ptr)) {
            return 0;
        }  else {
            return 1;
        }        
    }
    return 0L;
}
int IsEmpty( void* ptr) {  return (ptr==Py_None||GetNumberOfElements(ptr)==0);}
int IsSingle(void* ptr) {
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_FLOAT32) {
        return 1L;        
    }
    return 0L;
}
int IsDouble(void* ptr) {
    if (PyFloat_Check(ptr)) {
        return 1L;
    } 
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_FLOAT64) {
        return 1L;
    }
    PyGetItem pyGetItem=NULL;
    if (PyList_Check(ptr))           pyGetItem=PyList_GetItem;
    else if (PyTuple_Check(ptr))     pyGetItem=PyDict_GetItem;
    if (pyGetItem) {
        int sz=PyObject_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyFloat_Check(pyGetItem(ptr,i))) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_OBJECT) {
        void** base=(void**)PyArray_DATA(ptr);
        int sz=(int)PyArray_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyFloat_Check(base[i])) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    return 0;
}
int IsInt16(void* ptr) {   
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_INT16) {
        return 1L;
    }
    return 0;
}
int IsInt32(void* ptr) {
    if (PyLong_Check(ptr)) {
        return 1L;
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_INT32) {
        return 1L;
    }
    PyGetItem pyGetItem=NULL;
    if      (PyList_Check(ptr))      pyGetItem=PyList_GetItem;
    else if (PyTuple_Check(ptr))     pyGetItem=PyDict_GetItem;
    if (pyGetItem) {
        int sz=PyObject_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyLong_Check(pyGetItem(ptr,i))) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_OBJECT) {
        void** base=(void**)PyArray_DATA(ptr);
        int sz=(int)PyArray_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyLong_Check(base[i])) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    return 0;
}
int IsInt64(void* ptr) {
     if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_INT64) {
         return 1L;
    }
    return 0;
}
int IsLogical(void* ptr) {
    if (PyBool_Check(ptr)) {
        return 1L;
    }  
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_BOOL) {
        return 1L;
    }
    PyGetItem pyGetItem=NULL;
    if      (PyList_Check(ptr))      pyGetItem=PyList_GetItem;
    else if (PyTuple_Check(ptr))     pyGetItem=PyDict_GetItem;
    if (pyGetItem) {
        int sz=PyObject_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyBool_Check(pyGetItem(ptr,i))) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_OBJECT) {
        void** base=(void**)PyArray_DATA(ptr);
        int sz=(int)PyArray_Size(ptr);
        int ok=1;
        for (int i=0; i < sz;++i) {
            if (!PyBool_Check(base[i])) {
                ok=0;
                break;
            }
        }
        if (ok) { return 1; }
    }
    return 0;
}
int IsNumeric(void* ptr) {
    return IsDouble(ptr)||IsSingle(ptr)||IsInt32(ptr)||IsInt16(ptr)||IsInt64(ptr)||IsLogical(ptr);
}
VOID_PTR GetFieldByIdx(VOID_PTR strucVar,I32 ind) {
    if (PyList_Check(strucVar)) {
        PyObject* out=PyList_GetItem(strucVar,ind); 
        return out;
    }  
    if (PyTuple_Check(strucVar)) {
        PyObject* out=PyTuple_GetItem(strucVar,ind); 
        return out;
    }
    if (PyDict_Check(strucVar)) {
        Pob list=PyDict_Values(strucVar);
        Pob item=PyList_GetItem(list,ind); 
        Py_DECREF(list);
        return item;
    }
    Pob dict=PyGetDict(strucVar);
    if (dict) { 
            Pob list=PyDict_Values(strucVar);
            Pob item=PyList_GetItem(list,ind); 
            Py_DECREF(list);
            return item;   
    }
    return NULL;
}
void  GetFieldNameByIdx(VOID_PTR strucVar,I32 ind0,char *str,int buflen) {
    Pob dict=PyGetDict(strucVar);
    if (dict) {
        PyObject* keys=PyDict_Keys(dict);              
        int       n=PyList_Size(keys);
        PyObject* tmpkey=PyList_GetItem(keys,ind0);  
        if (IsChar(tmpkey)) {
            int       len=GetCharArray(tmpkey,str,buflen);
        }   else {
            str[0]=0;
        }        
        Py_DECREF(keys);
    }    else {
        str[0]=0;
    }
}
void* CreateStructVar(FIELD_ITEM* fieldList,int nfields); 
void  DestoryStructVar(VOID_PTR strutVar) {  Py_XDECREF(strutVar);}
void  RemoveField(FIELD_ITEM* fieldList,int nfields,char* fieldName); 
void AddStringAttribute(VOID_PTR listVar,const char* field,const char* value) {
    Pob dict=PyGetDict(listVar);
    if (dict) {
        Pob pyValue=PyUnicode_FromString(value);
        PyObject_SetAttrString(listVar,field,pyValue); 
        Py_XDECREF(pyValue);  
    }
}
void AddIntegerAttribute(VOID_PTR listVar,const char* field,I32 value) {
    Pob dict=PyGetDict(listVar);
    if (dict) {
        Pob pyValue=PyLong_FromLong(value);
        PyObject_SetAttrString(listVar,field,pyValue); 
        Py_XDECREF(pyValue);  
    }
}
void RemoveAttribute(VOID_PTR listVar,const char* field) {
}
I32   GetConsoleWidth() {
    return 85;
    PyObject* osModule=PyImport_AddModule("os");  
    if (osModule==NULL) {
        osModule=PyImport_ImportModule("os");  
    }
    if (osModule==NULL) {
        return 80;
    }
    PyObject* get_console_size=PyObject_GetAttrString(osModule,"get_terminal_size"); 
    Pob result=PyObject_CallMethod(osModule,"get_terminal_size",NULL);                    
    Pob col=PyObject_GetAttrString(result,"columns");  
    int width=PyLong_AsLong(col);
    Py_XDECREF(get_console_size);
    Py_XDECREF(result);
    Py_XDECREF(col);
    return width;
}
I32 GetCharArray(void* ptr,char* dst,int n) {
    dst[0]=0;
    if (ptr==NULL||!IsChar(ptr)) return 0;
    if (PyUnicode_Check(ptr)) {
        Py_ssize_t  len;
        char* str=PyUnicode_AsUTF8AndSize(ptr,&len);
        strncpy(dst,str,n);
        return len;
    }
    int idx=0;
    return  GetCharVecElem(ptr,idx,dst,n);
}
I32 GetCharVecElem(void* ptr,int idx,char* dst,int n) {
    Py_ssize_t len=0; 
    PyObject* tmpItem=NULL;
    if (PyUnicode_Check(ptr) && idx==0) {
        tmpItem=ptr;
    } else   if (PyList_Check(ptr)) {
        tmpItem=PyList_GetItem(ptr,idx);
    } else  if (PyTuple_Check(ptr)) {
        tmpItem=PyTuple_GetItem(ptr,idx);
    }
    if (tmpItem && PyUnicode_Check(tmpItem)) {   
        char* str=PyUnicode_AsUTF8AndSize(tmpItem,&len);
        len=min(len,n - 1);
        memcpy(dst,str,len);
        dst[len]=0;
        return len;
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_STRING) {
        char* base=PyArray_DATA(ptr);
        int   elsize=PyArray_ITEMSIZE(ptr);
        char* str=base+elsize * idx;
        len=min(elsize,n-1);
        memcpy(dst,str,len);
        dst[len]=0;
        return len;
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_UNICODE) {
        char* base=PyArray_DATA(ptr);
        int   elsize=PyArray_ITEMSIZE(ptr);
        char* str=base+elsize * idx;
        len=min(elsize/4,n - 1);  
        for (int i=0; i < len; i++) {
            dst[i]=str[i * 4];
        }
        dst[len]=0;
        return len;
    }
    if (PyArray_Check(ptr) && PyArray_TYPE(ptr)==NPY_OBJECT) {
        void** base=PyArray_DATA(ptr);
        int    elsize=PyArray_ITEMSIZE(ptr);
        void* tmpItem=base[idx];
        if (tmpItem && PyUnicode_Check(tmpItem)) {
            char* str=PyUnicode_AsUTF8AndSize(tmpItem,&len);
            len=min(len,n - 1);
            memcpy(dst,str,len);
            dst[len]=0;
            return len;
        }
    }
    dst[len]=0;
    return 0;
}
void* GetField123(const void* structVar,char* fname,int nPartial) {
    Pob dict=NULL;
    if (PyDict_Check((void*)structVar)) {
        dict=structVar;
    }   else {
        dict=PyGetDict( (void *) structVar);
    }
    if (dict) {
        return PyGetDictItemString123(dict,fname,nPartial);
    } 
    return NULL;
}
void* GetField(const void* structVar,char* fname) {
    if (PyDict_Check(structVar)) {
        PyObject* item=PyGetDictItemString((void*)structVar,fname);
        return item;
    }  else {
        if (PyObject_HasAttrString((void*)structVar,fname)) {
            PyObject *attr=PyObject_GetAttrString((void*)structVar,fname); 
            Py_DECREF(attr);  
            return  attr;
        }        
    }
    return NULL;
}
static F64 NumpyGetNumericElemt(void* ptr,int ind) {
    int    ndim=PyArray_NDIM(ptr);
    if (ndim !=1) {
        return getNaN();
    }
    npy_intp indices[]={ ind };    
    void     *dptr=PyArray_GetPtr(ptr,indices);
    int      dtype=PyArray_TYPE(ptr);
    if (dtype==NPY_INT16) { 
        return *(int16_t*)dptr;
    }
    else if (dtype==NPY_INT32) {
        return *(int32_t*)dptr;
    }
    else if (dtype==NPY_INT64) {
        return (F64)  * (int64_t*)dptr;
    }
    else if (dtype==NPY_FLOAT) {
        return *(float*)dptr;
    }
    else if (dtype==NPY_DOUBLE) {
        return *(double*)dptr;
    }
    return getNaN();
}
F64   GetScalar(const void* ptr) {
    Pob item=ptr;
    if (PyList_Check(ptr) ) {
        item=PyList_GetItem(ptr,0);
    }
    if (PyTuple_Check(ptr)) {
        item=PyTuple_GetItem(ptr,0);
    }    
    if (PyDict_Check(ptr)) {
        item=PyGetDictItem(ptr,0);
    }
    if (PyLong_Check(ptr)) {
        return PyLong_AsLong(item);
    }
    if (PyFloat_Check(ptr)) {
        return PyFloat_AsDouble(item);
    }
    if (PyArray_Check(ptr)) {
        return NumpyGetNumericElemt(ptr,0);
    }
    return getNaN();
}
F64   GetNumericElement(const void* Y,I32 idx0) {
    I32 idx=idx0; 
    Pob item=NULL;
    if (PyList_Check(Y)) {
        item=PyList_GetItem(Y,idx);
    }
    if (PyTuple_Check(Y)) {
        item=PyTuple_GetItem(Y,idx);
    }
    if (PyDict_Check(Y)) {
        item=PyGetDictItem(Y,idx);
    }
    if (item) {
        if (PyLong_Check(item)) {
            return PyLong_AsLong(item);
        }
        if (PyFloat_Check(item)) {
            return PyFloat_AsDouble(item);
        }
    }
    if (PyArray_Check(Y)) {
        return NumpyGetNumericElemt(Y,idx);
    }
    return getNaN();
}
void* GetData(const void* ptr) { 
    return PyArray_DATA(ptr);
}
int  GetDim1(const void* ptr) {
    if (PyArray_Check(ptr)) {
        npy_intp* dims=PyArray_DIMS(ptr);
        return dims[0];
    }
    if (PyList_Check(ptr)||PyTuple_Check(ptr) ){
        return PyObject_Size(ptr);
    }
    return -9999L;
}
int  GetDim2(const void* ptr) {
    if (!PyArray_Check(ptr)) {
        return -9999L;
    }
    npy_intp* dims=PyArray_DIMS(ptr);
    return dims[1];
}
int  GetNumOfDim(const void* ptr) {
    if (PyArray_Check(ptr)) {
        return  PyArray_NDIM(ptr);        
    }
    if (PyList_Check(ptr)||PyTuple_Check(ptr)) {
        return 1L;
    }       
    return -9999L;
}
void GetDimensions(const void* ptr,int dims[],int ndims) {
    if (PyArray_Check(ptr)) {
        int       N=min(ndims,PyArray_NDIM(ptr));
        npy_intp* npdims=PyArray_DIMS(ptr);
        for (int i=0; i < N; i++) {
            dims[i]=npdims[i];
        }
    }
    if (PyList_Check(ptr)||PyTuple_Check(ptr) ) {
        dims[0]=PyObject_Size(ptr);
    }   
}
void * SetDimensions(const void* ptr,int dims[],int ndims) {
    if (PyArray_Check(ptr)) {
        PyArray_Dims newshape;
        npy_intp  newdims[100];
        newshape.len=ndims;
        newshape.ptr=newdims;
         for (int i=0; i < ndims; i++) {
             newdims[i]=dims[i];
        }
         PyObject *newptr=PyArray_Newshape(ptr,&newshape,NPY_ANYORDER);
         return newptr;
    }
    return NULL;
    if (PyList_Check(ptr)||PyTuple_Check(ptr)) {
    }
}
int  GetNumberOfElements(const void* ptr) {
    if (PyArray_Check(ptr)) {
        return PyArray_SIZE(ptr);
    }
    if (PyList_Check(ptr)||PyTuple_Check(ptr)) {
        return PyObject_Size(ptr);
    }
    if (PyUnicode_Check(ptr) ) {
        return 1;
    }
    PyObject* dict=PyGetDict(ptr);
    if (dict) {
        return PyObject_Size(dict);
    }
    if (PyLong_Check(ptr)||PyFloat_Check(ptr)) {
        return 1;
    }
    return 0;
}
I32  GetNumberOfFields(const void* structVar) {
    Pob dict=PyGetDict(structVar);
    if (dict) {
        return PyDict_Size(dict);
    }
    return -999;
}
int HaveEqualDimesions(const void* p1,const void* p2);
int CopyNumericObjToF32Arr(F32PTR outmem,VOID_PTR infield,int N);
void* CreateNumVar(DATA_TYPE dtype,int* dims,int ndims,VOIDPTR* data_ptr) {
    npy_intp dimsnp[10];
    for (int i=0; i < ndims; i++) {
        dimsnp[i]=dims[i];
    }
    int dtypenp;
    if   (dtype==DATA_INT16)
        dtypenp=NPY_INT16;
    else if (dtype==DATA_INT32)
        dtypenp=NPY_INT32;
    else if (dtype==DATA_FLOAT)
        dtypenp=NPY_FLOAT;
    else if (dtype==DATA_DOUBLE)
        dtypenp=NPY_DOUBLE;
    else if (dtype==DATA_INT64)
        dtypenp=NPY_INT64;
    else {
        *data_ptr=NULL;
        return NULL;
    }
    PyObject* array=PyArray_NewFromDescr(
        &PyArray_Type, 
        PyArray_DescrFromType(dtypenp),
        ndims,dimsnp,NULL,NULL,NPY_ARRAY_F_CONTIGUOUS|NPY_ARRAY_WRITEABLE|NPY_ARRAY_ALIGNED,NULL); 
    if (array && data_ptr) {
        *data_ptr=PyArray_DATA(array);
    }
    return array;
}
static void* pyCreateStructObject() {
    PyObject* obj;
    obj=PyObject_CallObject(classOutput,NULL);
    return obj;
}
static void pySetAddField(void *obj,char *fname,void *value) {
    if (value==NULL) return ;
    PyObject_SetAttrString(obj,fname,value); 
    Py_XDECREF(value);  
}
void* CreateStructVar(FIELD_ITEM* fieldList,int nfields) {
	int nfields_actual=0;
	for (int i=0; i < nfields;++i) {
        nfields_actual++;
        if (fieldList[i].name[0]==0) {
            nfields_actual--;
            break;
        }
	}
	nfields=nfields_actual;
	PyObject * _restrict out;
	{
        out=pyCreateStructObject(); 
	}
	for (int i=0; i < nfields; i++){	 
		if (fieldList[i].ptr==NULL) continue;
        Pob optr=NULL;
		if (fieldList[i].type==DATA_STRUCT){
            optr=(void*)fieldList[i].ptr;            
        }  else {
            optr=CreateNumVar(fieldList[i].type,fieldList[i].dims,fieldList[i].ndim,fieldList[i].ptr);
        }
        if (optr) {
            pySetAddField(out,fieldList[i].name,optr);  
        }        
	}
	return (void*)out; 
}
typedef struct {
    PyObject_HEAD
    PyObject* inst_dict;
    PyObject* weakreflist;
} BarObject;
static PyMemberDef BarObject_members[]={
    {"__dict__",T_OBJECT_EX,offsetof(BarObject,inst_dict),0,"instance's dictonary"},
    {NULL}  
};
static PyObject * BarObject_new(PyTypeObject* type,PyObject* args,PyObject* kwds) {
    BarObject* self=type->tp_alloc(type,0);
    if (self) {
        self->inst_dict=PyDict_New();
        self->weakreflist=Py_None;
       Py_INCREF(Py_None);
    }   
    r_printf("New called...%p  inst_dict %p \n",self,self->inst_dict);
    return self;
}
static int BarObject_init(BarObject* self,PyObject* args,PyObject* kwds) {
    static char* kwlist[]={ "first","last","number",NULL };
    PyObject* first=NULL,* last=NULL,* tmp;
    r_printf("Init called... %p\n",self);
    return 0;
}
static void  BarObject_dealloc(BarObject* self) {
    self->weakreflist=NULL;
    Py_DECREF(Py_None);
    Py_XDECREF(self->inst_dict);
    Py_TYPE(self)->tp_free((PyObject*)self);        
}
 PyTypeObject BarObject_Type={
  PyObject_HEAD_INIT(NULL)
  .tp_new=BarObject_new, 
  .tp_name="Rbeast.pyobject",
  .tp_base=NULL,
  .tp_basicsize=sizeof(BarObject),
  .tp_getattro=PyObject_GenericGetAttr,
  .tp_setattro=PyObject_GenericSetAttr,
  .tp_flags=Py_TPFLAGS_DEFAULT, 
  .tp_dictoffset=offsetof(BarObject,inst_dict),
  .tp_weaklistoffset=0, 
  .tp_members=BarObject_members,
  .tp_doc="Doc string for class Bar in module Foo.",
  .tp_init=(initproc)BarObject_init,        
  .tp_dealloc=(destructor)BarObject_dealloc,
};
I32  CheckInterrupt() {
    return PyErr_CheckSignals() !=0; 
}
void ConsumeInterruptSignal() { return; }
#endif
#include "abc_000_warning.h"
