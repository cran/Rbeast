#pragma once
#include "abc_000_macro.h"
#define QUOTE_IT(x) #x
#ifdef COMPILER_MSVC
	#if   R_INTERFACE==1
		#define DllExport   __declspec( dllexport ) 
	#elif P_INTERFACE==1
		#define DllExport   __declspec( dllexport ) 
	#elif M_INTERFACE==1
		#define DllExport 
		#pragma comment(linker,"/export:mexFunction")  
    #endif
	#define INCLUDE_PTHREAD(_X_) QUOTE_IT(C:/USERS/zhaokg/Documents/Visual Studio 2013/Projects/Matlab_Mex_Test/Pthread_IncludeLib/_X_ )
	#define INCLUDE_MATLAB(_X_)  QUOTE_IT(C:/Program Files/MATLAB/R2019a/extern/include/_X_ )
	#define INCLUDE_MKL(_X_)     QUOTE_IT(C:/Program Files (x86)/Intel/oneAPI/mkl/latest/include/_X_ )
	#define INCLUDE_IPP(_X_)     QUOTE_IT(C:/Program Files (x86)/Intel/oneAPI/ipp/latest/include/_X_ )
	#define INCLUDE_R(_X_)       QUOTE_IT(C:/Program Files/R/R-4.2.2/include/_X_ )
    #define LIB_FORTRAN(_X_)     QUOTE_IT(C:/Program Files (x86)/Intel/oneAPI/compiler/latest/windows/compiler/lib/intel64_win/_X_ )
	#define LIB_PTHREAD(_X_)     QUOTE_IT(C:/USERS/zhaokg/Documents/Visual Studio 2013/Projects/Matlab_Mex_Test/Pthread_IncludeLib/_X_ )
	#define LIB_MATLAB(_X_)      QUOTE_IT(C:/Program Files/MATLAB/R2019a/extern/lib/win64/microsoft/_X_ )
	#define LIB_TBB(_X_)         QUOTE_IT(C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/tbb/lib/intel64_win/vc_mt/_X_ )
	#define LIB_MKL(_X_)         QUOTE_IT(C:/Program Files (x86)/Intel/oneAPI/mkl/latest/lib/intel64/_X_ )
	#define LIB_IPP(_X_)         QUOTE_IT(C:/Program Files (x86)/Intel/oneAPI/ipp/latest/lib/intel64/_X_ )
    #define LIB_OpenMP(_X_)      QUOTE_IT(C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/compiler/lib/intel64_win/_X_ )
	#define LIB_MyLIB(_X_)       QUOTE_IT(D:/Share/Fortran_blas_lib/_X_ )
    #define LIB_Python(_X_)      QUOTE_IT(C:/Anaconda3/libs/_X_ )
    #define LIB_R(_X_)           QUOTE_IT(C:/Program Files/R/R-4.5.2/implib/_X_ )
	#ifdef TARGET_32
		#define LIB_R(_X_)   QUOTE_IT(C:/Program Files/R/R-4.2.2/implib/i386/_X_ )			
	#endif
	#if R_INTERFACE==1
		#pragma comment( lib,LIB_R(R.lib)       )
		#pragma comment( lib,LIB_R(Rblas.lib)   )
		#pragma comment( lib,LIB_R(Rlapack.lib) )
	#endif
#elif defined(COMPILER_CLANG)||defined(COMPILER_GCC)||defined(COMPILER_SOLARIS)
	#define  DllExport   
#endif
#define PTHREAD_INOUT 0
#if PTHREAD_INOUT==1
	#include INCLUDE_PTHREAD(pthread.h) 
	#pragma comment(lib,LIB_PTHREAD(pthreadVC2.lib) )
#endif
#if MKL_LIBRARY==1
	#include INCLUDE_IPP(ipps.h)
	#include INCLUDE_MKL(mkl.h)
	#pragma comment(lib,LIB_MKL(mkl_intel_ilp64.lib) )
	#pragma comment(lib,LIB_MKL(mkl_core.lib))
	#pragma comment(lib,LIB_MKL(mkl_sequential.lib) )
	#pragma comment(lib,LIB_IPP(ippcoremt.lib))
	#pragma comment(lib,LIB_IPP(ippvmmt.lib))  
	#pragma comment(lib,LIB_IPP(ippsmt.lib))   
#endif
#if (MYMAT_LIBRARY==1) && defined(COMPILER_MSVC) && 0
	#pragma comment(lib,LIB_MyLIB(blas_oneAPI.lib))
    #pragma comment(lib,LIB_FORTRAN(ifconsol.lib))
    #pragma comment(lib,LIB_FORTRAN(libifcoremt.lib))
	#pragma comment(lib,LIB_FORTRAN(libifport.lib))
	#pragma comment(lib,LIB_FORTRAN(libirc.lib))
	#pragma comment(lib,LIB_FORTRAN(libircmt.lib)) 
	#pragma comment(lib,LIB_FORTRAN(libmmt.lib))	
    #pragma comment(lib,LIB_FORTRAN(svml_dispmt.lib))	
	#pragma comment(lib,LIB_FORTRAN(ifmodintr.lib))	
#endif
#if  MATLAB_LIBRARY==1
	#include "blas.h" 
	#include "lapack.h"
	#pragma comment(lib,LIB_MATLAB(libmwblas.lib) )
	#pragma comment(lib,LIB_MATLAB(libmwlapack.lib) )
	#pragma comment(lib,LIB_MyLIB(blas.lib))
	#pragma comment(lib,LIB_FORTRAN(libifcoremdd.lib))
#endif
#if M_INTERFACE==1
	#define MEX_DOUBLE_HANDLE
	#include  "mex.h"
	#pragma comment(lib,LIB_MATLAB(libmx.lib) )
	#pragma comment(lib,LIB_MATLAB(libmex.lib) )
	#pragma comment(lib,LIB_MATLAB(libmat.lib) )
    #pragma comment(lib,LIB_MATLAB(libmwservices.lib) )	 
    #pragma comment(lib,LIB_MATLAB(libut.lib) ) 
#endif
#if R_INTERFACE==1
   #ifdef ERROR 
       #undef ERROR  
   #endif
#if defined(COMPILER_CLANG)||defined(COMPILER_GCC)||defined(COMPILER_SOLARIS)
	#ifndef _GNU_SOURCE
		#define _GNU_SOURCE 1
	#endif
#endif
	#include <R.h>
	#include <Rinternals.h>
	#include <Rdefines.h>
	#include <Rmath.h>
	#include <R_ext/GraphicsEngine.h>
	#include <R_ext/GraphicsDevice.h>	
	#if MYMAT_LIBRARY==1
	#endif
	#define mwSize size_t
#endif
#if P_INTERFACE==1
	#include "Python.h"     
	#include "structmember.h"
#define  NPY_NO_DEPRECATED_API   NPY_1_7_API_VERSION  
#ifdef  USE_STANDARD_METHOD_IMPORT_NUMPY
	#ifdef  IMPORT_NUMPY
	   #define PY_ARRAY_UNIQUE_SYMBOL NumpyAPIList
	   #include "numpy/arrayobject.h"
	#else
		#define NO_IMPORT_ARRAY
		#define PY_ARRAY_UNIQUE_SYMBOL NumpyAPIList
		#include "numpy/arrayobject.h"
	#endif
#endif
#endif
#if R_INTERFACE==1||P_INTERFACE==1
	#ifndef Bool
		#define  Bool  unsigned char
	#endif
#elif M_INTERFACE==1
	#ifndef Bool
		 #include "mex.h"
		 #define  Bool  bool     
    #endif
#endif
#ifdef COMPILER_CLANG
	#undef sign    
	#undef warning 
#endif
#if defined(COMPILER_CLANG)||defined(COMPILER_GCC)||defined(COMPILER_SOLARIS)
	#ifndef _GNU_SOURCE
		#define _GNU_SOURCE 1
	#endif
	#include <fenv.h>	
#endif
