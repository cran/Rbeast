#pragma once
#define R_INTERFACE    0
#define M_INTERFACE    0
#define P_INTERFACE    0
#define MYMAT_LIBRARY   1
#define MKL_LIBRARY     0
#define MATLAB_LIBRARY  0 
#define PCGRAND_LIBRARY 1
#define MKLRAND_LIBRARY 0
#ifdef R_RELEASE
		#undef   R_INTERFACE
		#undef   M_INTERFACE
		#undef   P_INTERFACE
		#define  R_INTERFACE 1
		#define  M_INTERFACE 0
        #define  P_INTERFACE 0
		#undef   MYMAT_LIBRARY
	    #undef   MKL_LIBRARY
		#define MYMAT_LIBRARY 1
	    #define MKL_LIBRARY   0
	    #define PCGRAND_LIBRARY 1
        #define MKLRAND_LIBRARY 0
#endif
#ifdef P_RELEASE
		#undef   R_INTERFACE
		#undef   M_INTERFACE
		#undef   P_INTERFACE
		#define  R_INTERFACE 0
		#define  M_INTERFACE 0
        #define  P_INTERFACE 1
		#undef   MYMAT_LIBRARY
	    #undef   MKL_LIBRARY
		#define MYMAT_LIBRARY 1
	    #define MKL_LIBRARY   0
	    #define PCGRAND_LIBRARY 1
        #define MKLRAND_LIBRARY 0
#endif
#ifdef M_RELEASE
		#undef   R_INTERFACE
		#undef   M_INTERFACE
		#undef   P_INTERFACE
		#define  R_INTERFACE 0
		#define  M_INTERFACE 1
        #define  P_INTERFACE 0
		#undef   MYMAT_LIBRARY
	    #undef   MKL_LIBRARY
		#define MYMAT_LIBRARY 1
	    #define MKL_LIBRARY   0
	    #define PCGRAND_LIBRARY 1
        #define MKLRAND_LIBRARY 0
        #ifndef MATLAB_MEX_FILE
			#define MATLAB_MEX_FILE
        #endif
        #define MATLAB_DEFAULT_RELEASE  R2017b
#endif
#ifdef _MSC_VER
	#define COMPILER_MSVC
#elif defined(__clang__)
	#define COMPILER_CLANG
#elif (defined(__GNUC__)||defined(__GNUG__)) && !(defined(__clang__)||defined(__INTEL_COMPILER))
	#define COMPILER_GCC
#elif defined(__SUNPRO_C)||defined(__SUNPRO_CC)
	#define COMPILER_SOLARIS
#endif
#if defined(_WIN64)||defined (__MINGW64__) && defined(_WIN32) && !defined(__i386)  && !defined(__i686) && !defined(i386) && !defined(__i686)
	#define OS_WIN64
#elif defined(_WIN32) && !defined(_WIN64)
	#define OS_WIN32
#endif
#if defined(__APPLE__ ) && defined (__MACH__) 
	#define OS_MAC
#endif
#if defined(__linux__) 
	#define OS_LINUX
#endif
#if (defined(unix)||defined(__unix__)||defined(__unix) ) && !defined(__APPLE__)
	#define OS_UNIX	
#endif
#if defined(sun) && defined(__sun) && defined(__SVR4) 
	#define OS_SOLARIS
#endif
#if _WIN64||__amd64__||defined(__LP64__)||(defined(__x86_64__) &&    !defined(__ILP32__) )||defined(_M_X64)||defined(__ia64)||defined (_M_IA64)||defined(__aarch64__)||defined(__powerpc64__)
	#define TARGET_64
#else
	#define TARGET_32
#endif
#if __GNUC__
	#if __x86_64__||__ppc64__
		#define TARGET_64
	#else
		#define TARGET_32
	#endif
#endif
#if defined(__x86_64__)||defined(_M_X64)
	#define  cpu_x86_64
#elif defined(i386)||defined(__i386__)||defined(__i386)||defined(_M_IX86)
	#define  cpu_x86_32 
#elif defined(__aarch64__)||defined(_M_ARM64)
	#define   cpu_ARM64
#elif defined(mips)||defined(__mips__)||defined(__mips)
	#define   cpu_MIPS
#elif defined(__sh__)
	#define   cpu_SUPERH
#elif defined(__powerpc)||defined(__powerpc__)||defined(__powerpc64__)||defined(__POWERPC__)||defined(__ppc__)||defined(__PPC__)||defined(_ARCH_PPC)
	#define   cpu_POWERPC
#elif defined(__PPC64__)||defined(__ppc64__)||defined(_ARCH_PPC64)
	#define   cpu_POWERPC64
#elif defined(__sparc__)||defined(__sparc)
	#define   cpu_SPARC	
#elif defined(__ARM_ARCH_2__)
	#define   cpu_ARM2
#elif defined(__ARM_ARCH_3__)||defined(__ARM_ARCH_3M__)
	#define   cpu_ARM3
#elif defined(__ARM_ARCH_4T__)||defined(__TARGET_ARM_4T)
	#define   cpu_ARM4T
#elif defined(__ARM_ARCH_5_)||defined(__ARM_ARCH_5E_)
	#define   cpu_ARM5
#elif defined(__ARM_ARCH_6T2_)||defined(__ARM_ARCH_6T2_)
	#define   cpu_ARM6T2
#elif defined(__ARM_ARCH_6__)||defined(__ARM_ARCH_6J__)||defined(__ARM_ARCH_6K__)||defined(__ARM_ARCH_6Z__)||defined(__ARM_ARCH_6ZK__)
	#define   cpu_ARM6
#elif defined(__ARM_ARCH_7__)||defined(__ARM_ARCH_7A__)||defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
	#define   cpu_ARM7
#elif defined(__ARM_ARCH_7A__)||defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
	#define   cpu_ARM7A
#elif defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
	#define   cpu_ARM7R
#elif defined(__ARM_ARCH_7M__)
	#define   cpu_ARM7M
#elif defined(__ARM_ARCH_7S__)
	#define   cpu_ARM7S
#elif defined(__m68k__)
	#define   cpu_M68K 
#endif
#if defined(TARGET_32) && defined (COMPILER_MSVC)
	#define _CRT_SECURE_NO_WARNINGS
	#pragma warning (disable: 4703) 
#endif
#if MYMAT_LIBRARY==1
		#define PCGRAND_LIBRARY 1
		#define MKLRAND_LIBRARY 0
#endif
#if defined(COMPILER_MSVC)
		#define INLINE    __inline
		#define _restrict __restrict
        #define UNUSED_DECORATOR 
#elif defined(COMPILER_SOLARIS) 
		#define INLINE     inline 
		#define _restrict _Restrict 
        #define UNUSED_DECORATOR 
#elif defined(COMPILER_GCC)||defined(COMPILER_CLANG)
		#define INLINE     inline
		#define _restrict __restrict__		
        #define UNUSED_DECORATOR  __attribute__((unused))
#endif
#if   defined(OS_LINUX) && ( defined(COMPILER_GCC)||defined(COMPILER_CLANG) )
	#ifdef _GNU_SOURCE 
		#include <features.h>
		#ifndef __USE_GNU
			#define __MUSL__ 
		#endif
	#else
		#define _GNU_SOURCE
		#include <features.h>
		#ifndef __USE_GNU
		#define __MUSL__ 
		#endif
		#undef _GNU_SOURCE 
	#endif
#endif
#ifdef COMPILER_MSVC
    # define ALIGN32_BEG __declspec(align(32))
    # define ALIGN32_END 
#else
    # define ALIGN32_BEG
    # define ALIGN32_END __attribute__((aligned(32)))
#endif
	#define DIAG_STR(s) #s
	#define DIAG_JOINSTR(x,y) DIAG_STR(x ## y)
	#ifdef COMPILER_MSVC
		#define DIAG_DO_PRAGMA(x) __pragma (#x)
		#define DIAG_PRAGMA(compiler,x) DIAG_DO_PRAGMA(warning(x))
	#else
		#define DIAG_DO_PRAGMA(x)       _Pragma (#x)
		#define DIAG_PRAGMA(compiler,x) DIAG_DO_PRAGMA(compiler diagnostic x)
	#endif
	#if defined(COMPILER_CLANG)
		# define DISABLE_WARNING(gcc_unused,clang_option,msvc_unused) DIAG_PRAGMA(clang,push) DIAG_PRAGMA(clang,ignored DIAG_JOINSTR(-W,clang_option))
		# define ENABLE_WARNING(gcc_unused,clang_option,msvc_unused) DIAG_PRAGMA(clang,pop)
	#elif defined(COMPILER_MSVC)
		# define DISABLE_WARNING(gcc_unused,clang_unused,msvc_errorcode) DIAG_PRAGMA(msvc,push) DIAG_DO_PRAGMA(warning(disable:##msvc_errorcode))
		# define ENABLE_WARNING(gcc_unused,clang_unused,msvc_errorcode) DIAG_PRAGMA(msvc,pop)
	#elif defined(COMPILER_GCC)
		#if ((__GNUC__ * 100)+__GNUC_MINOR__) >=406
			# define DISABLE_WARNING(gcc_option,clang_unused,msvc_unused) DIAG_PRAGMA(GCC,push) DIAG_PRAGMA(GCC,ignored DIAG_JOINSTR(-W,gcc_option))
			# define ENABLE_WARNING(gcc_option,clang_unused,msvc_unused) DIAG_PRAGMA(GCC,pop)
		#else
			# define DISABLE_WARNING(gcc_option,clang_unused,msvc_unused) DIAG_PRAGMA(GCC,ignored DIAG_JOINSTR(-W,gcc_option))
			# define ENABLE_WARNING(gcc_option,clang_option,msvc_unused) DIAG_PRAGMA(GCC,warning DIAG_JOINSTR(-W,gcc_option))
		#endif
	#endif
#if defined(COMPILER_GCC) 
	#define  DISABLE_MANY_WARNINGS   \
	DISABLE_WARNING(unknown-pragmas,unknown-pragmas,NOT_USED) \
	DISABLE_WARNING(pragmas,pragmas,NOT_USED) \
	DISABLE_WARNING(unused-variable,unused-variable,NOT_USED) \
	DISABLE_WARNING(unused-function,unused-function,NOT_USED) \
	DISABLE_WARNING(pointer-sign,pointer-sign,NOT_USED) \
	DISABLE_WARNING(implicit-function-declaration,implicit-function-declaration,NOT_USED) \
	DISABLE_WARNING(strict-aliasing,strict-aliasing,NOT_USED) \
	DISABLE_WARNING(unused-but-set-variable,unused-but-set-variable,NOT_USED) \
	DISABLE_WARNING(maybe-uninitialized,maybe-uninitialized,NOT_USED)\
	DISABLE_WARNING(pointer-to-int-cast,pointer-to-int-cast,NOT_USED)\
	DISABLE_WARNING(misleading-indentation,NOT_USED,NOT_USED)\
	DISABLE_WARNING(discarded-qualifiers,discarded-qualifiers,NOT_USED)\
	DISABLE_WARNING(int-to-pointer-cast,int-to-pointer-cast,NOT_USED)\
	DISABLE_WARNING(unused-result,unused-result,NOT_USED) \
    DISABLE_WARNING(unused-const-variable,unused-const-variable,NOT_USED)\
    DISABLE_WARNING(incompatible-pointer-types-discards-qualifiers,incompatible-pointer-types-discards-qualifiers,NOT_USED)\
	DISABLE_WARNING(incompatible-pointer-types,incompatible-pointer-types,NOT_USED)\
	DISABLE_WARNING(self-assign,self-assign,NOT_USED) \
    DISABLE_WARNING(unused-value,unused-value,NOT_USED) \
    DISABLE_WARNING(int-conversion,int-conversion,NOT_USED) \
    DISABLE_WARNING(restrict,restrict,NOT_USED)\
    DISABLE_WARNING(switch,switch,NOT_USED) \
    DISABLE_WARNING(uninitialized,uninitialized,NOT_USED)\
    DISABLE_WARNING(pedantic,pedantic,NOT_USED) \
    DISABLE_WARNING(div-by-zero,div-by-zero,NOT_USED)\
    DISABLE_WARNING(constant-logical-operand,constant-logical-operand,NOT_USED) 
	#define  ENABLE_MANY_WARNINGS   \
    ENABLE_WARNING(constant-logical-operand,constant-logical-operand,NOT_USED) \
    ENABLE_WARNING(div-by-zero,div-by-zero,NOT_USED) \
    ENABLE_WARNING(pedantic,pedantic,NOT_USED)\
    ENABLE_WARNING(uninitialized,uninitialized,NOT_USED)\
    ENABLE_WARNING(switch,switch,NOT_USED)\
    ENABLE_WARNING(restrict,restrict,NOT_USED)\
    ENABLE_WARNING(int-conversion,int-conversion,NOT_USED) \
    ENABLE_WARNING(unused-value,unused-value,NOT_USED) \
	ENABLE_WARNING(self-assign,self-assign,NOT_USED) \
	ENABLE_WARNING(incompatible-pointer-types,incompatible-pointer-types,NOT_USED) \
	ENABLE_WARNING(incompatible-pointer-types-discards-qualifiers,incompatible-pointer-types-discards-qualifiers,NOT_USED)\
    ENABLE_WARNING(unused-const-variable,unused-const-variable,NOT_USED)\
	ENABLE_WARNING(unused-result,unused-result,NOT_USED)\
	ENABLE_WARNING(int-to-pointer-cast,int-to-pointer-cast,NOT_USED)\
	ENABLE_WARNING(discarded-qualifiers,discarded-qualifiers,NOT_USED)\
	ENABLE_WARNING(misleading-indentation,NOT_USED,NOT_USED)\
	ENABLE_WARNING(pointer-to-int-cast,pointer-to-int-cast,NOT_USED)\
	ENABLE_WARNING(maybe-uninitialized,maybe-uninitialized,NOT_USED)\
	ENABLE_WARNING(unused-but-set-variable,unused-but-set-variable,NOT_USED) \
	ENABLE_WARNING(strict-aliasing,strict-aliasing,NOT_USED) \
	ENABLE_WARNING(implicit-function-declaration,implicit-function-declaration,NOT_USED) \
	ENABLE_WARNING(pointer-sign,pointer-sign,NOT_USED) \
	ENABLE_WARNING(unused-function,unused-function,NOT_USED) \
	ENABLE_WARNING(unused-variable,unused-variable,NOT_USED)  \
	ENABLE_WARNING(pragmas,pragmas,NOT_USED) \
	ENABLE_WARNING(unknown-pragmas,unknown-pragmas,NOT_USED) 
#elif defined(COMPILER_CLANG)||1
	#define  DISABLE_MANY_WARNINGS  \
	DISABLE_WARNING(unknown-pragmas,unknown-pragmas,NOT_USED)  \
	DISABLE_WARNING(pragmas,pragmas,NOT_USED) \
	DISABLE_WARNING(unused-variable,unused-variable,NOT_USED)  \
	DISABLE_WARNING(unused-function,unused-function,NOT_USED)  \
	DISABLE_WARNING(pointer-sign,pointer-sign,NOT_USED)  \
	DISABLE_WARNING(implicit-function-declaration,implicit-function-declaration,NOT_USED) \
	DISABLE_WARNING(strict-aliasing,strict-aliasing,NOT_USED) \
	DISABLE_WARNING(xxxxxxx,unknown-warning-option,xxxxxxxxx) \
    DISABLE_WARNING(pointer-to-int-cast,pointer-to-int-cast,NOT_USED)\
	DISABLE_WARNING(int-to-pointer-cast,int-to-pointer-cast,NOT_USED) \
	DISABLE_WARNING(unused-result,unused-result,NOT_USED)             \
    DISABLE_WARNING(unused-const-variable,unused-const-variable,NOT_USED) \
	DISABLE_WARNING(incompatible-pointer-types-discards-qualifiers,incompatible-pointer-types-discards-qualifiers,NOT_USED)\
	DISABLE_WARNING(incompatible-pointer-types,incompatible-pointer-types,NOT_USED)\
	DISABLE_WARNING(self-assign,self-assign,NOT_USED)  \
    DISABLE_WARNING(unused-value,unused-value,NOT_USED) \
    DISABLE_WARNING(int-conversion,int-conversion,NOT_USED) \
    DISABLE_WARNING(switch,switch,NOT_USED) \
    DISABLE_WARNING(uninitialized,uninitialized,NOT_USED)\
    DISABLE_WARNING(pedantic,pedantic,NOT_USED) \
    DISABLE_WARNING(typedef-redefinition,typedef-redefinition,NOT_USED) \
    DISABLE_WARNING(div-by-zero,div-by-zero,NOT_USED) \
	DISABLE_WARNING(unused-but-set-variable,unused-but-set-variable,NOT_USED) \
    DISABLE_WARNING(constant-logical-operand,constant-logical-operand,NOT_USED) 
	#define  ENABLE_MANY_WARNINGS  \
    ENABLE_WARNING(constant-logical-operand,constant-logical-operand,NOT_USED)  \
    ENABLE_WARNING(unused-but-set-variable,unused-but-set-variable,NOT_USED) \
    ENABLE_WARNING(div-by-zero,div-by-zero,NOT_USED) \
    ENABLE_WARNING(typedef-redefinition,typedef-redefinition,NOT_USED)\
    ENABLE_WARNING(pedantic,pedantic,NOT_USED)\
    ENABLE_WARNING(uninitialized,uninitialized,NOT_USED)\
    ENABLE_WARNING(switch,switch,NOT_USED)\
    ENABLE_WARNING(int-conversion,int-conversion,NOT_USED) \
    ENABLE_WARNING(unused-value,unused-value,NOT_USED) \
	ENABLE_WARNING(self-assign,self-assign,NOT_USED) \
	ENABLE_WARNING(incompatible-pointer-types,incompatible-pointer-types,NOT_USED) \
	ENABLE_WARNING(incompatible-pointer-types-discards-qualifiers,incompatible-pointer-types-discards-qualifiers,NOT_USED)\
    ENABLE_WARNING(unused-const-variable,unused-const-variable,NOT_USED)\
	ENABLE_WARNING(unused-result,unused-result,NOT_USED)\
	ENABLE_WARNING(int-to-pointer-cast,int-to-pointer-cast,NOT_USED)\
	ENABLE_WARNING(pointer-to-int-cast,pointer-to-int-cast,NOT_USED)\
	ENABLE_WARNING(xxxxxxx,unknown-warning-option,xxxxxxxxx) \
	ENABLE_WARNING(strict-aliasing,strict-aliasing,NOT_USED) \
	ENABLE_WARNING(implicit-function-declaration,implicit-function-declaration,NOT_USED) \
	ENABLE_WARNING(pointer-sign,pointer-sign,NOT_USED) \
	ENABLE_WARNING(unused-function,unused-function,NOT_USED) \
	ENABLE_WARNING(unused-variable,unused-variable,NOT_USED) \
	ENABLE_WARNING(pragmas,pragmas,NOT_USED) \
	ENABLE_WARNING(unknown-pragmas,unknown-pragmas,NOT_USED) 
#else
	#define  DISABLE_MANY_WARNINGS 
	#define  ENABLE_MANY_WARNINGS 	
#endif
#ifdef _WIN32_WINNT
#undef  _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#include <stdint.h>
#if   INTPTR_MAX==INT32_MAX
	#define TARGET_32
#elif INTPTR_MAX==INT64_MAX
	#define TARGET_64
#else
	#error "Environment not 32 or 64-bit."
#endif
#define  CHANGE_TO_AVX_GCC  \
          DIAG_DO_PRAGMA(GCC optimization_level 3) \
          DIAG_DO_PRAGMA(GCC optimize("O3,Ofast,inline,omit-frame-pointer,no-asynchronous-unwind-tables")) \
          DIAG_DO_PRAGMA(GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,avx,avx2,fma,tune=haswell"))  
#define _in_
#define _inout_
#define _out_
#define mv(n,src,dest)	r_cblas_scopy( n,src,1L,dest,1L) 
#define cp(n,src,dest)    memcpy(dest,src,sizeof(F32)*(size_t)(n))
#define SCPY(n,src,dest)  memcpy(dest,src,sizeof(F32)*(size_t)(n))
#define FILL0(dest,n)       memset(dest,0L,sizeof(F32)*(size_t)(n))
