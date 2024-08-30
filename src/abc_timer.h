#pragma once
#include "abc_000_macro.h"
#include "abc_datatype.h"
#ifdef __cplusplus
extern "C" {
#endif
#ifdef COMPILER_MSVC
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>          
#elif defined( __MACH__)
    #include <mach/mach_time.h>
#endif
#ifdef _WIN32
    #include <windows.h>
#elif _POSIX_C_SOURCE >=199309L||defined (__MUSL__)  
    #include <time.h>  
    #include <time.h>    
#else
    #include <unistd.h> 
    #include <sys/time.h>  
#endif
static INLINE void Sleep_ms(int milliseconds) {
    #ifdef WIN32
        Sleep(milliseconds);
    #elif _POSIX_C_SOURCE >=199309L||defined (__MUSL__)  
        struct timespec ts;
        ts.tv_sec=milliseconds/1000;
        ts.tv_nsec=(milliseconds%1000) * 1000000;
        nanosleep(&ts,NULL);
    #else
        struct timeval tv; 
        tv.tv_sec=milliseconds/1000;
        tv.tv_usec=milliseconds%1000 * 1000; 
        select(0,NULL,NULL,NULL,&tv);
    #endif
}
extern void InitTimerFunc(void);
extern void StartTimer(void);
extern F64  GetElapsedSecondsSinceStart(void);
extern void SetBreakPointForStartedTimer(void);
extern F64  GetElaspedTimeFromBreakPoint(void);
extern U64  TimerGetTickCount(void);
#ifdef COMPILER_MSVC
    #include <intrin.h>    
#elif defined(COMPILER_SOLARIS)
   #include <sys/time.h>
    static INLINE   unsigned long long __rdtsc(void) {
        return  gethrtime();
    }
#elif defined(cpu_ARM64)
  static INLINE  U64 __rdtsc(void)   {  return  __builtin_readcyclecounter();  }
    static INLINE U64 rdtsc(void)     {
        U64 val;
        asm volatile("mrs %0,cntvct_el0" : "=r" (val));
        return val;
    }
#else
    #include <x86intrin.h> 
#endif
static INLINE unsigned long long readTSC(void) {
    return __rdtsc();
}
extern void               tic(void);
extern unsigned long long toc(void);
#ifdef __cplusplus
}
#endif
