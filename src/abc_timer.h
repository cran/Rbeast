#pragma once
#include "abc_000_macro.h"
#include "abc_datatype.h"
#ifdef MSVC_COMPILER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>          
#elif defined( __MACH__)
#include <mach/mach_time.h>
#endif
#ifdef _WIN32
    #include <windows.h>
#elif _POSIX_C_SOURCE >=199309L
    #include <time.h>   
#else
    #include <unistd.h> 
#endif
static INLINE void Sleep_ms(int milliseconds) {
    #ifdef WIN32
        Sleep(milliseconds);
    #elif _POSIX_C_SOURCE >=199309L
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
extern void InitTimerFunc();
extern void StartTimer();
extern F64  GetElapsedSecondsSinceStart();
extern void SetBreakPointForStartedTimer();
extern F64  GetElaspedTimeFromBreakPoint();
extern U64  TimerGetTickCount();
#ifdef MSVC_COMPILER
    #include <intrin.h>  
#elif defined(SOLARIS_COMPILER)
   #include <sys/time.h>
    static  unsigned long long __rdtsc() {
        return  gethrtime();
    }
#else
    #include <x86intrin.h> 
#endif
static INLINE unsigned long long readTSC() {
    return __rdtsc();
}
extern void tic();
extern unsigned long long toc();
