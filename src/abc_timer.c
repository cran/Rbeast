#include "abc_000_warning.h"
#include "abc_timer.h"
static F64 conversionFactor;
static F64 elapsedTimeAtBreakpoint;
#if defined(MSVC_COMPILER)	
	static LARGE_INTEGER T0;
#elif ( defined(CLANG_COMPILER)||defined(GCC_COMPILER)||defined(SOLARIS_COMPILER) ) && !(defined(__APPLE__)||defined(__MACH__))
	static struct timespec T0;
	#include "time.h" 
#elif defined(__MACH__)
	static U64 T0;
#endif
void InitTimerFunc()
{
#if defined(MSVC_COMPILER)
	LARGE_INTEGER Frequency;
	QueryPerformanceFrequency(&Frequency); 
	conversionFactor=1./(double)Frequency.QuadPart;
#elif defined(__MACH__)
	mach_timebase_info_data_t timebase;
	mach_timebase_info(&timebase);
	F64 conversionTonanosecs;
	conversionTonanosecs=timebase.numer/timebase.denom;
	conversionFactor=conversionTonanosecs/1e9;
#endif
}
void StartTimer()
{
#if defined(MSVC_COMPILER)  
	QueryPerformanceCounter(&T0);	
#elif ( defined(CLANG_COMPILER)||defined(GCC_COMPILER)||defined(SOLARIS_COMPILER) ) && !(defined(__APPLE__)||defined(__MACH__))
	clock_gettime(CLOCK_MONOTONIC,&T0);	
#elif defined(__MACH__) 
	T0=mach_absolute_time();	
#endif
}
F64 GetElapsedSecondsSinceStart()
{	
	F64 elapsedTime;
#if defined(MSVC_COMPILER)  
	LARGE_INTEGER T;
	QueryPerformanceCounter(&T);	
	elapsedTime=(double)(T.QuadPart-T0.QuadPart)* conversionFactor;
#elif ( defined(CLANG_COMPILER)||defined(GCC_COMPILER)||defined(SOLARIS_COMPILER) ) && !(defined(__APPLE__)||defined(__MACH__))
	struct timespec T;
	clock_gettime(CLOCK_MONOTONIC,&T);
	elapsedTime=(double) (T.tv_sec - T0.tv_sec)+(double)(T.tv_nsec-T0.tv_nsec)/1000000000LL;
#elif defined(__MACH__) 
	U64 T=mach_absolute_time();
	elapsedTime=(T - T0)*conversionFactor;
#endif
	return elapsedTime;
}
void SetBreakPointForStartedTimer() {
	elapsedTimeAtBreakpoint=GetElapsedSecondsSinceStart();
}
F64 GetElaspedTimeFromBreakPoint() {
	return GetElapsedSecondsSinceStart() - elapsedTimeAtBreakpoint;
}
U64 TimerGetTickCount() {
	U64 tick;
#if	defined(MSVC_COMPILER) 
	tick=GetTickCount64();
#elif ( defined(CLANG_COMPILER)||defined(GCC_COMPILER)||defined(SOLARIS_COMPILER) ) && !(defined(__APPLE__)||defined(__MACH__))
	struct timespec T;
	clock_gettime(CLOCK_REALTIME,&T);
	tick=T.tv_sec * 1000000000LL+T.tv_nsec;
#elif defined(__MACH__) 
	tick=mach_absolute_time();
#endif
	return tick;
}
static unsigned long long ReadTSC_InternalTime;
void				tic() { ReadTSC_InternalTime=readTSC();}
unsigned long long  toc() {	return readTSC()- ReadTSC_InternalTime;}
#include "abc_000_warning.h"
