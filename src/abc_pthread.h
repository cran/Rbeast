#pragma once
#include "abc_000_macro.h"
#include <inttypes.h>
extern int GetNumCores(void);
#if defined(_WIN32)||defined(OS_WIN64)||defined(OS_MAC)   
typedef struct cpu_set { 
        int        core_count; 
        uint64_t   core_mask[4]; 
       } cpu_set_t;
extern void  CPU_ZERO(cpu_set_t* cs);
extern void  CPU_SET(int num,cpu_set_t* cs);
extern int   CPU_ISSET(int num,cpu_set_t* cs);
extern int   CPU_get_first_bit_id(cpu_set_t* cs);
#endif
#if defined(OS_WIN64)||defined(OS_WIN32)
#include "stdlib.h"  
 #define WIN32_LEAN_AND_MEAN
 #include <windows.h> 
#if defined(TARGET_32)
    #define __in 
    #define _Inout_
    #define _Out_
    #include <synchapi.h> 
    extern WINBASEAPI BOOL WINAPI SleepConditionVariableCS(_Inout_ PCONDITION_VARIABLE ConditionVariable,_Inout_ PCRITICAL_SECTION CriticalSection,_In_ DWORD dwMilliseconds);
    extern WINBASEAPI VOID WINAPI WakeConditionVariable(_Inout_ PCONDITION_VARIABLE ConditionVariable);
    extern WINBASEAPI VOID WINAPI InitializeConditionVariable(_Out_ PCONDITION_VARIABLE ConditionVariable);
#endif
#if (_WIN32_WINNT < 0x0601)	
    #ifndef ___PROCESSOR_NUMBER_DEFINED
        #define ___PROCESSOR_NUMBER_DEFINED
        typedef struct _PROCESSOR_NUMBER {
            WORD Group;
            BYTE Number;
            BYTE Reserved;
        } PROCESSOR_NUMBER,* PPROCESSOR_NUMBER;
    #endif
    #define ProcThreadAttributeIdealProcessor       5
    #define PROC_THREAD_ATTRIBUTE_NUMBER    0x0000FFFF
    #define PROC_THREAD_ATTRIBUTE_THREAD    0x00010000  
    #define PROC_THREAD_ATTRIBUTE_INPUT     0x00020000  
    #define PROC_THREAD_ATTRIBUTE_ADDITIVE  0x00040000  
    #define ProcThreadAttributeValue(Number,Thread,Input,Additive) \
        (((Number) & PROC_THREAD_ATTRIBUTE_NUMBER)|\
         ((Thread !=FALSE) ? PROC_THREAD_ATTRIBUTE_THREAD : 0)|\
         ((Input !=FALSE) ? PROC_THREAD_ATTRIBUTE_INPUT : 0)|\
         ((Additive !=FALSE) ? PROC_THREAD_ATTRIBUTE_ADDITIVE : 0))
    #define PROC_THREAD_ATTRIBUTE_IDEAL_PROCESSOR \
        ProcThreadAttributeValue (ProcThreadAttributeIdealProcessor,TRUE,TRUE,FALSE)
#endif
#ifdef OS_WIN64
    #include "Processthreadsapi.h"   
#endif
typedef HANDLE    pthread_t;
typedef struct {
         SIZE_T                       sizeAttributeList;
         PPROC_THREAD_ATTRIBUTE_LIST  lpAttributeList;
	     SIZE_T                       dwStackSize;    
    #ifdef OS_WIN64
	    PROCESSOR_NUMBER ProcNumber;
    #else
         void * ProcNumber;
    #endif
}   pthread_attr_t;
typedef CRITICAL_SECTION   pthread_mutex_t;
typedef int                pthread_mutexattr_t;
typedef CONDITION_VARIABLE pthread_cond_t;
typedef int                pthread_condattr_t;
static INLINE int pthread_attr_init(pthread_attr_t * attr) {
    if (attr==NULL) return 0;
    attr->dwStackSize=0;
    attr->lpAttributeList=NULL;
    attr->sizeAttributeList=0;
    #ifdef OS_WIN64    
        DWORD  attributeCounts=1L;
        SIZE_T size;
        if ( InitializeProcThreadAttributeList(NULL,attributeCounts,0,&size)||GetLastError()==ERROR_INSUFFICIENT_BUFFER) 
        {      
           attr->sizeAttributeList=size;
        }
    #endif
    return 0;
}
static INLINE  int pthread_attr_destroy(pthread_attr_t* attr) {
#ifdef OS_WIN64
    if (attr->lpAttributeList !=NULL) {
        DeleteProcThreadAttributeList(attr->lpAttributeList);
        free(attr->lpAttributeList);
    }
#endif
    return 0;
}
static INLINE int pthread_attr_setstacksize(pthread_attr_t* tattr,size_t  size) {tattr->dwStackSize=size;    return 0;}
extern        int pthread_attr_getstacksize_win32(pthread_attr_t* attr,size_t* stacksize);
static INLINE int pthread_attr_getstacksize(pthread_attr_t* attr,size_t* stacksize) {    return pthread_attr_getstacksize_win32(attr,stacksize); }
extern int pthread_attr_setaffinity_np(pthread_attr_t* attr,size_t cpusetsize,const cpu_set_t* cpuset);
static INLINE int pthread_mutex_init(pthread_mutex_t* mutex,const pthread_mutexattr_t* attr) {    InitializeCriticalSection(mutex);    return 0;}
static INLINE int pthread_mutex_destroy(pthread_mutex_t* mutex) {     DeleteCriticalSection(mutex);   return 0;}
static INLINE int pthread_mutex_lock(pthread_mutex_t* mutex)    {     EnterCriticalSection(mutex);    return 0; }
static INLINE int pthread_mutex_unlock(pthread_mutex_t* mutex)  {     LeaveCriticalSection(mutex);   return 0; }
static INLINE int pthread_cond_init(pthread_cond_t * cond,const pthread_condattr_t * attr) {     InitializeConditionVariable(cond);  return 0;}
static INLINE int pthread_cond_wait(pthread_cond_t * cond,pthread_mutex_t * mutex) {   SleepConditionVariableCS(cond,mutex,INFINITE);  return 0;}
static INLINE int pthread_cond_signal(pthread_cond_t * cond)    {   WakeConditionVariable(cond);    return 0;}
static INLINE int pthread_cond_broadcast(pthread_cond_t* cond)  {   WakeAllConditionVariable(cond);    return 0; }
static INLINE int pthread_cond_destroy(pthread_cond_t * cond)   {
    return 0;
}
static INLINE void pthread_exit(void *value_ptr) {
}
#define PTHREAD_CREATE_JOINABLE  1
static INLINE int         pthread_attr_setdetachstate(pthread_attr_t * attr,int detachstate) {   return 0;}
static INLINE  pthread_t  pthread_self(void) { return (pthread_t)GetCurrentThreadId(); }
static INLINE int         pthread_join(pthread_t thread,void **retvalue_ptr) {
	WaitForSingleObject(thread,INFINITE);
    if (retvalue_ptr) {
        GetExitCodeThread(thread,(LPDWORD) retvalue_ptr);
    }
	CloseHandle(thread);
    return 0;
}
extern int  GetCPUInfo(void);
extern void PrintCPUInfo(void);
extern int  sched_getcpu(void); 
extern int  pthread_create0(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg);
static INLINE int  pthread_create(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg) {
    return  pthread_create0(tid,attr,start,arg);
}
#elif   defined(OS_LINUX)
	    #ifndef _GNU_SOURCE
		    #define _GNU_SOURCE  
	    #endif
        #include <sched.h>       
	    #include <pthread.h>
#elif    defined(OS_MAC) 
    #include <mach/thread_policy.h> 
    #include <mach/thread_act.h> 
    #include <sys/sysctl.h> 
    #include <pthread.h>
    #include <sys/types.h> 
    #include <unistd.h>    
    #define SYSCTL_CORE_COUNT   "machdep.cpu.core_count"
    static inline int sched_getaffinity(pid_t pid,size_t cpu_size,cpu_set_t* cpu_set)    {
        int32_t core_count=0;
        size_t  len=sizeof(core_count);
        int     ret=sysctlbyname(SYSCTL_CORE_COUNT,&core_count,&len,0,0);
        if (ret) {			
            return -1;     
        }
        CPU_ZERO(cpu_set);
        for (int i=0; i < core_count; i++) {
            CPU_SET(i,cpu_set);
        }
        return 0;
    }
    #if defined(__arm__)||defined(__aarch64__ ) 
		static int sched_getcpu() {			 
			return 0;
		}
    #else
		#include <cpuid.h>   
		#define CPUID(INFO,LEAF,SUBLEAF) __cpuid_count(LEAF,SUBLEAF,INFO[0],INFO[1],INFO[2],INFO[3])
		static int sched_getcpu() {
			uint32_t CPUInfo[4];
			CPUID(CPUInfo,1,0);     
			int CPU;
			if ((CPUInfo[3] & (1 << 9))==0) {
				CPU=-1;  
			}   else {
				CPU=(unsigned)CPUInfo[1] >> 24;
			}
			return CPU;
		}
	#endif
    static int pthread_setaffinity_np(pthread_t thread,size_t cpu_size,cpu_set_t *cpu_set) {            
            thread_port_t mach_thread=pthread_mach_thread_np(thread);             
            int core=CPU_get_first_bit_id(cpu_set);
            thread_affinity_policy_data_t policy;
            policy.affinity_tag=core;            
            thread_policy_set(mach_thread,THREAD_AFFINITY_POLICY,(thread_policy_t)&policy,1);
            return 0;
        }
#elif  defined(OS_SOLARIS)
    #include <sched.h>   
    #include <pthread.h>
    #include <sys/types.h> 
    #include <sys/processor.h>
    #include <sys/procset.h>
    typedef  int cpu_set_t;
    static inline void   CPU_ZERO(cpu_set_t* cs) { *cs=0; }
    static inline void   CPU_SET(int num,cpu_set_t* cs) { num=num%GetNumCores(); *cs=num; }
    static int sched_getaffinity(pthread_t pid,size_t cpu_size,cpu_set_t* cpu_set) {
        processorid_t obind;
		int cpu=CPU_get_first_bit_id(cpu_set);
        processor_bind(P_LWPID,pid,cpu,&obind);          
        return 0;
    }
#endif
