#pragma once
#include "abc_000_macro.h"
extern int GetNumCores();
#if defined(WIN64_OS)||defined(WIN32_OS)
 #define WIN32_LEAN_AND_MEAN
 #include <windows.h> 
 #include <stdlib.h>    
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
#ifdef WIN64_OS
    #include "Processthreadsapi.h" 
#endif
typedef HANDLE        pthread_t;
typedef struct {
                  LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList;
	              SIZE_T                       dwStackSize;    
}   pthread_attr_t;
typedef CRITICAL_SECTION   pthread_mutex_t;
typedef int                pthread_mutexattr_t;
typedef CONDITION_VARIABLE pthread_cond_t;
typedef int                pthread_condattr_t;
typedef struct {
    #ifdef WIN64_OS
	PROCESSOR_NUMBER ProcNumber;
    #else
    void * ProcNumber;
    #endif
} cpu_set_t;
static INLINE int pthread_attr_init(pthread_attr_t * attr)
{
#ifdef WIN64_OS
    attr->dwStackSize=0;
    attr->lpAttributeList=NULL;
    DWORD  attributeCounts=1L;
    SIZE_T size;
    if (InitializeProcThreadAttributeList(NULL,attributeCounts,0,&size)
        ||GetLastError()==ERROR_INSUFFICIENT_BUFFER)  {
        attr->lpAttributeList=malloc(size);
        InitializeProcThreadAttributeList(attr->lpAttributeList,attributeCounts,0,&size);
    }
#endif
    return 0;
}
static INLINE  int pthread_attr_destroy(pthread_attr_t* attr)
{
#ifdef WIN64_OS
    if (attr->lpAttributeList !=NULL) {
        DeleteProcThreadAttributeList(attr->lpAttributeList);
        free(attr->lpAttributeList);
    }    
#endif
    return 0;
}
static INLINE  int pthread_attr_setaffinity_np ( pthread_attr_t* attr,size_t cpusetsize,const cpu_set_t* cpuset)
{
    #ifdef WIN64_OS
    BOOL fok=UpdateProcThreadAttribute(
                    attr->lpAttributeList,
                    0L,
                    PROC_THREAD_ATTRIBUTE_IDEAL_PROCESSOR,
                    &( ((cpu_set_t *)cpuset)->ProcNumber),
                    sizeof(PROCESSOR_NUMBER),NULL,NULL);
    return fok;
    #else
    return 0;
    #endif
}
static int pthread_mutex_init(pthread_mutex_t* mutex,const pthread_mutexattr_t* attr) {
	InitializeCriticalSection(mutex); 
    return 0;
}
static INLINE int pthread_mutex_destroy(pthread_mutex_t* mutex) {
	DeleteCriticalSection(mutex);
    return 0;
}
static INLINE int pthread_mutex_lock(pthread_mutex_t* mutex) {
	EnterCriticalSection(mutex);
    return 0;
}
static INLINE int pthread_mutex_unlock(pthread_mutex_t* mutex){
	LeaveCriticalSection(mutex);
    return 0;
}
static INLINE int pthread_cond_init(pthread_cond_t * cond,const pthread_condattr_t * attr) {
	InitializeConditionVariable(cond);
    return 0;
}
static INLINE int pthread_cond_wait(pthread_cond_t * cond,pthread_mutex_t * mutex) {
	SleepConditionVariableCS(cond,mutex,INFINITE);
    return 0;
}
static INLINE int pthread_cond_signal(pthread_cond_t * cond) {
	WakeConditionVariable(cond);
    return 0;
}
static INLINE int pthread_cond_destroy(pthread_cond_t * cond) {
    return 0;
}
static INLINE void pthread_exit(void *value_ptr)
{
}
#define PTHREAD_CREATE_JOINABLE  1
static INLINE int  pthread_attr_setdetachstate(pthread_attr_t * attr,int detachstate)
{
    return 0;
}
static INLINE int pthread_join(pthread_t thread,void **value_ptr)
{
	WaitForSingleObject(thread,INFINITE);
	CloseHandle(thread);
    return 0;
}
static INLINE  pthread_t  pthread_self(void) {
    return (pthread_t) GetCurrentThreadId();
}
extern int  GetCPUInfo();
extern void PrintCPUInfo();
extern void CPU_ZERO(cpu_set_t* cpus);
extern void CPU_SET(int i,cpu_set_t* cpus);
extern int  pthread_create0(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg);
static int  pthread_create(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg)
{
    pthread_create0(tid,attr,start,arg);
    return 0;
}
#elif   defined(LINUX_OS)
        #define _GNU_SOURCE
        #include <sched.h>  
	    #include <pthread.h>
#elif   defined(MAC_OS) 
    #include <mach/thread_policy.h> 
    #include <pthread.h>
    #include <sys/types.h> 
    #include <unistd.h>   
    #include <inttypes.h>
    #define SYSCTL_CORE_COUNT   "machdep.cpu.core_count"
    typedef struct cpu_set {    uint32_t    count; } cpu_set_t;
    static inline void   CPU_ZERO(cpu_set_t* cs) {
        cs->count=0; 
    }
    static inline void    CPU_SET(int num,cpu_set_t* cs) { 
        num=num%GetNumCores(); cs->count|=(1 << num); 
    }
    static inline int     CPU_ISSET(int num,cpu_set_t* cs) {
        num=num%GetNumCores(); return (cs->count & (1 << num)); 
    }
    static int sched_getaffinity(pid_t pid,size_t cpu_size,cpu_set_t* cpu_set)    {
        int32_t core_count=0;
        size_t  len=sizeof(core_count);
        int ret=sysctlbyname(SYSCTL_CORE_COUNT,&core_count,&len,0,0);
        if (ret) {
            return -1;
        }
        cpu_set->count=0;
        for (int i=0; i < core_count; i++) {
            cpu_set->count|=(1 << i);
        }
        return 0;
    }
    static int pthread_setaffinity_np(pthread_t thread,size_t cpu_size,cpu_set_t *cpu_set) {
            thread_port_t mach_thread;
            int core=0;
            for (core=0; core < 8 * cpu_size; core++) {
                  if (CPU_ISSET(core,cpu_set)) break;
            }
            thread_affinity_policy_data_t policy={ core };
            mach_thread=pthread_mach_thread_np(thread);
            thread_policy_set(mach_thread,THREAD_AFFINITY_POLICY,
                            (thread_policy_t)&policy,1);
            return 0;
        }
#elif  defined(SOLARIS_OS)
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
        int cpu=*cpu_set;
        processor_bind(P_LWPID,pid,cpu,&obind);
        return 0;
    }
#endif
