#include "abc_000_warning.h"
#include "abc_000_macro.h"
#include "abc_ide_util.h"  
#if defined(_WIN32)||defined(WIN64_OS)
    #include "abc_pthread.h"
#elif defined(MAC_OS)
	#include <sys/param.h>
	#include <sys/sysctl.h>
#elif defined(LINUX_OS)||defined(SOLARIS_OS)
	#include <unistd.h> 
#endif
#if  defined(LINUX_OS) 
    #ifndef _GNU_SOURCE
        #define _GNU_SOURCE
    #endif
    #include <sched.h>  
    #include <pthread.h>
#elif defined(SOLARIS_OS)
    #include <sched.h>  
    #include <pthread.h>
#endif
int CountSetBits32(uint32_t x)  {
    x=x - ((x >> 1) & 0x55555555);
    x=(x & 0x33333333)+((x >> 2) & 0x33333333);
    x=(x+(x >> 4)) & 0x0F0F0F0F;
    x=x+(x >> 8);
    x=x+(x >> 16);
    return x & 0x0000003F;
}
int CountSetBits64(uint64_t x) {
    static const uint64_t m1=0x5555555555555555; 
    static const uint64_t m2=0x3333333333333333; 
    static const uint64_t m4=0x0f0f0f0f0f0f0f0f; 
    static const uint64_t m8=0x00ff00ff00ff00ff; 
    static const uint64_t m16=0x0000ffff0000ffff; 
    static const uint64_t m32=0x00000000ffffffff; 
    static const uint64_t h01=0x0101010101010101; 
    x -=(x >> 1) & m1;             
    x=(x & m2)+((x >> 2) & m2); 
    x=(x+(x >> 4)) & m4;        
    x+=x >> 8;  
    x+=x >> 16;  
    x+=x >> 32;  
    return x & 0x7f;
}
int GetNumCores(void)  {
#if defined(_WIN32)||defined(WIN64_OS)    
    uint32_t count;
	count=GetCPUInfo(); 
#elif defined(__MACH__)
    int nm[2];
    size_t   len=4;
    uint32_t count;
    nm[0]=CTL_HW; 
    nm[1]=HW_AVAILCPU;
    sysctl(nm,2,&count,&len,NULL,0);
    if(count < 1) {
        nm[1]=HW_NCPU;
        sysctl(nm,2,&count,&len,NULL,0);
        if(count < 1) { count=1; }
    }
#else
    uint32_t count;
    count=sysconf(_SC_NPROCESSORS_ONLN);
#endif
	return count >=1 ? count : 1L;
}
#if defined(_WIN32)||defined(WIN64_OS)
typedef struct __CPUINFO {
    DWORD (WINAPI* GetActiveProcessorGroupCount)     (void);
    DWORD (WINAPI* GetActiveProcessorCount)          (WORD GroupNumber);
    BOOL  (WINAPI* GetLogicalProcessorInformationEx) (
            LOGICAL_PROCESSOR_RELATIONSHIP           RelationshipType,        
            PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX Buffer,
            PDWORD                                   ReturnedLength );
    HANDLE (WINAPI*  CreateRemoteThreadEx) (
            HANDLE                       hProcess,
            LPSECURITY_ATTRIBUTES        lpThreadAttributes,
            SIZE_T                       dwStackSize,
            LPTHREAD_START_ROUTINE       lpStartAddress,
            LPVOID                       lpParameter,
            DWORD                        dwCreationFlags,
            LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList,
            LPDWORD                      lpThreadId   );
    BOOL (WINAPI* GetThreadGroupAffinity) (
            HANDLE          hThread, 
            PGROUP_AFFINITY GroupAffinity );
    void  (WINAPI*  GetCurrentProcessorNumberEx) ( PPROCESSOR_NUMBER ProcNumber );
    DWORD (WINAPI* GetCurrentProcessorNumber) (VOID ); 
    BOOL  (WINAPI*  GetNumaHighestNodeNumber)(  PULONG HighestNodeNumber   );
    BOOL  (WINAPI*  GetNumaProcessorNodeEx)  (
                PPROCESSOR_NUMBER Processor,
                PUSHORT           NodeNumber   );
    BOOL(WINAPI* GetNumaAvailableMemoryNode) (
                UCHAR      Node,
                PULONGLONG AvailableBytes   ); 
    char isInitilized;
} CPUFUCINFO;
typedef struct __CPUINFO1 {
    DWORD logicalProcessorCount;
    DWORD numaNodeCount;    
    DWORD processorCoreCount;
    DWORD processorL1CacheCount;
    DWORD processorL2CacheCount;
    DWORD processorL3CacheCount ;
    DWORD processorPackageCount;
    DWORD processorGroupCount;
    DWORD coreCountPerGrp[10];
    DWORD currentGroup;
    DWORD currentCoreNumber;
    uint64_t currentThreadAffinity;
    char cpuGroup[256];
    char numCPUCoresToUseber[256];
} CPUINFO;
static CPUFUCINFO cpuFunc={0,};
static CPUINFO    cpuInfo={0,};
static void InitCPUFuncs(void) {
    if (cpuFunc.isInitilized)  {
        return;
    }
    HMODULE kerHandle=GetModuleHandle(TEXT("kernel32"));
    cpuFunc.GetActiveProcessorGroupCount=GetProcAddress(kerHandle,"GetActiveProcessorGroupCount");
    cpuFunc.GetActiveProcessorCount=GetProcAddress(kerHandle,"GetActiveProcessorCount");
    cpuFunc.GetLogicalProcessorInformationEx=GetProcAddress(kerHandle,"GetLogicalProcessorInformationEx");
    cpuFunc.CreateRemoteThreadEx=GetProcAddress(kerHandle,"CreateRemoteThreadEx");
    cpuFunc.GetThreadGroupAffinity=GetProcAddress(kerHandle,"GetThreadGroupAffinity");
    cpuFunc.GetCurrentProcessorNumberEx=GetProcAddress(kerHandle,"GetCurrentProcessorNumberEx");
    cpuFunc.GetCurrentProcessorNumber=GetProcAddress(kerHandle,"GetCurrentProcessorNumber");
    cpuFunc.GetNumaHighestNodeNumber=GetProcAddress(kerHandle,"GetNumaHighestNodeNumber");
    cpuFunc.GetNumaProcessorNodeEx=GetProcAddress(kerHandle,"GetNumaProcessorNodeEx");
    cpuFunc.GetNumaAvailableMemoryNode=GetProcAddress(kerHandle,"GetNumaAvailableMemoryNode");
    cpuFunc.isInitilized=1;
}
static int  GetCoreNumbers_WIN32(void) {
    cpuInfo=(CPUINFO){ 0,};
    SYSTEM_INFO   sysinfo;
    GetSystemInfo(&sysinfo);   
    cpuInfo.logicalProcessorCount=sysinfo.dwNumberOfProcessors;
    cpuInfo.processorGroupCount=1;
    uint64_t currentGroupAffinity;
    return cpuInfo.logicalProcessorCount;
}
static int GetCoreNumbers_WIN7V1(void) {
    cpuInfo=(CPUINFO){ 0,};
    InitCPUFuncs();
    if (NULL==cpuFunc.GetActiveProcessorGroupCount)   {
        return -1;
    }
    WORD procGroupNum=cpuFunc.GetActiveProcessorGroupCount();
    procGroupNum=procGroupNum > 10 ? 10 : procGroupNum;
    int numCore=0;
    for (DWORD i=0; i < procGroupNum; i++)   {
        int count=cpuFunc.GetActiveProcessorCount(i);
        numCore+=count;
        cpuInfo.coreCountPerGrp[i]=count;
    }
    cpuInfo.processorGroupCount=procGroupNum;
    cpuInfo.logicalProcessorCount=numCore;
    ULONG numaCount;
    cpuFunc.GetNumaHighestNodeNumber(&numaCount);
    cpuInfo.numaNodeCount=numaCount+1L;
    return cpuInfo.logicalProcessorCount; 
}
static int GetCoreNumbers_WINXP(void)
{         
    cpuInfo=(CPUINFO){ 0,};
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer=NULL;
    PCACHE_DESCRIPTOR Cache;
    DWORD returnLength=0;
    BOOL  done=FALSE;
    while (!done)  {
        DWORD flagOK=GetLogicalProcessorInformation(buffer,&returnLength);
        if (FALSE==flagOK)  {
            if (GetLastError()==ERROR_INSUFFICIENT_BUFFER)   {
                if (buffer) {
                    free(buffer);
                }                
                buffer=(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION) malloc(returnLength);
                if (NULL==buffer) { 
                    return (-1);
                }
            }  else   {   
                return (-1);
            }
        } else {
            done=TRUE;
        }
    }
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr=buffer;
    DWORD byteOffset=0;
    while (byteOffset+sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <=returnLength)     {
        switch (ptr->Relationship)
        {
        case RelationNumaNode:    
            cpuInfo.numaNodeCount++;     
            break;
        case RelationProcessorCore:
            cpuInfo.processorCoreCount++;
            cpuInfo.logicalProcessorCount+=CountSetBits64(ptr->ProcessorMask);
            break;
        case RelationCache:
            Cache=&ptr->Cache;
            if (Cache->Level==1) {
                cpuInfo.processorL1CacheCount++;
            } else if (Cache->Level==2)   {
                cpuInfo.processorL2CacheCount++;
            } else if (Cache->Level==3)   {
                cpuInfo.processorL3CacheCount++;
            }
            break;
        case RelationProcessorPackage:
            cpuInfo.processorPackageCount++;
            break;
        default:
            break;
        }
        byteOffset+=sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
        ptr++;
    }
    free(buffer);
    return cpuInfo.logicalProcessorCount;
}
static int GetCoreNumbers_WIN7V2(void) {
    #ifdef WIN64_OS
    cpuInfo=(CPUINFO){0,};
    InitCPUFuncs();
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX buffer=NULL;
    DWORD returnLength=0;
    BOOL  done=FALSE;
    while (!done) {
         DWORD fOK=cpuFunc.GetLogicalProcessorInformationEx(
                             RelationAll,(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX) buffer,&returnLength);         
         if (FALSE==fOK) {
            if (GetLastError()==ERROR_INSUFFICIENT_BUFFER)  {
               if (buffer) 
                   free(buffer);
               buffer=(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)malloc(returnLength);
               if (NULL==buffer) {  
                    return (-1);
               }
            } else {       
                return (-1);
            }
         }  else {
                done=TRUE;
         }
    }
     PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX  ptr=buffer;
     DWORD byteOffset=0;
     while (byteOffset < returnLength) {
            switch (ptr->Relationship)
            {
            case RelationNumaNode:
                cpuInfo.numaNodeCount++;
                break;
            case RelationProcessorCore:
                cpuInfo.processorCoreCount++;
                cpuInfo.logicalProcessorCount+=ptr->Processor.Flags+1L;
                break;
            case RelationCache:
                break;
            case RelationProcessorPackage:
                cpuInfo.processorPackageCount++;
                break;
            case RelationGroup:
                {
                    int activeCount=ptr->Group.ActiveGroupCount;
                    for (int i=0; i < activeCount; i++) {
                          cpuInfo.coreCountPerGrp[cpuInfo.processorGroupCount+i]=ptr->Group.GroupInfo[i].ActiveProcessorCount;
                     }
                    cpuInfo.processorGroupCount+=activeCount;
                }
                break;
            default:
                break;
            }
            byteOffset+=ptr->Size;
            ptr=(char*)ptr+ptr->Size;
        }
     PROCESSOR_NUMBER procNum;
     cpuFunc.GetCurrentProcessorNumberEx(&procNum);
     cpuInfo.currentGroup=procNum.Group;
     cpuInfo.currentCoreNumber=procNum.Number;
     GROUP_AFFINITY grpAffinity;
     cpuFunc.GetThreadGroupAffinity( GetCurrentThread(),&grpAffinity);
     cpuInfo.currentThreadAffinity=grpAffinity.Mask;
     return cpuInfo.logicalProcessorCount;
    #endif
     return 0;
}
static void RankCPU(void) {
#ifdef WIN64_OS
    int nGrp=cpuInfo.processorGroupCount;
    int curGrp=cpuInfo.currentGroup;
    int curNo=cpuInfo.currentCoreNumber;
    int coreCounts=cpuInfo.coreCountPerGrp[curGrp];
    int idx=0;
    for (int i=0; i < coreCounts; i++) {
        if (i !=curNo) {
            cpuInfo.cpuGroup[idx]=curGrp;
            cpuInfo.numCPUCoresToUseber[idx]=i;
            idx++;
        }
    }
    idx=coreCounts - 1;
    cpuInfo.cpuGroup[idx]=curGrp;
    cpuInfo.numCPUCoresToUseber[idx]=curNo;
    idx=coreCounts;
    for (int grp=0; grp < nGrp; grp++) {
        if (grp==curGrp)
            continue;        
        coreCounts=cpuInfo.coreCountPerGrp[grp];
        for (int i=0; i < coreCounts; i++) {
            cpuInfo.cpuGroup[idx%256]=grp;
            cpuInfo.numCPUCoresToUseber[idx%256]=i;
            idx++;
        }
    }
#endif
}
int GetCPUInfo() {
    InitCPUFuncs();
    int nCores;
    if (cpuFunc.GetLogicalProcessorInformationEx !=NULL) {    
        nCores=GetCoreNumbers_WIN7V2();
        RankCPU();
    }  else  {
        nCores=GetCoreNumbers_WIN32();
    }
    PrintCPUInfo();
    return nCores;
}
void PrintCPUInfo() {
    r_printf("\nCPU Information:\n");
    r_printf(" - Number of NUMA nodes: %d\n",cpuInfo.numaNodeCount);
    r_printf((" - Number of physical processors (sockets): %d\n"),cpuInfo.processorPackageCount);
    r_printf((" - Number of processor cores: %d\n"),cpuInfo.processorCoreCount);
    r_printf((" - Number of logical processors: %d\n"),cpuInfo.logicalProcessorCount);
    r_printf(" - Number of processor groups: %d\n",cpuInfo.processorGroupCount);
    for (int i=0; i < cpuInfo.processorGroupCount; i++) {
        r_printf(" -- Processor group #%d: %d cores\n",i,cpuInfo.coreCountPerGrp[i]);
    }
    r_printf((" - Number of processor L1/L2/L3 caches: %d/%d/%d\n"),cpuInfo.processorL1CacheCount,
        cpuInfo.processorL2CacheCount,cpuInfo.processorL3CacheCount);
    r_printf(" - Group ID of current thread: %d\n",cpuInfo.currentGroup);
    r_printf(" - Core ID of current thread: %d\n",cpuInfo.currentCoreNumber);
    r_printf(" - CPU affinity mask of current thread: %#x\n",cpuInfo.currentThreadAffinity);
}
 void CPU_ZERO(cpu_set_t* cpus) {
    memset(cpus,0,sizeof(cpu_set_t));
}
void CPU_SET(int i,cpu_set_t* cpus) {     
    #ifdef WIN64_OS
    cpus->ProcNumber.Group=cpuInfo.cpuGroup[i];
    cpus->ProcNumber.Number=cpuInfo.numCPUCoresToUseber[i];
    #endif
}
int  pthread_create0(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg)
{
#ifdef WIN64_OS
    if (cpuFunc.CreateRemoteThreadEx==NULL) {
        r_printf("the CreateRemoteThreadEx funnction is not detected!\n");
        *tid=CreateThread(NULL,NULL,(LPTHREAD_START_ROUTINE) start,arg,0,0);
    }  else {
        *tid=cpuFunc.CreateRemoteThreadEx(
            GetCurrentProcess(),
            (LPSECURITY_ATTRIBUTES)NULL,
            (SIZE_T)attr->dwStackSize, 
            (LPTHREAD_START_ROUTINE)start,
            (LPVOID)arg,
            (DWORD)0,
            (LPPROC_THREAD_ATTRIBUTE_LIST)attr->lpAttributeList,
            (LPDWORD)NULL
        );
    }
#else
    * tid=CreateThread(NULL,attr->dwStackSize,(LPTHREAD_START_ROUTINE)start,arg,0,0);
#endif
    return 0;
}
#endif
#if defined(_WIN32)||defined(WIN64_OS)
    #if _WIN32_WINNT >=0x0602
        int get_thread_stacksize(void) {
            ULONG_PTR lowLimit;
            ULONG_PTR highLimit; 
            GetCurrentThreadStackLimits(&lowLimit,&highLimit);
            return (highLimit - lowLimit);
        }
    #else
        #if defined(MSVC_COMPILER)
        int get_thread_stacksize(void) {
             NT_TIB* tib=(NT_TIB*)NtCurrentTeb();
             LPVOID   StackLimit=tib->StackLimit;
             LPVOID   StackBase=tib->StackBase;
             return  StackLimit;
        }
        #elif  defined(GCC_COMPILER)
            static INLINE unsigned __int64 readgsqword_bad(unsigned __int64 Offset) {
                unsigned char ret;
                __asm__( "mov{" "q" " %%" "gs" ":%[offset],%[ret]|%[ret],%%" "gs" ":%[offset]}" 
                         : [ret] "=r" (ret) 
                        : [offset] "m" ((*(unsigned __int64*)(size_t)Offset)));
                return ret; 
            }
            static INLINE unsigned __int64  readgsqword_good(unsigned __int64 Offset) {
                unsigned __int64 ret;
                unsigned __int64 volatile Offset1=Offset;
                __asm__("mov{" "q" " %%" "gs" ":%[offset],%[ret]|%[ret],%%" "gs" ":%[offset]}"
                    : [ret] "=r" (ret)
                    : [offset] "m" ((*(unsigned __int64*)(size_t)Offset1)));
                return ret;
            }
            int get_thread_stacksize(void) {
                NT_TIB* tib=(NT_TIB*)readgsqword_good(FIELD_OFFSET(NT_TIB,Self));
                LPVOID   StackLimit=tib->StackLimit;
                LPVOID   StackBase=tib->StackBase;
                return  StackLimit;
            }  
       #else
        int get_thread_stacksize(void) { 
             return  0;
        }
       #endif
    #endif
    static void * __getstacksize(void * arg) {  
        *((size_t*) arg)=get_thread_stacksize();  return 0; 
    }
    int pthread_attr_getstacksize_win32(pthread_attr_t* attr,size_t* stacksize) {
        static int default_stacksize=0;
        if (attr->dwStackSize > 0) {
            *stacksize=attr->dwStackSize;            
        } else{
            if (default_stacksize==0) {
                pthread_t tid=CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)__getstacksize,stacksize,0,0);
                pthread_join(tid,NULL);
                default_stacksize=*stacksize;
            }
            *stacksize=default_stacksize;
        }       
        return 0;        
    }
#elif defined(LINUX_OS) 
int get_thread_stacksize(void) {
    pthread_attr_t attr;
    size_t   stksize;
    void*    stkaddr;
    (void)pthread_getattr_np(pthread_self(),&attr);
    (void)pthread_attr_getstack(&attr,&stkaddr,&stksize);
    (void)pthread_attr_destroy(&attr);
    return stksize;
}
#else 
int get_thread_stacksize(void) {
    return  0;
    }
#endif
#include "abc_000_warning.h"
