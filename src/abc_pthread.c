#include "abc_000_warning.h"
#include "abc_000_macro.h"
#include "abc_ide_util.h"  
#if defined(_WIN32)||defined(OS_WIN64)
    #include "abc_pthread.h"
#elif defined(OS_MAC)
	#include <sys/param.h>
	#include <sys/sysctl.h>
   #include "abc_pthread.h"  
#elif defined(OS_LINUX)||defined(OS_SOLARIS)
	#include <unistd.h> 
#endif
#if  defined(OS_LINUX) 
    #ifndef _GNU_SOURCE
        #define _GNU_SOURCE  
    #endif
    #include <sched.h>       
    #include <pthread.h>
#elif defined(OS_SOLARIS)
    #include <sched.h>       
    #include <pthread.h>
#endif
void PrintBits(size_t const size,void const* const ptr) {	
    unsigned char* b=(unsigned char*)ptr;
    unsigned char byte;
    int i,j;
    for (i=size - 1; i >=0; i--) {
        for (j=7; j >=0; j--) {
            byte=(b[i] >> j) & 1;
            r_printf("%u",byte);
        }
    }
}
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
    static int CORE_COUNT=0;
    if (CORE_COUNT > 0) {
        return CORE_COUNT;
    }
#if defined(_WIN32)||defined(OS_WIN64)    
    uint32_t count=GetCPUInfo(); 
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
    uint32_t count=sysconf(_SC_NPROCESSORS_ONLN);
#endif
    CORE_COUNT=count >=1 ? count : 1L;
    return  CORE_COUNT;
}
#if defined(_WIN32)||defined(OS_WIN64)||defined(OS_MAC) 
void  CPU_ZERO(cpu_set_t* cs) {
    cs->core_count=GetNumCores();
    if (cs->core_count > 256) cs->core_count=256; 
    cs->core_mask[0]=cs->core_mask[1]=cs->core_mask[2]=cs->core_mask[3]=0;
}
void    CPU_SET(int num,cpu_set_t* cs) {
    num=num%cs->core_count;;
    int grpId=num/64;
    int bitId=num - grpId * 64;
    cs->core_mask[grpId]|=(1 << bitId);
}
int   CPU_ISSET(int num,cpu_set_t* cs) {
    num=num%cs->core_count;;
    int grpId=num/64;
    int bitId=num - grpId * 64;
    return (cs->core_mask[grpId] & (1 << bitId));
}
int   CPU_get_first_bit_id(cpu_set_t* cs) {
    int grpId=0;
    for (grpId=0; grpId < 4; grpId++) {
        if (cs->core_mask[grpId] !=0)   break;
    }
    if (grpId < 4) {
        int      num=0;
        uint64_t mask=cs->core_mask[grpId];
        for (num=0; num < 64; num++) {
            if (mask & (1 << num)) {
                break;
            }
        }
        return grpId * 64+num;
    }
    else {
        return 0;
    }
}
#endif
#if defined(_WIN32)||defined(OS_WIN64)
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
    DWORD numaNodeCount;
    DWORD processorPackageCount;
    DWORD processorCoreCount;
    DWORD logicalProcessorCount;        
    DWORD processorL1CacheCount;
    DWORD processorL2CacheCount;
    DWORD processorL3CacheCount ;
    DWORD    processorGroupCount;
    DWORD    coreCountPerGrp[10];
    uint64_t affinityPerGrp[10];
    char    cpuGroup[256];
    char    coreIDinGroup[256];
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
static int  GetCoreNumbers_WIN7V1(void) {
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
static int GetCoreNumbers_WINXP(void) {         
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
    #ifdef OS_WIN64
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
               if (buffer)     free(buffer);
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
    int num_RelationProcessorCore=0;
    int num_RelationGroup=0;
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
                if (ptr->Cache.Level==1)                {
                    cpuInfo.processorL1CacheCount++;
                }  else if (ptr->Cache.Level==2)            {
                    cpuInfo.processorL2CacheCount++;
                } else if (ptr->Cache.Level==3)   {
                    cpuInfo.processorL3CacheCount++;
                }
                break;
            case RelationProcessorPackage:
                cpuInfo.processorPackageCount++;
                break;
            case RelationGroup:
                {
                    int numGrp_sofar=cpuInfo.processorGroupCount;
                    int activeCount=ptr->Group.ActiveGroupCount;
                    for (int i=0; i < activeCount; i++) {
                          cpuInfo.coreCountPerGrp[numGrp_sofar+i]=ptr->Group.GroupInfo[i].ActiveProcessorCount;
                          cpuInfo.affinityPerGrp [numGrp_sofar+i]=ptr->Group.GroupInfo[i].ActiveProcessorMask;
                     }
                    numGrp_sofar+=activeCount;
                    cpuInfo.processorGroupCount+=numGrp_sofar;
                }
                break;
            default:
                break;
            }
            byteOffset+=ptr->Size;
            ptr=(char*)ptr+ptr->Size;
        }
     int idx=0;
     for (int grp=0; grp < cpuInfo.processorGroupCount; grp++) {
         int coreCountsPerGrp=cpuInfo.coreCountPerGrp[grp];
         for (int i=0; i < coreCountsPerGrp; i++) {
             cpuInfo.cpuGroup[idx%256]=grp;
             cpuInfo.coreIDinGroup[idx%256]=i;
             idx++;
         }
     }
     return cpuInfo.logicalProcessorCount;
    #endif
     return 0;
}
static void RankCPU(void) {
   #ifdef OS_WIN64
    PROCESSOR_NUMBER procNum;
    cpuFunc.GetCurrentProcessorNumberEx(&procNum);
    int curGrp=procNum.Group;
    int curNo=procNum.Number;
    int nGrp=cpuInfo.processorGroupCount;
    int coreCountsPerGrp=cpuInfo.coreCountPerGrp[curGrp];
    int idx=0;
    for (int i=0; i < coreCountsPerGrp; i++) {
        if (i !=curNo) {
            cpuInfo.cpuGroup[idx]=curGrp;
            cpuInfo.coreIDinGroup[idx]=i;
            idx++;
        }
    }
    idx=coreCountsPerGrp - 1;
    cpuInfo.cpuGroup[idx]=curGrp;
    cpuInfo.coreIDinGroup[idx]=curNo;
    idx=coreCountsPerGrp;
    for (int grp=0; grp < nGrp; grp++) {
        if (grp==curGrp)
            continue;        
        coreCountsPerGrp=cpuInfo.coreCountPerGrp[grp];
        for (int i=0; i < coreCountsPerGrp; i++) {
            cpuInfo.cpuGroup[idx%256]=grp;
            cpuInfo.coreIDinGroup[idx%256]=i;
            idx++;
        }
    }
  #endif
}
int sched_getcpu(void) {
    if (cpuFunc.GetCurrentProcessorNumberEx !=NULL) {
        PROCESSOR_NUMBER procNum;
        cpuFunc.GetCurrentProcessorNumberEx(&procNum);
        int currentGroup=procNum.Group;
        int currentCoreNumber=procNum.Number;
        int coreNum=0;
        for (int nGrp=0; nGrp < (currentGroup - 1); nGrp++) {
            coreNum+=cpuInfo.coreCountPerGrp[nGrp];
        }
        coreNum+=currentCoreNumber;
        return coreNum;
    }    else {
        return GetCurrentProcessorNumber();
    }
    GROUP_AFFINITY grpAffinity;
    GetThreadGroupAffinity(GetCurrentThread(),&grpAffinity);
    uint64_t  currentThreadAffinity=grpAffinity.Mask;
}
int GetCPUInfo(void) {
    if (cpuInfo.logicalProcessorCount > 0) {
        return cpuInfo.logicalProcessorCount;
    }
    InitCPUFuncs();
    int nCores;
    if (cpuFunc.GetLogicalProcessorInformationEx !=NULL) {    
        nCores=GetCoreNumbers_WIN7V2();
    }  else  {
        nCores=GetCoreNumbers_WIN32();
    }
    PrintCPUInfo();
    return nCores;
}
void PrintCPUInfo() {
    r_printf("\nCPU Information:\n");
    r_printf(" - Number of NUMA nodes: %d\n",(int)cpuInfo.numaNodeCount);
    r_printf((" - Number of physical processors (sockets): %d\n"),(int)cpuInfo.processorPackageCount);
    r_printf((" - Number of processor cores: %d\n"),(int)cpuInfo.processorCoreCount);
    r_printf((" - Number of logical processors: %d\n"),(int)cpuInfo.logicalProcessorCount);
    r_printf(" - Number of processor groups: %d\n",(int)cpuInfo.processorGroupCount);
    for (int i=0; i < cpuInfo.processorGroupCount; i++) {
        r_printf(" -- Processor group #%d: %d cores\n",i,(int)cpuInfo.coreCountPerGrp[i]);
    }
    r_printf((" - Number of processor L1/L2/L3 caches: %d/%d/%d\n"),(int)cpuInfo.processorL1CacheCount,(int)cpuInfo.processorL2CacheCount,(int)cpuInfo.processorL3CacheCount);
}
 #include <stdlib.h>     
 int pthread_attr_setaffinity_np(pthread_attr_t* attr,size_t cpusetsize,const cpu_set_t* cpuset) {
     if (cpuset==NULL)   return 0; 
#ifdef OS_WIN64
     if (attr->lpAttributeList !=NULL) {
         DeleteProcThreadAttributeList(attr->lpAttributeList);
         free(attr->lpAttributeList);
         attr->lpAttributeList=NULL;
     }
    DWORD  attributeCounts=1L;
    attr->lpAttributeList=malloc(attr->sizeAttributeList);
    InitializeProcThreadAttributeList(attr->lpAttributeList,attributeCounts,0,&attr->sizeAttributeList);
    int coreId=CPU_get_first_bit_id(cpuset);    
    attr->ProcNumber.Group=cpuInfo.cpuGroup[coreId];
    attr->ProcNumber.Number=cpuInfo.coreIDinGroup[coreId];
    attr->ProcNumber.Reserved=0;    
    BOOL fok=UpdateProcThreadAttribute( attr->lpAttributeList,0L,PROC_THREAD_ATTRIBUTE_IDEAL_PROCESSOR,
                                         &attr->ProcNumber,sizeof(PROCESSOR_NUMBER),NULL,NULL);
    return fok;
#else
    return 0;
#endif
}
int  pthread_create0(pthread_t* tid,const pthread_attr_t* attr,void* (*start) (void*),void* arg) {
#ifdef OS_WIN64
    if (cpuFunc.CreateRemoteThreadEx==NULL||attr==NULL||attr->lpAttributeList==NULL ) {
        *tid=CreateThread(NULL,NULL,(LPTHREAD_START_ROUTINE) start,arg,0,0);
    }  else {
        *tid=cpuFunc.CreateRemoteThreadEx(
            GetCurrentProcess(),
            (LPSECURITY_ATTRIBUTES)NULL,
            (SIZE_T) attr->dwStackSize, 
            (LPTHREAD_START_ROUTINE) start,
            (LPVOID) arg,
            (DWORD) 0,
            (LPPROC_THREAD_ATTRIBUTE_LIST) attr->lpAttributeList,
            (LPDWORD)NULL
        );
    }
#else
    if (attr!=NULL)
        *tid=CreateThread(NULL,attr->dwStackSize,(LPTHREAD_START_ROUTINE)start,arg,0,0);
    else
        *tid=CreateThread(NULL,NULL,(LPTHREAD_START_ROUTINE)start,arg,0,0);
#endif
    return  (tid==NULL);               
}
#endif
#if defined(_WIN32)||defined(OS_WIN64)
    #if _WIN32_WINNT >=0x0602
        int get_thread_stacksize(void) {
            ULONG_PTR lowLimit;
            ULONG_PTR highLimit; 
            GetCurrentThreadStackLimits(&lowLimit,&highLimit);
            return (highLimit - lowLimit);
        }
    #else
        #if defined(COMPILER_MSVC)
        int get_thread_stacksize(void) {
             NT_TIB* tib=(NT_TIB*)NtCurrentTeb();
             LPVOID   StackLimit=tib->StackLimit;
             LPVOID   StackBase=tib->StackBase;
             return  StackLimit;
        }
        #elif  defined(COMPILER_GCC)
            static INLINE unsigned __int64 readgsqword_bad(unsigned __int64 Offset) {
                unsigned char ret;
                __asm__( "mov{" "q" " %%" "gs" ":%[offset],%[ret]|%[ret],%%" "gs" ":%[offset]}" 
                         : [ret] "=r" (ret) 
                        : [offset] "m" ((*(unsigned __int64*)(size_t)Offset)));
                return ret; 
            }
            static INLINE unsigned __int64  readgsqword_good(unsigned __int64 Offset) {
                unsigned __int64 ret;
                unsigned __int64 volatile OffsetVolatile=Offset;
                __asm__("mov{" "q" " %%" "gs" ":%[offset],%[ret]|%[ret],%%" "gs" ":%[offset]}"
                    : [ret]    "=r" (ret)
                    : [offset] "m" ((*(unsigned __int64*)(size_t)OffsetVolatile)));
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
        static size_t default_stacksize=0;
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
#elif defined(OS_LINUX) 
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
