#include <stdio.h>
#include <string.h>
#include "abc_000_warning.h"
#include "abc_common.h"
#include "abc_ide_util.h" 
#include "abc_cpu.h"
#if defined(__x86_64__)||defined(_M_X64)||defined(__i386)||defined(_M_IX86)
    #if defined(MSVC_COMPILER) 
        #define WIN32_LEAN_AND_MEAN
        #include <Windows.h>
        #include <intrin.h>
        #define _XCR_XFEATURE_ENABLED_MASK  0 
        void     cpuid(int32_t out[4],int32_t eax,int32_t ecx) {   __cpuidex(out,eax,ecx);      }
        uint64_t xgetbv(unsigned int x)                          {   return _xgetbv(x);             }
        typedef BOOL(WINAPI* LPFN_ISWOW64PROCESS) (HANDLE,PBOOL);
        BOOL IsWow64()           {
            BOOL                bIsWow64=FALSE;
            LPFN_ISWOW64PROCESS fnIsWow64Process=(LPFN_ISWOW64PROCESS)GetProcAddress(
                GetModuleHandle(TEXT("kernel32")),"IsWow64Process");
            if (NULL !=fnIsWow64Process)       {
                if (!fnIsWow64Process(GetCurrentProcess(),&bIsWow64))     {
                    r_printf("Error Detecting Operating System.\n");
                    r_printf("Defaulting to 32-bit OS.\n\n");
                    bIsWow64=FALSE;
                }
            }
            return bIsWow64;
        }
        int detect_OS_x64(void) {
            #ifdef _M_X64
                    return 1;
            #else
                    return IsWow64() !=0;
            #endif
        }
    #elif defined(__GNUC__)||defined(__clang__)
        #include <cpuid.h>
        void cpuid(int32_t out[4],int32_t eax,int32_t ecx) {
            __cpuid_count(eax,ecx,out[0],out[1],out[2],out[3]);
        }
        uint64_t xgetbv(unsigned int index) {
            uint32_t eax,edx;
            __asm__ __volatile__("xgetbv" : "=a"(eax),"=d"(edx) : "c"(index));
            return ((uint64_t)edx << 32)|eax;
        }
        #define _XCR_XFEATURE_ENABLED_MASK  0
        int detect_OS_x64(void) {
            return 1;
        }
    #elif defined (SOLARIS_OS)
        #define __cpuid(level,a,b,c,d)                                             \
          __asm__("cpuid\n\t" : "=a"(a),"=b"(b),"=c"(c),"=d"(d) : "0"(level))
        #define __cpuid_count(level,count,a,b,c,d)                                \
          __asm__("cpuid\n\t"                                                          \
                  : "=a"(a),"=b"(b),"=c"(c),"=d"(d)                                 \
                  : "0"(level),"2"(count))
        void cpuid(int32_t out[4],int32_t eax,int32_t ecx) {
            __cpuid_count(eax,ecx,out[0],out[1],out[2],out[3]);
        }
        #define _XCR_XFEATURE_ENABLED_MASK  0
        uint64_t xgetbv(unsigned int index) {
            uint32_t eax,edx;
            __asm__ ("xgetbv" : "=a"(eax),"=d"(edx) : "c"(index));
            return ((uint64_t)edx << 32)|eax;
        }
        int detect_OS_x64(void) {
            return 1;
        }
        #warning "Warning:No cpuid intrinsic defined for compiler."
    #else
        #define _XCR_XFEATURE_ENABLED_MASK  0
        void cpuid(int32_t out[4],int32_t eax,int32_t ecx) {
            out[0]=out[1]=out[2]=out[3]=0;
        }
        uint64_t xgetbv(unsigned int index) {            
            return 0;
        }
        int detect_OS_x64(void) {
            return 1;
        }
        #warning "No cpuid intrinsic defined for compiler: a placeholder created!"
    #endif
#elif defined(ARM64_OS)||defined (POWERPC_OS)
        #define _XCR_XFEATURE_ENABLED_MASK  0
        void cpuid(int32_t out[4],int32_t eax,int32_t ecx) {
            out[0]=out[1]=out[2]=out[3]=0;
        }
        uint64_t xgetbv(unsigned int index) {
            return 0;
        }
        int detect_OS_x64(void) {
            return 1;
        }
#else
#   error "No cpuid intrinsic defined for processor architecture."
#endif
static void cpu_print(const char* label,uint8_t yes) {
    r_printf("%s%s\n",label,(yes ? "Yes" : "No"));
}
uint8_t detect_OS_AVX(void){
    Bool avxSupported=_False_;
    int  cpuInfo[4];
    cpuid(cpuInfo,1,0);
    Bool osUsesXSAVE_XRSTORE=(cpuInfo[2] & (1 << 27)) !=0;
    Bool cpuAVXSuport=(cpuInfo[2] & (1 << 28)) !=0;
    if (osUsesXSAVE_XRSTORE && cpuAVXSuport)   {
        uint64_t xcrFeatureMask=xgetbv(_XCR_XFEATURE_ENABLED_MASK);
        avxSupported=(xcrFeatureMask & 0x6)==0x6;
    }
    return avxSupported;
}
Bool detect_OS_AVX512(void){
    if (!detect_OS_AVX())
        return _False_;
    uint64_t xcrFeatureMask=xgetbv(_XCR_XFEATURE_ENABLED_MASK);
    return (xcrFeatureMask & 0xe6)==0xe6;
}
void get_vendor_string(char * name){
    int32_t CPUInfo[4];
    cpuid(CPUInfo,0,0);
    memcpy(name+0,&CPUInfo[1],4);
    memcpy(name+4,&CPUInfo[3],4);
    memcpy(name+8,&CPUInfo[2],4);
    name[12]='\0';
}
void detect_host(struct cpu_x86 *cpu){
    memset(cpu,0,sizeof(struct cpu_x86));
    cpu->OS_x64=detect_OS_x64();
    cpu->OS_AVX=detect_OS_AVX();
    cpu->OS_AVX512=detect_OS_AVX512();
    char vendor[13];
    get_vendor_string(vendor);
    if (strcmp(vendor,"GenuineIntel")==0){
        cpu->Vendor_Intel=_True_;
    }else if (strcmp(vendor,"AuthenticAMD")==0){
        cpu->Vendor_AMD=_True_;
    }
    int info[4];
    cpuid(info,0,0);
    int nIds=info[0];
    cpuid(info,0x80000000,0);
    uint32_t nExIds=info[0];
    if (nIds >=0x00000001){
        cpuid(info,0x00000001,0);
        cpu->HW_MMX=(info[3] & ((uint32_t)1 << 23)) !=0;
        cpu->HW_SSE=(info[3] & ((uint32_t)1 << 25)) !=0;
        cpu->HW_SSE2=(info[3] & ((uint32_t)1 << 26)) !=0;
        cpu->HW_SSE3=(info[2] & ((uint32_t)1 <<  0)) !=0;
        cpu->HW_SSSE3=(info[2] & ((uint32_t)1 <<  9)) !=0;
        cpu->HW_SSE41=(info[2] & ((uint32_t)1 << 19)) !=0;
        cpu->HW_SSE42=(info[2] & ((uint32_t)1 << 20)) !=0;
        cpu->HW_AES=(info[2] & ((uint32_t)1 << 25)) !=0;
        cpu->HW_AVX=(info[2] & ((uint32_t)1 << 28)) !=0;
        cpu->HW_FMA3=(info[2] & ((uint32_t)1 << 12)) !=0;
        cpu->HW_RDRAND=(info[2] & ((uint32_t)1 << 30)) !=0;
    }
    if (nIds >=0x00000007){
        cpuid(info,0x00000007,0);
        cpu->HW_AVX2=(info[1] & ((int)1 <<  5)) !=0;
        cpu->HW_BMI1=(info[1] & ((int)1 <<  3)) !=0;
        cpu->HW_BMI2=(info[1] & ((int)1 <<  8)) !=0;
        cpu->HW_ADX=(info[1] & ((int)1 << 19)) !=0;
        cpu->HW_MPX=(info[1] & ((int)1 << 14)) !=0;
        cpu->HW_SHA=(info[1] & ((int)1 << 29)) !=0;
        cpu->HW_RDSEED=(info[1] & ((int)1 << 18)) !=0;
        cpu->HW_PREFETCHWT1=(info[2] & ((int)1 <<  0)) !=0;
        cpu->HW_RDPID=(info[2] & ((int)1 << 22)) !=0;
        cpu->HW_AVX512_F=(info[1] & ((int)1 << 16)) !=0;
        cpu->HW_AVX512_CD=(info[1] & ((int)1 << 28)) !=0;
        cpu->HW_AVX512_PF=(info[1] & ((int)1 << 26)) !=0;
        cpu->HW_AVX512_ER=(info[1] & ((int)1 << 27)) !=0;
        cpu->HW_AVX512_VL=(info[1] & ((uint32_t)1 << 31)) !=0;
        cpu->HW_AVX512_BW=(info[1] & ((int)1 << 30)) !=0;
        cpu->HW_AVX512_DQ=(info[1] & ((int)1 << 17)) !=0;
        cpu->HW_AVX512_IFMA=(info[1] & ((int)1 << 21)) !=0;
        cpu->HW_AVX512_VBMI=(info[2] & ((int)1 <<  1)) !=0;
        cpu->HW_AVX512_VPOPCNTDQ=(info[2] & ((int)1 << 14)) !=0;
        cpu->HW_AVX512_4FMAPS=(info[3] & ((int)1 <<  2)) !=0;
        cpu->HW_AVX512_4VNNIW=(info[3] & ((int)1 <<  3)) !=0;
        cpu->HW_AVX512_VNNI=(info[2] & ((int)1 << 11)) !=0;
        cpu->HW_AVX512_VBMI2=(info[2] & ((int)1 <<  6)) !=0;
        cpu->HW_GFNI=(info[2] & ((int)1 <<  8)) !=0;
        cpu->HW_VAES=(info[2] & ((int)1 <<  9)) !=0;
        cpu->HW_AVX512_VPCLMUL=(info[2] & ((int)1 << 10)) !=0;
        cpu->HW_AVX512_BITALG=(info[2] & ((int)1 << 12)) !=0;
        cpuid(info,0x00000007,1);
        cpu->HW_AVX512_BF16=(info[0] & ((int)1 <<  5)) !=0;
    }
    if (nExIds >=0x80000001){
        cpuid(info,0x80000001,0);
        cpu->HW_x64=(info[3] & ((int)1 << 29)) !=0;
        cpu->HW_ABM=(info[2] & ((int)1 <<  5)) !=0;
        cpu->HW_SSE4a=(info[2] & ((int)1 <<  6)) !=0;
        cpu->HW_FMA4=(info[2] & ((int)1 << 16)) !=0;
        cpu->HW_XOP=(info[2] & ((int)1 << 11)) !=0;
    }
}
void print_cpuinfo(struct cpu_x86 *cpu) {
    r_printf("CPU Vendor:\n");
    cpu_print("    AMD=",cpu->Vendor_AMD);
    cpu_print("    Intel=",cpu->Vendor_Intel);
    r_printf(" \n");
    r_printf("OS Features:\n");
#ifdef _WIN32
    cpu_print("    64-bit=",cpu->OS_x64);
#endif
    cpu_print("    OS AVX=",cpu->OS_AVX);
    cpu_print("    OS AVX512=",cpu->OS_AVX512);
    r_printf("\nHardware Features:\n");
    cpu_print("    MMX=",cpu->HW_MMX);
    cpu_print("    x64=",cpu->HW_x64);
    cpu_print("    ABM=",cpu->HW_ABM);
    cpu_print("    RDRAND=",cpu->HW_RDRAND);
    cpu_print("    RDSEED=",cpu->HW_RDSEED);
    cpu_print("    BMI1=",cpu->HW_BMI1);
    cpu_print("    BMI2=",cpu->HW_BMI2);
    cpu_print("    ADX=",cpu->HW_ADX);
    cpu_print("    MPX=",cpu->HW_MPX);
    cpu_print("    PREFETCHW=",cpu->HW_PREFETCHW);
    cpu_print("    PREFETCHWT1=",cpu->HW_PREFETCHWT1);
    cpu_print("    RDPID=",cpu->HW_RDPID);
    cpu_print("    GFNI=",cpu->HW_GFNI);
    cpu_print("    VAES=",cpu->HW_VAES);
    r_printf("\nSIMD: 128-bit\n");
    cpu_print("    SSE=",cpu->HW_SSE);
    cpu_print("    SSE2=",cpu->HW_SSE2);
    cpu_print("    SSE3=",cpu->HW_SSE3);
    cpu_print("    SSSE3=",cpu->HW_SSSE3);
    cpu_print("    SSE4a=",cpu->HW_SSE4a);
    cpu_print("    SSE4.1=",cpu->HW_SSE41);
    cpu_print("    SSE4.2=",cpu->HW_SSE42);
    cpu_print("    AES-NI=",cpu->HW_AES);
    cpu_print("    SHA=",cpu->HW_SHA);
    r_printf("\nSIMD: 256-bit\n");
    cpu_print("    AVX=",cpu->HW_AVX);
    cpu_print("    XOP=",cpu->HW_XOP);
    cpu_print("    FMA3=",cpu->HW_FMA3);
    cpu_print("    FMA4=",cpu->HW_FMA4);
    cpu_print("    AVX2=",cpu->HW_AVX2);
    r_printf("\nSIMD: 512-bit\n");
    cpu_print("    AVX512-F=",cpu->HW_AVX512_F);
    cpu_print("    AVX512-CD=",cpu->HW_AVX512_CD);
    cpu_print("    AVX512-PF=",cpu->HW_AVX512_PF);
    cpu_print("    AVX512-ER=",cpu->HW_AVX512_ER);
    cpu_print("    AVX512-VL=",cpu->HW_AVX512_VL);
    cpu_print("    AVX512-BW=",cpu->HW_AVX512_BW);
    cpu_print("    AVX512-DQ=",cpu->HW_AVX512_DQ);
    cpu_print("    AVX512-IFMA=",cpu->HW_AVX512_IFMA);
    cpu_print("    AVX512-VBMI=",cpu->HW_AVX512_VBMI);
    cpu_print("    AVX512-VPOPCNTDQ=",cpu->HW_AVX512_VPOPCNTDQ);
    cpu_print("    AVX512-4FMAPS=",cpu->HW_AVX512_4FMAPS);
    cpu_print("    AVX512-4VNNIW=",cpu->HW_AVX512_4VNNIW);
    cpu_print("    AVX512-VBMI2=",cpu->HW_AVX512_VBMI2);
    cpu_print("    AVX512-VPCLMUL=",cpu->HW_AVX512_VPCLMUL);
    cpu_print("    AVX512-VNNI=",cpu->HW_AVX512_VNNI);
    cpu_print("    AVX512-BITALG=",cpu->HW_AVX512_BITALG);
    cpu_print("    AVX512-BF16=",cpu->HW_AVX512_BF16);
    r_printf("\nSummary\n");
    cpu_print("    Safe to use AVX:     ",cpu->HW_AVX && cpu->OS_AVX);
    cpu_print("    Safe to use AVX512:  ",cpu->HW_AVX512_F && cpu->OS_AVX512);
    r_printf("\n");
}
void detect_print_cpu() {
    struct cpu_x86 cpuinfo;
     detect_host(&cpuinfo);
     print_cpuinfo(&cpuinfo);
}
void i386_cpuid_caches (Bool quiet) {
    for (int i=0; i < 32; i++) {
        uint32_t eax,ebx,ecx,edx; 
        eax=4; 
        ecx=i; 
        #if !defined(MSVC_COMPILER) && !defined(ARM64_OS)
            __asm__ (
                "cpuid" 
                : "+a" (eax) 
                ,"=b" (ebx)
                ,"+c" (ecx) 
                ,"=d" (edx)
            ); 
        #else
            int32_t out[4];
            cpuid(out,eax,ecx);
            eax=out[0];
            ebx=out[1];
            ecx=out[2];
            edx=out[3];
        #endif
        int cache_type=eax & 0x1F; 
        if (cache_type==0) 
            break;
        char * cache_type_string;
        switch (cache_type) {
            case 1: cache_type_string="Data Cache"; break;
            case 2: cache_type_string="Instruction Cache"; break;
            case 3: cache_type_string="Unified Cache"; break;
            default: cache_type_string="Unknown Type Cache"; break;
        }
        int cache_level=(eax >>=5) & 0x7;
        int cache_is_self_initializing=(eax >>=3) & 0x1; 
        int cache_is_fully_associative=(eax >>=1) & 0x1;
        unsigned int cache_sets=ecx+1;
        unsigned int cache_coherency_line_size=(ebx & 0xFFF)+1;
        unsigned int cache_physical_line_partitions=((ebx >>=12) & 0x3FF)+1;
        unsigned int cache_ways_of_associativity=((ebx >>=10) & 0x3FF)+1;
        size_t cache_total_size=cache_ways_of_associativity * cache_physical_line_partitions * cache_coherency_line_size * cache_sets;
        if (!quiet)
            r_printf(
                "Cache ID %d:\n"
                "- Level: %d\n"
                "- Type: %s\n"
                "- Sets: %d\n"
                "- System Coherency Line Size: %d bytes\n"
                "- Physical Line partitions: %d\n"
                "- Ways of associativity: %d\n"
                "- Total Size: %zu bytes (%zu kb)\n"
                "- Is fully associative: %s\n"
                "- Is Self Initializing: %s\n"
                "\n"
                ,i
                ,cache_level
                ,cache_type_string
                ,cache_sets
                ,cache_coherency_line_size
                ,cache_physical_line_partitions
                ,cache_ways_of_associativity
                ,cache_total_size,cache_total_size >> 10
                ,cache_is_fully_associative ? "true" : "false"
                ,cache_is_self_initializing ? "true" : "false"
            );
    }
}
#include "abc_000_warning.h"
