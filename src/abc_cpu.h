#pragma once
#include "abc_datatype.h" 
struct cpu_x86 {
    bool Vendor_AMD;
    bool Vendor_Intel;
    bool OS_x64;
    bool OS_AVX;
    bool OS_AVX512;
    bool HW_MMX;
    bool HW_x64;
    bool HW_ABM;
    bool HW_RDRAND;
    bool HW_RDSEED;
    bool HW_BMI1;
    bool HW_BMI2;
    bool HW_ADX;
    bool HW_MPX;
    bool HW_PREFETCHW;
    bool HW_PREFETCHWT1;
    bool HW_RDPID;
    bool HW_SSE;
    bool HW_SSE2;
    bool HW_SSE3;
    bool HW_SSSE3;
    bool HW_SSE41;
    bool HW_SSE42;
    bool HW_SSE4a;
    bool HW_AES;
    bool HW_SHA;
    bool HW_AVX;
    bool HW_XOP;
    bool HW_FMA3;
    bool HW_FMA4;
    bool HW_AVX2;
    bool HW_AVX512_F;
    bool HW_AVX512_CD;
    bool HW_AVX512_PF;
    bool HW_AVX512_ER;
    bool HW_AVX512_VL;
    bool HW_AVX512_BW;
    bool HW_AVX512_DQ;
    bool HW_AVX512_IFMA;
    bool HW_AVX512_VBMI;
    bool HW_AVX512_VPOPCNTDQ;
    bool HW_AVX512_4FMAPS;
    bool HW_AVX512_4VNNIW;
    bool HW_AVX512_VNNI;
    bool HW_AVX512_BF16;
    bool HW_AVX512_VBMI2;
    bool HW_GFNI;
    bool HW_VAES;
    bool HW_AVX512_VPCLMUL;
    bool HW_AVX512_BITALG;
};
extern void cpu_print_info();
extern void detect_host(struct cpu_x86* cpu);
extern void i386_cpuid_caches();
