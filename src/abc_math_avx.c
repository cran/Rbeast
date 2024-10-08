#include "abc_000_warning.h"
#if defined(COMPILER_CLANG) && !defined(cpu_ARM64)
    #pragma clang optimize on
    #pragma clang attribute push (__attribute__((target("sse,sse2,sse3,ssse3,sse4,popcnt,avx,fma,avx2"))),apply_to=function)
#endif
#if  defined(COMPILER_GCC) && !defined(cpu_ARM64)  
    #pragma optimization_level 3
    #pragma GCC optimize("O3,Ofast,inline,omit-frame-pointer,no-asynchronous-unwind-tables")  
    #pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,avx,avx2,fma,tune=haswell")
#endif
#define NF         8
#define NF2        (NF*2)
#define NF3        (NF*3)
#define NF4        (NF*4)
#define load        _mm256_loadu_ps
#define store       _mm256_storeu_ps
#define loadi       _mm256_loadu_si256
#define storei      _mm256_storeu_si256
#define maskload    _mm256_maskload_ps
#define maskstore   _mm256_maskstore_ps
#define maskloadi    _mm256_maskload_epi32
#define maskstorei  _mm256_maskstore_epi32
#define set1        _mm256_set1_ps
#define set1_i32    _mm256_set1_epi32
#define set0        _mm256_setzero_ps
#define set0i       _mm256_setzero_si256
#define mul         _mm256_mul_ps
#define add         _mm256_add_ps
#define sub         _mm256_sub_ps
#define addi32      _mm256_add_epi32
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
#include <immintrin.h>
#include "abc_vec.h"
#include "abc_math_avx.h"
#ifdef COMPILER_MSVC
    # define ALIGN32_BEG __declspec(align(32))
    # define ALIGN32_END 
#else
    # define ALIGN32_BEG
    # define ALIGN32_END __attribute__((aligned(32)))
#endif
#define _PI32AVX_CONST(Name,Val)                                            \
  static const ALIGN32_BEG int _pi32avx_##Name[4] ALIGN32_END={ Val,Val,Val,Val }
_PI32AVX_CONST(1,1);
_PI32AVX_CONST(inv1,~1);
_PI32AVX_CONST(2,2);
_PI32AVX_CONST(4,4);
#define _PS256_CONST(Name,Val)                                            \
  static const ALIGN32_BEG float _ps256_##Name[8] ALIGN32_END={ Val,Val,Val,Val,Val,Val,Val,Val }
#define _PI32_CONST256(Name,Val)                                            \
  static const ALIGN32_BEG int _pi32_256_##Name[8] ALIGN32_END={ Val,Val,Val,Val,Val,Val,Val,Val }
#define _PS256_CONST_TYPE(Name,Type,Val)                                 \
  static const ALIGN32_BEG Type _ps256_##Name[8] ALIGN32_END={ Val,Val,Val,Val,Val,Val,Val,Val }
_PS256_CONST(1,1.0f);
_PS256_CONST(0p5,0.5f);
_PS256_CONST_TYPE(min_norm_pos,int,0x00800000);
_PS256_CONST_TYPE(mant_mask,int,0x7f800000);
_PS256_CONST_TYPE(inv_mant_mask,int,~0x7f800000);
_PS256_CONST_TYPE(sign_mask,int,(int)0x80000000);
_PS256_CONST_TYPE(inv_sign_mask,int,~0x80000000);
_PI32_CONST256(0,0);
_PI32_CONST256(1,1);
_PI32_CONST256(inv1,~1);
_PI32_CONST256(2,2);
_PI32_CONST256(4,4);
_PI32_CONST256(0x7f,0x7f);
_PS256_CONST(cephes_SQRTHF,0.707106781186547524);
_PS256_CONST(cephes_log_p0,7.0376836292E-2);
_PS256_CONST(cephes_log_p1,-1.1514610310E-1);
_PS256_CONST(cephes_log_p2,1.1676998740E-1);
_PS256_CONST(cephes_log_p3,-1.2420140846E-1);
_PS256_CONST(cephes_log_p4,+1.4249322787E-1);
_PS256_CONST(cephes_log_p5,-1.6668057665E-1);
_PS256_CONST(cephes_log_p6,+2.0000714765E-1);
_PS256_CONST(cephes_log_p7,-2.4999993993E-1);
_PS256_CONST(cephes_log_p8,+3.3333331174E-1);
_PS256_CONST(cephes_log_q1,-2.12194440e-4);
_PS256_CONST(cephes_log_q2,0.693359375);
#define avx2_mm256_slli_epi32 _mm256_slli_epi32
#define avx2_mm256_srli_epi32 _mm256_srli_epi32
#define avx2_mm256_and_si256 _mm256_and_si256
#define avx2_mm256_andnot_si256 _mm256_andnot_si256
#define avx2_mm256_cmpeq_epi32 _mm256_cmpeq_epi32
#define avx2_mm256_sub_epi32 _mm256_sub_epi32
#define avx2_mm256_add_epi32 _mm256_add_epi32
#if defined(COMPILER_MSVC)
v8sf log256_ps(F32PTR px) {
    v8sf   x=_mm256_loadu_ps(px);
    v8si imm0;
    v8sf one=*(v8sf*)_ps256_1;
    v8sf invalid_mask=_mm256_cmp_ps(x,_mm256_setzero_ps(),_CMP_LE_OS);
    x=_mm256_max_ps(x,*(v8sf*)_ps256_min_norm_pos);  
    imm0=avx2_mm256_srli_epi32(_mm256_castps_si256(x),23);
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_mant_mask);
    x=_mm256_or_ps(x,*(v8sf*)_ps256_0p5);
    imm0=avx2_mm256_sub_epi32(imm0,*(v8si*)_pi32_256_0x7f);
    v8sf e=_mm256_cvtepi32_ps(imm0);
    e=_mm256_add_ps(e,one);
    v8sf mask=_mm256_cmp_ps(x,*(v8sf*)_ps256_cephes_SQRTHF,_CMP_LT_OS);
    v8sf tmp=_mm256_and_ps(x,mask);
    x=_mm256_sub_ps(x,one);
    e=_mm256_sub_ps(e,_mm256_and_ps(one,mask));
    x=_mm256_add_ps(x,tmp);
    v8sf z=_mm256_mul_ps(x,x);
    v8sf y=*(v8sf*)_ps256_cephes_log_p0;
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p1);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p2);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p3);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p4);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p5);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p6);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p7);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p8);
    y=_mm256_mul_ps(y,x);
    y=_mm256_mul_ps(y,z);
    tmp=_mm256_mul_ps(e,*(v8sf*)_ps256_cephes_log_q1);
    y=_mm256_add_ps(y,tmp);
    tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    tmp=_mm256_mul_ps(e,*(v8sf*)_ps256_cephes_log_q2);
    x=_mm256_add_ps(x,y);
    x=_mm256_add_ps(x,tmp);
    x=_mm256_or_ps(x,invalid_mask); 
    return x;
}
_PS256_CONST(exp_hi,88.3762626647949f);
_PS256_CONST(exp_lo,-88.3762626647949f);
_PS256_CONST(cephes_LOG2EF,1.44269504088896341);
_PS256_CONST(cephes_exp_C1,0.693359375);
_PS256_CONST(cephes_exp_C2,-2.12194440e-4);
_PS256_CONST(cephes_exp_p0,1.9875691500E-4);
_PS256_CONST(cephes_exp_p1,1.3981999507E-3);
_PS256_CONST(cephes_exp_p2,8.3334519073E-3);
_PS256_CONST(cephes_exp_p3,4.1665795894E-2);
_PS256_CONST(cephes_exp_p4,1.6666665459E-1);
_PS256_CONST(cephes_exp_p5,5.0000001201E-1);
v8sf exp256_ps(F32PTR px) {
    v8sf   x=_mm256_loadu_ps(px);
    v8sf tmp=_mm256_setzero_ps(),fx;
    v8si imm0;
    v8sf one=*(v8sf*)_ps256_1;
    x=_mm256_min_ps(x,*(v8sf*)_ps256_exp_hi);
    x=_mm256_max_ps(x,*(v8sf*)_ps256_exp_lo);
    fx=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_LOG2EF);
    fx=_mm256_add_ps(fx,*(v8sf*)_ps256_0p5);
    tmp=_mm256_floor_ps(fx);
    v8sf mask=_mm256_cmp_ps(tmp,fx,_CMP_GT_OS);
    mask=_mm256_and_ps(mask,one);
    fx=_mm256_sub_ps(tmp,mask);
    tmp=_mm256_mul_ps(fx,*(v8sf*)_ps256_cephes_exp_C1);
    v8sf z=_mm256_mul_ps(fx,*(v8sf*)_ps256_cephes_exp_C2);
    x=_mm256_sub_ps(x,tmp);
    x=_mm256_sub_ps(x,z);
    z=_mm256_mul_ps(x,x);
    v8sf y=*(v8sf*)_ps256_cephes_exp_p0;
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p1);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p2);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p3);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p4);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p5);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,x);
    y=_mm256_add_ps(y,one);
    imm0=_mm256_cvttps_epi32(fx);
    imm0=avx2_mm256_add_epi32(imm0,*(v8si*)_pi32_256_0x7f);
    imm0=avx2_mm256_slli_epi32(imm0,23);
    v8sf pow2n=_mm256_castsi256_ps(imm0);
    y=_mm256_mul_ps(y,pow2n);
    return y;
}
_PS256_CONST(minus_cephes_DP1,-0.78515625);
_PS256_CONST(minus_cephes_DP2,-2.4187564849853515625e-4);
_PS256_CONST(minus_cephes_DP3,-3.77489497744594108e-8);
_PS256_CONST(sincof_p0,-1.9515295891E-4);
_PS256_CONST(sincof_p1,8.3321608736E-3);
_PS256_CONST(sincof_p2,-1.6666654611E-1);
_PS256_CONST(coscof_p0,2.443315711809948E-005);
_PS256_CONST(coscof_p1,-1.388731625493765E-003);
_PS256_CONST(coscof_p2,4.166664568298827E-002);
_PS256_CONST(cephes_FOPI,1.27323954473516); 
v8sf sin256_ps(F32PTR px) { 
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2=_mm256_setzero_ps(),xmm3,sign_bit,y;
    v8si imm0,imm2;
    sign_bit=x;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    sign_bit=_mm256_and_ps(sign_bit,*(v8sf*)_ps256_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm0=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf swap_sign_bit=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    sign_bit=_mm256_xor_ps(sign_bit,swap_sign_bit);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    y=*(v8sf*)_ps256_coscof_p0;
    v8sf z=_mm256_mul_ps(x,x);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm256_and_ps(xmm3,y2); 
    y=_mm256_andnot_ps(xmm3,y);
    y=_mm256_add_ps(y,y2);
    y=_mm256_xor_ps(y,sign_bit);
    return y;
}
v8sf cos256_ps(F32PTR px) { 
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2=_mm256_setzero_ps(),xmm3,y;
    v8si imm0,imm2;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm2=avx2_mm256_sub_epi32(imm2,*(v8si*)_pi32_256_2);
    imm0=avx2_mm256_andnot_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf sign_bit=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    y=*(v8sf*)_ps256_coscof_p0;
    v8sf z=_mm256_mul_ps(x,x);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm256_and_ps(xmm3,y2); 
    y=_mm256_andnot_ps(xmm3,y);
    y=_mm256_add_ps(y,y2);
    y=_mm256_xor_ps(y,sign_bit);
    return y;
}
void sincos256_ps(F32PTR px,F32PTR s,F32PTR c) {
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2,xmm3=_mm256_setzero_ps(),sign_bit_sin,y;
    v8si imm0,imm2,imm4;
    sign_bit_sin=x;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    sign_bit_sin=_mm256_and_ps(sign_bit_sin,*(v8sf*)_ps256_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm4=imm2;
    imm0=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf swap_sign_bit_sin=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    imm4=avx2_mm256_sub_epi32(imm4,*(v8si*)_pi32_256_2);
    imm4=avx2_mm256_andnot_si256(imm4,*(v8si*)_pi32_256_4);
    imm4=avx2_mm256_slli_epi32(imm4,29);
    v8sf sign_bit_cos=_mm256_castsi256_ps(imm4);
    sign_bit_sin=_mm256_xor_ps(sign_bit_sin,swap_sign_bit_sin);
    v8sf z=_mm256_mul_ps(x,x);
    y=*(v8sf*)_ps256_coscof_p0;
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    v8sf ysin2=_mm256_and_ps(xmm3,y2);
    v8sf ysin1=_mm256_andnot_ps(xmm3,y);
    y2=_mm256_sub_ps(y2,ysin2);
    y=_mm256_sub_ps(y,ysin1);
    xmm1=_mm256_add_ps(ysin1,ysin2);
    xmm2=_mm256_add_ps(y,y2);
    _mm256_storeu_ps(s,_mm256_xor_ps(xmm1,sign_bit_sin));
    _mm256_storeu_ps(c,_mm256_xor_ps(xmm2,sign_bit_cos));
}
v8sf pow256_ps(F32PTR px,float n) {
    int    nInteger=n;
    float  nRemainder=n - nInteger;
    if (nRemainder !=0) {
        __m256 res;        
        res=_mm256_mul_ps( log256_ps(px),_mm256_set1_ps(n));
        res=exp256_ps(&res);
        return res;
    } else {
        v8sf   x=_mm256_loadu_ps(px);
        nInteger=nInteger >=0 ? nInteger : -nInteger;
        __m256 res=_mm256_set1_ps(1.0f);
        while (nInteger) {
            if (nInteger & 1L)
                res=_mm256_mul_ps(res,x);
            x=_mm256_mul_ps(x,x);
            nInteger=nInteger >> 1;
        }       
        if (nInteger < 0)   res=_mm256_rcp_ps(res);        
        return res;
    }
} 
#else 
void   log256_ps_ptr(F32PTR px,F32PTR out) {
    v8sf   x __attribute__((aligned(8)))=_mm256_loadu_ps(px);
    v8si imm0;
    v8sf one=*(v8sf*)_ps256_1;
    v8sf invalid_mask=_mm256_cmp_ps(x,_mm256_setzero_ps(),_CMP_LE_OS);
    x=_mm256_max_ps(x,*(v8sf*)_ps256_min_norm_pos);  
    imm0=avx2_mm256_srli_epi32(_mm256_castps_si256(x),23);
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_mant_mask);
    x=_mm256_or_ps(x,*(v8sf*)_ps256_0p5);
    imm0=avx2_mm256_sub_epi32(imm0,*(v8si*)_pi32_256_0x7f);
    v8sf e=_mm256_cvtepi32_ps(imm0);
    e=_mm256_add_ps(e,one);
    v8sf mask=_mm256_cmp_ps(x,*(v8sf*)_ps256_cephes_SQRTHF,_CMP_LT_OS);
    v8sf tmp=_mm256_and_ps(x,mask);
    x=_mm256_sub_ps(x,one);
    e=_mm256_sub_ps(e,_mm256_and_ps(one,mask));
    x=_mm256_add_ps(x,tmp);
    v8sf z=_mm256_mul_ps(x,x);
    v8sf y=*(v8sf*)_ps256_cephes_log_p0;
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p1);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p2);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p3);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p4);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p5);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p6);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p7);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_log_p8);
    y=_mm256_mul_ps(y,x);
    y=_mm256_mul_ps(y,z);
    tmp=_mm256_mul_ps(e,*(v8sf*)_ps256_cephes_log_q1);
    y=_mm256_add_ps(y,tmp);
    tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    tmp=_mm256_mul_ps(e,*(v8sf*)_ps256_cephes_log_q2);
    x=_mm256_add_ps(x,y);
    x=_mm256_add_ps(x,tmp);
    x=_mm256_or_ps(x,invalid_mask); 
    _mm256_storeu_ps(out,x);
}
_PS256_CONST(exp_hi,88.3762626647949f);
_PS256_CONST(exp_lo,-88.3762626647949f);
_PS256_CONST(cephes_LOG2EF,1.44269504088896341);
_PS256_CONST(cephes_exp_C1,0.693359375);
_PS256_CONST(cephes_exp_C2,-2.12194440e-4);
_PS256_CONST(cephes_exp_p0,1.9875691500E-4);
_PS256_CONST(cephes_exp_p1,1.3981999507E-3);
_PS256_CONST(cephes_exp_p2,8.3334519073E-3);
_PS256_CONST(cephes_exp_p3,4.1665795894E-2);
_PS256_CONST(cephes_exp_p4,1.6666665459E-1);
_PS256_CONST(cephes_exp_p5,5.0000001201E-1);
void exp256_ps_ptr(F32PTR px,F32PTR out) {
    v8sf   x=_mm256_loadu_ps(px);
    v8sf tmp=_mm256_setzero_ps(),fx;
    v8si imm0;
    v8sf one=*(v8sf*)_ps256_1;
    x=_mm256_min_ps(x,*(v8sf*)_ps256_exp_hi);
    x=_mm256_max_ps(x,*(v8sf*)_ps256_exp_lo);
    fx=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_LOG2EF);
    fx=_mm256_add_ps(fx,*(v8sf*)_ps256_0p5);
    tmp=_mm256_floor_ps(fx);
    v8sf mask=_mm256_cmp_ps(tmp,fx,_CMP_GT_OS);
    mask=_mm256_and_ps(mask,one);
    fx=_mm256_sub_ps(tmp,mask);
    tmp=_mm256_mul_ps(fx,*(v8sf*)_ps256_cephes_exp_C1);
    v8sf z=_mm256_mul_ps(fx,*(v8sf*)_ps256_cephes_exp_C2);
    x=_mm256_sub_ps(x,tmp);
    x=_mm256_sub_ps(x,z);
    z=_mm256_mul_ps(x,x);
    v8sf y=*(v8sf*)_ps256_cephes_exp_p0;
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p1);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p2);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p3);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p4);
    y=_mm256_mul_ps(y,x);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_cephes_exp_p5);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,x);
    y=_mm256_add_ps(y,one);
    imm0=_mm256_cvttps_epi32(fx);
    imm0=avx2_mm256_add_epi32(imm0,*(v8si*)_pi32_256_0x7f);
    imm0=avx2_mm256_slli_epi32(imm0,23);
    v8sf pow2n=_mm256_castsi256_ps(imm0);
    y=_mm256_mul_ps(y,pow2n);
    _mm256_storeu_ps(out,y);
}
_PS256_CONST(minus_cephes_DP1,-0.78515625);
_PS256_CONST(minus_cephes_DP2,-2.4187564849853515625e-4);
_PS256_CONST(minus_cephes_DP3,-3.77489497744594108e-8);
_PS256_CONST(sincof_p0,-1.9515295891E-4);
_PS256_CONST(sincof_p1,8.3321608736E-3);
_PS256_CONST(sincof_p2,-1.6666654611E-1);
_PS256_CONST(coscof_p0,2.443315711809948E-005);
_PS256_CONST(coscof_p1,-1.388731625493765E-003);
_PS256_CONST(coscof_p2,4.166664568298827E-002);
_PS256_CONST(cephes_FOPI,1.27323954473516); 
void  sin256_ps_ptr(F32PTR px,F32PTR  out) { 
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2=_mm256_setzero_ps(),xmm3,sign_bit,y;
    v8si imm0,imm2;
    sign_bit=x;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    sign_bit=_mm256_and_ps(sign_bit,*(v8sf*)_ps256_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm0=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf swap_sign_bit=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    sign_bit=_mm256_xor_ps(sign_bit,swap_sign_bit);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    y=*(v8sf*)_ps256_coscof_p0;
    v8sf z=_mm256_mul_ps(x,x);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm256_and_ps(xmm3,y2); 
    y=_mm256_andnot_ps(xmm3,y);
    y=_mm256_add_ps(y,y2);
    y=_mm256_xor_ps(y,sign_bit);
    _mm256_storeu_ps(out,y);
}
void cos256_ps_ptr(F32PTR px,F32PTR out) { 
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2=_mm256_setzero_ps(),xmm3,y;
    v8si imm0,imm2;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm2=avx2_mm256_sub_epi32(imm2,*(v8si*)_pi32_256_2);
    imm0=avx2_mm256_andnot_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf sign_bit=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    y=*(v8sf*)_ps256_coscof_p0;
    v8sf z=_mm256_mul_ps(x,x);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm256_and_ps(xmm3,y2); 
    y=_mm256_andnot_ps(xmm3,y);
    y=_mm256_add_ps(y,y2);
    y=_mm256_xor_ps(y,sign_bit);
    _mm256_storeu_ps(out,y);
}
void sincos256_ps_ptr(F32PTR px,F32PTR s,F32PTR c) {
    v8sf   x=_mm256_loadu_ps(px);
    v8sf xmm1,xmm2,xmm3=_mm256_setzero_ps(),sign_bit_sin,y;
    v8si imm0,imm2,imm4;
    sign_bit_sin=x;
    x=_mm256_and_ps(x,*(v8sf*)_ps256_inv_sign_mask);
    sign_bit_sin=_mm256_and_ps(sign_bit_sin,*(v8sf*)_ps256_sign_mask);
    y=_mm256_mul_ps(x,*(v8sf*)_ps256_cephes_FOPI);
    imm2=_mm256_cvttps_epi32(y);
    imm2=avx2_mm256_add_epi32(imm2,*(v8si*)_pi32_256_1);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_inv1);
    y=_mm256_cvtepi32_ps(imm2);
    imm4=imm2;
    imm0=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_4);
    imm0=avx2_mm256_slli_epi32(imm0,29);
    imm2=avx2_mm256_and_si256(imm2,*(v8si*)_pi32_256_2);
    imm2=avx2_mm256_cmpeq_epi32(imm2,*(v8si*)_pi32_256_0);
    v8sf swap_sign_bit_sin=_mm256_castsi256_ps(imm0);
    v8sf poly_mask=_mm256_castsi256_ps(imm2);
    xmm1=*(v8sf*)_ps256_minus_cephes_DP1;
    xmm2=*(v8sf*)_ps256_minus_cephes_DP2;
    xmm3=*(v8sf*)_ps256_minus_cephes_DP3;
    xmm1=_mm256_mul_ps(y,xmm1);
    xmm2=_mm256_mul_ps(y,xmm2);
    xmm3=_mm256_mul_ps(y,xmm3);
    x=_mm256_add_ps(x,xmm1);
    x=_mm256_add_ps(x,xmm2);
    x=_mm256_add_ps(x,xmm3);
    imm4=avx2_mm256_sub_epi32(imm4,*(v8si*)_pi32_256_2);
    imm4=avx2_mm256_andnot_si256(imm4,*(v8si*)_pi32_256_4);
    imm4=avx2_mm256_slli_epi32(imm4,29);
    v8sf sign_bit_cos=_mm256_castsi256_ps(imm4);
    sign_bit_sin=_mm256_xor_ps(sign_bit_sin,swap_sign_bit_sin);
    v8sf z=_mm256_mul_ps(x,x);
    y=*(v8sf*)_ps256_coscof_p0;
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p1);
    y=_mm256_mul_ps(y,z);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_coscof_p2);
    y=_mm256_mul_ps(y,z);
    y=_mm256_mul_ps(y,z);
    v8sf tmp=_mm256_mul_ps(z,*(v8sf*)_ps256_0p5);
    y=_mm256_sub_ps(y,tmp);
    y=_mm256_add_ps(y,*(v8sf*)_ps256_1);
    v8sf y2=*(v8sf*)_ps256_sincof_p0;
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p1);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_add_ps(y2,*(v8sf*)_ps256_sincof_p2);
    y2=_mm256_mul_ps(y2,z);
    y2=_mm256_mul_ps(y2,x);
    y2=_mm256_add_ps(y2,x);
    xmm3=poly_mask;
    v8sf ysin2=_mm256_and_ps(xmm3,y2);
    v8sf ysin1=_mm256_andnot_ps(xmm3,y);
    y2=_mm256_sub_ps(y2,ysin2);
    y=_mm256_sub_ps(y,ysin1);
    xmm1=_mm256_add_ps(ysin1,ysin2);
    xmm2=_mm256_add_ps(y,y2);
    _mm256_storeu_ps(s,_mm256_xor_ps(xmm1,sign_bit_sin));
    _mm256_storeu_ps(c,_mm256_xor_ps(xmm2,sign_bit_cos));
}
void  pow256_ps_ptr(F32PTR px,float n,F32PTR out) {
    int    nInteger=n;
    float  nRemainder=n - nInteger;
    if (nRemainder !=0) {
        __m256 res;
        res=_mm256_mul_ps(log256_ps(px),_mm256_set1_ps(n));
        res=exp256_ps(&res);
        _mm256_storeu_ps(out,res);
        return;
    } else {
        v8sf   x=_mm256_loadu_ps(px);
        nInteger=nInteger >=0 ? nInteger : -nInteger;
        __m256 res=_mm256_set1_ps(1.0f);
        while (nInteger) {
            if (nInteger & 1L)
                res=_mm256_mul_ps(res,x);
            x=_mm256_mul_ps(x,x);
            nInteger=nInteger >> 1;
        }       
        if (nInteger < 0)   res=_mm256_rcp_ps(res);
        _mm256_storeu_ps(out,res);
        return;
    }
} 
#endif
#endif 
#if defined(COMPILER_CLANG) && !defined(cpu_ARM64)
    #pragma clang attribute pop
#endif
#include "abc_000_warning.h"
