#include "abc_000_warning.h"
#if  defined(COMPILER_CLANG) && !defined(cpu_ARM64)  
    #pragma clang optimize on
	#pragma clang attribute push (__attribute__((target("avx,avx2,avx512f,avx512dq,avx512bw"))),apply_to=function)
#endif
#if  defined(COMPILER_GCC) && !defined(cpu_ARM64)  
    #pragma optimization_level 3
    #pragma GCC optimize("O3,Ofast,inline,omit-frame-pointer,no-asynchronous-unwind-tables")  
	 #pragma GCC target("avx,avx2,avx512f,avx512dq,avx512bw") 
#endif
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
#include <immintrin.h>
#include "abc_vec.h"
#ifdef COMPILER_MSVC
    # define ALIGN64_BEG __declspec(align(64))
    # define ALIGN64_END 
#else
    # define ALIGN64_BEG
    # define ALIGN64_END __attribute__((aligned(64)))
#endif
typedef __m512  v16sf; 
typedef __m512i v16si; 
typedef __m256  v8sf; 
typedef __m256i v8si; 
typedef __m128i v4si; 
#define _PS512_CONST(Name,Val)                                            \
  static const ALIGN64_BEG float _ps512_##Name[16] ALIGN64_END={ Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val }
#define _PI32_CONST512(Name,Val)                                            \
  static const ALIGN64_BEG int _pi32_256_##Name[16] ALIGN64_END={ Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val }
#define _PS512_CONST_TYPE(Name,Type,Val)                                 \
  static const ALIGN64_BEG Type _ps512_##Name[16] ALIGN64_END={ Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val,Val }
_PS512_CONST(1,1.0f);
_PS512_CONST(0p5,0.5f);
_PS512_CONST_TYPE(min_norm_pos,int,0x00800000);
_PS512_CONST_TYPE(mant_mask,int,0x7f800000);
_PS512_CONST_TYPE(inv_mant_mask,int,~0x7f800000);
_PS512_CONST_TYPE(sign_mask,int,(int)0x80000000);
_PS512_CONST_TYPE(inv_sign_mask,int,~0x80000000);
_PI32_CONST512(0,0);
_PI32_CONST512(1,1);
_PI32_CONST512(inv1,~1);
_PI32_CONST512(2,2);
_PI32_CONST512(4,4);
_PI32_CONST512(0x7f,0x7f);
_PS512_CONST(cephes_SQRTHF,0.707106781186547524);
_PS512_CONST(cephes_log_p0,7.0376836292E-2);
_PS512_CONST(cephes_log_p1,-1.1514610310E-1);
_PS512_CONST(cephes_log_p2,1.1676998740E-1);
_PS512_CONST(cephes_log_p3,-1.2420140846E-1);
_PS512_CONST(cephes_log_p4,+1.4249322787E-1);
_PS512_CONST(cephes_log_p5,-1.6668057665E-1);
_PS512_CONST(cephes_log_p6,+2.0000714765E-1);
_PS512_CONST(cephes_log_p7,-2.4999993993E-1);
_PS512_CONST(cephes_log_p8,+3.3333331174E-1);
_PS512_CONST(cephes_log_q1,-2.12194440e-4);
_PS512_CONST(cephes_log_q2,0.693359375);
#define avx2_mm512_slli_epi32   _mm512_slli_epi32
#define avx2_mm512_srli_epi32   _mm512_srli_epi32
#define avx2_mm512_and_si512    _mm512_and_si512
#define avx2_mm512_andnot_si512 _mm512_andnot_si512
#define avx2_mm512_cmpeq_epi32  _mm512_cmpeq_epi32_mask
#define avx2_mm512_sub_epi32    _mm512_sub_epi32
#define avx2_mm512_add_epi32    _mm512_add_epi32
v16sf log512_ps(v16sf x) {
    v16si imm0;
    v16sf one=*(v16sf*)_ps512_1;
    v16sf invalid_mask=_mm512_castsi512_ps( _mm512_movm_epi32 (_mm512_cmp_ps_mask(x,_mm512_setzero_ps(),_CMP_LE_OS)  ) );
    x=_mm512_max_ps(x,*(v16sf*)_ps512_min_norm_pos);  
    imm0=avx2_mm512_srli_epi32(_mm512_castps_si512(x),23);
    x=_mm512_and_ps(x,*(v16sf*)_ps512_inv_mant_mask);
    x=_mm512_or_ps(x,*(v16sf*)_ps512_0p5);
    imm0=avx2_mm512_sub_epi32(imm0,*(v16si*)_pi32_256_0x7f);
    v16sf e=_mm512_cvtepi32_ps(imm0);
    e=_mm512_add_ps(e,one);
    v16sf mask=_mm512_castsi512_ps(_mm512_movm_epi32(_mm512_cmp_ps_mask(x,*(v16sf*)_ps512_cephes_SQRTHF,_CMP_LT_OS) ));
    v16sf tmp=_mm512_and_ps(x,mask);
    x=_mm512_sub_ps(x,one);
    e=_mm512_sub_ps(e,_mm512_and_ps(one,mask));
    x=_mm512_add_ps(x,tmp);
    v16sf z=_mm512_mul_ps(x,x);
    v16sf y=*(v16sf*)_ps512_cephes_log_p0;
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p1);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p2);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p3);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p4);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p5);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p6);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p7);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_log_p8);
    y=_mm512_mul_ps(y,x);
    y=_mm512_mul_ps(y,z);
    tmp=_mm512_mul_ps(e,*(v16sf*)_ps512_cephes_log_q1);
    y=_mm512_add_ps(y,tmp);
    tmp=_mm512_mul_ps(z,*(v16sf*)_ps512_0p5);
    y=_mm512_sub_ps(y,tmp);
    tmp=_mm512_mul_ps(e,*(v16sf*)_ps512_cephes_log_q2);
    x=_mm512_add_ps(x,y);
    x=_mm512_add_ps(x,tmp);
    x=_mm512_or_ps(x,invalid_mask ); 
    return x;
}
_PS512_CONST(exp_hi,88.3762626647949f);
_PS512_CONST(exp_lo,-88.3762626647949f);
_PS512_CONST(cephes_LOG2EF,1.44269504088896341);
_PS512_CONST(cephes_exp_C1,0.693359375);
_PS512_CONST(cephes_exp_C2,-2.12194440e-4);
_PS512_CONST(cephes_exp_p0,1.9875691500E-4);
_PS512_CONST(cephes_exp_p1,1.3981999507E-3);
_PS512_CONST(cephes_exp_p2,8.3334519073E-3);
_PS512_CONST(cephes_exp_p3,4.1665795894E-2);
_PS512_CONST(cephes_exp_p4,1.6666665459E-1);
_PS512_CONST(cephes_exp_p5,5.0000001201E-1);
v16sf exp512_ps(v16sf x) {
    v16sf tmp=_mm512_setzero_ps(),fx;
    v16si imm0;
    v16sf one=*(v16sf*)_ps512_1;
    x=_mm512_min_ps(x,*(v16sf*)_ps512_exp_hi);
    x=_mm512_max_ps(x,*(v16sf*)_ps512_exp_lo);
    fx=_mm512_mul_ps(x,*(v16sf*)_ps512_cephes_LOG2EF);
    fx=_mm512_add_ps(fx,*(v16sf*)_ps512_0p5);
    tmp=_mm512_floor_ps(fx);
    v16sf mask=_mm512_castsi512_ps(_mm512_movm_epi32(_mm512_cmp_ps_mask(tmp,fx,_CMP_GT_OS)));
    mask=_mm512_and_ps(mask,one);
    fx=_mm512_sub_ps(tmp,mask);
    tmp=_mm512_mul_ps(fx,*(v16sf*)_ps512_cephes_exp_C1);
    v16sf z=_mm512_mul_ps(fx,*(v16sf*)_ps512_cephes_exp_C2);
    x=_mm512_sub_ps(x,tmp);
    x=_mm512_sub_ps(x,z);
    z=_mm512_mul_ps(x,x);
    v16sf y=*(v16sf*)_ps512_cephes_exp_p0;
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_exp_p1);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_exp_p2);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_exp_p3);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_exp_p4);
    y=_mm512_mul_ps(y,x);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_cephes_exp_p5);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,x);
    y=_mm512_add_ps(y,one);
    imm0=_mm512_cvttps_epi32(fx);
    imm0=avx2_mm512_add_epi32(imm0,*(v16si*)_pi32_256_0x7f);
    imm0=avx2_mm512_slli_epi32(imm0,23);
    v16sf pow2n=_mm512_castsi512_ps(imm0);
    y=_mm512_mul_ps(y,pow2n);
    return y;
}
_PS512_CONST(minus_cephes_DP1,-0.78515625);
_PS512_CONST(minus_cephes_DP2,-2.4187564849853515625e-4);
_PS512_CONST(minus_cephes_DP3,-3.77489497744594108e-8);
_PS512_CONST(sincof_p0,-1.9515295891E-4);
_PS512_CONST(sincof_p1,8.3321608736E-3);
_PS512_CONST(sincof_p2,-1.6666654611E-1);
_PS512_CONST(coscof_p0,2.443315711809948E-005);
_PS512_CONST(coscof_p1,-1.388731625493765E-003);
_PS512_CONST(coscof_p2,4.166664568298827E-002);
_PS512_CONST(cephes_FOPI,1.27323954473516); 
v16sf sin512_ps(v16sf x) { 
    v16sf xmm1,xmm2=_mm512_setzero_ps(),xmm3,sign_bit,y;
    v16si imm0,imm2;
    sign_bit=x;
    x=_mm512_and_ps(x,*(v16sf*)_ps512_inv_sign_mask);
    sign_bit=_mm512_and_ps(sign_bit,*(v16sf*)_ps512_sign_mask);
    y=_mm512_mul_ps(x,*(v16sf*)_ps512_cephes_FOPI);
    imm2=_mm512_cvttps_epi32(y);
    imm2=avx2_mm512_add_epi32(imm2,*(v16si*)_pi32_256_1);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_inv1);
    y=_mm512_cvtepi32_ps(imm2);
    imm0=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_4);
    imm0=avx2_mm512_slli_epi32(imm0,29);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_2);
    imm2=_mm512_movm_epi32 (  avx2_mm512_cmpeq_epi32(imm2,*(v16si*)_pi32_256_0) );
    v16sf swap_sign_bit=_mm512_castsi512_ps(imm0);
    v16sf poly_mask=_mm512_castsi512_ps(imm2);
    sign_bit=_mm512_xor_ps(sign_bit,swap_sign_bit);
    xmm1=*(v16sf*)_ps512_minus_cephes_DP1;
    xmm2=*(v16sf*)_ps512_minus_cephes_DP2;
    xmm3=*(v16sf*)_ps512_minus_cephes_DP3;
    xmm1=_mm512_mul_ps(y,xmm1);
    xmm2=_mm512_mul_ps(y,xmm2);
    xmm3=_mm512_mul_ps(y,xmm3);
    x=_mm512_add_ps(x,xmm1);
    x=_mm512_add_ps(x,xmm2);
    x=_mm512_add_ps(x,xmm3);
    y=*(v16sf*)_ps512_coscof_p0;
    v16sf z=_mm512_mul_ps(x,x);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p1);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p2);
    y=_mm512_mul_ps(y,z);
    y=_mm512_mul_ps(y,z);
    v16sf tmp=_mm512_mul_ps(z,*(v16sf*)_ps512_0p5);
    y=_mm512_sub_ps(y,tmp);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_1);
    v16sf y2=*(v16sf*)_ps512_sincof_p0;
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p1);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p2);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_mul_ps(y2,x);
    y2=_mm512_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm512_and_ps(xmm3,y2); 
    y=_mm512_andnot_ps(xmm3,y);
    y=_mm512_add_ps(y,y2);
    y=_mm512_xor_ps(y,sign_bit);
    return y;
}
v16sf cos512_ps(v16sf x) { 
    v16sf xmm1,xmm2=_mm512_setzero_ps(),xmm3,y;
    v16si imm0,imm2;
    x=_mm512_and_ps(x,*(v16sf*)_ps512_inv_sign_mask);
    y=_mm512_mul_ps(x,*(v16sf*)_ps512_cephes_FOPI);
    imm2=_mm512_cvttps_epi32(y);
    imm2=avx2_mm512_add_epi32(imm2,*(v16si*)_pi32_256_1);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_inv1);
    y=_mm512_cvtepi32_ps(imm2);
    imm2=avx2_mm512_sub_epi32(imm2,*(v16si*)_pi32_256_2);
    imm0=avx2_mm512_andnot_si512(imm2,*(v16si*)_pi32_256_4);
    imm0=avx2_mm512_slli_epi32(imm0,29);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_2);
    imm2=_mm512_movm_epi32( avx2_mm512_cmpeq_epi32(imm2,*(v16si*)_pi32_256_0) );
    v16sf sign_bit=_mm512_castsi512_ps(imm0);
    v16sf poly_mask=_mm512_castsi512_ps(imm2);
    xmm1=*(v16sf*)_ps512_minus_cephes_DP1;
    xmm2=*(v16sf*)_ps512_minus_cephes_DP2;
    xmm3=*(v16sf*)_ps512_minus_cephes_DP3;
    xmm1=_mm512_mul_ps(y,xmm1);
    xmm2=_mm512_mul_ps(y,xmm2);
    xmm3=_mm512_mul_ps(y,xmm3);
    x=_mm512_add_ps(x,xmm1);
    x=_mm512_add_ps(x,xmm2);
    x=_mm512_add_ps(x,xmm3);
    y=*(v16sf*)_ps512_coscof_p0;
    v16sf z=_mm512_mul_ps(x,x);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p1);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p2);
    y=_mm512_mul_ps(y,z);
    y=_mm512_mul_ps(y,z);
    v16sf tmp=_mm512_mul_ps(z,*(v16sf*)_ps512_0p5);
    y=_mm512_sub_ps(y,tmp);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_1);
    v16sf y2=*(v16sf*)_ps512_sincof_p0;
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p1);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p2);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_mul_ps(y2,x);
    y2=_mm512_add_ps(y2,x);
    xmm3=poly_mask;
    y2=_mm512_and_ps(xmm3,y2); 
    y=_mm512_andnot_ps(xmm3,y);
    y=_mm512_add_ps(y,y2);
    y=_mm512_xor_ps(y,sign_bit);
    return y;
}
void sincos512_ps(v16sf x,v16sf* s,v16sf* c) {
    v16sf xmm1,xmm2,xmm3=_mm512_setzero_ps(),sign_bit_sin,y;
    v16si imm0,imm2,imm4;
    sign_bit_sin=x;
    x=_mm512_and_ps(x,*(v16sf*)_ps512_inv_sign_mask);
    sign_bit_sin=_mm512_and_ps(sign_bit_sin,*(v16sf*)_ps512_sign_mask);
    y=_mm512_mul_ps(x,*(v16sf*)_ps512_cephes_FOPI);
    imm2=_mm512_cvttps_epi32(y);
    imm2=avx2_mm512_add_epi32(imm2,*(v16si*)_pi32_256_1);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_inv1);
    y=_mm512_cvtepi32_ps(imm2);
    imm4=imm2;
    imm0=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_4);
    imm0=avx2_mm512_slli_epi32(imm0,29);
    imm2=avx2_mm512_and_si512(imm2,*(v16si*)_pi32_256_2);
    imm2=_mm512_movm_epi32( avx2_mm512_cmpeq_epi32(imm2,*(v16si*)_pi32_256_0));
    v16sf swap_sign_bit_sin=_mm512_castsi512_ps(imm0);
    v16sf poly_mask=_mm512_castsi512_ps(imm2);
    xmm1=*(v16sf*)_ps512_minus_cephes_DP1;
    xmm2=*(v16sf*)_ps512_minus_cephes_DP2;
    xmm3=*(v16sf*)_ps512_minus_cephes_DP3;
    xmm1=_mm512_mul_ps(y,xmm1);
    xmm2=_mm512_mul_ps(y,xmm2);
    xmm3=_mm512_mul_ps(y,xmm3);
    x=_mm512_add_ps(x,xmm1);
    x=_mm512_add_ps(x,xmm2);
    x=_mm512_add_ps(x,xmm3);
    imm4=avx2_mm512_sub_epi32(imm4,*(v16si*)_pi32_256_2);
    imm4=avx2_mm512_andnot_si512(imm4,*(v16si*)_pi32_256_4);
    imm4=avx2_mm512_slli_epi32(imm4,29);
    v16sf sign_bit_cos=_mm512_castsi512_ps(imm4);
    sign_bit_sin=_mm512_xor_ps(sign_bit_sin,swap_sign_bit_sin);
    v16sf z=_mm512_mul_ps(x,x);
    y=*(v16sf*)_ps512_coscof_p0;
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p1);
    y=_mm512_mul_ps(y,z);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_coscof_p2);
    y=_mm512_mul_ps(y,z);
    y=_mm512_mul_ps(y,z);
    v16sf tmp=_mm512_mul_ps(z,*(v16sf*)_ps512_0p5);
    y=_mm512_sub_ps(y,tmp);
    y=_mm512_add_ps(y,*(v16sf*)_ps512_1);
    v16sf y2=*(v16sf*)_ps512_sincof_p0;
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p1);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_add_ps(y2,*(v16sf*)_ps512_sincof_p2);
    y2=_mm512_mul_ps(y2,z);
    y2=_mm512_mul_ps(y2,x);
    y2=_mm512_add_ps(y2,x);
    xmm3=poly_mask;
    v16sf ysin2=_mm512_and_ps(xmm3,y2);
    v16sf ysin1=_mm512_andnot_ps(xmm3,y);
    y2=_mm512_sub_ps(y2,ysin2);
    y=_mm512_sub_ps(y,ysin1);
    xmm1=_mm512_add_ps(ysin1,ysin2);
    xmm2=_mm512_add_ps(y,y2);
    _mm512_storeu_ps(s,_mm512_xor_ps(xmm1,sign_bit_sin) );
    _mm512_storeu_ps(c,_mm512_xor_ps(xmm2,sign_bit_cos) );
}
v16sf pow512_ps(v16sf x,float n) {
    int    nInteger=n;
    float  nRemainder=n - nInteger;
    if (nRemainder !=0) {
        __m512 res=log512_ps(x);
        res=_mm512_mul_ps(res,_mm512_set1_ps(n));
        res=exp512_ps(res);
        return res;
    } else {
        nInteger=nInteger >=0 ? nInteger : -nInteger;
        __m512 res=_mm512_set1_ps(1.0f);
        while (nInteger) {
            if (nInteger & 1L)
                res=_mm512_mul_ps(res,x);
            x=_mm512_mul_ps(x,x);
            nInteger=nInteger >> 1;
        }       
        if (nInteger < 0)   res=_mm512_rcp14_ps(res);        
        return res;
    }
}
#endif 
#if defined(COMPILER_CLANG) && !defined(cpu_ARM64)
    #pragma clang attribute pop
#endif
#include "abc_000_warning.h"
