#include <stdio.h> 
#include <string.h>
#include <math.h>  
#include "abc_000_warning.h"
#include "abc_datatype.h"
#include "abc_ide_util.h"
#include "abc_vec.h"
#include "abc_rand_pcg_global.h"
#include "abc_rand_pcg_local.h"
#if  defined(COMPILER_CLANG) && !defined(cpu_ARM64) 
	#pragma clang optimize on
	#pragma clang attribute push (__attribute__((target("avx,avx2,avx512f,avx512dq,avx512bw,avx512vl"))),apply_to=function)
#endif
#if  defined(COMPILER_GCC) && !defined(cpu_ARM64) 
    #pragma optimization_level 3
#pragma GCC optimize("O3,Ofast,inline,omit-frame-pointer,no-asynchronous-unwind-tables")  
	 #pragma GCC target("avx,avx2,avx512f,avx512dq,avx512bw,avx512vl") 
#endif
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
#include "abc_math_avx.h"
#define PCG_DEFAULT_MULTIPLIER_64  6364136223846793005ULL 
#define PCG_DEFAULT_INCREMENT_64   1442695040888963407ULL 
#define PCG_DEFAULT_GLOBAL_STATE_64     0x853c49e6748fea9bULL
#define PCG_DEFAULT_GLOBAL_INCREMENT_64 0xda3e39cb94b95bdbULL
void avx512_pcg_print_state(local_pcg32_random_t* rng) {
	r_printf("PCG State: \n");
	r_printf("State: %"  PRIx64 " %" PRIx64 " %" PRIx64 " %" PRIx64 "\n",rng->state512[0],rng->state512[1],rng->state512[2],rng->state512[3]);
	r_printf("State: %"  PRIx64 " %" PRIx64 " %" PRIx64 " %" PRIx64 "\n",rng->state512[4],rng->state512[5],rng->state512[6],rng->state512[7]);
	r_printf("Increment: %"  PRIx64  "\n",rng->increment512);
	r_printf("Mutiplier8: %"  PRIx64  "\n",rng->MULTIPLIER_8steps);
	r_printf("INcrementr8: %"  PRIx64  "\n\n",rng->INCREMENT_8steps);
}
void avx512_pcg_set_seed(local_pcg32_random_t* rng,U64 initstate,U64 initseq)
{
	initstate=PCG_DEFAULT_GLOBAL_STATE_64 ^ initseq; 
	initstate=initstate==0 ? PCG_DEFAULT_GLOBAL_STATE_64 : initstate;
	initseq=initseq==0 ? PCG_DEFAULT_GLOBAL_INCREMENT_64 : initseq;
	rng->increment512=(initseq << 1u)|1u; 
	U64 state=0U;
	state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;	
	state+=initstate;
	rng->state512[0]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[4]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[1]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[5]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[2]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[6]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[3]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	rng->state512[7]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment512;
	__m512i state512=_mm512_set_epi64(rng->state512[7],rng->state512[6],rng->state512[5],rng->state512[4],
									   rng->state512[3],rng->state512[2],rng->state512[1],rng->state512[0]);
	pcg_get_lcg_multiplier_shift_multistep(8L,PCG_DEFAULT_MULTIPLIER_64,rng->increment512,&rng->MULTIPLIER_8steps,&rng->INCREMENT_8steps);
	extern void init_gauss_rnd(void);
	init_gauss_rnd(); 
}
static __mmask16        masktemplate[16];
static INLINE void      FillMaskTemplate(void) { for (I32 i=0; i < 16; i++)    masktemplate[i]=(1UL << i) - 1UL; }
static INLINE __mmask16 GetMoveMask(int n) { return masktemplate[n]; }
void avx512_pcg_random(local_pcg32_random_t* rng,U32PTR rnd,I32 N) {
	const __m512i	INCREMENT_SHIFT=_mm512_set1_epi64(rng->INCREMENT_8steps);
	const __m512i	MULITPLIER=_mm512_set1_epi64(rng->MULTIPLIER_8steps);
	#define srl	_mm512_srli_epi64
	#define xor _mm512_xor_si512
	__m512i			oldstate=_mm512_loadu_si512(rng->state512);
	I32 N8=(N+7)/8 * 8;
	for (int i=0; i < N8; i+=8) {
		__m512i xorshifted=srl(xor (srl(oldstate,18u),oldstate),27u);
		__m512i rot=srl(oldstate,59u);
		oldstate=_mm512_add_epi64(_mm512_mullo_epi64(oldstate,MULITPLIER),INCREMENT_SHIFT); 
		__m512i result=_mm512_or_si512(
			_mm512_srlv_epi32(xorshifted,rot),
			_mm512_sllv_epi32(xorshifted,_mm512_sub_epi32(_mm512_set1_epi32(32),rot)) 
		);	
		__m256i r0=_mm512_castsi512_si256(result);
		__m256i r1=_mm512_extracti32x8_epi32(result,1);
		r1=_mm256_castps_si256(  _mm256_moveldup_ps(_mm256_castsi256_ps(r1)) );
		__m256i  r=_mm256_mask_blend_epi32(0xAA,r0,r1); 
		if (i < N - 7) 
			_mm256_storeu_si256(rnd+i,r);		
		else {
			int n=N - i;
			__mmask16 mask=GetMoveMask(n);
			_mm512_mask_storeu_epi32(rnd+i,mask,_mm512_castsi256_si512( r));
		}
	}
	_mm512_storeu_si512(rng->state512,oldstate);
	_mm256_zeroupper();
}
void SetupPCG_AVX512(void) {
	FillMaskTemplate();
	local_pcg_set_seed=avx512_pcg_set_seed;
	local_pcg_random=avx512_pcg_random;
	local_pcg_print_state=avx512_pcg_print_state;
}
#endif
#if defined(COMPILER_CLANG) && !defined(cpu_ARM64)
#pragma clang attribute pop
#endif
#include "abc_000_warning.h"
