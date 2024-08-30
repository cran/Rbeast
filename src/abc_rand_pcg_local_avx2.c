#include <stdio.h> 
#include <string.h>
#include <math.h>  
#include "abc_000_warning.h"
#include "abc_datatype.h"
#include "abc_ide_util.h"
#include "abc_vec.h"
#include "abc_rand_pcg_global.h"
#include "abc_rand_pcg_local.h"
#include "assert.h"
#ifdef COMPILER_MSVC
#define __attribute__(x)
#endif
#if  defined(COMPILER_CLANG) && !defined(cpu_ARM64)  
    #pragma clang optimize on
    #pragma clang attribute push (__attribute__((target("sse,sse2,sse3,ssse3,sse4,popcnt,avx,fma,avx2"))),apply_to=function)
#endif
#if  defined(COMPILER_GCC) && !defined(cpu_ARM64)  
    #pragma optimization_level 3
    #pragma GCC optimize("O3,Ofast,inline,omit-frame-pointer,no-asynchronous-unwind-tables")  
     #pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,avx,avx2,fma,tune=haswell")
#endif
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
#include "abc_math_avx.h"
#define PCG_DEFAULT_MULTIPLIER_64  6364136223846793005ULL 
#define PCG_DEFAULT_INCREMENT_64   1442695040888963407ULL 
#define PCG_DEFAULT_GLOBAL_STATE_64     0x853c49e6748fea9bULL
#define PCG_DEFAULT_GLOBAL_INCREMENT_64 0xda3e39cb94b95bdbULL
void avx_pcg_print_state(local_pcg32_random_t* rng) {
	r_printf("PCG State: \n");
	r_printf("State: %"  PRIx64 " %" PRIx64 " %" PRIx64 " %" PRIx64 "\n",rng->state[0],rng->state[1],rng->state[2],rng->state[3] );
	r_printf("Increment: %"  PRIx64  "\n",rng->increment);
	r_printf("Mutiplier4: %"  PRIx64  "\n",rng->MULTIPLIER_4steps);
	r_printf("INcrementr4: %"  PRIx64  "\n\n",rng->INCREMENT_4steps);
}
 void avx_pcg_set_seed(local_pcg32_random_t *rng,U64 initstate,U64 initseq)
{
	 initstate=PCG_DEFAULT_GLOBAL_STATE_64 ^ initseq; 
	initstate=initstate==0 ? PCG_DEFAULT_GLOBAL_STATE_64 : initstate;
	initseq=initseq==0 ?   PCG_DEFAULT_GLOBAL_INCREMENT_64 : initseq;
	rng->increment=(initseq << 1u)|1u; 
	U64 state=0U;						
	state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;	
	state+=initstate;	
	rng->state[0]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;
	rng->state[1]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;
	rng->state[2]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;
	rng->state[3]=state=state * PCG_DEFAULT_MULTIPLIER_64+rng->increment;
	__m256i state256=_mm256_set_epi64x(rng->state[3],rng->state[2],rng->state[1],rng->state[0]);;
	pcg_get_lcg_multiplier_shift_multistep(4L,PCG_DEFAULT_MULTIPLIER_64,rng->increment,&rng->MULTIPLIER_4steps,&rng->INCREMENT_4steps);
	rng->BUF_PTR=4;
	extern void init_gauss_rnd(void);
	init_gauss_rnd(); 
}
 static INLINE  __m256i  __attribute__((always_inline)) GetMoveMask(int n)    {
    __m128i maskIdx=_mm_cvtsi64_si128(0x0706050403020100);
    __m256i maskNum=_mm256_cvtepu8_epi32(maskIdx);
    __m256i nvec=_mm256_set1_epi32(n);
    __m256i maskmov=_mm256_cmpgt_epi32(nvec,maskNum);
    return maskmov;
}
#if defined(COMPILER_MSVC)
 static INLINE __m256i __attribute__((always_inline)) __mul64_haswell(__m256i a,__m256i b)
 {
	 __m256i bswap=_mm256_shuffle_epi32(b,0xB1);           
	 __m256i prodlh=_mm256_mullo_epi32(a,bswap);            
	 __m256i zero=_mm256_setzero_si256();                 
	 __m256i prodlh2=_mm256_hadd_epi32(prodlh,zero);         
	 __m256i prodlh3=_mm256_shuffle_epi32(prodlh2,0x73);     
	 __m256i prodll=_mm256_mul_epu32(a,b);                  
	 __m256i prod=_mm256_add_epi64(prodll,prodlh3);       
	 return  prod;
 }
#else
 static INLINE __m256i __mul64_haswell_ptr(__m256i * pa,__m256i *pb)
 {
	 __m256i a=_mm256_loadu_si256(pa);
	 __m256i b=_mm256_loadu_si256(pb);
	 __m256i bswap=_mm256_shuffle_epi32(b,0xB1);           
	 __m256i prodlh=_mm256_mullo_epi32(a,bswap);            
	 __m256i zero=_mm256_setzero_si256();                 
	 __m256i prodlh2=_mm256_hadd_epi32(prodlh,zero);         
	 __m256i prodlh3=_mm256_shuffle_epi32(prodlh2,0x73);     
	 __m256i prodll=_mm256_mul_epu32(a,b);                  
	 __m256i prod=_mm256_add_epi64(prodll,prodlh3);       
	 return  prod;
 }
#define __mul64_haswell(a,b) __mul64_haswell_ptr(&(a),&(b))
#endif
void avx_pcg_random(local_pcg32_random_t* rng,U32PTR rnd,I32 N) {
	const __m256i	INCREMENT_SHIFT=_mm256_set1_epi64x(rng->INCREMENT_4steps);
	const __m256i	MULITPLIER=_mm256_set1_epi64x(rng->MULTIPLIER_4steps);
	#define srl	_mm256_srli_epi64
	#define xor _mm256_xor_si256
	__m256i			oldstate=_mm256_loadu_si256(rng->state);
	I32 N4=(N+3)/4 * 4;		
	for (int i=0; i < N4; i+=4)	{
		__m256i xorshifted=srl ( xor(srl(oldstate,18u),oldstate),27u) ;
		__m256i rot=srl(oldstate,59u);	
		oldstate=_mm256_add_epi64(__mul64_haswell(oldstate,MULITPLIER),INCREMENT_SHIFT); 
		__m256i result=_mm256_or_si256(
							_mm256_srlv_epi32(xorshifted,rot),							
							_mm256_sllv_epi32(xorshifted,_mm256_sub_epi32(_mm256_set1_epi32(32),rot)) 
						);	
		__m128i r1=_mm256_castsi256_si128(result);
		__m128i r2=_mm256_extracti128_si256(result,1);		
		__m128  r=_mm_shuffle_ps(_mm_castsi128_ps(r1),_mm_castsi128_ps(r2),_MM_SHUFFLE(2,0,2,0));
		if (i < N - 3) {
			_mm_storeu_ps(rnd+i,r);
		} else {
			int n=N - i;
			__m256i mask=GetMoveMask(n);
			_mm_maskstore_ps(rnd+i,_mm256_castsi256_si128(mask),r);
		}
	}
	_mm256_storeu_si256(rng->state,oldstate);
	_mm256_zeroupper();
}
void avx_pcg_random_with_internalbuf(local_pcg32_random_t* rng,U32PTR rnd,I32 N) {
	__m256i oldstate=_mm256_loadu_si256(rng->state);
	while (N > 0) {
		if (rng->BUF_PTR < 4) {
			U32PTR rndbuf=(U32PTR) rng->INTERNAL_RNDBUF+rng->BUF_PTR;
			if (N==1L||rng->BUF_PTR==3) {
				rnd[0]=rndbuf[0];
				++rng->BUF_PTR;
				++rnd;
				--N;				
			}
			else if (rng->BUF_PTR==0 && N >=4) {
				_mm_storeu_ps(rnd,_mm_loadu_ps(rng->INTERNAL_RNDBUF));
				rng->BUF_PTR=4L;
				rnd+=4;
				N       -=4;
			} else {
				int nAvailable=(4 - rng->BUF_PTR);
				int nCopy=min(nAvailable,N);
				rnd[0]=rndbuf[0];
				rnd[1]=rndbuf[1];
                if (nCopy==3) {
					rnd[2]=rndbuf[2];
				}
				rng->BUF_PTR+=nCopy;
				rnd+=nCopy;
				N            -=nCopy;
			}
		}
		if (rng->BUF_PTR !=4) {
			continue;
		}
		const __m256i	INCREMENT_SHIFT=_mm256_set1_epi64x(rng->INCREMENT_4steps);
		const __m256i	MULITPLIER=_mm256_set1_epi64x(rng->MULTIPLIER_4steps);
		#define srl	_mm256_srli_epi64
		#define xor _mm256_xor_si256
		__m256i xorshifted=srl(xor (srl(oldstate,18u),oldstate),27u);
		__m256i rot=srl(oldstate,59u);
		oldstate=_mm256_add_epi64(__mul64_haswell(oldstate,MULITPLIER),INCREMENT_SHIFT); 
		__m256i result=_mm256_or_si256(
			_mm256_srlv_epi32(xorshifted,rot),
			_mm256_sllv_epi32(xorshifted,_mm256_sub_epi32(_mm256_set1_epi32(32),rot)) 
		);	
		__m128i r1=_mm256_castsi256_si128(result);
		__m128i r2=_mm256_extracti128_si256(result,1);
		__m128  r=_mm_shuffle_ps(_mm_castsi128_ps(r1),_mm_castsi128_ps(r2),_MM_SHUFFLE(2,0,2,0));
		_mm_storeu_ps(rng->INTERNAL_RNDBUF,r);
		rng->BUF_PTR=0;
	}
	_mm256_storeu_si256(rng->state,oldstate);
	_mm256_zeroupper();
}
void avx_pcg_random_vec8_slow(local_pcg32_random_t* rng,U32PTR rnd,I32 N) {
	__m256i			oldstate=_mm256_loadu_si256(rng->state );
	const __m256i	SHIFT=_mm256_set1_epi64x(rng->INCREMENT_4steps);
	const __m256i	MULITPLIER=_mm256_set1_epi64x(rng->MULTIPLIER_4steps);
	#define srl	_mm256_srli_epi64
	#define xor _mm256_xor_si256
	I32 N8=(N+7)/8 * 8;
	for (int i=0; i < N8; i+=8)	{
		__m256i xorshifted=srl ( xor(srl(oldstate,18u),oldstate),27u) ;
		__m256i rot=srl(oldstate,59u);	
		oldstate=_mm256_add_epi64(__mul64_haswell(oldstate,MULITPLIER),SHIFT); 
		__m256i xorshifted1=srl(xor (srl(oldstate,18u),oldstate),27u);
		__m256i rot1=srl(oldstate,59u);
		xorshifted1=_mm256_slli_epi64(xorshifted1,32);
		rot1=_mm256_slli_epi64(rot1,32);
		xorshifted=_mm256_blend_epi32(xorshifted,xorshifted1,0xAA); 
		rot=_mm256_blend_epi32(rot,rot1,0xAA); 
		__m256i result=_mm256_or_si256(
							_mm256_srlv_epi32(xorshifted,rot),							
							_mm256_sllv_epi32(xorshifted,_mm256_sub_epi32(_mm256_set1_epi32(32),rot))
						);	
		oldstate=_mm256_add_epi64(__mul64_haswell(oldstate,MULITPLIER),SHIFT); 
		if (i < N - 7) {
			_mm256_storeu_si256(rnd+i,result);
		} else {
			int n=N - i; 
			_mm256_maskstore_epi32((I32PTR)rnd+i,GetMoveMask(n),result);
		}
	}
	_mm256_storeu_si256(rng->state,oldstate);	
}
void SetupPCG_AVX2(void){
	 local_pcg_set_seed=avx_pcg_set_seed;
	 local_pcg_random=avx_pcg_random_with_internalbuf;
	 local_pcg_print_state=avx_pcg_print_state;
}
#endif
#if defined(COMPILER_CLANG) && !defined(cpu_ARM64)
    #pragma clang attribute pop
#endif
#include "abc_000_warning.h"
