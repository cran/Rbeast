#pragma once
#include "abc_datatype.h"
typedef union local_pcg32_random_struct {
	struct {
	U64 ALIGN32_BEG
		state[4]
		ALIGN32_END;  
	U64	increment;	  
	U64 MULTIPLIER_4steps;
	U64 INCREMENT_4steps;
	U32 INTERNAL_RNDBUF[4];  
	int BUF_PTR;
	};
	struct {
		U64 ALIGN32_BEG
			state512[8]
			ALIGN32_END;    
		U64	increment512;	
		U64 MULTIPLIER_8steps;
		U64 INCREMENT_8steps;
	};
	struct {
		U64 STATE;
		U64	INCREMENT;	
	};
} local_pcg32_random_t;
extern void (*local_pcg_set_seed)(local_pcg32_random_t* rng,U64 initstate,U64 initseq);
extern void (*local_pcg_random)(local_pcg32_random_t* rng,U32PTR rnd,I32 N);
extern void (*local_pcg_print_state)(local_pcg32_random_t* rng);
extern F32  local_pcg_trgauss_oneside_scalar(local_pcg32_random_t* rng,F32 a,int whichside);
extern  void local_pcg_randint(local_pcg32_random_t* rng,int bnd,I32PTR RND,int N);
extern void local_pcg_gauss(local_pcg32_random_t* rng,F32PTR RND,int N);
extern void local_pcg_gamma(local_pcg32_random_t* rng,F32PTR rnd,F32 a,int N);
extern void local_pcg_wishart_unit_lowtriangle_zeroout(local_pcg32_random_t* rng,F32PTR rnd,F32PTR tmp,I32 m,F32 df);
extern void local_pcg_wishart_unit_lowtriangle_zeroout_notmp(local_pcg32_random_t* rng,F32PTR wishrnd,I32 m,F32 df);
extern void local_pcg_invwishart_upper(local_pcg32_random_t* rng,F32PTR iwrnd_upper,F32PTR iwrnd_upper_inv,F32PTR tmp,I32 m,F32PTR Qup,F32 df);
extern void SetupPCG_GENERIC(void);
extern void SetupPCG_AVX2(void);
extern void SetupPCG_AVX512(void);
#define VSLStreamStatePtr													  local_pcg32_random_t														 
#define r_vslNewStream(stream,METHOD_dummy,seed)							  local_pcg_set_seed(stream,0x853c49e6748fea9bULL,seed)  
#define r_viRngUniformBits32(  METHOD_dummy,stream,N,rnd)				  local_pcg_random(&stream,rnd,N)
#define r_vsRngGamma(		   METHOD_dummy,stream,N,rnd,a,b,beta_dummy) local_pcg_gamma(&stream,rnd,a,N)
#define r_vsRngGaussian(	   MTEHDO_dummy,stream,N,rnd,a,b)			  local_pcg_gauss(&stream,rnd,N)
#define r_vslDeleteStream(stream_dummy)  
