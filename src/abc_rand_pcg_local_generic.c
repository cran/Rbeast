#include <math.h>  
#include "abc_000_warning.h"
#include "abc_datatype.h" 
#include "abc_ide_util.h"
#include "abc_rand_pcg_global.h" 
#include "abc_rand_pcg_local.h" 
#define PCG_DEFAULT_MULTIPLIER_64  6364136223846793005ULL 
#define PCG_DEFAULT_INCREMENT_64   1442695040888963407ULL 
#define PCG_DEFAULT_GLOBAL_STATE_64     0x853c49e6748fea9bULL
#define PCG_DEFAULT_GLOBAL_INCREMENT_64 0xda3e39cb94b95bdbULL
void gen_pcg_print_state(local_pcg32_random_t* rng) {
	r_printf("PCG State: \n");
	r_printf("State: %"  PRIx64 "\n",rng->STATE);	
	r_printf("Increment: %"  PRIx64  "\n",rng->INCREMENT);
}
void gen_pcg_random(local_pcg32_random_t* rng,U32PTR rnd,I32 N);
void gen_pcg_set_seed(local_pcg32_random_t* rng,U64 initstate,U64 initseq)
{
	initstate=PCG_DEFAULT_GLOBAL_STATE_64 ^ initseq; 
	initstate=initstate==0 ? PCG_DEFAULT_GLOBAL_STATE_64     : initstate;
	initseq=initseq==0   ? PCG_DEFAULT_GLOBAL_INCREMENT_64 : initseq;
	rng->STATE=0U;
	rng->INCREMENT=(initseq << 1u)|1u;  	
	U32 rnd;
	gen_pcg_random(rng,&rnd,1);
	rng->STATE+=initstate;
	gen_pcg_random(rng,&rnd,1);
	extern void init_gauss_rnd(void);
	init_gauss_rnd(); 
}
void gen_pcg_random(local_pcg32_random_t* rng,U32PTR rnd,I32 N) 
{
	U64 oldstate=rng->STATE;
	U64 shift=rng->INCREMENT;
	for (int i=0; i < N; i++)
	{
		U32 xorshifted=((oldstate >> 18u) ^ oldstate) >> 27u   ;
		U32 rot=oldstate >> 59u;
		*rnd++=(xorshifted >> rot)|(xorshifted << ((- *((I32*)(&rot))  ) & 31)); 
		oldstate=oldstate * PCG_DEFAULT_MULTIPLIER_64+shift;
	}
	rng->STATE=oldstate;
}
void SetupPCG_GENERIC() {
	local_pcg_set_seed=gen_pcg_set_seed;
	local_pcg_random=gen_pcg_random;
	local_pcg_print_state=gen_pcg_print_state;
}
#include "abc_000_warning.h"
