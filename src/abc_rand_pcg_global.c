#include <math.h>  
#include <string.h>     
#include "abc_000_warning.h"
#include "abc_datatype.h" 
#include "abc_rand_pcg_global.h" 
#include "abc_ide_util.h"    
#include "abc_vec.h"  
#include "assert.h"  
#define PCG_DEFAULT_MULTIPLIER_64  6364136223846793005ULL 
#define PCG_DEFAULT_INCREMENT_64   1442695040888963407ULL 
#define PCG_DEFAULT_GLOBAL_STATE_64     0x853c49e6748fea9bULL
#define PCG_DEFAULT_GLOBAL_INCREMENT_64 0xda3e39cb94b95bdbULL
typedef struct pcg32_random_struct { 	
	U64 state; 
	U64 inc;   
} pcg32_random_t;
static pcg32_random_t global_state={
	.state=PCG_DEFAULT_GLOBAL_STATE_64,
	.inc=PCG_DEFAULT_GLOBAL_INCREMENT_64 };
void pcg_set_seed(U64 initstate,U64 initseq)
{	
	extern void init_gauss_rnd(void);
	init_gauss_rnd();
	if (initstate==0 ) {
		return;
	}
	initstate=initstate & initseq; 
	initstate=initstate==0 ? PCG_DEFAULT_GLOBAL_STATE_64    : initstate;
	initseq=initseq==0   ? PCG_DEFAULT_GLOBAL_INCREMENT_64 : initseq;
	global_state.state=0U;
	global_state.inc=(initseq << 1u)|1u;  	
	U32 rnd;
	pcg_random(&rnd,1);
	global_state.state+=initstate;
	pcg_random(&rnd,1);
}
void pcg_print_state(void) {r_printf("state: %30" PRIu64 " inc: %30"  PRIu64 "\n",global_state.state,global_state.inc);}
void pcg_random(U32PTR rnd,I32 N)
{
	U64 oldstate=global_state.state;
	U64 shift=global_state.inc;
	for (int i=0; i < N; i++) {
		U32 xorshifted=((oldstate >> 18u) ^ oldstate) >> 27u   ;
		U32 rot=oldstate >> 59u;
		*rnd++=(xorshifted >> rot)|(xorshifted << ((- *((I32*)(&rot))  ) & 31));
		oldstate=oldstate * PCG_DEFAULT_MULTIPLIER_64+shift;
	}
	global_state.state=oldstate;
}
U64 pcg_advance_lcg_64(U64 state,U64 delta,U64 cur_mult,U64 cur_plus)
{   
	U64 acc_mult=1u;
	U64 acc_plus=0u;
	while (delta > 0) {
		if (delta & 1) {
			acc_mult *=cur_mult;
			acc_plus=acc_plus * cur_mult+cur_plus;
		}
		cur_plus=(cur_mult+1) * cur_plus;
		cur_mult *=cur_mult;
		delta/=2;
	}
	return acc_mult * state+acc_plus;
}
void pcg_get_lcg_multiplier_shift_multistep( I32 delta,U64 cur_mult,U64 cur_plus,U64 * mult,U64 *shift) 
{
	U64 acc_mult=1u;
	U64 acc_plus=0u;
	while (delta > 0) {
		if (delta & 1) {
			acc_mult *=cur_mult;
			acc_plus=acc_plus * cur_mult+cur_plus;
		}
		cur_plus=(cur_mult+1) * cur_plus;
		cur_mult *=cur_mult;
		delta/=2;
	}	
	mult[0]=acc_mult;
	shift[0]=acc_plus;
}
GAUSS_CONSTANT GAUSS
={ 
.x={ 0,0.019584285230127,0.039176085503098,0.058782936068943,0.078412412733112,
0.098072152488661,0.117769874579095,0.137513402144336,0.157310684610171,0.177169820991740,0.197099084294312,
0.217106947210130,0.237202109328788,0.257393526100938,0.277690439821577,0.298102412930487,0.318639363964375,
0.339311606538817,0.360129891789569,0.381105454763557,0.402250065321725,0.423576084201200,0.445096524985516,
0.466825122852590,0.488776411114670,0.510965806738247,0.533409706241281,0.556125593618691,0.579132162255556,
0.602449453164424,0.626099012346421,0.650104070647995,0.674489750196082,0.699283302383220,0.724514383492366,
0.750215375467941,0.776421761147928,0.803172565597918,0.830510878205399,0.858484474141832,0.887146559018876,
0.916556667533113,0.946781756301046,0.977897543940542,1.009990169249582,1.043158263318454,1.077515567040280,
1.113194277160929,1.150349380376008,1.189164350199337,1.229858759216589,1.272698641190536,1.318010897303537,
1.366203816372098,1.417797137996268,1.473467577947101,1.534120544352547,1.601008664886076,1.675939722773444,
1.761670410363067,1.862731867421652,1.987427885929896,2.153874694061456,2.41755901623650 },
.yRatio={ 16773999,16767562,16761112,16754640,16748136,16741589,16734989,16728325,16721587,16714763,
16707840,16700807,16693651,16686358,16678913,16671302,16663507,16655512,16647297,16638843,16630128,16621128,
16611818,16602170,16592154,16581736,16570879,16559544,16547684,16535250,16522186,16508431,16493913,16478554,
16462265,16444942,16426470,16406715,16385522,16362712,16338074,16311363,16282287,16250497,16215576,16177016,
16134195,16086343,16032497,15971429,15901559,15820813,15726414,15614561,15479906,15314686,15107183,14838856,
14478538,13969524,13196768,11886086,9182634 },
};
#define INV_2p24  ((F64)1./(1LL<<24))
#define INV_2p25  ((F64)1./(1LL<<25))
#define INV_2p32  ((F64)1./(1LL<<32))
extern void init_gauss_rnd(void);
void init_gauss_rnd(void) {
	static I08 isInitialized=0;
	if (isInitialized) {
		return;
	}
	for (int i=0; i < 63; i++) {
		GAUSS.yRatio[i]=exp(-0.5 * (GAUSS.x[i+1] * GAUSS.x[i+1] - GAUSS.x[i] * GAUSS.x[i]));
	}
	for (int i=0; i < 63; i++) {
		if (GAUSS.x[i+1] >=1.0) {
			GAUSS.inflectionId=i;
			break;
		}
	}
	F32 a=GAUSS.x[63];
	GAUSS.exp_lamda=(a+sqrt(a * a+4))/2;
	F32PTR x=GAUSS.x;
	F32    del=GAUSS.x[1];
	I32    Imax=ceil(GAUSS.x[63]/del); 
	for (int i=0; i < Imax; i++) {
		F32 low=i * del;
		F32 up=(i+1) * del;
		GAUSS.indices[i]=-9999;
		int bingo=0;
		for (int k=0; k < 64; k++) {
			if (x[k] >=low && x[k] <=up) {
				if (x[k]==low) {
					GAUSS.indices[i]=k;					
				}	else {
					GAUSS.indices[i]=k - 1;
				}	
				bingo=1;
				break;		
			}
			if (x[k] <=low && up <=x[k+1]) {
				GAUSS.indices[i]=k;
				bingo=1;
				break;
			}
		}
		assert(bingo);
	}
	isInitialized=1;
}
void pcg_gauss(F32PTR RND,int N)
{
	for (int i=1; i <=N; i++) {
		U32 rnd[2];
		pcg_random(rnd,2);
		U32  U24_32=(rnd[0] >> 7) * INV_2p25;
		U32  BinIdx=rnd[0] & 0x3f;
		I32  sign=rnd[0] & 0x40;  
		F32 x; 
		if (BinIdx < 63) 	{
			F32 delta=GAUSS.x[BinIdx+1] - GAUSS.x[BinIdx];
			F32 yRatio=GAUSS.yRatio[BinIdx] ;
			while (1) {
				if (U24_32 <=yRatio) {
					x=GAUSS.x[BinIdx]+delta * U24_32/yRatio;
					break;
				}
				F64 U1=rnd[1] * INV_2p32;
				int check=U24_32 <=yRatio+(1.f - yRatio) * U1;	
				x=GAUSS.x[BinIdx+1] -  delta * U1;
				if ((I32) BinIdx < GAUSS.inflectionId && check)      { break; }
				if ((I32)BinIdx > GAUSS.inflectionId && check==0) { goto UpdateSamplerLabel; }
				if ( log(U24_32) <=-0.5f* (x * x- GAUSS.x[BinIdx]* GAUSS.x[BinIdx])  ) {
					break;
				}				
				UpdateSamplerLabel:
				pcg_random(rnd,2);
				U24_32=(F64)rnd[0] * INV_2p32; 
			}
		} else	{
		while (1) {
				F64 U1=rnd[1] * INV_2p32;                   
				x=GAUSS.x[63] - log(U1)/GAUSS.exp_lamda;        
				if (log(U24_32) < -0.5f * ((x - GAUSS.exp_lamda) * (x - GAUSS.exp_lamda)))
					break;
				pcg_random(rnd,2);
				U24_32=rnd[0] * INV_2p32; 
			}
		}
		*RND++=sign? x:-x;
	}
}
void pcg_gamma(F32PTR rnd,F32 a,int N)
{
	F32 INV_MAX=2.328306436538696e-10f;
	if (a >=1)
	{
		F32 b=a - 1.0f;
		F32 c=a+a+a - 0.75f;
		for (int i=0; i < N; i++)
		{
			F32 gam;
			while (1)
			{
				F32 u[2];
				pcg_random((U32PTR)u,2);
				u[0]=(F32)(*(U32PTR)&u[0]) * INV_MAX;
				u[1]=(F32)(*(U32PTR)&u[1]) * INV_MAX;
				F32 w=u[0] * (1 - u[0]);
				F32 y=sqrtf(c/w) * (u[0] - 0.5f);
				gam=b+y;
				if (gam >=0)
				{
					F32 z=64.0f * (w*w*w) * (u[1] * u[1]);
					if (z <=(1 - 2 * (y*y)/gam))
						break;
					F32 logZ=logf(z);
					if (b==0)					{
						if (logZ <=-2 * y) break;
					}		else					{
						if (logZ <=2 * (b * logf(gam/b) - y)) break;
					}
				} 
			}
			*rnd++=gam;
		}
		return;
	}
	if (a > 0) 
	{
		F32 b=1+.3678794f * a;
		for (int i=0; i < N; i++)
		{
			F32 gam;
			while (1)
			{
				F32 u[2];
				pcg_random((U32PTR)u,2);
				u[0]=(F32)(*(U32PTR)&u[0]) * INV_MAX;
				u[1]=(F32)(*(U32PTR)&u[1]) * INV_MAX;
				F32 c=b * u[0];
				if (c < 1)
				{
					gam=expf(logf(c)/a);
					if (-logf(1 - u[1]) >=gam) break;
				}
				else
				{
					gam=-logf((b - c)/a);
					if (-logf(1 - u[1]) >=(1 - a) * logf(gam))
						break;
				}
			}
			*rnd++=gam;
		}
		return;
	}
	if (a < 0.f) 	{
		F32 nan=getNaN();
		for (int i=0; i < N; i++)		*rnd++=nan;
		return;
	}
	if (a==0.f)	{
		for (int i=0; i < N; i++)	*rnd++=0.f;
		return;
	}
}
void  pcg_wishart_unit_lowtriangle_nozeroout(F32PTR rnd,F32PTR tmp,I32 m,F32 df )
{
	I32 N=(m - 1)*m/2;
     pcg_gauss(tmp,N);
	F32PTR tpt;
	tpt=rnd+1;
	for (int i=1; i<m  ; i++)
	{			
		I32  movElem=m - i;
		for (int j=0; j < movElem; j++)
		{
			*tpt++=*tmp++;
		}
		tpt=tpt+i+1;
	}
	tmp=rnd;
	for (int i=1; i <=m; i++)
	{
		pcg_gamma(tmp,(F32)(df-i+1.)/2.f,1);
		*tmp=sqrtf((*tmp) * 2.f);
		tmp=tmp+(m+1);
	}
}
void pcg_wishart_unit_lowtriangle_nozeroout_notmp(F32PTR wishrnd,I32 m,F32 df)
{
	memset(wishrnd,0,sizeof(F32) * m * m);
	I32     numGaussRnd=(m - 1) * m/2;
	F32PTR  gauss=wishrnd+m * m  - numGaussRnd;
	pcg_gauss(gauss,numGaussRnd);
	for (int col=1; col < m;++col) {
		for (int row=col+1; row <=m; row++) {
			wishrnd[row - 1]=*gauss++;
		}
		wishrnd+=m;
	}
	wishrnd -=(m - 1) * m;  
	for (int i=1; i <=m; i++) {
		F32 chisqaure;
		pcg_gamma(&chisqaure,(F32)(df - i+1)/2.f,1);
		*wishrnd=chisqaure=sqrtf(chisqaure * 2.f);
		wishrnd+=(m+1);
	}
}
void  pcg_wishart_unit_lowtriangle_zeroout_notmp(F32PTR wishrnd,I32 m,F32 df)
{
	I32     numGaussRnd=(m - 1) * m/2;
	F32PTR  gauss=wishrnd+1;
	pcg_gauss(gauss,numGaussRnd);
	gauss=gauss+numGaussRnd-1; 
	wishrnd+=(m-2) * m; 
	for (int col=m-1; col >=2; col--) {
		for (int row=m; row >=col+1; row--) {
			wishrnd[row - 1]=*gauss--;
		}
		wishrnd -=m;
	}
	 wishrnd=wishrnd;  
	 for (int col=0; col <m; col++) {
		 for (int row=0; row<col; row++) {
			 wishrnd[row]=0;
		 }
		 wishrnd+=m;
	 }
	 wishrnd=wishrnd -m*m;  
	for (int i=1; i <=m; i++) {
		F32 chisqaure;
		pcg_gamma(&chisqaure,(F32)(df - i+1)/2.f,1);
		*wishrnd=chisqaure=sqrtf(chisqaure * 2.f);
		wishrnd+=(m+1);
	}
}
#include "abc_mat.h"
void  pcg_invwishart_upper(F32PTR iwrnd_upper,F32PTR iwrnd_upper_inv,F32PTR tmp,I32 m,F32PTR Qup,F32 df)
{   
	F32PTR Wlower=tmp;
	pcg_wishart_unit_lowtriangle_zeroout_notmp(Wlower,m,df);
	f32_copy(Qup,iwrnd_upper,m * m);
	for (int i=0; i < m;++i) {
		solve_L_as_L(Wlower,iwrnd_upper,m,m);
		iwrnd_upper=iwrnd_upper+m;
	}
	if (iwrnd_upper_inv) {
		f32_copy(Wlower,iwrnd_upper_inv,m * m);
		for (int i=0; i < m;++i) {
			solve_U_as_U(Qup,iwrnd_upper_inv,m,m);
			iwrnd_upper_inv=iwrnd_upper_inv+m;
		}
	}
}
#include "abc_000_warning.h"
