#include "math.h"  
#include "abc_000_warning.h"
#include "abc_rand_pcg_global.h"
#include "abc_rand_pcg_local.h"
#include "abc_mat.h"  
 void (*local_pcg_set_seed)(local_pcg32_random_t* rng,U64 initstate,U64 initseq);
 void (*local_pcg_random)(local_pcg32_random_t* rng,U32PTR rnd,I32 N);
void local_pcg_gauss(local_pcg32_random_t* rng,F32PTR RND,int N)
{
	 U32 RAND[200];
	 I32 SIZEminus2=( (2*N < 200) ? (2*N+2) : 200 )   -  2 ;
	 I32 CURSOR;
	 local_pcg_random(rng,RAND,SIZEminus2+2); CURSOR=0;
	for (int i=1; i <=N; i++) 	{
		if (CURSOR > SIZEminus2) { 
			local_pcg_random(rng,RAND,SIZEminus2+2);
			CURSOR=0;
		}
		U32 U24=RAND[CURSOR] >> 8;
		I64 IDX=RAND[CURSOR] & 0x3f;				
		I32 sign=(RAND[CURSOR] & 0x80) ?+1 : -1;  
		++CURSOR;
		F32 x;		
		if (IDX < 63) 	{
			F32 delta=(GAUSS.x[IDX+1] - GAUSS.x[IDX]) * 2.328306436538696e-10f;
			while (1) {
				x=GAUSS.x[IDX]+delta * (F32)RAND[CURSOR++]; 
				if (U24 <=GAUSS.yRatio[IDX])					break;
				if (U24 <=expf(-0.5f*x*x)* GAUSS.yInverse[IDX])break;
				if (CURSOR > SIZEminus2) { 
					local_pcg_random(rng,RAND,SIZEminus2+2);
					CURSOR=0;
				}
				U24=RAND[CURSOR++] >> 8;
			}
		}
		else
		{
			F32 U1,U2;
			U2=(F32)U24* 5.960464477539063e-08f; 
			while (1)
			{
				U1=RAND[CURSOR++] * 2.328306436538696e-10f; 
				U1=1.f - U1;
				x=GAUSS.PARAM_R - logf(U1) *GAUSS.INV_PARAM_R;
				if ( U2 < expf(-0.5f * x * x+GAUSS.PARAM_R * (x - 0.5f * GAUSS.PARAM_R) ))
					break;
				if (CURSOR > SIZEminus2) { 
					local_pcg_random(rng,RAND,SIZEminus2+2);
					CURSOR=0;
				}
				U2=RAND[CURSOR++] * 2.328306436538696e-10f;
			}
		}
		*RND++=x*sign;
	}
}
  void local_pcg_gamma(local_pcg32_random_t* rng,F32PTR rnd,F32 a,int N)
{
	const F32 INV_MAX=2.328306436538696e-10f;
	U32 RAND[200];
	I32 SIZEminus2=((2 * N < 200) ? (2 * N+2) : 200) - 2;
	I32 CURSOR;
	local_pcg_random(rng,RAND,SIZEminus2+2); CURSOR=0;
	if (a > 1.f)
	{
		F32 b=a - 1.0f;             
		F32 c=a+a+a - 0.75f;
		for (int i=0; i < N; i++)
		{
			F32 gam;
			while (1){
				if (CURSOR > SIZEminus2) { 
					local_pcg_random(rng,RAND,SIZEminus2+2);	CURSOR=0;
				}
				F32 u[2];
				u[0]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				u[1]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				F32 w=u[0] * (1 - u[0]);
				F32 y=sqrtf( c/w ) * (u[0] - 0.5f);
				gam=b+y;
				if (gam >=0)
				{
					F32 z=64.0f * (w*w*w) * (u[1] * u[1]);
					if (z <=(1 - 2 * (y*y)/gam)) break;
					F32 logZ=logf(z);
					if (logZ <=2 * (b * logf(gam/b) - y)) break;
				} 
			}
			*rnd++=gam;
		}
		return;
	}
	if (a==1.f)	{
		F32 b=a - 1.0f;         
		F32 c=a+a+a - 0.75f;
		for (int i=0; i < N; i++)
		{
			F32 gam;
			while (1){
				if (CURSOR > SIZEminus2) { 
					local_pcg_random(rng,RAND,SIZEminus2+2);	CURSOR=0;
				}
				F32 u[2];
				u[0]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				u[1]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				F32 w=u[0] * (1 - u[0]);
				F32 y=sqrtf( c/w ) * (u[0] - 0.5f);
				gam=b+y;
				if (gam >=0)
				{
					F32 z=64.0f * (w*w*w) * (u[1] * u[1]);
					if (z <=(1 - 2 * (y*y)/gam)) break;
					F32 logZ=logf(z);					
   				    if (logZ <=-2 * y) break;
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
				if (CURSOR > SIZEminus2) { 
					local_pcg_random(rng,RAND,SIZEminus2+2);	CURSOR=0;
				}
				F32 u[2];
				u[0]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				u[1]=(F32)(*(U32PTR)&RAND[CURSOR++]) * INV_MAX;
				F32 c=b * u[0];
				if (c < 1) {
					gam=expf(logf(c)/a);
					if (-logf(1 - u[1]) >=gam) break;
				} else	{
					gam=-logf((b - c)/a);
					if (-logf(1 - u[1]) >=(1 - a) * logf(gam))
						break;
				}
			}
			*rnd++=gam;
		}
		return;
	}
	if (a < 0.f){
		F32 nan=getNaN();
		for (int i=0; i < N; i++)	*rnd++=nan;		
		return;
	}
	if (a==0.f){
		for (int i=0; i < N; i++)		*rnd++=0.f;		
		return;
	}
}
void local_pcg_wishart_unit_lowtriangle_zeroout(local_pcg32_random_t* rng,F32PTR wishrnd,F32PTR tmp,I32 m,F32 df)
{   
	memset(wishrnd,0,sizeof(F32) * m * m);
	I32 numGaussRnd=(m-1)*m/2;
	local_pcg_gauss(rng,tmp,numGaussRnd);
	for (int col=1; col< m;++col) {				
		for (int row=col+1; row <=m; row++) 	{
			wishrnd[row-1]=*tmp++;
		}		
		wishrnd+=m;
	}
	wishrnd -=(m-1) * m;  
	for (int i=1; i <=m; i++) 	{
		F32 chisqaure;
		local_pcg_gamma(rng,&chisqaure,(F32)(df-i+1)/2.f,1);
		*wishrnd=chisqaure=sqrtf(chisqaure * 2.f);				
		wishrnd+=(m+1);
	}
}
#include "abc_vec.h"
void local_pcg_wishart_unit_lowtriangle_zeroout_notmp(local_pcg32_random_t* rng,F32PTR wishrnd,I32 m,F32 df)
{
	I32     numGaussRnd=(m - 1) * m/2;
	F32PTR  gauss=wishrnd+1;
	local_pcg_gauss(rng,gauss,numGaussRnd);
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
		local_pcg_gamma(rng,&chisqaure,(F32)(df - i+1)/2.f,1);
		*wishrnd=chisqaure=sqrtf(chisqaure * 2.f);
		wishrnd+=(m+1);
	}
}
void local_pcg_invwishart_upper(local_pcg32_random_t* rng,F32PTR iwrnd_upper,F32PTR iwrnd_upper_inv,F32PTR tmp,I32 m,F32PTR Qup,F32 df)
{   
	F32PTR Wlower=tmp;
	local_pcg_wishart_unit_lowtriangle_zeroout_notmp(rng,Wlower,m,df);
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
