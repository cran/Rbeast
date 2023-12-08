#include <math.h> 
#include "abc_000_macro.h" 
#include "abc_000_warning.h"
#include "abc_vec.h"
#include "abc_blas_lapack_lib.h" 
#include "abc_common.h"          
#include "beastv2_header.h"
static int TT_03(F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST * ptr) {
	#define TREND  (*((TREND_CONST*)ptr))	
	I32    Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32    Nseg=seg->R2 - seg->R1+1L;		
	I32    Kterms=(seg->ORDER2 - seg->ORDER1)+1; 
	F32PTR TERMS=TREND.TERMS+seg->ORDER1 * N+seg->R1 - 1L;
	F32    scale=TREND.INV_SQR[(Nseg)-1];
	r_ippsSet_32f(0,X,Kterms * Npad);
	I32  k=0;
	for (I32 order=seg->ORDER1; order <=seg->ORDER2; order++) {
		if (order==0) {
			r_ippsSet_32f(scale,X+(seg->R1)-1,Nseg);
		} else if (order==1) {			
			F32      b=TREND.COEFF_B[Nseg - 1];
			F32      a=TREND.COEFF_A[Nseg - 1];
			f32_seq(X+seg->R1 -1,a,b,Nseg);
		} else { 
			r_cblas_scopy(Nseg,TERMS,1,X+seg->R1 - 1,1);
			f32_normalize_x_factor_inplace(X+seg->R1 -1,Nseg,scale);
		}
		k++;
		X+=Npad;
		TERMS+=N;
	} 
	return k;
	#undef TREND
}
static int TT_1( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
	#define TREND  (*((TREND_CONST*)ptr))	
	I32    Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32    Nseg=seg->R2 - seg->R1+1L;		
	I32    Kterms=(seg->ORDER2 - seg->ORDER1)+1; 
	F32PTR TERMS=TREND.TERMS+seg->ORDER1 * N+seg->R1 - 1L;
	F32    scale=TREND.INV_SQR[(Nseg)-1];
	r_ippsSet_32f(0,X,Kterms * Npad);
	I32  k=0;
	for (I32 order=seg->ORDER1; order <=seg->ORDER2; order++) {
		r_cblas_scopy(Nseg,TERMS,1,X+seg->R1 - 1,1);
		if (order !=0) {
			r_ippsSubC_32f_I( *(X+seg->R1 - 1),X+seg->R1 - 1,Nseg); 
		}
		k++;
		X+=Npad;
		TERMS+=N;
	} 
	return k;
#undef TREND
}
static int TT_2( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
	#define TREND  (*((TREND_CONST*)ptr))	
	I32    Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32    Nseg=seg->R2 - seg->R1+1L;		
	I32    Kterms=(seg->ORDER2 - seg->ORDER1)+1; 
	F32PTR TERMS=TREND.TERMS+seg->ORDER1 * N+seg->R1 - 1L;
	F32    scale=TREND.INV_SQR[(Nseg)-1];
	r_ippsSet_32f(0,X,Kterms * Npad);
	I32  k=0;
	for (I32 order=seg->ORDER1; order <=seg->ORDER2; order++) {
 		r_cblas_scopy(Nseg,TERMS,1,X+seg->R1 - 1,1);
		if ( Nseg !=N||order !=0) f32_normalize_inplace(X,N);
 		k++;
		X+=Npad;
		TERMS+=N;
	} 
	return k;
#undef TREND
}
static int DD_0( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
	#define dummy (*((DUMMY_CONST*)ptr))
	I32  Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32  Nseg=seg->R2 - seg->R1+1L;
	I32  period=dummy.period;
	I32 Kterms=Nseg >=period ? period : Nseg;
	r_ippsSet_32f(0,X,Kterms * Npad);
	F32PTR SCALE=dummy.SQRT_N_div_n;
	I32    nFullPeriod=Nseg/period;
	I32    nRemainder=Nseg - nFullPeriod*period;
	for (I32 j=1; j <=Kterms; j++) {
		I32 n=(j <=nRemainder) ? (nFullPeriod+1) : nFullPeriod;
		F32 value=SCALE[n];
		for (I32 r=seg->R1+(j-1) - 1; r<seg->R2-1; r+=period) {		
			X[r]=value;		
		}
		X+=Npad;
	} 
	return Kterms;
#undef dummy
}
static int VV_0( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
#define SVD  (*((SVD_CONST*)ptr))
	I32     Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32     Nseg=seg->R2 - seg->R1+1L;
	I32 kTerms=(seg->ORDER2 - seg->ORDER1)+1 ;
	r_ippsSet_32f(0,X,kTerms * Npad);
	F32PTR TERM=SVD.TERMS+N * (seg->ORDER1 - 1)+seg->R1 - 1;
	F32PTR svd_csum=SVD.SQR_CSUM+1L+(N+1) * (seg->ORDER1 - 1);
	I32    k=0;
	for (I32 j=seg->ORDER1; j <=seg->ORDER2; j++){
		r_cblas_scopy(Nseg,TERM,1L,X+seg->R1 - 1,1L);
		F32 csum_diff=(svd_csum[seg->R2 - 1] - svd_csum[(seg->R1 - 1) - 1]);
		F32 scalingFactor=csum_diff==0?0.0: sqrtf(N/csum_diff);  
		k+=1;
		TERM+=N ;
		X+=Npad;
		svd_csum+=(N+1L) ;	
	}
	return k;
#undef SEASON
}
static int SS_0( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
	#define SEASON  (*((SEASON_CONST*)ptr))
	I32     Npad=((N+7L)/8L) * 8L;  Npad=N;
	I32     Nseg=seg->R2 - seg->R1+1L;
	I32 kTerms=((seg->ORDER2 - seg->ORDER1)+1) * 2;
	r_ippsSet_32f(0,X,kTerms * Npad);
	F32PTR TERM=SEASON.TERMS+N * (seg->ORDER1-1) * 2+seg->R1 - 1;
    F32PTR season_csum=SEASON.SQR_CSUM+1L+(N+1) * (seg->ORDER1-1)*2;
	I32    k=0;
	for (I32 j=seg->ORDER1; j <=seg->ORDER2; j++) 	{
		F32   scalingFactor;
		r_cblas_scopy(Nseg,TERM,1L,X+seg->R1 - 1,1L);
		scalingFactor=sqrtf(N/(season_csum[seg->R2 - 1] - season_csum[(seg->R1 - 1) - 1]));
		r_cblas_sscal(Nseg,scalingFactor,X+seg->R1 - 1,1L);
		r_cblas_scopy(Nseg,TERM+N,1L,(X+Npad)+seg->R1 - 1,1L);
		scalingFactor=sqrtf(N/(season_csum[(N+1)+seg->R2 - 1] - season_csum[(N+1)+(seg->R1 - 1) - 1]));
		r_cblas_sscal(Nseg,scalingFactor,(X+Npad)+seg->R1 - 1,1L);
		k+=2;
		TERM+=N * 2;
		X+=Npad * 2;
		season_csum+=(N+1L) * 2;
	}
	return k;
#undef SEASON
}
static int SS_1( F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* ptr) {
	#define SEASON  (*((SEASON_CONST*)ptr))
	I32     Npad=((N+7L)/8L) * 8L; Npad=N;
	I32     Nseg=seg->R2 - seg->R1+1L;
	I32 kTerms=((seg->ORDER2 - seg->ORDER1)+1) * 2;
	r_ippsSet_32f(0,X,kTerms * Npad);
	F32PTR TERM=SEASON.TERMS+N * (seg->ORDER1-1) * 2+seg->R1 - 1;
    F32PTR season_csum=SEASON.SQR_CSUM+1L+(N+1) * (seg->ORDER1-1)*2;
	I32    k=0;
	for (I32 order=seg->ORDER1; order <=seg->ORDER2; order++)
	{
		r_cblas_scopy(Nseg,TERM,1,X+seg->R1 - 1,1);
		r_cblas_scopy(Nseg,TERM+N,1,X+Npad+seg->R1 - 1,1);
		k+=2;
		TERM+=N*2;
		X+=Npad*2;
		season_csum+=(N+1L) * 2;
	}
	return k;
#undef SEASON
}
static int OO_0(F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* bConst) {
	I32 Npad=((N+7L)/8L) * 8L; Npad=N;
	I32 Kterms=1L; 
	I32 knotOutlier=seg->outlierKnot;
	r_ippsSet_32f(0,X,Kterms * N);	
	X[knotOutlier - 1]=bConst->outlier.SQRTN;;
	return 1;
}
static int OO_1(F32PTR X,I32 N,BEAST2_BASESEG_PTR seg,BASIS_CONST* bConst) {
	I32 Npad=((N+7L)/8L) * 8L; Npad=N;
	I32 Kterms=1L;
	I32 knotOutlier=seg->outlierKnot;
	r_ippsSet_32f(0,X,Kterms * N);
	X[knotOutlier - 1]=1.0;
	return 1;
}
pfnGenTerms Get_GenTerms(I08 id,BEAST2_PRIOR_PTR prior) {
	switch (id) {
	case DUMMYID:
		 return DD_0; 
	case SVDID:
		 return VV_0;
	case SEASONID:
		if      (prior->seasonBasisFuncType==0)		    return SS_0;
		else if (prior->seasonBasisFuncType==1) 		return SS_1;
	case TRENDID:  
		if		(prior->trendBasisFuncType==0)		return TT_03;
		else if (prior->trendBasisFuncType==1)		return TT_1;
		else if (prior->trendBasisFuncType==2)		return TT_2;
		else if (prior->trendBasisFuncType==3)		return TT_03;
	case OUTLIERID: 
		if		(prior->outlierBasisFuncType==0)		return OO_0;
		else if (prior->outlierBasisFuncType==1)		return OO_1;			
	}
	return NULL;
}
#include "abc_000_warning.h"
