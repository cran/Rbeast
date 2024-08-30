
#include "math.h"
#include "string.h" 
#include "abc_000_warning.h"
#include "abc_datatype.h"
#include "abc_vec.h"
#if defined(OS_WIN64)||defined(OS_WIN32)
    #include <malloc.h> 
#else
    #include <alloca.h> 
#endif
#define STATIC static 
STATIC void  gen_i32_add_val_inplace(const int C,const I32PTR X,const int N) {
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]+=C; 
		X[i+1]+=C;
		X[i+2]+=C;
		X[i+3]+=C;
	}
	for (; i < N;++i) X[i]+=C;
}
STATIC I32   gen_i32_sum(const I32PTR X,const int N) 
{
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 sum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER)  	sum+=X[i]+X[i+1]+X[i+2]+X[i+3];	
	for (; i < N;++i)								sum+=X[i];
	return sum;
}
STATIC void gen_f32_fill_val(const F32 C,F32PTR X,int N) {
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=C;
		X[i+1]=C;
		X[i+2]=C;
		X[i+3]=C;
	}
	for (; i < N;++i) 		X[i]=C;
}
STATIC  F32 gen_f32_sum(const F32PTR X,int N ) {
     #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F64 sum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER)  	sum=sum+(X[i]+X[i+1]+X[i+2]+X[i+3]);	
	for (; i < N;++i)								sum=sum+X[i];
	return (F32)sum;
}
STATIC void gen_f32_add_vec(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N) {
#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]=SRC2[i]+SRC1[i];
		DST[i+1]=SRC2[i+1]+SRC1[i+1];
		DST[i+2]=SRC2[i+2]+SRC1[i+2];
		DST[i+3]=SRC2[i+3]+SRC1[i+3];
	}
	for (; i < N;++i) DST[i]=SRC2[i]+SRC1[i];
}
STATIC  void gen_f32_sub_vec(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N) {
#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]=SRC2[i] - SRC1[i];
		DST[i+1]=SRC2[i+1] - SRC1[i+1];
		DST[i+2]=SRC2[i+2] - SRC1[i+2];
		DST[i+3]=SRC2[i+3] - SRC1[i+3];
	}
	for (; i < N;++i) DST[i]=SRC2[i] - SRC1[i];
}
STATIC  void gen_f32_add_vec_inplace(const F32PTR SRC,const F32PTR DST,const int N)
{
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]+=SRC[i];
		DST[i+1]+=SRC[i+1];
		DST[i+2]+=SRC[i+2];
		DST[i+3]+=SRC[i+3];
	}
	for (; i < N;++i) DST[i]+=SRC[i];
}
STATIC void gen_f32_sub_vec_inplace(const F32PTR SRC,F32PTR DST,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]   -=SRC[i];
		DST[i+1] -=SRC[i+1];
		DST[i+2] -=SRC[i+2];
		DST[i+3] -=SRC[i+3];
	}
	for (; i < N;++i) DST[i] -=SRC[i];
}
STATIC void gen_f32_subrev_val_inplace(const F32 C,F32PTR X,int N) {
    const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=C-(X[i]);
		X[i+1]=C-(X[i+1]);
		X[i+2]=C - (X[i+2]);
		X[i+3]=C - (X[i+3]);
	}
	for (; i < N;++i) 		X[i]=C - (X[i]);
}
STATIC void gen_f32_add_val_inplace(const F32 C,F32PTR X,int N) {
    const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=X[i]+C;
		X[i+1]=X[i+1]+C;
		X[i+2]=X[i+2]+C;
		X[i+3]=X[i+3]+C;
	}
	for (; i < N;++i) 		X[i]=X[i]+C;
}
STATIC void gen_f32_mul_val_inplace(const F32 C,F32PTR X,const int N) {
    const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=X[i]*C;
		X[i+1]=X[i+1]* C;
		X[i+2]=X[i+2]* C;
		X[i+3]=X[i+3]* C;
	}
	for (; i < N;++i) 		X[i]=X[i]*C;
}
STATIC void gen_f32_mul_vec_inplace(const F32PTR SRC,F32PTR DST,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]   *=SRC[i];
		DST[i+1] *=SRC[i+1];
		DST[i+2] *=SRC[i+2];
		DST[i+3] *=SRC[i+3];
	}
	for (; i < N;++i) DST[i] *=SRC[i];
}
STATIC void gen_f32_mul_vec(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		DST[i]=SRC1[i]* SRC2[i];
		DST[i+1]=SRC1[i+1] * SRC2[i+1];
		DST[i+2]=SRC1[i+2] * SRC2[i+2];
		DST[i+3]=SRC1[i+3] * SRC2[i+3];
	}
	for (; i < N;++i) DST[i]=SRC1[i] * SRC2[i];
}
STATIC void gen_f32_add_v_v2_vec_inplace(const F32PTR SRC,const F32PTR x,F32PTR x2,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		x[i]+=SRC[i] ; x2[i]+=SRC[i]* SRC[i];
		x[i+1]+=SRC[i+1];		x2[i+1]+=SRC[i+1] * SRC[i+1];
		x[i+2]+=SRC[i+2];		x2[i+2]+=SRC[i+2] * SRC[i+2];
		x[i+3]+=SRC[i+3];		x2[i+3]+=SRC[i+3] * SRC[i+3];
	}
	for (; i < N;++i) {
		x[i]+=SRC[i]; x2[i]+=SRC[i] * SRC[i];
	}
}
STATIC void gen_f32_sqrt_vec_inplace(const F32PTR X,const int N)
{
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=sqrtf(X[i]);
		X[i+1]=sqrtf(X[i+1]);
		X[i+2]=sqrtf(X[i+2]);
		X[i+3]=sqrtf(X[i+3]);
	}
	for (; i < N;++i) 		X[i]=sqrtf(X[i]);
}
STATIC  void gen_f32_cos_vec_inplace(const F32PTR X,const int N)
{
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=cosf(X[i]);
		X[i+1]=cosf(X[i+1]);
		X[i+2]=cosf(X[i+2]);
		X[i+3]=cosf(X[i+3]);
	}
	for (; i < N;++i) 		X[i]=cosf(X[i]);
}
STATIC  void gen_f32_sin_vec_inplace(const F32PTR X,const int N)
{
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=sinf(X[i]);
		X[i+1]=sinf(X[i+1]);
		X[i+2]=sinf(X[i+2]);
		X[i+3]=sinf(X[i+3]);
	}
	for (; i < N;++i) 		X[i]=sinf(X[i]);
    #undef  UNROLL_NUMBER 
}
STATIC void gen_f32_sincos_vec_inplace(const F32PTR in_outsin,F32PTR outcos,const int N)
{
#define UNROLL_NUMBER  2
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		outcos[i]=cosf(in_outsin[i]);
		outcos[i+1]=cosf(in_outsin[i+1]);
		in_outsin[i]=sinf(in_outsin[i]);
		in_outsin[i+1]=sinf(in_outsin[i+1]);		
	}
	for (; i < N;++i) {
		outcos[i]=cosf(in_outsin[i]);
		in_outsin[i]=sinf(in_outsin[i]);
	}
#undef  UNROLL_NUMBER 
}
STATIC void gen_f32_pow_vec_inplace(F32PTR X,F32 pow,int N){
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=powf(X[i],pow);
		X[i+1]=powf(X[i+1],pow);
		X[i+2]=powf(X[i+2],pow);
		X[i+3]=powf(X[i+3],pow);
	}
	for (; i < N;++i) 		X[i]=powf(X[i],pow);
	#undef  UNROLL_NUMBER 
}
STATIC void gen_f32_log_vec_inplace(const F32PTR X,const int N)
{
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=logf(X[i]);
		X[i+1]=logf(X[i+1]);
		X[i+2]=logf(X[i+2]);
		X[i+3]=logf(X[i+3]);
	}
	for (; i < N;++i) 		X[i]=logf(X[i]);
	 #undef  UNROLL_NUMBER 
}
STATIC void gen_f32_exp_vec_inplace(const F32PTR X,const int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=expf(X[i]);
		X[i+1]=expf(X[i+1]);
		X[i+2]=expf(X[i+2]);
		X[i+3]=expf(X[i+3]);
	}
	for (; i < N;++i) 		X[i]=expf(X[i]);
	#undef  UNROLL_NUMBER 
}
STATIC void gen_f32_sqrt_vec(const F32PTR X,F32PTR Y,int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER);
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		Y[i]=X[i]  *  X[i];
		Y[i+1]=X[i+1] * X[i+1];
		Y[i+2]=X[i+2] * X[i+2];
		Y[i+3]=X[i+3] * X[i+3];;
	}
	for (; i < N;++i) Y[i]=X[i]*X[i];
	#undef  UNROLL_NUMBER 
}
STATIC void  gen_f32_avgstd(const F32PTR X,int N,F32PTR avg,F32PTR std) {
     #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F64 sumx=0;
	F64 sumxx=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER)  	{
		sumx=sumx+(X[i]+X[i+1]+X[i+2]+X[i+3]);	
		sumxx=sumxx+(X[i]* X[i]+X[i+1]* X[i+1]+X[i+2]* X[i+2]+X[i+3]* X[i+3]);
	}
	for (; i < N;++i) {
		sumx=sumx+X[i];
		sumxx=sumxx+X[i]* X[i];
	}
	F64 AVG=sumx/N;
	std[0]=sqrtf( (sumxx - AVG*sumx)/(N - 1));
	avg[0]=AVG;
	#undef  UNROLL_NUMBER 
}
STATIC void gen_f32_sx_sxx_to_avgstd_inplace(F32PTR SX,F32PTR SXX,I32 Nsample,F32 scale,F32 offset,int N) {
	F32 inv_sample_scale=1./(F64) Nsample *scale;
	F32 inv_sample_scale2=1./(F64)Nsample * scale*scale;
	I32 i=0;
	for (; i < N;++i) {
		F32  avg=SX[i] * inv_sample_scale;
		F32  sxx_n=SXX[i] * inv_sample_scale2;
		SXX[i]=sqrtf(sxx_n - avg * avg);
		SX[i]=avg+offset;
	}
}
STATIC I32 gen_f32_maxidx_slow(const F32PTR  X,const int N,F32PTR val) {
	F32 maxVal=X[0];
	I32 maxIdx=0;
	for (I32 i=1; i < N; i++) {
		if (maxVal < X[i]) {
			maxVal=X[i];
			maxIdx=i;
		}
	}
	*val=maxVal;
	return maxIdx;
}
STATIC I32 gen_f32_maxidx(const F32PTR  X,const  int N,F32PTR val) {
	#define UNROLL_NUMBER  2 
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 maxVal=X[0];
	I32 maxIdx=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		F32 val;
		I32 idx=X[i+1] > X[i] ? (val=X[i+1],i+1) : (val=X[i],i);
		if (maxVal < val) {
			maxVal=val;
			maxIdx=idx;
		}		
	}
	for (; i < N; i++) {
		if (maxVal < X[i]) {
			maxVal=X[i];
			maxIdx=i;
		}
	}
	*val=maxVal;
	return maxIdx;
	#undef UNROLL_NUMBER
}
STATIC I32 gen_f32_minidx(const F32PTR  X,const int  N,F32PTR val) {
	#define UNROLL_NUMBER  2 
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 minVal=X[0];
	I32 minIdx=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		F32 val;
		I32 idx=X[i+1] < X[i] ? (val=X[i+1],i+1) : (val=X[i],i);
		if (minVal > val) {
			minVal=val;
			minIdx=idx;
		}		
	}
	for (; i < N; i++) {
		if (minVal > X[i]) {
			minVal=X[i];
			minIdx=i;
		}
	}
	*val=minVal;
	return minIdx;
	#undef UNROLL_NUMBER
}
STATIC void gen_f32_diff_back(const F32PTR  X,F32PTR result,const int N) {
	#define UNROLL_NUMBER  4 
	I32 i=1;       
	for (; i < N-(UNROLL_NUMBER-1); i+=UNROLL_NUMBER) {		
		result[i]=X[i] - X[i-1];
		result[i+1]=X[i+1] - X[i];
		result[i+2]=X[i+2] - X[i+1];
		result[i+3]=X[i+3] - X[i+2];
	}
	for (; i < N; i++) {
		result[i]=X[i] - X[i - 1];
	}
	result[0]=result[1];
}
STATIC void gen_f32_seq(F32PTR p,F32 x0,F32 dX,int N)
{ 
	#define UNROLL_NUMBER  4 
	const int regularPart=N & (-UNROLL_NUMBER); 
	int i=0;
	const F32 dX2=dX+dX;
	const F32 dX3=dX2+dX;
	for ( ; i < regularPart; i+=UNROLL_NUMBER) {
		p[i]=x0;
		p[i+1]=x0+dX;
		p[i+2]=x0+dX2;
		p[i+3]=x0+dX3;
		x0=x0+dX3+dX;
	}
	for (; i < N;++i) {
		p[i]=x0;
		x0+=dX;
	}
#undef UNROLL_NUMBER
}
STATIC  void gen_i32_seq(I32PTR p,I32 x0,I32 dX,int N) {
#define UNROLL_NUMBER  4 
	const int regularPart=N & (-UNROLL_NUMBER); 
	int i=0;
	const I32 dX2=dX+dX;
	const I32 dX3=dX2+dX;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		p[i]=x0;
		p[i+1]=x0+dX;
		p[i+2]=x0+dX2;
		p[i+3]=x0+dX3;
		x0=x0+dX3+dX;
	}
	for (; i < N;++i) {
		p[i]=x0;
		x0+=dX;
	}
#undef UNROLL_NUMBER
}
STATIC void gen_f32_to_f64_inplace(F32PTR data32,int N) {
	#define UNROLL_NUMBER  4 
	F64PTR data64=(F64PTR) data32;	
	int i;
	for (i=N-UNROLL_NUMBER; i >=0; i -=UNROLL_NUMBER) {
		F64 x3,x2,x1,x0;
		data64[i+3]=x3=(F64)data32[i+3];
		data64[i+2]=x2=(F64)data32[i+2];
		data64[i+1]=x1=(F64)data32[i+1];
		data64[i]=x0=(F64)data32[i];
	}
	i=i+UNROLL_NUMBER;
	for (int j=(i-1); j >=0; --j) {
		data64[j]=(F64)data32[j];
	}
}
STATIC void gen_f64_to_f32_inplace(F64PTR data64,int N) {
	#define UNROLL_NUMBER  4 
	F32PTR data32=(F64PTR) data64;	
	int i=0;
	for (; i < (N- (UNROLL_NUMBER-1)); i+=UNROLL_NUMBER) {
		data32[i]=data64[i];
		data32[i+1]=data64[i+1];
		data32[i+2]=data64[i+2];
		data32[i+3]=data64[i+3];
	}	
	for (; i < N;++i) {
		data32[i]=data64[i];
	}
}
STATIC void gen_i32_to_f32_scaleby_inplace(I32PTR X,int N,F32 scale) {
    const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		*(F32PTR)(X+i)=(F32)X[i]* scale;
		*(F32PTR)(X+i+1)=(F32)X[i+1]* scale;
		*(F32PTR)(X+i+2)=(F32)X[i+2]* scale;
		*(F32PTR)(X+i+3)=(F32)X[i+3]* scale;
	}
	for (; i < N;++i) 		*(F32PTR)(X+i)=X[i]*scale;
}
STATIC void gen_i32_increment_bycond_inplace(I32PTR x,F32PTR cond,int N) {
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		if (cond[i]   >0)++*(x+i);
		if (cond[i+1] > 0)++*(x+i+1);
		if (cond[i+2] > 0)++*(x+i+2);
		if (cond[i+3] > 0)++*(x+i+3);;
	}
	for (; i < N;++i) if (cond[i] > 0)++x[i]; 
}
STATIC void gen_i32_increment_vec2_bycond_inplace(I32PTR x,I32PTR y,F32PTR cond,int N) {
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		if (cond[i]   > 1e-10)++*(x+i);
		if (cond[i+1] > 1e-10)++*(x+i+1);
		if (cond[i+2] > 1e-10)++*(x+i+2);
		if (cond[i+3] > 1e-10)++*(x+i+3);
		if (cond[i]     < 1e-10 && cond[i]>-1e-10)++* (y+i);
		if (cond[i+1] < 1e-10 && cond[i+1] > -1e-10)++* (y+i+1);
		if (cond[i+2] < 1e-10 && cond[i+2] > -1e-10)++* (y+i+2);
		if (cond[i+3] < 1e-10 && cond[i+3] > -1e-10)++* (y+i+3);
	}
	for (; i < N;++i) {
		if (cond[i] > 1e-10)++x[i];
		if (cond[i] < 1e-10 && cond[i]>-1e-10)++*(y+i);
	}
}
STATIC I32 gen_i08_sum_binvec(U08PTR binvec,I32 N) {
	I32   SUM=0;
	I32	  i=0;
	U08PTR binvec_old=binvec;
	for (; i < N - (32-1); i+=32) {		
		I64 sum=*((I64PTR)binvec)+*((I64PTR)binvec+1)+*((I64PTR)binvec+2)+*((I64PTR)binvec+3);
		*((I32PTR)&sum)=*((I32PTR)&sum)+*((I32PTR)&sum+1);
		*((I16PTR)&sum)=*((I16PTR)&sum)+*((I16PTR)&sum+1);
		*((I08PTR)&sum)=*((I08PTR)&sum)+*((I08PTR)&sum+1);
		SUM=SUM+*((I08PTR)&sum);
		binvec+=32;
	}
	for (; i < N - (8 - 1); i+=8) {
		I32 sum=*((I32PTR)binvec)+*((I32PTR)binvec+1);						
		*((I16PTR)&sum)=*((I16PTR)&sum)+*((I16PTR)&sum+1);
		*((I08PTR)&sum)=*((I08PTR)&sum)+*((I08PTR)&sum+1);
		SUM=SUM+*((I08PTR)&sum);
		binvec+=8;
	} 
	for (; i < N;++i ) { 
		SUM=SUM+binvec_old[i];
	}
	return SUM;
}
STATIC F32  gen_f32_dot( const F32PTR x,const F32PTR y,const int N) {
	  #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		sum+=x[i] * y[i]+x[i+1] * y[i+1]+x[i+2] * y[i+2]+x[i+3] * y[i+3];
	}
	for (; i < N;++i) 		sum+=x[i]*y[i];
	return sum;
}
STATIC F32  gen_f32_dot2x1( const F32PTR x,const F32PTR y,const F32PTR v,const int N,F32PTR res) {
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sumX=0;
	F32 sumY=0;	
		I32 i=0;
		for (; i < regularPart; i+=UNROLL_NUMBER) {
			sumX+=x[i] * v[i]+x[i+1] * v[i+1]+x[i+2] * v[i+2]+x[i+3] * v[i+3];
			sumY+=y[i] * v[i]+y[i+1] * v[i+1]+y[i+2] * v[i+2]+y[i+3] * v[i+3];
		}
		for (; i < N;++i) {
			sumX+=x[i] * v[i];
			sumY+=y[i] * v[i];
		}
	res[0]=sumX;
	return sumY;
}
STATIC F32  gen_f32_dot2x1_dir( const F32PTR x,const F32PTR y,const F32PTR v,const int N,F32PTR res,int dir) {
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sumX=0;
	F32 sumY=0;	
	if (dir) {
		I32 i=0;
		for (; i < regularPart; i+=UNROLL_NUMBER) {
			sumX+=x[i] * v[i]+x[i+1] * v[i+1]+x[i+2] * v[i+2]+x[i+3] * v[i+3];
			sumY+=y[i] * v[i]+y[i+1] * v[i+1]+y[i+2] * v[i+2]+y[i+3] * v[i+3];
		}
		for (; i < N;++i) {
			sumX+=x[i] * v[i];
			sumY+=y[i] * v[i];
		}
	} else {
		I32 i=N - UNROLL_NUMBER;
		for (; i >=0; i -=UNROLL_NUMBER) {
			sumX+=x[i] * v[i]+x[i+1] * v[i+1]+x[i+2] * v[i+2]+x[i+3] * v[i+3];
			sumY+=y[i] * v[i]+y[i+1] * v[i+1]+y[i+2] * v[i+2]+y[i+3] * v[i+3];
		}
		int I=i+UNROLL_NUMBER;
		for (i=0; i < I ;++i) {
			sumX+=x[i] * v[i];
			sumY+=y[i] * v[i];
		}
	}
	res[0]=sumX;
	return sumY;
}
STATIC void gen_f32_dot2x2(const F32PTR x1,const F32PTR x2,const F32PTR y1,const F32PTR y2,const int N,F32PTR res1,F32PTR res2 ) {
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sum11=0;
	F32 sum21=0;
	F32 sum12=0;
	F32 sum22=0;	
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		sum11+=x1[i] * y1[i]+x1[i+1] * y1[i+1]+x1[i+2] * y1[i+2]+x1[i+3] * y1[i+3];
		sum21+=x2[i] * y1[i]+x2[i+1] * y1[i+1]+x2[i+2] * y1[i+2]+x2[i+3] * y1[i+3];
		sum12+=x1[i] * y2[i]+x1[i+1] * y2[i+1]+x1[i+2] * y2[i+2]+x1[i+3] * y2[i+3];
		sum22+=x2[i] * y2[i]+x2[i+1] * y2[i+1]+x2[i+2] * y2[i+2]+x2[i+3] * y2[i+3];
	}
	for (; i < N;++i) {
		sum11+=x1[i] * y1[i];
		sum21+=x2[i] * y1[i];
		sum12+=x1[i] * y2[i];
		sum22+=x2[i] * y2[i];
	}
	res1[0]=sum11;
	res1[1]=sum21;
	res2[0]=sum12;
	res2[1]=sum22;
}
STATIC void gen_f32_gemm_XtY2x2(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc)
{
	int COL;
 	for (COL=0; COL < N-(2-1); COL+=2) {
		int ROW;
		for (ROW=0; ROW < M-(2-1); ROW+=2) {			
            gen_f32_dot2x2(A+ROW*lda,A+(ROW+1)*lda,B,B+ldb,K,C+ROW,C+ldc+ROW);
		}
	    if(ROW<M){
			*(C+ldc+ROW)=gen_f32_dot2x1(B,B+ldb,A+ROW* lda,K,C+ROW);
		}
		B+=ldb+ldb;
		C+=ldc+ldc;
	}
	if (COL < N) {
		int ROW;
		for (ROW=0; ROW < M - (2 - 1); ROW+=2) 
			*(C+ROW+1)=gen_f32_dot2x1(A+ROW * lda,A+(ROW+1) * lda,B,K,C+ROW);
		if (ROW < M) 
			C[ROW]=gen_f32_dot(A+ROW * lda,B,K);
	}
}
static void __F32copyFrmStidedMem(F32PTR dst,F32PTR src,int N,int stride) {
	#define UNROLL_NUMBER  4 
 	int i=0;
	for (; i < (N - (UNROLL_NUMBER - 1)); i+=UNROLL_NUMBER) {
		dst[i]=src[0];
		dst[i+1]=src[stride];
		dst[i+2]=src[stride*2];
		dst[i+3]=src[stride*3];
		src+=stride * 4;
	}
	for (; i < N;++i) {
		dst[i]=src[0];
		src+=stride;
	}
}
STATIC void gen_f32_gemm_XY2x2(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc)
{ 
    F32     XROW_FIXEXD[4096];
    F32PTR  Xrow=(2*K <=4096) ? XROW_FIXEXD : alloca(2*K * sizeof(F32));
    F32PTR  Xrow1=Xrow,Xrow2=Xrow1+K; 
    int row=0;
    for (; row < M-1; row+=2) {
		__F32copyFrmStidedMem(Xrow1,A+row,K,lda);
		__F32copyFrmStidedMem(Xrow2,A+row+1,K,lda);
        F32PTR Crow=C+row;        
        int    col=0;
        for (; col < N - 1; col+=2)
            gen_f32_dot2x2(Xrow1,Xrow2,B+col * ldb,B+col * ldb+ldb,K,Crow+ldc * col,Crow+ldc*(col+1));
        if (col < N)
            *(Crow+ldc * col+1)=gen_f32_dot2x1(Xrow1,Xrow2,B+col * ldb,K,Crow+ldc * col);;   
    }
	if (row < M) {
		__F32copyFrmStidedMem(Xrow1,A+row,K,lda);        
        F32PTR Crow=C+row;
        int    col=0;
        for (; col < N - 1; col+=2)
             *(Crow+ldc * (col+1))=gen_f32_dot2x1(B+col*ldb,B+(col+1)*ldb,Xrow1,K,Crow+col*ldc);              
        if (col < N)
            *(Crow+ldc * col )=gen_f32_dot(Xrow1,B+col*ldb,K);            
	}
}
STATIC void gen_f32_gemm_XtYt2x2(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc)
{ 
    F32     YROW_FIXEXD[4096];
    F32PTR  Yrow=(2*K <=4096) ? YROW_FIXEXD : alloca(2*K * sizeof(F32));
    F32PTR  Yrow1=Yrow,Yrow2=Yrow1+K; 
    int col=0;
    for (; col < N-1; col+=2) {
		__F32copyFrmStidedMem(Yrow1,B+col,K,ldb);
		__F32copyFrmStidedMem(Yrow2,B+col+1,K,ldb);
        int    row=0;
        for (; row < M - 1; row+=2)
            gen_f32_dot2x2(A+row*lda,A+(row+1)*lda,Yrow1,Yrow2,K,C+row,C+ldc+row);
        if (row < M)
            *(C+ldc+row)=gen_f32_dot2x1(Yrow1,Yrow2,A+row * lda,K,C+row);
        C+=ldc+ldc;
    }
	if (col < N) {
		__F32copyFrmStidedMem(Yrow1,B+col,K,ldb);        
        int    row=0;
        for (; row < M - 1; row+=2)
             *(C+row+1)=gen_f32_dot2x1(A+row*lda,A+(row+1)*lda,Yrow1,K,C+row);              
        if (row < M)
            *(C+row)=gen_f32_dot(A+row*lda,Yrow1,K);          
	}
}
static INLINE F32 __gen_f32_dot_stride(F32PTR  x,F32PTR y,int N,I32 ystride) {
	 #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		sum+=x[i] * y[0]+x[i+1] * y[ystride]+x[i+2] * y[ystride*2]+x[i+3] * y[ystride*3];
		y+=ystride * 4;
	}
	for (; i < N;++i) {
		sum+=x[i] * y[0];
		y+=ystride;
	}
	return sum;
}
STATIC void  gen_f32_gemm_XYt2x1(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc)
{
    F32     XROW_FIXEXD[4096];
    F32PTR  Xrow=(2 * K <=4096) ? XROW_FIXEXD : alloca( 2*K * sizeof(F32) );
    F32PTR  Xrow1=Xrow,Xrow2=Xrow1+K;
    int row=0; 
	 for (; row < M; row+=1) {
		 __F32copyFrmStidedMem(Xrow1,A+row,K,lda);
        F32PTR Crow=C+row;
        int    col=0;
        for (; col < N; col++)
            *(Crow+ldc * col)=__gen_f32_dot_stride(Xrow1,B+col,K,ldb);
    }
}
STATIC   void gen_f32_axpy_inplace(const F32 c,const F32PTR x,F32PTR y,const int N) {
#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		y[i]+=x[i] * c;
		y[i+1]+=x[i+1] * c;
		y[i+2]+=x[i+2] * c;
		y[i+3]+=x[i+3] * c;
	}
	for (; i < N;++i)
		y[i]+=x[i] * c;
}
STATIC  INLINE void _f32_axpy_inplace(const F32 c,const F32PTR x,F32PTR y,const int N) {
#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		y[i]+=x[i] * c;
		y[i+1]+=x[i+1] * c;
		y[i+2]+=x[i+2] * c;
		y[i+3]+=x[i+3] * c;
	}
	for (; i < N;++i)
		y[i]+=x[i] * c;
}
static INLINE void _f32_axpy_inplace_x2(const F32PTR a,const F32PTR x1,const F32PTR x2,F32PTR y,const int N) {
 	 #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		y[i]+=x1[i] * a[0]+x2[i] * a[1];
		y[i+1]+=x1[i+1] * a[0]+x2[i+1] * a[1];
		y[i+2]+=x1[i+2] * a[0]+x2[i+2] * a[1];
		y[i+3]+=x1[i+3] * a[0]+x2[i+3] * a[1];		
	}
	for (;i<N;++i)
		y[i]+=x1[i] * a[0]+x2[i] * a[1];
}
STATIC  void gen_f32_gemv_Xb(int N,int K,F32PTR X,int lda,F32PTR b,F32PTR C)
{
	memset(C,0,sizeof(F32) * N);
	int row=0;
	for (; row < N - (256 * 2 - 1); row+=256 * 2) {
		int col=0;
		for (; col < K - 1; col+=2) {
			_f32_axpy_inplace_x2(b+col,X+col * lda+row,X+(col+1) * lda+row,C+row,256 * 2);	
		}
		if (col < K)
			_f32_axpy_inplace(b[col],X+col * lda+row,C+row,256 * 2);
	}
	int n=N - row;
	if (n > 0) {
		int col=0;
		for (; col < K - 1; col+=2) {
			_f32_axpy_inplace_x2(b+col,X+col * lda+row,X+(col+1) * lda+row,C+row,n);
		}
		if (col < K)
			_f32_axpy_inplace(b[col],X+col * lda+row,C+row,n);
	}
}
STATIC I32 gen_f32_findindex(F32PTR  x,I32PTR indices,F32 value,int N,CmpFlag flag) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  cnt=0;
    int  i=0;
    switch (flag) {
        case CMP_LT:
            for (; i < regularPart; i+=UNROLL_NUMBER) {
                int keep1=x[i] <=value;    indices[cnt]=i;     cnt+=keep1;
                int keep2=x[i+1] < value; indices[cnt]=i+1; cnt+=keep2;
                int keep3=x[i+2] < value; indices[cnt]=i+2; cnt+=keep3;
                int keep4=x[i+3] < value; indices[cnt]=i+3; cnt+=keep4;
            }
            for (; i < N;++i) {
                int keep=x[i] < value;       indices[cnt]=i;   cnt+=keep;
            }
            return cnt;
        case CMP_LE:
            for (; i < regularPart; i+=UNROLL_NUMBER) {
                int keep1=x[i] <=value; indices[cnt]=i;         cnt+=keep1;
                int keep2=x[i+1] <=value; indices[cnt]=i+1; cnt+=keep2;
                int keep3=x[i+2] <=value; indices[cnt]=i+2; cnt+=keep3;
                int keep4=x[i+3] <=value; indices[cnt]=i+3; cnt+=keep4;
            }
            for (; i < N;++i) {
                int keep=x[i] <=value;       indices[cnt]=i;   cnt+=keep;
            }
            return cnt;
        case CMP_GT:
            for (; i < regularPart; i+=UNROLL_NUMBER) {
                int keep1=x[i] > value; indices[cnt]=i;         cnt+=keep1;
                int keep2=x[i+1] > value; indices[cnt]=i+1; cnt+=keep2;
                int keep3=x[i+2] > value; indices[cnt]=i+2; cnt+=keep3;
                int keep4=x[i+3] > value; indices[cnt]=i+3; cnt+=keep4;
            }
            for (; i < N;++i) {
                int keep=x[i] > value;       indices[cnt]=i;   cnt+=keep;
            }
            return cnt;
        case CMP_GE:
            for (; i < regularPart; i+=UNROLL_NUMBER) {
                int keep1=x[i] >=value; indices[cnt]=i;         cnt+=keep1;
                int keep2=x[i+1] >=value; indices[cnt]=i+1; cnt+=keep2;
                int keep3=x[i+2] >=value; indices[cnt]=i+2; cnt+=keep3;
                int keep4=x[i+3] >=value; indices[cnt]=i+3; cnt+=keep4;
            }
            for (; i < N;++i) {
                int keep=x[i] >=value;       indices[cnt]=i;   cnt+=keep;
            }
            return cnt;
        case CMP_EQ:
            for (; i < regularPart; i+=UNROLL_NUMBER) {
                int keep1=fabsf(x[i] - value)<1e-15; indices[cnt]=i;         cnt+=keep1;
                int keep2=fabsf(x[i+1] - value) < 1e-15; indices[cnt]=i+1; cnt+=keep2;
                int keep3=fabsf(x[i+2] - value) < 1e-15; indices[cnt]=i+2; cnt+=keep3;
                int keep4=fabsf(x[i+3] - value) < 1e-15; indices[cnt]=i+3; cnt+=keep4;
            }
            for (; i < N;++i) {
                int keep=fabsf(x[i] - value)<1e-15;       indices[cnt]=i;   cnt+=keep;
            }
            return cnt;
    }
	return cnt;
}
STATIC void  gen_f32_scatter_val_byindex(F32PTR  x,I32PTR indices,F32 value,int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	int  i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		x[indices[i]]=value;
		x[indices[i+1]]=value;
		x[indices[i+2]]=value;
		x[indices[i+3]]=value;
	}
	for (; i < N;++i)    x[indices[i]]=value;
}
STATIC void  gen_f32_scatter_vec_byindex(F32PTR  x,I32PTR indices,F32PTR values,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
        x[indices[i]]=values[i];
        x[indices[i+1]]=values[i+1];
        x[indices[i+2]]=values[i+2];
        x[indices[i+3]]=values[i+3];
    }   
    for (; i < N;++i)    x[indices[i]]=values[i]; 
}
STATIC void gen_f32_gatherVec_scatterVal_byindex(F32PTR  x,I32PTR indices,F32PTR values,F32 newValue,int N) {
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
        values[i]=x[indices[i]];
        values[i+1]=x[indices[i+1]];
        values[i+2]=x[indices[i+2]];
        values[i+3]=x[indices[i+3]];
        x[indices[i]]=newValue;
        x[indices[i+1]]=newValue;
        x[indices[i+2]]=newValue;
        x[indices[i+3]]=newValue;
    }   
    for (; i < N;++i) {
        values[i]=x[indices[i]];
        x[indices[i]]=newValue;
    }
}
STATIC void gen_f32_gather2Vec_scatterVal_byindex(F32PTR  x,F32PTR  y,I32PTR indices,F32PTR values,F32 newValue,int N){
    #define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
        values[i]=x[indices[i]];
        values[i+1]=x[indices[i+1]];
        values[i+2]=x[indices[i+2]];
        values[i+3]=x[indices[i+3]];
        values[i+N]=y[indices[i]];
        values[i+N+1]=y[indices[i+1]];
        values[i+N+2]=y[indices[i+2]];
        values[i+N+3]=y[indices[i+3]];
        y[indices[i]]=x[indices[i]]=newValue;
        y[indices[i+1]]=x[indices[i+1]]=newValue;
        y[indices[i+2]]=x[indices[i+2]]=newValue;
        y[indices[i+3]]=x[indices[i+3]]=newValue;
    }   
    for (; i < N;++i) {
        values[i]=x[indices[i]];
        values[N+i]=y[indices[i]];
        x[indices[i]]=newValue;
        y[indices[i]]=newValue;
    }
}
STATIC void gen_f32_scale_inplace(const F32 gain,const F32 offset,const F32PTR x,const int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
		x[i]=x[i] * gain+offset;
		x[i+1]=x[i+1] * gain+offset;
		x[i+2]=x[i+2] * gain+offset;
		x[i+3]=x[i+3] * gain+offset;
    }   
	for (; i < N;++i) {
		x[i]=x[i] * gain+offset;
	}
}
STATIC void gen_f32_hinge_pos(const F32PTR X,const F32PTR Y,const F32 knot,const int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
		Y[i]=X[i]   >=knot ? X[i]-knot:0;
		Y[i+1]=X[i+1] >=knot ? X[i+1] - knot : 0;
		Y[i+2]=X[i+2] >=knot ? X[i+2] - knot : 0;
		Y[i+3]=X[i+3] >=knot ? X[i+3] - knot : 0;
    }   
	for (; i < N;++i) {
		Y[i]=X[i] >=knot ? X[i] - knot : 0;
	}
}
STATIC void gen_f32_hinge_neg(const F32PTR X,const F32PTR Y,const F32 knot,const int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
		Y[i]=X[i]   <=knot ? knot - X[i]   : 0;
		Y[i+1]=X[i+1] <=knot ? knot - X[i+1] : 0;
		Y[i+2]=X[i+2] <=knot ? knot - X[i+2] : 0;
		Y[i+3]=X[i+3] <=knot ? knot - X[i+3] : 0;
    }   
	for (; i < N;++i) {
		Y[i]=X[i] <=knot ? knot - X[i] : 0;
	}
}
STATIC void gen_f32_step_pos(const F32PTR X,const F32PTR Y,const F32PTR Z,const F32 knot,const int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
    int  i=0;
    for (; i < regularPart; i+=UNROLL_NUMBER) {
		Z[i]=X[i]   >=knot ? Y[i] : 0;
		Z[i+1]=X[i+1] >=knot ? Y[i+1] : 0;
		Z[i+2]=X[i+2] >=knot ? Y[i+2] : 0;
		Z[i+3]=X[i+3] >=knot ? Y[i+3] : 0;
    }   
	for (; i < N;++i) {
		Z[i]=X[i] >=knot ? Y[i] : 0;
	}
}
STATIC void gen_f32_step_neg(const F32PTR X,const F32PTR Y,const F32PTR Z,const F32 knot,const int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	int  i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		Z[i]=X[i]     >=knot ? 0 : Y[i];
		Z[i+1]=X[i+1] >=knot ? 0 : Y[i+1];
		Z[i+2]=X[i+2] >=knot ? 0 : Y[i+2];
		Z[i+3]=X[i+3] >=knot ? 0 : Y[i+3];
	}
	for (; i < N;++i) {
		Z[i]=X[i] >=knot ? 0 : Y[i];
	}
}
#include "abc_vec.h"
  void SetupVectorFunction_Generic(void) {
    i32_add_val_inplace=&gen_i32_add_val_inplace;;
    i32_sum=&gen_i32_sum;
    f32_fill_val=&gen_f32_fill_val;
    f32_sum=&gen_f32_sum;
    f32_add_vec=&gen_f32_add_vec;
    f32_sub_vec=&gen_f32_sub_vec;
    f32_add_vec_inplace=&gen_f32_add_vec_inplace;
    f32_sub_vec_inplace=&gen_f32_sub_vec_inplace;
    f32_subrev_val_inplace=&gen_f32_subrev_val_inplace;
    f32_add_val_inplace=&gen_f32_add_val_inplace;
    f32_mul_val_inplace=&gen_f32_mul_val_inplace;
    f32_mul_vec_inplace=&gen_f32_mul_vec_inplace;
    f32_mul_vec=&gen_f32_mul_vec;
    f32_dot=&gen_f32_dot;
    f32_dot2x1=&gen_f32_dot2x1;
    f32_dot2x2=&gen_f32_dot2x2;
    f32_add_v_v2_vec_inplace=&gen_f32_add_v_v2_vec_inplace;
    f32_cos_vec_inplace=&gen_f32_cos_vec_inplace;
    f32_sin_vec_inplace=&gen_f32_sin_vec_inplace;
    f32_sincos_vec_inplace=& gen_f32_sincos_vec_inplace;
    f32_pow_vec_inplace=& gen_f32_pow_vec_inplace;
    f32_log_vec_inplace=&gen_f32_log_vec_inplace;
	f32_exp_vec_inplace=&gen_f32_exp_vec_inplace;
    f32_sqrt_vec_inplace=&gen_f32_sqrt_vec_inplace;
    f32_sqrt_vec=&gen_f32_sqrt_vec;
    f32_avgstd=&gen_f32_avgstd;
    f32_sx_sxx_to_avgstd_inplace=& gen_f32_sx_sxx_to_avgstd_inplace;
    f32_maxidx=&gen_f32_maxidx;
    f32_minidx=&gen_f32_minidx;
    f32_diff_back=& gen_f32_diff_back;
    f32_seq=& gen_f32_seq;
	i32_seq=&gen_i32_seq;
    f32_to_f64_inplace=& gen_f32_to_f64_inplace;
	f64_to_f32_inplace=&gen_f64_to_f32_inplace;
    i32_to_f32_scaleby_inplace=&gen_i32_to_f32_scaleby_inplace;
    i32_increment_bycond_inplace=&  gen_i32_increment_bycond_inplace;
	i32_increment_vec2_bycond_inplace=gen_i32_increment_vec2_bycond_inplace;
    i08_sum_binvec=& gen_i08_sum_binvec;
	f32_gemm_XtY2x1=NULL;
    f32_gemm_XtY2x2=gen_f32_gemm_XtY2x2;
    f32_gemm_XY1x2=NULL;
    f32_gemm_XY2x2=gen_f32_gemm_XY2x2;
    f32_gemm_XtYt2x2=gen_f32_gemm_XtYt2x2;
    f32_gemm_XYt2x1=NULL;
	f32_gemv_Xb=&gen_f32_gemv_Xb;
	f32_findindex=gen_f32_findindex;
	f32_scatter_vec_byindex=gen_f32_scatter_vec_byindex;
    f32_gatherVec_scatterVal_byindex=gen_f32_gatherVec_scatterVal_byindex;
    f32_gather2Vec_scatterVal_byindex=gen_f32_gather2Vec_scatterVal_byindex;
	f32_scale_inplace=gen_f32_scale_inplace;
	f32_hinge_pos=gen_f32_hinge_pos;
	f32_hinge_neg=gen_f32_hinge_neg;
	f32_step_pos=gen_f32_step_pos;
	f32_step_neg=gen_f32_step_neg;
	f32_axpy_inplace=gen_f32_axpy_inplace;
}
#include "abc_000_warning.h"
