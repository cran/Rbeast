﻿#include <math.h>
#include <string.h>
#include "abc_000_warning.h"
#include "abc_datatype.h"
#include "abc_vec.h"
#include "abc_ide_util.h"
void  (*i32_add_val_inplace)(const int C,const I32PTR X,const int N)=NULL;
I32   (*i32_sum)(const I32PTR X,const int N)=NULL;
void  (*f32_fill_val)(const F32 C,F32PTR X,int N);
F32   (*f32_sum)(const F32PTR X,int N)=NULL;
void (*f32_add_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
void (*f32_sub_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
void (*f32_add_vec_inplace)(const F32PTR SRC,const F32PTR DST,const int N);
void (*f32_sub_vec_inplace)(const F32PTR SRC,F32PTR DST,int N);
void (*f32_subrev_val_inplace)(const F32 C,F32PTR X,int N);
void (*f32_add_val_inplace)(const F32 C,F32PTR X,int N);
void (*f32_mul_val_inplace)(const F32 C,F32PTR X,const int N);
void (*f32_mul_vec_inplace)(const F32PTR SRC,F32PTR DST,int N);
void (*f32_mul_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
F32 (*f32_dot)(const F32PTR x,const F32PTR y,const int N);
F32 (*f32_dot2x1)(const F32PTR x,const F32PTR y,const F32PTR v,const int N,F32PTR res);
void (*f32_dot2x2)(const F32PTR x1,const F32PTR x2,const F32PTR y1,const F32PTR y2,const int N,F32PTR res1,F32PTR res2);
void (*f32_add_v_v2_vec_inplace)(const F32PTR SRC,const F32PTR x,F32PTR x2,int N);
void (*f32_cos_vec_inplace)(const F32PTR X,const int N);
void (*f32_sin_vec_inplace)(const F32PTR X,const int N);
void (*f32_sincos_vec_inplace)(const F32PTR in_outsin,F32PTR outcos,const int N);
void (*f32_pow_vec_inplace)(F32PTR X,F32 pow,int N);
void (*f32_log_vec_inplace)(const F32PTR X,const int N);
void (*f32_exp_vec_inplace)(const F32PTR X,const int N);
void (*f32_sqrt_vec_inplace)(const F32PTR X,const int N);
void (*f32_sqrt_vec)(const F32PTR X,F32PTR Y,int N);
void  (*f32_avgstd)(const F32PTR X,int N,F32PTR AVG,F32PTR STD);
void (*f32_sx_sxx_to_avgstd_inplace)(F32PTR SX,F32PTR SXX,I32 Nsample,F32 scale,F32 offset,int N);
I32 (*f32_maxidx_slow)(const F32PTR  X,const int N,F32PTR val);
I32 (*f32_maxidx)(const F32PTR  X,const  int N,F32PTR val);
I32 (*f32_minidx)(const F32PTR  X,const int  N,F32PTR val);
void (*f32_diff_back)(const F32PTR  X,F32PTR result,const int N);
void (*f32_seq)(F32PTR p,F32 x0,F32 dX,int N);
void (*i32_seq)(I32PTR p,I32 x0,I32 dX,int N);
void (*f32_to_f64_inplace)(F32PTR data32,int N);
void (*f64_to_f32_inplace)(F64PTR data64,int N);
void (*i32_to_f32_scaleby_inplace)(I32PTR X,int N,F32 scale);
void (*i32_increment_bycond_inplace)(I32PTR x,F32PTR cond,int N);
void (*i32_increment_vec2_bycond_inplace)(I32PTR x,I32PTR y,F32PTR cond,int N);
I32  (*i08_sum_binvec)(U08PTR binvec,I32 N);
void (*f32_gemm_XtY2x1)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void (*f32_gemm_XtY2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void (*f32_gemm_XY1x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void (*f32_gemm_XY2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void (*f32_gemm_XtYt2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void (*f32_gemm_XYt2x1)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
void(*f32_gemv_Xb)(int N,int K,F32PTR X,int lda,F32PTR b,F32PTR C);
I32   (*f32_findindex)(F32PTR  x,I32PTR indices,F32 value,int N,CmpFlag flag);
void  (*f32_scatter_vec_byindex)(F32PTR  x,I32PTR indices,F32PTR values,int N);
void  (*f32_gatherVec_scatterVal_byindex)(F32PTR  x,I32PTR indices,F32PTR values,F32 newValue,int N);
void  (*f32_gather2Vec_scatterVal_byindex)(F32PTR  x,F32PTR  y,I32PTR indices,F32PTR values,F32 newValue,int N);
void  (*f32_scale_inplace)(const F32 gain,const F32 offset,const F32PTR x,const int N);
void  (*f32_hinge_neg)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
void  (*f32_hinge_pos)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
void  (*f32_step_neg)(const F32PTR X,const F32PTR Y,const F32PTR Z,const F32 knot,const int N);
void  (*f32_step_pos)(const F32PTR X,const F32PTR Y,const F32PTR Z,const F32 knot,const int N);
void  (*f32_axpy_inplace)(const F32 a,const F32PTR x,F32PTR y,const int N);
void print_funcs(void) {
	r_printf("\n\n"  );
	r_printf("%s:%05x\n","i32_add_val_inplace",i32_add_val_inplace);
	r_printf("%s:%05x\n","i32_sum",i32_sum);
	r_printf("%s:%05x\n","f32_fill_val",f32_fill_val);
	r_printf("%s:%05x\n","f32_sum",f32_sum);
	r_printf("%s:%05x\n","f32_add_vec",f32_add_vec);
	r_printf("%s:%05x\n","f32_sub_vec",f32_sub_vec);
	r_printf("%s:%05x\n","f32_add_vec_inplace",f32_add_vec_inplace);
	r_printf("%s:%05x\n","f32_sub_vec_inplace",f32_sub_vec_inplace);
	r_printf("%s:%05x\n","f32_subrev_val_inplace",f32_subrev_val_inplace);
	r_printf("%s:%05x\n","f32_add_val_inplace",f32_add_val_inplace);
	r_printf("%s:%05x\n","f32_mul_val_inplace",f32_mul_val_inplace);
	r_printf("%s:%05x\n","f32_mul_vec_inplace",f32_mul_vec_inplace);
	r_printf("%s:%05x\n","f32_mul_vec",f32_mul_vec);
	r_printf("%s:%05x\n","f32_dot",f32_dot);
	r_printf("%s:%05x\n","*f32_dot2x1",f32_dot2x1);
	r_printf("%s:%05x\n","f32_dot2x2",f32_dot2x2);
	r_printf("%s:%05x\n","f32_add_v_v2_vec_inplace",f32_add_v_v2_vec_inplace);
	r_printf("%s:%05x\n","f32_cos_vec_inplace",f32_cos_vec_inplace);
	r_printf("%s:%05x\n","f32_sin_vec_inplace",f32_sin_vec_inplace);
	r_printf("%s:%05x\n","f32_sincos_vec_inplace",f32_sincos_vec_inplace);
	r_printf("%s:%05x\n","f32_pow_vec_inplace",f32_pow_vec_inplace);
	r_printf("%s:%05x\n","f32_log_vec_inplace",f32_log_vec_inplace);
	r_printf("%s:%05x\n","f32_exp_vec_inplace",f32_exp_vec_inplace);
	r_printf("%s:%05x\n","f32_sqrt_vec_inplace",f32_sqrt_vec_inplace);
	r_printf("%s:%05x\n","f32_sqrt_vec",f32_sqrt_vec);
	r_printf("%s:%05x\n","f32_avgstd",f32_avgstd);
	r_printf("%s:%05x\n","f32_sx_sxx_to_avgstd_inplace",f32_sx_sxx_to_avgstd_inplace);
	r_printf("%s:%05x\n","f32_maxidx_slow",f32_maxidx_slow);
	r_printf("%s:%05x\n","f32_maxidx",f32_maxidx);
	r_printf("%s:%05x\n","f32_minidx",f32_minidx);
	r_printf("%s:%05x\n","f32_diff_back",f32_diff_back);
	r_printf("%s:%05x\n","f32_seq",f32_seq);
	r_printf("%s:%05x\n","f32_to_f64_inplace",f32_to_f64_inplace);
	r_printf("%s:%05x\n","f64_to_f32_inplace",f64_to_f32_inplace);
	r_printf("%s:%05x\n","i32_to_f32_scaleby_inplace",i32_to_f32_scaleby_inplace);
	r_printf("%s:%05x\n","i32_increment_bycond_inplace",i32_increment_bycond_inplace);
	r_printf("%s:%05x\n","i32_increment_vec2_bycond_inplace",i32_increment_vec2_bycond_inplace);
	r_printf("%s:%05x\n","i08_sum_binvec",i08_sum_binvec);
	r_printf("%s:%05x\n","f32_gemm_XtY2x1",f32_gemm_XtY2x1);
	r_printf("%s:%05x\n","f32_gemm_XtY2x2",f32_gemm_XtY2x2);
	r_printf("%s:%05x\n","f32_gemm_XY1x2",f32_gemm_XY1x2);
	r_printf("%s:%05x\n","f32_gemm_XY2x2",f32_gemm_XY2x2);
	r_printf("%s:%05x\n","f32_gemm_XtYt2x2",f32_gemm_XtYt2x2);
	r_printf("%s:%05x\n","f32_gemm_XYt2x1",f32_gemm_XYt2x1);
	r_printf("%s:%05x\n","f32_gemv_Xb",f32_gemv_Xb);
	r_printf("%s:%05x\n","f32_findindex",f32_findindex);
	r_printf("%s:%05x\n","f32_scatter_vec_byindex",f32_scatter_vec_byindex);
	r_printf("%s:%05x\n","f32_gatherVec_scatterVal_byindex",f32_gatherVec_scatterVal_byindex);
	r_printf("%s:%05x\n","f32_gather2Vec_scatterVal_byindex",f32_gather2Vec_scatterVal_byindex);
}
void f32_cumsum_inplace(const F32PTR X,int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 csum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		csum+=X[i];     X[i]=csum;
		csum+=X[i+1]; X[i+1]=csum;
		csum+=X[i+2]; X[i+2]=csum;
		csum+=X[i+3]; X[i+3]=csum;
	}
	for (; i < N;++i) {
		csum+=X[i];     X[i]=csum;
	}
	#undef UNROLL_NUMBER
}
void f32_cumsumsqr_inplace(const F32PTR X,int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 csum=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		csum+=X[i]* X[i];     X[i]=csum;
		csum+=X[i+1]* X[i+1]; X[i+1]=csum;
		csum+=X[i+2] * X[i+2]; X[i+2]=csum;
		csum+=X[i+3] * X[i+3]; X[i+3]=csum;
	}
	for (; i < N;++i) {
		csum+=X[i]*X[i];     X[i]=csum;
	}
}
void f32_sumfilter(const F32PTR X,F32PTR Y,int N,int winSize) {
	if (winSize <=1) {
		memcpy(Y,X,sizeof(F32) * N);
		return;
	}
	I32 wLeft=winSize/2;          
	I32 wRight=(winSize - wLeft)-1;  
	F32 csumLeftEnd=0;
	for (int i=0; i< wLeft;++i){
		csumLeftEnd+=X[i];
		Y[i]=csumLeftEnd;
	}
	I32 Nadj=min(winSize,N);
	F32 csumAll=csumLeftEnd;
	for (int i=wLeft; i < Nadj;++i) 
		csumAll+=X[i];
	for (int i=wLeft; i < N-wRight;++i) {
		Y[i]=csumAll;
		csumAll+=X[i+wRight+1] - X[i - wLeft]; 
	}
	F32 csumRightEnd=0;
	for (int i=N - 1; i >=N - wRight; --i) {
		Y[i]=X[i]+csumRightEnd;
		csumRightEnd+=X[i];
	}
		#undef UNROLL_NUMBER
}
F32  f32_corr_rmse_nan(const F32PTR X,const F32PTR Y,int N,F32PTR rmse) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	F32 sumX=0;
	F32 sumXX=0;
	F32 sumY=0;
	F32 sumYY=0;
	F32 sumXY=0;
	F32 DXY2=0;
	I32 n=0;	
	for (int i=0; i < N;++i) {
		I32 isGood=(X[i]==X[i]) & (Y[i]==Y[i]);
		n+=isGood;
		F32 x=isGood ? X[i]:0;
		F32 y=isGood ? Y[i]:0;
		sumX+=x;   sumY+=y;
		sumXX+=x*x; sumYY+=y*y;
		sumXY+=x * y; 
		DXY2+=(x-y)*(x-y);
	}
	F32 r=(n*sumXY-sumX*sumY)/sqrtf((n*sumXX-sumX* sumX)*(n * sumYY - sumY * sumY));
	*rmse=sqrtf(DXY2/n);
	return r;
		#undef UNROLL_NUMBER
}
void f32_truncate_inplace(const F32PTR X,F32 value,int N) {
	#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		X[i]=X[i] > value ? value : X[i];
		X[i+1]=X[i+1] > value ? value : X[i+1];
		X[i+2]=X[i+1] > value ? value : X[i+2];
		X[i+3]=X[i+1] > value ? value : X[i+3];
	}
	for (; i < N;++i) {
		X[i]=X[i] > value ? value : X[i];
	}
		#undef UNROLL_NUMBER
}
I32  f32_find_nans(const F32PTR X,int N,I32PTR index ) {
#define UNROLL_NUMBER  4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 nMissing=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		index[nMissing]=i;
		nMissing+=X[i] !=X[i];		
		index[nMissing]=i+1;
		nMissing+=X[i+1] !=X[i+1];		
		index[nMissing]=i+2;
		nMissing+=X[i+2] !=X[i+2];		
		index[nMissing]=i+3;	
		nMissing+=X[i+3] !=X[i+3];
	}
	for (; i < N;++i) {
		index[nMissing]=i;
		nMissing+=X[i] !=X[i];
	}
	return nMissing;
   #undef UNROLL_NUMBER
}
#include "math.h"
I32 i32_maxidx(const I32PTR  X,const  int N,I32PTR val) {
	#define UNROLL_NUMBER  2 
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 maxVal=X[0];
	I32 maxIdx=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		I32 val;
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
I32 i32_minidx(const I32PTR  X,const int  N,I32PTR val) {
	#define UNROLL_NUMBER  2 
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 minVal=X[0];
	I32 minIdx=0;
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		I32 val;
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
F32 f32_sumlog(const F32PTR  X,const int N) {
	F64 sumlog=0;
	F64 cumprod=1.;
	for (I32 i=0; i < N; i++) {		
		F64 cumprod_new=cumprod * X[i];
		if (cumprod_new > 1e-307 && cumprod_new < 1e308) { 
			cumprod=cumprod_new;
		} else {
			sumlog+=log(cumprod);
			cumprod=X[i];
		}
	}
	sumlog+=logf(cumprod);
	return sumlog;
}
I32  i08_find_nth_onebyte_binvec(U08PTR binvec,I32 N,I32 nth)
{
	I32 nthPos;
	I32 count=0;
	I32 failed=1;
	{
		I32 i=0,N16=N/16;		
		I32 deltaSum=0;
		for (i=0; i < N16; i++) {
			I64 sum=*((I64PTR)binvec)+*((I64PTR)binvec+1);
			*((I32PTR)&sum)+=*((I32PTR)&sum+1);
			*((I16PTR)&sum)+=*((I16PTR)&sum+1);
			*((I08PTR)&sum)+=*((I08PTR)&sum+1);
			deltaSum=*((I08PTR)&sum);
			count+=deltaSum;
			if (count >=nth) { 
				failed=0;
				break; 
			}
			binvec+=16;
		}
		count -=deltaSum;
		nthPos=i * 16;
	}
	if (failed) {
		return 0xffffffff;
	}
	{
		I32 j;
		for (j=0; j < 16; j++) {
			count+=binvec[j];
			if (count==nth) break;
		}
		nthPos+=(j+1);
	}
	return nthPos;
}
#include "stdio.h"
I32  i08_find_nth_onebyte_binvec_v2(U08PTR binvec,I32 N,I32 numOneBytes,U32 rnd)
{
	static int I1=0,I2=0;
	I32 nth=rnd%N;	
	if (binvec[nth]) {
		I1++;
		return nth+1;
	}
	nth=(rnd%numOneBytes)+1;
	I2++;
	if (I2%100==0) {
		r_printf("%d %d\n",I1,I2);
	}
	I32 nthPos;
	I32 count=0;
	{
		I32 i=0,N16=N/16;
		I32 deltaSum=0;
		for (i=0; i < N16; i++) {
			I64 sum=*((I64PTR)binvec)+*((I64PTR)binvec+1);
			*((I32PTR)&sum)+=*((I32PTR)&sum+1);
			*((I16PTR)&sum)+=*((I16PTR)&sum+1);
			*((I08PTR)&sum)+=*((I08PTR)&sum+1);
			deltaSum=*((I08PTR)&sum);
			count+=deltaSum;
			if (count >=nth) break;
			binvec+=16;
		}
		count -=deltaSum;
		nthPos=i * 16;
	}
	{
		I32 j;
		for (j=0; j < 16; j++) {
			count+=binvec[j];
			if (count==nth) break;
		}
		nthPos+=(j+1);
	}
	return nthPos;
}
I64 i08_sum(I08PTR x,int N) {
#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	I64 sum=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) sum+=(I64) x[i]+(I64)x[i+1]+(I64)x[i+2]+(I64)x[i+3];
	for (; i < N;++i)  sum+=(I64) x[i];
	return sum;
#undef UNROLL_NUMBER
}
void f32_transpose_inplace(F32PTR Mat,I32 ROW,I32 COL) { 
	I32 totalElement=ROW * COL;
	for (I32 start=0; start < totalElement; start++) {
		I32 next=start;
		I32 i=0;
		do {
			++i;
			next=(next%COL) * ROW+next/COL;
		} while (next > start);
		if (next < start||i==1) continue;
		F32 tmp=Mat[next=start];
		do {
			i=(next%COL) * ROW+next/COL;
			Mat[next]=(i==start) ? tmp : Mat[i];
			next=i;
		} while (next > start);
	}
}
void f32_fill_val_matrixdiag(F32PTR mat,const F32 value,I32 N) {
	for (int i=0; i < N;++i) {
		*mat=value;
		mat+=(N+1);
	}
}
void f32_add_val_matrixdiag(F32PTR mat,const F32 value,I32 N) {
	for (int i=0; i < N;++i) {
		(*mat)+=value;
		mat+=(N+1);
	}
}
F32 f32_sum_matrixdiag(F32PTR mat,I32 N) {
	F64 sum=0;
	for (int i=0; i < N;++i) {
		sum+=*mat;
		mat+=(N+1);		
	}
	return sum;
}
F32 f32_abs_sum(F32PTR X,I32 N) {
	F64 sum=0;
	for (int i=0; i < N;++i) {
		sum+=fabs(X[i]);		 
	}
	return sum;
}
void f32_mat_multirows_extract_set_by_scalar(F32PTR X,I32 ROW,I32 COL,F32PTR Xcopy,I32PTR RowIndices,I32 nRows,F32 newValue) {
	int i=0;
	for (; i < COL - 1; i+=2) f32_gather2Vec_scatterVal_byindex(X+i * ROW,X+i * ROW+ROW,(I32PTR) RowIndices,Xcopy+i * nRows,newValue,nRows);
	if (i < COL)          		f32_gatherVec_scatterVal_byindex( X+i * ROW,(I32PTR)RowIndices,Xcopy+i * nRows,newValue,nRows);
}
void f32_mat_multirows_set_by_submat(F32PTR X,I32 ROW,I32 COL,F32PTR Xcopy,I32PTR RowIndices,I32 nRows) {
	for (int i=0; i < COL;++i) f32_scatter_vec_byindex(X+i * ROW,(I32PTR)RowIndices,Xcopy+nRows * i,nRows);
}
void f32_to_strided_f64(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset) {
	dst=(F64PTR)dst+dstOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		*(F64PTR)dst=src[i];
		*((F64PTR)dst+stride)=src[i+1];
		*((F64PTR)dst+2 * stride)=src[i+2];
		*((F64PTR)dst+3 * stride)=src[i+3];
		dst=(F64PTR)dst+4 * stride;
	}
	for (; i < N;++i) {
		*(F64PTR)dst=src[i];
		dst=(F64PTR)dst+stride;
	}
#undef UNROLL_NUMBER
}
void f32_to_strided_i64(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset) {
	dst=(I64PTR)dst+dstOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		*(I64PTR)dst=src[i];
		*((I64PTR)dst+stride)=src[i+1];
		*((I64PTR)dst+2 * stride)=src[i+2];
		*((I64PTR)dst+3 * stride)=src[i+3];
		dst=(I64PTR)dst+4 * stride;
	}
	for (; i < N;++i) {
		*(I64PTR)dst=src[i];
		dst=(I64PTR)dst+stride;
	}
}
void f32_to_strided_f32(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset) {
	dst=(F32PTR)dst+dstOffset;
	if (stride==1)
		memcpy(dst,src,sizeof(F32) * N);
	else {
		#define UNROLL_NUMBER 4
		const int regularPart=N & (-UNROLL_NUMBER); 
		I32 i=0;
		for (; i < regularPart; i+=UNROLL_NUMBER) {
			*(F32PTR)dst=src[i];			*( (F32PTR)dst+stride)=src[i+1];
			*( (F32PTR)dst+2*stride)=src[i+2];		*( (F32PTR)dst+3*stride)=src[i+3];
			dst=(F32PTR)dst+4*stride;
		}
		for (; i < N;++i) {
			*(F32PTR)dst=src[i];			
			dst=(F32PTR)dst+stride;
		}	 	 
	} 
#undef UNROLL_NUMBER
}
void f32_to_strided_i32(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset) {
	dst=(I32PTR)dst+dstOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		*(I32PTR)dst=src[i];			*((I32PTR)dst+stride)=src[i+1];
		*((I32PTR)dst+2 * stride)=src[i+2];		*((I32PTR)dst+3 * stride)=src[i+3];
		dst=(I32PTR)dst+4 * stride;
	}
	for (; i < N;++i) {
		*(I32PTR)dst=src[i];
		dst=(I32PTR)dst+stride;
	} 
#undef UNROLL_NUMBER
}
void f32_to_strided_i16(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset) {
	dst=(I16PTR)dst+dstOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		*(I16PTR)dst=src[i];		
		*((I16PTR)dst+stride)=src[i+1];
		*((I16PTR)dst+2 * stride)=src[i+2];	
		*((I16PTR)dst+3 * stride)=src[i+3];
		dst=(I16PTR)dst+4 * stride;
	}
	for (; i < N;++i) {
		*(I16PTR)dst=src[i];
		dst=(I16PTR)dst+stride;
	} 
#undef UNROLL_NUMBER
}
void f32_from_strided_f64(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset)
{ 
	src=(F64PTR)src+srcOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		dst[i]=*(F64PTR)src;
		dst[i+1]=*((F64PTR)src+srcStride);
		dst[i+2]=*((F64PTR)src+2 * srcStride);
		dst[i+3]=*((F64PTR)src+3 * srcStride);
		src=(F64PTR)src+4 * srcStride;
	}
	for (; i < N;++i) {
		dst[i]=*(F64PTR)src;
		src=(F64PTR)src+srcStride;
	}
#undef UNROLL_NUMBER		 
}
void f32_from_strided_i64(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset)
{ 
	src=(I64PTR)src+srcOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		dst[i]=*(I64PTR)src;
		dst[i+1]=*((I64PTR)src+srcStride);
		dst[i+2]=*((I64PTR)src+2 * srcStride);
		dst[i+3]=*((I64PTR)src+3 * srcStride);
		src=(I64PTR)src+4 * srcStride;
	}
	for (; i < N;++i) {
		dst[i]=*(I64PTR)src;
		src=(I64PTR)src+srcStride;
	}
#undef UNROLL_NUMBER	 
}
void f32_from_strided_f32(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset) {
	src=(F32PTR)src+srcOffset;
	if (srcStride==1) {
		memcpy(dst,src,sizeof(F32) * N);
	}
	else {
#define UNROLL_NUMBER 4
		const int regularPart=N & (-UNROLL_NUMBER); 
		I32 i=0;
		for (; i < regularPart; i+=UNROLL_NUMBER) {
			dst[i]=*(F32PTR)src;
			dst[i+1]=*((F32PTR)src+srcStride);
			dst[i+2]=*((F32PTR)src+2 * srcStride);
			dst[i+3]=*((F32PTR)src+3 * srcStride);
			src=(F32PTR)src+4 * srcStride;
		}
		for (; i < N;++i) {
			dst[i]=*(F32PTR)src;
			src=(F32PTR)src+srcStride;
		}
	}
#undef UNROLL_NUMBER
}
void f32_from_strided_i32(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset)
{ 
	src=(I32PTR)src+srcOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		dst[i]=*(I32PTR)src;
		dst[i+1]=*((I32PTR)src+srcStride);
		dst[i+2]=*((I32PTR)src+2 * srcStride);
		dst[i+3]=*((I32PTR)src+3 * srcStride);
		src=(I32PTR)src+4 * srcStride;
	}
	for (; i < N;++i) {
		dst[i]=*(I32PTR)src;
		src=(I32PTR)src+srcStride;
	}
#undef UNROLL_NUMBER	 
}
void f32_from_strided_i16(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset)
{ 
	src=(I16PTR)src+srcOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		dst[i]=*(I16PTR)src;
		dst[i+1]=*((I16PTR)src+srcStride);
		dst[i+2]=*((I16PTR)src+2 * srcStride);
		dst[i+3]=*((I16PTR)src+3 * srcStride);
		src=(I16PTR)src+4 * srcStride;
	}
	for (; i < N;++i) {
		dst[i]=*(I16PTR)src;
		src=(I16PTR)src+srcStride;
	}
#undef UNROLL_NUMBER	 
}	
void f64_from_strided_f64(F64PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset) { 
	src=(F64PTR)src+srcOffset;
	#define UNROLL_NUMBER 4
	const int regularPart=N & (-UNROLL_NUMBER); 
	I32 i=0;
	for (; i < regularPart; i+=UNROLL_NUMBER) {
		dst[i]=*(F64PTR)src;
		dst[i+1]=*((F64PTR)src+srcStride);
		dst[i+2]=*((F64PTR)src+2 * srcStride);
		dst[i+3]=*((F64PTR)src+3 * srcStride);
		src=(F64PTR)src+4 * srcStride;
	}
	for (; i < N;++i) {
		dst[i]=*(F64PTR)src;
		src=(F64PTR)src+srcStride;
	}
#undef UNROLL_NUMBER	 
}
void f32_to_strided_mem(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset,DATA_TYPE dstDtype) {
	if (dstDtype==DATA_FLOAT) 		    f32_to_strided_f32(src,dst,N,stride,dstOffset);
	else if (dstDtype==DATA_DOUBLE)    	f32_to_strided_f64(src,dst,N,stride,dstOffset);
	else if (dstDtype==DATA_INT64)    	f32_to_strided_i64(src,dst,N,stride,dstOffset);
	else if (dstDtype==DATA_INT32)    	f32_to_strided_i32(src,dst,N,stride,dstOffset);
	else if (dstDtype==DATA_INT16)    	f32_to_strided_i16(src,dst,N,stride,dstOffset);
}
void f32_from_strided_mem(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset,DATA_TYPE srcDataType) {
	if      (srcDataType==DATA_FLOAT) 	   f32_from_strided_f32(dst,src,N,srcStride,srcOffset);
	else if (srcDataType==DATA_DOUBLE)   f32_from_strided_f64(dst,src,N,srcStride,srcOffset);
	else if (srcDataType==DATA_INT64)    f32_from_strided_i64(dst,src,N,srcStride,srcOffset);
	else if (srcDataType==DATA_INT32) {
		f32_from_strided_i32(dst,src,N,srcStride,srcOffset);
		#if R_INTERFACE==1
		I32 INTMIN=0x80000001;
		F32 NAinteger=(F32)(INTMIN);  
		f32_set_nan_by_value(dst,N,NAinteger);
		#endif
	}
	else if (srcDataType==DATA_INT16)	   f32_from_strided_i16(dst,src,N,srcStride,srcOffset);
}
void arr_from_strided_mem(VOID_PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset,DATA_TYPE srcDstDataType) {
	if (srcDstDataType==DATA_FLOAT) {
		f32_from_strided_f32(dst,src,N,srcStride,srcOffset);
	} else if (srcDstDataType==DATA_DOUBLE) {
		f64_from_strided_f64(dst,src,N,srcStride,srcOffset);
	}
}
void f32_set_nan_by_value(F32PTR a,I32 N,F32 missingValue) {
	if (missingValue !=missingValue)		
		return;
	F32 nan=getNaN();
	for (I32 i=N-1; i >=0; i--) {
		a[i]=fabsf(a[i] - missingValue) < 1e-10||IsInf(a[i]) ? nan : a[i];
	}
}
int f32_normalize_multicols_zeroout_nans(F32PTR Y,I32PTR BadRowIndices,I32 ldy,I32 N,I32 q,F32PTR mean,F32PTR sd) {
	if (q==1) {
		I32     nMissing=0;
		F64     sumY=0,sumYY=0;
		for (I32 i=0; i < N; i++) {
			if (Y[i] !=Y[i]) 
				BadRowIndices[nMissing++]=i;			
			else {
				sumY+=Y[i];
				sumYY+=Y[i] * Y[i];
			}
		}
		I32 n=N - nMissing;
		F32 MEAN32=sumY/n;
		F64 SD32=(sumYY/n - MEAN32 * MEAN32); 
		SD32=SD32 > 0 ? sqrt(SD32) : 1.f;
		I32   jOmit=0;
		for (I32 i=0; i < N;++i) {
			if (jOmit < nMissing && i==BadRowIndices[jOmit]) {
				Y[i]=0.f; 
				jOmit++;
			}  else {
				Y[i]=(Y[i] - MEAN32) * (1./SD32);
			}
		}
		mean[0]=MEAN32;
		sd[0]=SD32;
		return nMissing;
	}
	I32PTR  isNANValue=(I32PTR)BadRowIndices;
	memset(isNANValue,0,sizeof(I32)* N);
	for (I32 i=0; i < q; i++) {
		for (I32 j=0; j < N; j++) { 	 
			if ( Y[j]!=Y[j])	isNANValue[j]=1;			 
		}
		Y+=ldy;
	}
	Y -=ldy * q;
	I32 nMissing=0; 	
	for (I32 j=0; j < N; j++) {
		I32 keep=BadRowIndices[j];
		BadRowIndices[nMissing]=j;
		nMissing+=keep;
	}
	I32 n=N - nMissing;
	for (I32 col=0; col < q; col++) {
		F64 sumY=0;
		F64 sumYY=0;
		I32 jOmit=0;
		for (I32 i=0; i < N;++i) {
			if (jOmit < nMissing && i==BadRowIndices[jOmit]) {
				jOmit++;
			} else {
				sumY+=Y[i];
				sumYY+=Y[i] * Y[i];
			}
		}
		F32 MEAN32=sumY/n;
		F64 SD32=(sumYY/n - MEAN32 * MEAN32); 
		SD32=SD32 > 0 ? sqrt(SD32) : 1.f;
		jOmit=0;
		for (I32 i=0; i < N;++i) {
			if (jOmit < nMissing && i==BadRowIndices[jOmit]) {
				Y[i]=0.f; 
				jOmit++;
			}	else {
				Y[i]=(Y[i] - MEAN32) * (1./SD32);
			}
		}
		mean[col]=MEAN32;
		sd[col]=SD32;
		Y=Y+ldy;
	}
	return nMissing;
}
void f32_normalize_std_avg_inplace(F32PTR X,I32 N,F32PTR avg,F32PTR std) {
	f32_avgstd(X,N,avg,std);
	F32 gain=1/std[0];
	F32 offset=-avg[0]/std[0];  
	f32_scale_inplace(gain,offset,X,N);
}
void f32_normalize_inplace(F32PTR X,I32 N) {
	F32 avg,std;
	f32_avgstd(X,N,&avg,&std);
	F32 gain=1/std;
	F32 offset=-avg/std;  
	f32_scale_inplace(gain,offset,X,N);
}
void f32_normalize_x_factor_inplace(F32PTR X,I32 N,F32 factor) {
	F32 avg,std;
	f32_avgstd(X,N,&avg,&std);
	F32 gain=factor/std;
	F32 offset=-factor*avg/std;  
	f32_scale_inplace(gain,offset,X,N);
}
I64  sub2ind(int *dims,int ndim,int * subs) {
	if (ndim==1) {
		return subs[0];
	} else if (ndim==2) {
		I32 rows=dims[0];		
		return subs[0]+(subs[1] - 1) * rows;
	} else if (ndim==3) {
		I64 rows=dims[0];
		I64 cols=dims[1];
		return subs[0]+(subs[1] - 1) * rows+(subs[2] - 1) * rows*cols;
	}	else {
		I64 idx=subs[0];
		I64 stride=1;
		for (int i=1; i < ndim; i++) {
			stride=stride *dims[i - 1];
			idx=idx+subs[i] * stride;		
		}		
		return  idx;
	}	
}
void ind2sub(int *dims,int ndim,I64 ind,int * subs) {
	if (ndim==1) {
		subs[0]=ind;
	} else if (ndim==2) {
		I32 ROW=dims[0];				
		int c=(ind - 1)/ROW;
		int r=ind - c * ROW;
		c=c+1;
		subs[0]=r;
		subs[1]=c;
	} else if (ndim==3) {
		I64 ROW=dims[0];
		I64 COL=dims[0];
		I64 ind0=ind - 1; 
		int d3=ind0/(ROW * COL);
		ind0=ind0 - d3 * (ROW * COL);
		int d2=ind0/ROW;
		int d1=ind0 - d2 * ROW;
		subs[0]=d1+1;
		subs[1]=d2+1;
		subs[1]=d2+1;
	}	else {	 
		I64 ind0=ind - 1; 
		subs[0]=1; 
		for (int i=1; i < ndim; i++) {
			subs[i]=dims[i-1] * subs[i - 1];
		}
		for (int i=ndim-1; i >0; i--) {
			I32 dimStride=subs[i];
			subs[i]=ind0/dimStride;          
			ind0=ind0 - subs[i] * dimStride;
			subs[i]++;
		}
		subs[0]=ind0+1;
	}	
}
int ndarray_get1d_stride_offset(int *dims,int ndim,int *subs,int whichdim,I64 * stride,I64 *offset) {
	int whichdim0=whichdim - 1;
	I64 STRIDE;
	I64 OFFSET=0;
	I64 CulDimStride=1;
	for (int i=0; i < ndim; i++) {
		if (i==whichdim0) {STRIDE=CulDimStride;	} 
		OFFSET+=CulDimStride * (subs[i] - 1);
		CulDimStride *=dims[i];		
	}		
	OFFSET -=STRIDE * (subs[whichdim0] - 1);
	*stride=STRIDE;
	*offset=OFFSET;
	return dims[whichdim0];
}
void f32_get1d_from_ndarray(F32PTR dst,VOID_PTR src,int* dims,int ndim,int* subs,int whichdim,DATA_TYPE srcDtype) {
	I64 stride,offset;
	int N=ndarray_get1d_stride_offset(dims,ndim,subs,whichdim,&stride,&offset);
	f32_from_strided_mem(dst,src,N,stride,offset,srcDtype);
}
void f32_set1d_to_ndarray(F32PTR src,VOID_PTR dst,int* dims,int ndim,int* subs,int whichdim,DATA_TYPE dstDtype) {
	I64 stride,offset;
	int N=ndarray_get1d_stride_offset(dims,ndim,subs,whichdim,&stride,&offset);
	f32_to_strided_mem(src,dst,N,stride,offset,dstDtype);
}
void f32_get2d_from_ndarray(F32PTR dst,VOID_PTR src,int* dims,int ndim,int* subs,int d1,int d2,DATA_TYPE srcDtype) {
	d1--; 
	d2--;
	if (d1 > d2) {
		int tmp=d1;
		d1=d2;
		d2=tmp;
	}
	I64 stride1,stride2;
	I64 cumstride=1;
	I64 offset=0;
	for (int i=0; i < ndim; i++) {
		if (i==d1) { stride1=cumstride; };
		if (i==d2) { stride2=cumstride; };
		offset+=cumstride * (subs[i]-1);
		cumstride *=dims[i];
	}
	offset -=stride1 * (subs[d1]-1);
	offset -=stride2 * (subs[d2]-1);
	int n1=dims[d1];
	int n2=dims[d2];
	if (d2 - d1==1) {		
		f32_from_strided_mem(dst,src,n1 * n2,stride1,offset,srcDtype);
	} else {
		for (int i=0; i < n2; i++) {
			f32_from_strided_mem(dst,src,n1,stride1,offset,srcDtype);
			dst+=n1;
			offset+=stride2;
		}
	}
}
void f32_set2d_from_ndarray(F32PTR src,VOID_PTR dst,int* dims,int ndim,int* subs,int d1,int d2,DATA_TYPE dstDtype) {
	d1--; 
	d2--;
	if (d1 > d2) {
		int tmp=d1;
		d1=d2;
		d2=tmp;
	}
	I64 stride1,stride2;
	I64 cumstride=1;
	I64 offset=0;
	for (int i=0; i < ndim; i++) {
		if (i==d1) { stride1=cumstride; };
		if (i==d2) { stride2=cumstride; };
		offset+=cumstride * (subs[i]-1);
		cumstride *=dims[i];
	}
	offset -=stride1 * (subs[d1]-1);
	offset -=stride2 * (subs[d2]-1);
	int n1=dims[d1];
	int n2=dims[d2];
	if (d2 - d1==1) {		
		f32_to_strided_mem(src,dst,n1 * n2,stride1,offset,dstDtype);
	} else {
		for (int i=0; i < n2; i++) {
			f32_to_strided_mem(src,dst,n1,stride1,offset,dstDtype);
			src+=n1;
			offset+=stride2;
		}
	}
}
#include "abc_000_warning.h"
