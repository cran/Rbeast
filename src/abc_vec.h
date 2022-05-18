#pragma once
#include "abc_datatype.h"
#ifdef __cplusplus
extern "C" {
#endif
	typedef enum { CMP_LT,CMP_LE,CMP_GT,CMP_GE,CMP_EQ }CmpFlag;
	extern void (*i32_add_val_inplace)(const int C,const I32PTR X,const int N);
	extern I32(*i32_sum)(const I32PTR X,const int N);
	extern void (*f32_fill_val)(const F32 C,F32PTR X,int N);
	extern F32(*f32_sum)(const F32PTR X,int N);
	extern void (*f32_add_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
	extern void (*f32_sub_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
	extern void (*f32_add_vec_inplace)(const F32PTR SRC,const F32PTR DST,const int N);
	extern void (*f32_sub_vec_inplace)(const F32PTR SRC,F32PTR DST,int N);
	extern void (*f32_subrev_val_inplace)(const F32 C,F32PTR X,int N);
	extern void (*f32_add_val_inplace)(const F32 C,F32PTR X,int N);
	extern void (*f32_mul_val_inplace)(const F32 C,F32PTR X,const int N);
	extern void (*f32_mul_vec_inplace)(const F32PTR SRC,F32PTR DST,int N);
	extern void (*f32_mul_vec)(const F32PTR SRC1,const F32PTR SRC2,F32PTR DST,int N);
	extern F32(*f32_dot)(const F32PTR x,const F32PTR y,const int N);
	extern F32(*f32_dot2x1)(const F32PTR x,const F32PTR y,const F32PTR v,const int N,F32PTR res);
	extern void (*f32_dot2x2)(const F32PTR x1,const F32PTR x2,const F32PTR y1,const F32PTR y2,const int N,F32PTR res1,F32PTR res2);
	extern void (*f32_add_v_v2_vec_inplace)(const F32PTR SRC,const F32PTR x,F32PTR x2,int N);
	extern void (*f32_cos_vec_inplace)(const F32PTR X,const int N);
	extern void (*f32_sin_vec_inplace)(const F32PTR X,const int N);
	extern void (*f32_sincos_vec_inplace)(const F32PTR in_outsin,F32PTR outcos,const int N);
	extern void (*f32_pow_vec_inplace)(F32PTR X,F32 pow,int N);
	extern void (*f32_log_vec_inplace)(const F32PTR X,const int N);
	extern void (*f32_exp_vec_inplace)(const F32PTR X,const int N);
	extern void (*f32_sqrt_vec_inplace)(const F32PTR X,const int N);
	extern void (*f32_sqrt_vec)(const F32PTR X,F32PTR Y,int N);
	extern void(*f32_avgstd)(const F32PTR X,int N,F32PTR AVG,F32PTR STD);
	extern void (*f32_sx_sxx_to_avgstd_inplace)(F32PTR SX,F32PTR SXX,I32 Nsample,F32 scale,F32 offset,int N);
	extern I32(*f32_maxidx_slow)(const F32PTR  X,const int N,F32PTR val);
	extern I32(*f32_maxidx)(const F32PTR  X,const  int N,F32PTR val);
	extern I32(*f32_minidx)(const F32PTR  X,const int  N,F32PTR val);
	extern void (*f32_diff_back)(const F32PTR  X,F32PTR result,const int N);
	extern void (*f32_seq)(F32PTR p,F32 x0,F32 dX,int N);
	extern void (*f32_to_f64_inplace)(F32PTR data32,int N);
	extern void (*f64_to_f32_inplace)(F64PTR data64,int N);
	extern void (*i32_to_f32_scaleby_inplace)(I32PTR X,int N,F32 scale);
	extern void (*i32_increment_bycond_inplace)(I32PTR x,F32PTR cond,int N);
	extern void (*i32_increment_vec2_bycond_inplace)(I32PTR x,I32PTR y,F32PTR cond,int N);
	extern I32(*i08_sum_binvec)(U08PTR binvec,I32 N);
	extern void f32_cumsum_inplace(const F32PTR X,int N);
	extern void f32_cumsumsqr_inplace(const F32PTR X,int N);
	extern F32 f32_sumlog_slow(const F32PTR  X,const int N);
	extern F32 f32_sumlog(const F32PTR  X,const int N);
	extern I32 i08_find_nth_onebyte_binvec(U08PTR binvec,I32 N,I32 nth);
	extern I32 i08_find_nth_onebyte_binvec_v2(U08PTR binvec,I32 N,I32 numOneBytes,U32 rnd);
	extern void f32_sumfilter(const F32PTR X,F32PTR Y,int N,int winSize);
	extern F32  f32_corr_rmse_nan(const F32PTR X,const F32PTR Y,int N,F32PTR rmse);
	extern void  f32_truncate_inplace(const F32PTR X,F32 value,int N);
	void f32_to_strided_f64(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset);
	void f32_to_strided_f32(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset);
	void f32_from_strided_f32(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset);
	void f32_from_strided_f64(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset);
	void f32_from_strided_i32(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset);
	void f32_from_strided_i16(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset);
	void f32_set_nan_by_value(F32PTR a,I32 N,F32 missingValue);
	int f32_normalize_multicols_zeroout_nans(F32PTR Y,I32PTR BadRowIndices,I32 ldy,I32 N,I32 q,F32PTR mean,F32PTR sd);
	extern void f32_transpose_inplace(F32PTR Mat,I32 ROW,I32 COL);
	extern void f32_fill_val_matrixdiag(F32PTR mat,const F32 value,I32 N);
	extern void f32_add_val_matrixdiag(F32PTR mat,const F32 value,I32 N);
	extern F32 f32_sum_matrixdiag(F32PTR mat,I32 N);
	extern F32 f32_abs_sum(F32PTR X,I32 N);
	extern void f32_mat_multirows_extract_set_by_scalar(F32PTR X,I32 ROW,I32 COL,F32PTR Xcopy,I32PTR RowIndices,I32 nRows,F32 newValue);
	extern void f32_mat_multirows_set_by_submat(F32PTR X,I32 ROW,I32 COL,F32PTR Xcopy,I32PTR RowIndices,I32 nRows);
	extern void f32_normalize_inplace(F32PTR X,I32 N);
	extern void f32_normalize_x_factor_inplace(F32PTR X,I32 N,F32 factor);
	extern I32  f32_find_nans(const F32PTR X,int N,I32PTR index);
	extern void  (*f32_hinge_neg)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
	extern void  (*f32_hinge_pos)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
	extern void  (*f32_step_neg)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
	extern void  (*f32_step_pos)(const F32PTR X,const F32PTR Y,const F32 knot,const int N);
#define f32_copy(src,dst,N)  memcpy(dst,src,sizeof(F32)*N)
	extern void (*f32_gemm_XtY2x1)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void (*f32_gemm_XtY2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void (*f32_gemm_XY1x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void (*f32_gemm_XY2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void (*f32_gemm_XtYt2x2)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void (*f32_gemm_XYt2x1)(int M,int N,int K,F32PTR A,int lda,F32PTR B,int ldb,F32PTR C,int ldc);
	extern void(*f32_gemv_Xb)(int N,int K,F32PTR X,int lda,F32PTR b,F32PTR C);
	extern I32(*f32_findindex)(F32PTR  x,I32PTR indices,F32 value,int N,CmpFlag flag);
	extern void  (*f32_scatter_vec_byindex)(F32PTR  x,I32PTR indices,F32PTR values,int N);
	extern void (*f32_gatherVec_scatterVal_byindex)(F32PTR  x,I32PTR indices,F32PTR values,F32 newValue,int N);
	extern void (*f32_gather2Vec_scatterVal_byindex)(F32PTR  x,F32PTR  y,I32PTR indices,F32PTR values,F32 newValue,int N);
	extern void (*f32_scale_inplace)(const F32 gain,const F32 offset,const F32PTR x,const int N);
	extern void SetupVectorFunction_AVX2();
	extern void SetupVectorFunction_AVX512();
	extern void SetupVectorFunction_Generic();
	void print_funcs();
#ifdef __cplusplus
}
#endif
