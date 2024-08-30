#pragma once
#include "abc_000_macro.h"
#include "abc_vec.h"
#include "abc_mat.h"
#if defined(COMPILER_MSVC)
	#define F77__CALL(x)  x
#elif defined(COMPILER_CLANG)||defined(COMPILER_GCC)||defined(COMPILER_SOLARIS)  
	#define PRIMITIVE_CAT(a,...) a##__VA_ARGS__
	#define F77__CALL(x)		PRIMITIVE_CAT(x,_)
	#define RSGEMV 			rsgemv 
	#define RSGEMM 			rsgemm 
	#define RSTRMV			rstrmv
	#define RSSYMV          rssymv
	#define RSPOTRS			rspotrs
	#define RSTRTRS 		rstrtrs 
	#define RSPOTRF 		rspotrf 
#endif
#define CblasUpper    121
#define CblasLower    122
#define CblasNonUnit  131
#define CblasUnit     132
#define CblasNoTrans  111
#define CblasTrans    112
void  F77__CALL(RSGEMV)(char *trans,int *m,int *n,F32PTR alpha,F32PTR a,const int *lda,F32PTR x,int *incx,F32PTR beta,F32PTR y,int *incy,size_t xxx_Char_Len3);
void  F77__CALL(RSGEMM)(char *,char*,int*,int*,int*,F32PTR,F32PTR,int*,F32PTR,int*,F32PTR,F32PTR,int*,size_t xxx_Char_Len1,size_t xxx_Char_Len2);
void  F77__CALL(RSTRMV)(char *uplo,char *trans,char *diag,int *n,F32PTR a,int *lda,F32PTR x,int *incx,size_t xxx_Char_Len1,size_t xxx_Char_Len2,size_t xxx_Char_Len3);
void  F77__CALL(RSSYMV)(char *UPLO,int* N,F32PTR  ALPHA,F32PTR  A,int * lda,F32PTR X,int * INCX,F32PTR  beta,F32PTR Y,int * INCY,size_t xxx_Char_Len1);
void  F77__CALL(RSTRTRS)(char* uplo,char* trans,char* diag,int* n,int* nrhs,F32PTR  a,int* lda,F32PTR  b,int* ldb,int* info,size_t xxx_Char_Len1,size_t xxx_Char_Len2,size_t xxx_Char_Len3);
void  F77__CALL(RSPOTRF)(char* uplo,int* n,F32PTR  a,int* lda,int* info,size_t xxx_Char_Len3);
void  F77__CALL(RSPOTRS)(char* uplo,int* n,int* nrhs,F32PTR  a,int* lda,F32PTR  b,int* ldb,int* info,size_t xxx_Char_Len1);
#define r_cblas_sgemv(CBLAS_LAYOUT,CBLAS_TRANSPOSE,M,N,alpha,A,lda,X,incX,beta,Y,incY)  \
	       f32__sgemv(CBLAS_TRANSPOSE,M,N,alpha,A,lda,X,incX,beta,Y,incY)
static INLINE void  f32__sgemv(int trans,int m,int n,F32 alpha,F32PTR a,int lda,F32PTR x,int incx,F32  beta,F32PTR y,int incy)
{
	if      (trans==CblasTrans)		f32_gemm_XtY2x2(n,1,m,a,lda,x,m,y,n);
	else if (trans==CblasNoTrans)		f32_gemv_Xb(m,n,a,lda,x,y);
}
#define r_cblas_sgemm(CBLAS_LAYOUT,CBLAS_TRANSPOSE_A,CBLAS_TRANSPOSE_B,M,N,K,alpha,A,lda,B,ldb,beta,C,ldc)  \
		   f32__sgemm(CBLAS_TRANSPOSE_A,CBLAS_TRANSPOSE_B,M,N,K,alpha,A,lda,B,ldb,beta,C,ldc)
static  INLINE void  f32__sgemm(int transa,int transb,int m,int n,int k,F32 alpha,F32PTR a,int lda,F32PTR b,int ldb,F32 beta,F32PTR c,int ldc)
{
	if (transa==CblasTrans) {
		if      (transb==CblasNoTrans)	f32_gemm_XtY2x2(m,n,k,a,lda,b,ldb,c,ldc);
		else if (transb==CblasTrans)		f32_gemm_XtYt2x2(m,n,k,a,lda,b,ldb,c,ldc);
	}
	else if (transa==CblasNoTrans) {
		if      (transb==CblasNoTrans)	f32_gemm_XY2x2(m,n,k,a,lda,b,ldb,c,ldc);
		else if (transb==CblasTrans)		f32_gemm_XYt2x1(m,n,k,a,lda,b,ldb,c,ldc);
	}
}
#define  r_LAPACKE_spotrf(matrix_layout,uplo,n,a,lda)        f32_chol(a,lda,n);
static INLINE void  f32_chol(F32PTR  a,int lda,int n )        {
	inplace_chol(a,lda,n);
}
#define r_LAPACKE_spotrs(matrix_layout,uplo,n,nrhs,a,lda,b,ldb)   \
             f32__spotrs(uplo,n,nrhs,a,lda,b,ldb)
static INLINE void  f32__spotrs(char uplo,int n,int nrhs,F32PTR  a,int lda,F32PTR  b,int ldb) {
	solve_U_as_LU_rectmat_multicols(a,b,b,lda,n,nrhs); 
}
#define r_LAPACKE_strtrs(matrix_layout,uplo,trans,diag,n,nrhs,a,lda,b,ldb )  \
      	f32__strtrs(uplo,trans,diag,n,nrhs,a,lda,b,ldb)
static INLINE void  f32__strtrs(char uplo,char trans,char diag,int n,int nrhs,F32PTR  a,int lda,F32PTR  b,int ldb)
{
	if     (uplo=='U' && trans=='N') {
		for (int i=0; i < nrhs;++i) {
			solve_U_as_U(a,b,lda,n);
			b=b+ldb;
		}	
	}
	else if (uplo=='U' && trans=='T') {
		for (int i=0; i < nrhs;++i) {
			solve_U_as_L(a,b,lda,n);
			b=b+ldb;
		}
	}
	else if (uplo=='L' && trans=='N') {
		for (int i=0; i < nrhs;++i) {
			solve_L_as_L(a,b,lda,n);
			b=b+ldb;
		}
	}
	else if (uplo=='L' && trans=='T') {
		for (int i=0; i < nrhs;++i) {
			solve_L_as_U(a,b,lda,n);
			b=b+ldb;
		}
	}
}
#define r_ippsMaxIndx_32f(X,N,val,idx)				*(I32PTR)(idx)=f32_maxidx( X,N,val)
#define r_ippsMinIndx_32f(X,N,val,idx)				*(I32PTR)(idx)=f32_minidx( X,N,val)
#define r_ippsSumLn_32f(pSrc,len,pSum)				*(F32PTR)(pSum)=f32_sumlog(pSrc,len) 
#define r_ippsMeanStdDev_32f(pSrc,N,pMean,pSd,hint) f32_avgstd( pSrc,N,pMean,pSd) 
#define r_vsPowx(n,src,power,res)						f32_pow_vec_inplace(src,power,n) 
#define r_vmsLn(n,src,res,mode)						f32_log_vec_inplace(src,n) 
#define r_ippsLn_32f_I(pSrcDst,N)						f32_log_vec_inplace(pSrcDst,N)
#define r_vmsCos(n,a,r,mode)							f32_cos_vec_inplace(a,n)
#define r_vmsSin(n,a,r,mode)							f32_sin_vec_inplace(a,n)
#define r_ippsSqrt_32f_I( pSrc,len)					f32_sqrt_vec_inplace(pSrc,len)
#define r_ippsSub_32f_I(pSrc,pDst,len)					f32_sub_vec_inplace( pSrc,pDst,len)
#define r_ippsSub_32f(pSrc1,pSrc2,pDst,len)			f32_sub_vec(pSrc1,pSrc2,pDst,len)
#define r_ippsAdd_32f_I(pSrc,pDst,len)					f32_add_vec_inplace(pSrc,pDst,len)
#define r_ippsSubC_32f_I(val,pSrcDst,N)				f32_add_val_inplace(-(val),pSrcDst,N)
#define r_ippsSubCRev_32f_I(val,pSrcDst,N)			f32_subrev_val_inplace(val,pSrcDst,N) 
#define r_ippsMul_32f( pSrc1,pSrc2,pDst,N)			f32_mul_vec(pSrc1,pSrc2,pDst,N)
#define r_ippsMul_32f_I(pSrc,pSrcDst,N)				f32_mul_vec_inplace(pSrc,pSrcDst,N)
#define ONE_STEP_DIFF(pSrcX,pSrcY,pDst,N)				f32_diff_back(pSrcX,pDst,N)
#define r_ippsSum_32f(Src,N,Sum,PrecisionLevel)		*(F32PTR)(Sum)=f32_sum(Src,N)
#define r_ippsSum_32s_Sfs(Src,N,Sum,ScaleFfactor)	*(I32PTR)(Sum)=i32_sum(Src,N)
#define r_cblas_scopy(N,src,incX,dst,incY)			memcpy(dst,src,(N)*sizeof(F32))
#define r_ippsSet_32f(value,dst,N)					f32_fill_val(value,dst,N)
#define r_ippsSet_32s(value,dst,N)					r_ippsSet_32s##value(dst,N)
#define r_ippsSet_32s0(dst,N)							memset(dst,0,sizeof(I32)*(N))
#define r_ippsSet_8u(val,pDst,N)						memset(pDst,val,N) 
#define r_cblas_sscal(N,alpha,X,incX)				f32_mul_val_inplace( alpha,X,N)
#define r_ippsMulC_32f_I(alpha,SrcDst,N)				f32_mul_val_inplace( alpha,SrcDst,N) 
#define r_ippsAddC_32s_ISfs(val,X,N,scaleFactor)     i32_add_val_inplace(val,X,N)
#define DOT(N,X,Y)										f32_dot(X,Y,N)
