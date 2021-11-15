#include "abc_000_warning.h"
#include "abc_mat.h"
static void chol(F32PTR XtX,F32PTR U,I32 K,I32 k)
{
	if (k==0) 	{
		U[0]=sqrtf(XtX[0]);
		U[K]=XtX[K]/U[0];
		U[K+1]=sqrtf(XtX[K+1] - U[K] * U[K]);
		return;
	}
	 for (rI32 m=0; m <=1; m++)
	{
		I32	   k_K=k*K;
		F32PTR    oldColPtr=U;
		F32PTR    newColPtr;
		memcpy(U+k_K,XtX+k_K,sizeof(F32)*(k+1));
		F32 sum=0.f;
		for (I32 i=0; i < k; i++)
		{
			newColPtr=U+k_K;
			F32   tmp=0.f;
			for (I32 j=0; j < i; j++)
			{
				tmp+=(*newColPtr++)* (*oldColPtr++);
			}
			*newColPtr=tmp=((*newColPtr) - tmp)/(*oldColPtr);
			sum=sum+tmp*tmp;
			oldColPtr=oldColPtr+(K - i);
		}
		newColPtr++;
		*newColPtr=sqrtf(*newColPtr - sum);
		k++;
	}
}
void chol_update_U(F32PTR U,F32PTR x,I32 ldu,I32 n) {
	F32PTR Ubase=U;
	for (I32 row=1; row <=n; row++) 	{
		U=Ubase+(row -1)* ldu+(row -1);
		F32 Ukk=(*U);		
		F32 r=sqrtf(Ukk*Ukk+(*x)*(*x));
		F32 c=r/Ukk,cinv=Ukk/r;
		F32 s=(*x)/Ukk;
		(*U)=r;
		for (I32 col=row+1; col <=n; col++)		{
				U=U+ldu;
				x=x+1;
				*U=(*U+s* (*x))*cinv;
				*x=c*(*x) - s*(*U);		
		}
		x=x - (n-1)+row; 
	}
}
void chol_dwdate_U(F32PTR U,F32PTR x,I32 ldu,I32 n) {
	F32PTR Ubase=U;
	for (I32 row=1; row <=n; row++) 	{
		U=Ubase+(row -1)* ldu+(row -1);
		F32 Ukk=(*U);		
		F32 r=sqrtf(Ukk*Ukk - (*x)*(*x));
		F32 c=r/Ukk,cinv=Ukk/r;
		F32 s=(*x)/Ukk;
		(*U)=r;
		for (I32 col=row+1; col <=n; col++)		{
				U=U+ldu;
				x=x+1;
				*U=(*U - s* (*x))*cinv;
				*x=c*(*x) - s*(*U);		
		}
		x=x - (n-1)+row; 
	}
}
void chol_update_L(F32PTR L,F32PTR x,I32 ldu,I32 n) {
	F32PTR Lbase=L;
	for (I32 col=1; col <=n;col++) 	{
		L=Lbase+(col -1)* ldu+(col -1);
		F32 Lkk=(*L);
		F32 r=sqrtf(Lkk* Lkk+(*x)*(*x));
		F32 c=r/Lkk,cinv=Lkk/r;
		F32 s=(*x)/Lkk;
		(*L)=r;
		for (I32 row=col+1; row<=n; row++)		{
				L=L+1;
				x=x+1;
				*L=(*L+s* (*x))*cinv;
				*x=c*(*x) - s*(*L);
		}
		x=x - (n-1)+col; 
	}
}
void chol_dwdate_L(F32PTR L,F32PTR x,I32 ldu,I32 n) {
	F32PTR Lbase=L;
	for (I32 col=1; col <=n;col++) 	{
		L=Lbase+(col -1)* ldu+(col -1);
		F32 Lkk=(*L);
		F32 r=sqrtf(Lkk* Lkk - (*x)*(*x));
		F32 c=r/Lkk,cinv=Lkk/r;
		F32 s=(*x)/Lkk;
		(*L)=r;
		for (I32 row=col+1; row<=n; row++)		{
				L=L+1;
				x=x+1;
				*L=(*L - s* (*x))*cinv;
				*x=c*(*x) - s*(*L);
		}
		x=x - (n-1)+col; 
	}
}
void chol_columwise(F32PTR A,F32PTR U,I64  N,I64 K)
{
	F32PTR A_base=A;
	F32PTR U_base=U;
	for (I64 COL=1; COL <=K; COL++) 	{
		A=A_base+(COL - 1)*N;
		U=U_base;
		F32PTR Ucol=U_base+(COL - 1) * N;
		F64    SUM_UxU=0.f;
		for (I64 col=1; col< COL; col++) 	{
			F64 sum=0.f;
			for (I32 row=1; row < col; row++) {
				sum+=(*U++)*(*Ucol++);
			}
			*Ucol=(A[col - 1] - sum)/(*U);
			SUM_UxU+=(*Ucol)*(*Ucol);
			Ucol=Ucol - (col - 1);  
			U=U - (col - 1)+N; 
		}
		Ucol[COL - 1]=sqrt( A[COL-1] - SUM_UxU);
	}
}
void chol_columwise_v2( F32PTR A,F32PTR U,I64  N,I64 K )
{   
	F32PTR A_base=A;
	F32PTR U_base=U;
	for (I32 COL=1; COL <=K;++COL) {			       
		U=U_base ;								       
		A=A_base+(COL - 1) * N ;				       
		F32PTR  Ucol=U_base+(COL - 1) * N; 
		F64		SUM_Ucol_x_Ucol=0;				
		for (I32 col=1; col < COL;++col) {       
			F64 sum=0.f;
			for (I32 row=1; row < col;++row)		
				sum+=U[row - 1] * Ucol[row - 1];
			F64 res=(A[col -1]-sum)/U[col -1];
			Ucol[col - 1]=res;
			SUM_Ucol_x_Ucol+=res * res;	
			U+=N;								 
		}
		Ucol[COL -1]=sqrt(A[COL -1]-SUM_Ucol_x_Ucol);
	}
}
void chol_rowwise( F32PTR A,F32PTR U,I64  N,I64 K ) { 
	F32PTR A_base=A;
	F32PTR U_base=U;
	for (I32 ROW=1; ROW <=K;++ROW) 	{
		U=U_base+(ROW-1)*N;         
		A=A_base+(ROW-1)*N ;        
		F64 sum=0.0; 	for (I32 row=1; row < ROW;++row) {sum+=U[row-1]* U[row-1]; }
		F32 Ukk=sqrt(A[ROW-1]-sum);				
		F32 Ukk_inv=1.f/Ukk;
		U[ROW - 1]=Ukk;
		F32PTR Ucurcol=U;			
		for (I32 col=ROW+1; col <=K;++col) {
			A=A+N;
			U=U+N;
			F64 sum=0.0;	for (I32 row=1; row < ROW;++row)	{sum+=U[row-1] * Ucurcol[row-1];}
			U[ROW-1]=( A[ROW-1]-sum)*Ukk_inv;			
		}
	}
}
void chol_addCol(F32PTR A,F32PTR U,I64 N,I64 K0,I64 K1)
{
	F32PTR Abase=A;
	F32PTR Ubase=U;
	for (I32 COL=K0; COL <=K1; COL++) 	{		
		A=Abase+(COL-K0)*N;		
		U=Ubase;
		F32PTR Ucol=Ubase+(COL - 1) * N;
		F64    SUM=0.f;
		for (I32 col=1; col< COL; col++)	{			
			F64 sum=0.f;
			for (I32 row=1; row < col; row++)	{sum+=(*U++)* (*Ucol++);}			
			F64 Uk=(A[col - 1] - sum)/(*U);
			*Ucol=Uk;
			SUM+=Uk * Uk;
			Ucol=Ucol - (col - 1);
			U=U - (col - 1)+N;
		}
		Ucol[COL - 1]=sqrt(A[COL - 1] - SUM);
	}
}
void inplace_chol(F32PTR A,I64  N,I64 K)
{
	F32PTR Abase=A;	
	for (I64 COL=1; COL <=K; COL++) 	{
		A=Abase+(COL-1)*N;		
		F32 Ukk_inv;
		{	F64 sum=0.f;
			for (I64 row=1; row < COL; row++) { sum+=A[row - 1] * A[row - 1]; };			
			F64  Ukk=sqrt(A[COL-1] - sum);			
			A[COL - 1]=Ukk;
			Ukk_inv=1.f/Ukk;
		}		
		F32PTR U_curCol_base=A;
		A=A+N;
		for (I64 col2=COL+1; col2 <=K; col2++) {
			F32PTR U_curCol=U_curCol_base;
			F32    sum=0.f;
			for (rI64 row=1; row < COL; row++) {	sum+=(*A++)*(*U_curCol++);	}
			*A=(*A - sum)* Ukk_inv;
			A=A - (COL - 1)+N;
		}
	}
}
void inplace_chol_addCol(F32PTR A,I64 N,I64 K0,I64 K1)
{	
	F32PTR Abase=A;
	for (I64 COL=K0; COL <=K1; COL++)
	{
		F32PTR Ucol=Abase+(COL - 1)*N;
		A=Abase;
		F32  SUM=0.f;
		for (I64 col=1; col< COL; col++) 	{
			F64 sum=0.f;
			for (I64 row=1; row < col; row++){	sum+=(*A++)*(*Ucol++);}
			F64  Uk=(*Ucol - sum)/(*A);
			*Ucol=Uk;
			SUM+=Uk * Uk;
			Ucol=Ucol - (col - 1);
			A=A - (col - 1)+N;
		}
		Ucol[COL - 1]=sqrt(Ucol[COL-1] - SUM);
	}
}
void solve_U_as_L(F32PTR A,F32PTR x,I64 lda,I64 K) {
	for (I64 col=1; col<=K; col++) 	{
		F64 sum=0.f;
		for (I64 row=1; row < col; row++)	{sum+=A[row-1]*x[row-1];}
		x[col-1]=(x[col-1] - sum)/A[col-1];			
		A=A+lda;
	}
}
void solve_U_as_U(F32PTR U,F32PTR x,I64 lda,I64 K)
{	
	x=x+(K-1);
	F32PTR UlastCol_end=U+(K-1)* lda+(K-1);
	for (I64 col=1; col <=K; col++) {
		U=UlastCol_end - (col - 1);
		F64 sum=0.f;
		for (I64 row=1; row < col; row++) {
			sum+=(*U)*(*x--);
			U   -=lda;
		}
		*x=(*x - sum)/(*U);
		 x=x+(col - 1);
	}
}
void solve_L_as_L(F32PTR A,F32PTR x,I64 lda,I64 K) {
	F32PTR Abase=A;
	for (I64 row=1; row<=K; row++) 	{
		A=Abase+row-1;
		F64 sum=0.f;
		for (I64 col=1; col < row;++col)	{
			sum+=(*A)*x[col-1];
			A+=lda;
		}
		x[row-1]=(x[row-1] - sum)/(*A);	
	}
}
void solve_L_as_U(F32PTR A,F32PTR x,I64 lda,I64 K) {
	x=x+(K-1);
	A=A+(K-1)* lda+(K-1); 
	for (I64 col=K; col >=1; col--) {		
		F64 sum=0.f;
		for (I64 row=K; row > col; row--) {
			sum+=(*A--)*(*x--);		 
		}
		*x=(*x - sum)/(*A);
		 x=x+(K-col);
		 A=A+(K - col) - lda;
	}
}
void pack_chol_update(F32PTR x,F32PTR  U,I64 K)
{
	for (I64 col=1; col <=K; col++)
	{
		F32 c,cinv,s;
		{
			rF32 Ukk=*U;
			rF32 r;
			*U=r=sqrtf(Ukk*Ukk+(*x)*(*x));
			s=(*x)/Ukk; 	c=r/Ukk;   cinv=Ukk/r;
		}
		U=U+col;
		F32PTR U_nextCol_diag=U+1;
		for (rI64 i=col+1; i <=K; i++)
		{			
			x++;
			*U=(*U+s*(*x))*cinv;
			*x=c*(*x) - s*(*U);
			U+=i;
		}
		x=x - (K - 1)+col;
		U=U_nextCol_diag;
	}
}
void pack_chol_dwdate(F32PTR x,F32PTR U,I64 K)
{
	for (I64 col=1; col <=K; col++)
	{
		F32 c,cinv,s;
		{
			rF32 Ukk=*U;
			rF32 r;
			*U=r=sqrtf(Ukk*Ukk - (*x)*(*x));
			s=(*x)/Ukk; 	c=r/Ukk;   cinv=Ukk/r;
		}
		U=U+col;
		F32PTR U_nextCol_diag=U+1;
		for (rI64 i=col+1; i <=K; i++)
		{
			x++;
			*U=(*U - s*(*x))*cinv;
			*x=c*(*x) - s*(*U);
			U+=i;
		}
		x=x - (K - 1)+col;
		U=U_nextCol_diag;
	}
}
void pack_chol(F32PTR Au,F32PTR U,I64  N)
{
	for (I64 COL=1; COL <=N ; COL++)
	{
		rF32 sum=0.f;
		for (rI64 row=1; row < COL; row++)
		{
			sum+=U[row-1]*U[row-1];
		}
		rF32 Ukk=sqrt( *Au - sum);
		U[COL-1]=Ukk;
		Ukk=1.f/Ukk;
		rF32PTR U_curCol_base=U;
		F32PTR Au_curCol_diagElem=Au;
		U=U_curCol_base+COL; 
		Au=Au_curCol_diagElem+COL ;
		for (rI64 col2=COL+1; col2 <=N; col2++)
		{
			rF32PTR U_curCol=U_curCol_base;
			sum=0.f;
			for (rI64 row=1; row < COL; row++)
			{
				sum+=(*U++)*(*U_curCol++);
			}
			*U=((*Au) - sum)*Ukk;
			U=U - (COL-1)+col2;
			Au=Au -(COL-1)+col2+(COL-1);
		}
		U=U_curCol_base+COL; 
		Au=Au_curCol_diagElem+COL+1;
	}
}
void pack_chol_addCol(F32PTR Au,F32PTR U,I64 K0,I64 K1)
{	
	F32PTR  U_base=U;
	rF32PTR U_curCol=U+(1+(K0-1))*(K0-1)/2 ;
	for (; K0 <=K1; K0++)
	{
		U=U_base;
		rF32  SUM=0.f;
		for (rI64 col=1; col< K0; col++)
		{
			rF32 sum=0.f;
			for (rI64 row=1; row < col; row++)
			{
				sum+=(*U++)*(*U_curCol++);
			}
			sum=( (*Au++) - sum)/(*U++);
			*U_curCol=sum;
			SUM=SUM+sum*sum;
			U_curCol=U_curCol - (col - 1);
		}
		U_curCol[K0 - 1]=sqrt((*Au++) - SUM);
		U_curCol=U_curCol+K0; 
	}
}
void pack_inplace_chol(F32PTR A,I64  N)
{
	for (I64 COL=1; COL <=N; COL++)
	{
		rF32 sum=0.f;
		for (rI64 row=1; row < COL; row++)
		{
			sum+=A[row - 1] * A[row - 1];
		}
		rF32 Ukk=sqrt(A[COL-1] - sum);
		A[COL-1]=Ukk;
		Ukk=1.f/Ukk;
		rF32PTR U_curCol_base=A;
		A=U_curCol_base+COL; 
		for (rI64 col2=COL+1; col2 <=N; col2++)
		{
			rF32PTR U_curCol=U_curCol_base;
			sum=0.f;
			for (rI64 row=1; row < COL; row++)
			{
				sum+=(*A++)*(*U_curCol++);
			}
			*A=(*A - sum)*Ukk;
			A=A - (COL - 1)+col2;			
		}
		A=U_curCol_base+COL; 
	}
}
void pack_inplace_chol_addCol(F32PTR A,I64 K0,I64 K1)
{	
	F32PTR  U_base=A;
	rF32PTR U_curCol=A+(1+(K0 - 1))*(K0 - 1)/2;
	for (; K0 <=K1; K0++)
	{
		A=U_base;
		rF32  SUM=0.f;
		for (rI64 col=1; col< K0; col++)
		{
			rF32 sum=0.f;
			for (rI64 row=1; row < col; row++)
			{
				sum+=(*A++) * (*U_curCol++);
			}
			sum=(*U_curCol - sum)/(*A++);
			*U_curCol=sum;
			SUM=SUM+sum*sum;
			U_curCol=U_curCol - (col - 1);
		}
		U_curCol[K0 - 1]=sqrt(U_curCol[K0 - 1] - SUM);
		U_curCol=U_curCol+K0; 
	}
}
void pack_solve_L(F32PTR A,F32PTR x,I64 K)
{	
		for (rI64 col=1; col<=K; col++)
		{
			rF32 sum=0.f;
			for (rI64 row=1; row < col; row++)
			{sum+=(*A++)*(*x++);}
			*x=(*x - sum)/(*A++);	 
			x=x - (col - 1);		
		}	
}
void pack_solve_U(F32PTR A,F32PTR x,I64 K)
{	
	rF32PTR A_lastCol_end=A+(K+1)*K/2L - 1L;
	x=x+(K - 1);
	for (rI64 col=1; col<=K; col++)
	{
		A=A_lastCol_end - (col - 1);
		rF32 sum=0.f;
		for (rI64 row=1; row < col; row++)
		{
			sum+=(*A)*(*x--);
			A=A - (K - row);
		}
		*x=(*x - sum)/(*A);
		x=x+(col - 1);
	}
}
void chol_addCol_skipleadingzeros(F32PTR Au,F32PTR U,I64 N,I64 K0,I64 K1) {
	F32PTR  Ubase=U;
	F32PTR  Ucol=Ubase+(K0-1) * N;
	for (I64 COL=K0; COL <=K1; COL++){
		I64 rIdxFirstNonZero=1;
		for (; Au[rIdxFirstNonZero-1]==0;  Ucol[rIdxFirstNonZero-1]=0,rIdxFirstNonZero++);
		U=Ubase+(rIdxFirstNonZero - 1) * N;
		F64  SUM=0.f;
		for (I64 col=rIdxFirstNonZero; col< COL; col++) 	{			
			F64 sum=0.f;
			for (I64 row=rIdxFirstNonZero; row < col; row++) {
				sum+=U[row-1]*Ucol[row-1];
			}
			F64  Ucol_curElem=(Au[col-1] - sum)/U[col - 1];
			Ucol[col-1]=Ucol_curElem;
			SUM+=Ucol_curElem * Ucol_curElem;
			U+=N;
		}
		Ucol[COL - 1]=sqrt( Au[COL-1] - SUM);
		Ucol+=N;
		Au+=N;
	}
}
void chol_addCol_skipleadingzeros_prec(F32PTR Au,F32PTR U,F32 precPrior,I64 N,I64 K0,I64 K1)
{
	F32PTR Ubase=U;
	F32PTR Ucol=Ubase+(K0-1) * N;
	for (I64 COL=K0; COL <=K1; COL++){
		I64 rIdxFirstNonZero=1;
		for (; Au[rIdxFirstNonZero-1]==0;  Ucol[rIdxFirstNonZero-1]=0,rIdxFirstNonZero++);
		U=Ubase+(rIdxFirstNonZero - 1) * N;
		F64  SUM=0.f;
		for (I64 col=rIdxFirstNonZero; col< COL; col++) 	{			
			F64 sum=0.f;for (I64 row=rIdxFirstNonZero; row < col; row++) {sum+=U[row-1]*Ucol[row-1];}
			F64  Ucol_curElem=(Au[col - 1] - sum)/U[col - 1]; 
			Ucol[col-1]=Ucol_curElem;
			SUM+=Ucol_curElem * Ucol_curElem;
			U+=N;
		}
		Ucol[COL - 1]=sqrt( (Au[COL-1]+precPrior)- SUM); 
		Ucol+=N;
		Au+=N;
	}
}
void chol_addCol_skipleadingzeros_prec_invdiag(F32PTR Au,F32PTR U,F32PTR precPrior,I64 N,I64 K0,I64 K1)
{
	F32PTR  Ubase=U;
	F32PTR  Ucol=Ubase+(K0-1) * N;
	for (I64 COL=K0; COL <=K1; COL++){
		I64 rIdxFirstNonZero=1;
		for (; Au[rIdxFirstNonZero-1]==0;  Ucol[rIdxFirstNonZero-1]=0,rIdxFirstNonZero++);
		U=Ubase+(rIdxFirstNonZero - 1) * N;
		F64  SUM=0.f;
		for (I64 col=rIdxFirstNonZero; col< COL; col++) 	{			
			F64 sum=0.f;
			for (I64 row=rIdxFirstNonZero; row < col; row++) {sum+=U[row-1]*Ucol[row-1];}
			F32  Ukk_invert=U[col - 1];
			F64  Ucol_curElem=(Au[col-1] - sum) * Ukk_invert; 
			Ucol[col-1]=Ucol_curElem;
			SUM+=Ucol_curElem * Ucol_curElem;
			U+=N;
		}
		Ucol[COL - 1]=1.f/sqrt( (Au[COL-1]+*precPrior)- SUM); 
		Ucol+=N;
		Au+=N;
	}
}
void chol_addCol_skipleadingzeros_precVec_invdiag(   F32PTR Au,F32PTR U,F32PTR precPrior,I64 N,I64 K0,I64 K1)
{
	F32PTR  Ubase=U;
	F32PTR  Ucol=Ubase+(K0-1) * N;
	for (I64 COL=K0; COL <=K1; COL++){
		I64 rIdxFirstNonZero=1;
		for (; Au[rIdxFirstNonZero-1]==0;  Ucol[rIdxFirstNonZero-1]=0,rIdxFirstNonZero++);
		U=Ubase+(rIdxFirstNonZero - 1) * N;
		F64  SUM=0.f;
		for (rI64 col=rIdxFirstNonZero; col< COL; col++) 
		{			
			F64 sum=0.f;	for (rI64 row=rIdxFirstNonZero; row < col; row++) {sum+=U[row-1]*Ucol[row-1];}
			F64  Ukk_invert=U[col - 1];
			F64  Ucol_curElem=(Au[col-1] - sum) * Ukk_invert; 
			Ucol[col-1]=Ucol_curElem;
			SUM+=Ucol_curElem * Ucol_curElem;
			U+=N;
		}
		Ucol[COL - 1]=1.f/sqrt( (Au[COL-1]+precPrior[COL-1])- SUM); 
		Ucol+=N;
		Au+=N;
	}
}
void solve_U_as_LU(F32PTR U,F32PTR y,F32PTR x,I64 N,I64 K) {	
	for (I64 col=1; col<=K; col++) {
		F64 sum=0.f;	
		for (I64 row=1; row < col; row++)	{sum+=U[row-1]*x[row-1];	}
		x[col-1]=(y[col-1] - sum)/U[col-1];			
		U+=N;
	}
	F32PTR U_lastCol_end=U-N+(K-1);
	for (I64 nCol=1; nCol <=K; nCol++) 	{
		U=U_lastCol_end - (nCol - 1);
		F64 sum=0.f;
		for (I64 col=K; col> (K- nCol)+1; col--) {
			sum+=(*U)*x[col-1];
			U   -=N;
		}
		x[K- nCol]=(x[K- nCol]-sum)/(*U);
	}
}
void solve_U_as_LU_rectmat_multicols(F32PTR U,F32PTR y,F32PTR x,I64 ldu,I64 K,I64 nCols) {	
	for (I32 I=1; I <=nCols;++I) {
		for (I64 col=1; col<=K;++col) {
			F32 sum=0.f;
			for (I64 row=1; row < col;++row)	{sum+=U[row-1]*x[row-1];}
			x[col-1]=( y[col-1]-sum )/U[col-1];	
			U+=ldu;
		}
		F32PTR U_LastCol_End=U- ldu+(K-1);
		for (I64 nCol=1; nCol <=K;++nCol)	{
			U=U_LastCol_End - (nCol-1);
			F32 sum=0.f;
			for (I64 col=K; col > (K- nCol)+1; --col) {
				sum+=(*U)*x[col-1];
				U   -=ldu;
			}
			x[K- nCol]=(x[K- nCol]-sum)/(*U);
		}	
		x+=K;
		y+=K;
	}
}
void solve_U_as_LU_invdiag_rectmat(F32PTR U,F32PTR y,F32PTR x,I64 ldu,I64 K) {	
	for (I64 col=1; col<=K;++col) {
		F32 sum=0.f;
		for (I64 row=1; row < col;++row)	{
			sum+=U[row-1]*x[row-1];
		}
		x[col-1]=( y[col-1]-sum ) * U[col-1];		 
		U+=ldu;
	}
	F32PTR U_LastCol_End=U- ldu+(K-1);
	for (I64 nCol=1; nCol <=K;++nCol)	{
		U=U_LastCol_End - (nCol-1);
		F32 sum=0.f;
		for (I64 col=K; col > (K- nCol)+1; --col) {
			sum+=(*U)*x[col-1];
			U   -=ldu;
		}
		x[K- nCol]=(x[K- nCol]-sum) * (*U);  	 
	}
}
void solve_U_as_LU_invdiag_sqrmat(F32PTR U,F32PTR y,F32PTR x,I64 K) {	
	for (I64 col=1; col<=K;++col) {
		F32 sum=0.f;
		for (I64 row=1; row < col;++row)	{
			sum+=U[row-1]*x[row-1];
		}
		x[col-1]=(y[col-1] - sum) * U[col-1];		 
		U+=K;
	}
	F32PTR U_LastCol_End=U-K+(K-1);
	for (I64 nCol=1; nCol <=K;++nCol) {
		U=U_LastCol_End - (nCol-1);
		F32 sum=0.f;
		for (I64 col=K; col> (K- nCol)+1; --col) {
			sum+=(*U)*x[col-1];
			U   -=K;
		}
		x[K- nCol]=(x[K- nCol]-sum) * (*U);  	 
	}
}
void solve_U_as_LU_invdiag_sqrmat_multicols(F32PTR U,F32PTR y,F32PTR x,I64 K,I64 nColY) {	
	for (int I=0; I < nColY;++I) {
		for (I64 col=1; col <=K;++col) {
			F64 sum=0.f;
			for (I64 row=1; row < col;++row) {sum+=U[row - 1] * x[row - 1];}
			x[col - 1]=(y[col - 1] - sum) * U[col - 1];		 
			U+=K;
		}
		F32PTR U_LastCol_End=U - K+(K - 1);
		for (I64 nCol=1; nCol <=K;++nCol) {
			U=U_LastCol_End - (nCol - 1);
			F64 sum=0.f;
			for (I64 col=K; col > (K - nCol)+1; --col) {
				sum+=(*U) * x[col - 1];
				U   -=K;
			}
			x[K - nCol]=(x[K - nCol] - sum) * (*U);  	 
		}
		x+=K;
		y+=K;
	}
}
void solve_U_as_U_invdiag(F32PTR U,F32PTR x,I64 ldu,I64 K) 
{	
	F32PTR U_lastCol_end=U+(K-1)* ldu+(K-1);
	for (I64 nCol=1; nCol <=K; nCol++) 	{
		U=U_lastCol_end - (nCol - 1);
		F32 sum=0.f;
		for (I64 col=K; col> (K- nCol)+1; col--) {
			sum+=(*U)*x[col-1];
			U   -=ldu;
		}
		x[K- nCol]=(x[K- nCol]-sum) * (*U);  
	}
}
void solve_U_as_U_invdiag_multicols(F32PTR U,F32PTR x,I64 ldu,I64 K,I32 nColx)
{	
	for (I32 I=0; I < nColx;++I) {
		F32PTR U_lastCol_end=U+(K - 1) * ldu+(K - 1);
		for (I64 nCol=1; nCol <=K;++nCol) {
			U=U_lastCol_end - (nCol - 1);
			F64 sum=0.f;
			for (I64 col=K; col > (K - nCol)+1; --col) {
				sum+=(*U) * x[col - 1];
				U -=ldu;
			}
			x[K - nCol]=(x[K - nCol] - sum) * (*U);  
		}
		x+=K;
	}
}
#include "abc_blas_lapack_lib.h"
void linear_regression(F32PTR Y,F32PTR X,int ldx,int N,int K,F32PTR B,F32PTR Yfit,F32PTR Yerror,F32PTR TMP) {
	r_cblas_sgemv(CblasColMajor,CblasTrans,N,K,1.f,X,ldx,Y,1L,0.f,B,1L);
	F32PTR XtX=TMP;
	r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,K,K,N,1.0f,X,ldx,X,ldx,0.,XtX,K);
	r_LAPACKE_spotrf(LAPACK_COL_MAJOR,'U',K,XtX,K);
	r_LAPACKE_spotrs(LAPACK_COL_MAJOR,'U',K,1L,XtX,K,B,K);
	r_cblas_sgemv(CblasColMajor,CblasNoTrans,N,K,1.f,X,ldx,B,1L,0.f,Yfit,1L);
	if (Yerror)
		r_ippsSub_32f(Yfit,Y,Yerror,N); 
}
#include "abc_000_warning.h"
