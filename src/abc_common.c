#include <math.h>
#include <string.h>
#include "abc_000_warning.h"
#include "abc_common.h"
#include "abc_001_config.h"
#include "abc_sort.h"
#include "abc_vec.h" 
#include "abc_blas_lapack_lib.h"
void ToUpper(char* s) { for (int i=0; s[i] !='\0'; i++) 	s[i]=s[i] >='a' && s[i] <='z' ? s[i] - 32 : s[i]; }
I32 strcicmp(char const * _restrict a,char const * _restrict b) {
	for (;; a++,b++) {
		I32 d=((*a)|(U08)32) - ((*b)|(U08)32);
		if (d !=0||!*a)	
			return d;
	}
}
F32 DeterminePeriod(F32PTR Y,I32 N)
{
	F32PTR TMP=(F32PTR)malloc(sizeof(F32)*N * 6);
	F32PTR X=TMP;        
	F32PTR Yfit=TMP+N*4;    
	U08PTR isNA=(U08PTR)(Yfit+N);   
	F32    delta=1.f/N;
	f32_fill_val(1.0,X,N);        
	f32_seq(X+N,0.0,delta,N);  
	memcpy( X+2*N,X+N,sizeof(F32) * N); f32_pow_vec_inplace(X+2 * N,2,N);
	memcpy( X+3*N,X+N,sizeof(F32) * N); f32_pow_vec_inplace(X+3 * N,3,N);
	memset(isNA,0,sizeof(char)*N); 
	for ( I32 i=0; i < N; i++) 	{
		if (Y[i] !=Y[i]  ) 		{
			isNA[i]=1L;
			X[i]=0.f;
			X[N+i]=0.f;
			X[N+N+i]=0.f;
			X[N+N+N+i]=0.f;
			Y[i]=0.f;
		}
	}
	{
		F32 XtX_tmp[16];
		F32 B[4];		
		linear_regression(Y,X,N,N,4L,B,Yfit,Y,XtX_tmp);
	}
	I32    M=(int)   ceil(N/2);
	F32PTR ans=TMP;       
	for (I32 i=1; i <=M; i++){
		I32 nLength=N - i;
		I32 start=i+1;
		F32 sumXY=0,sumXX=0,sumYY=0,sumX=0,sumY=0;
		I32 nValidNum=0;
		for (I32 j=1; j <=nLength; j++)		{
			I32 Ix=j - 1;
			I32 Iy=start+(j - 1) - 1;
			if ( (isNA[Ix]+isNA[Iy])==0)	{
				nValidNum++;
				F32 x=Y[Ix];
				F32 y=Y[Iy];
				sumX+=x;	
				sumY+=y;
				sumXY+=x*y; 				
				sumXX+=x*x;
				sumYY+=y*y;
			}
		}
		F32 MX=sumX/(F32)nValidNum;
		F32 MY=sumY/(F32)nValidNum;
		ans[i - 1]=(sumXY/nValidNum - MX*MY)/sqrtf((sumXX/N - MX*MX)*(sumYY/N - MY*MY));
	}
	U08PTR isPeak=isNA;
	I32PTR INDEX=(I32PTR)(TMP+M); 
	memset(isNA,0,M);
	I32  numPeaks=0;
	for ( I32 i=2; i <=(M - 1); i++)	{
		if (ans[(i)-1] > ans[(i - 1) - 1] && ans[(i)-1] > ans[(i+1) - 1]) {
			isPeak[i - 1]=1;
			INDEX[numPeaks++]=i;
		}
	}
	I32 period=-1L;	 
	if (numPeaks !=0) 	{
		for (I32 pID=1; pID <=max(1,(int)floorf(numPeaks/3.f)); pID++)	{
			period=INDEX[pID - 1];			
			I32  NumOfPeriod=(int)floorf((F32)(M - 1)/period);
			I32  goodTimes=0;
			for (I32 i=1; i <=NumOfPeriod; i++)	{			 
				I32 segLength=period * i;
				goodTimes+=isPeak[segLength - 1]||isPeak[segLength+1 - 1]||isPeak[segLength-1 - 1];
			}
			if (goodTimes >=min(3,NumOfPeriod))			{
				break;
			}
			period=-1L;
		}
	}
	free(TMP);
	return (F32)period;
}
 static F32 confidenceInterval(F32PTR half,I32 n,char leftOrRight)
{
	F32 sum=f32_sum(half,n);
	if (leftOrRight=='R') {		
		F32 cumSum=0;		
		I32 j=0;
		for (; j < n; j++)	{
			cumSum+=half[j];
			if ( cumSum/sum >=0.95f) break;
		}
		F32 J=j+1.f;
		return J - (cumSum - 0.95f*sum)/half[j];
	} 
	if (leftOrRight=='L')
	{		
		F32 cumSum=0;		
		I32 j=n-1;
		for (; j >=0; j--) 	{
			cumSum+=half[j];
			if (cumSum/sum >=0.95f) break;
		}
		F32 J=(F32)(n-j);
		F32 delta=J - (cumSum - 0.95f*sum)/half[j];
		return delta;
	}
	return -999;
}
static I32 find_changepoint_v0(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber)
{
	if (maxCptNumber==0)	{return maxCptNumber;}
	I32 w=(I32) round((minSepDist - 1)/2);
	w=w >=0 ? w : 0;
	I32 w2=w * 2+1;
	r_ippsSet_32f(0,mem,N);			
	I32PTR cpfromSum_Pos=(I32PTR)mem+N;
	F32PTR cpfromSum_Val=(F32PTR)mem+N * 2;
	I32PTR cpfromProb_Pos=(I32PTR) mem+N * 3;
	F32PTR cpfromProb_Val=(F32PTR)mem+N * 4;
	for (I32 i=-w; i <=w; i++)
	{
		I32 len=i > 0 ? i : -i;
		I32 startIdx_mem=i <=0 ? 0 : i;
		I32 startIdx_prob=i <=0 ? -i : 0;
		r_ippsAdd_32f_I(prob+startIdx_prob,mem+startIdx_mem,N - len);
	}
	I32  UPPERIDX=N - w;
	I32  numCpt=0;
	for (I32 i=w; i < UPPERIDX; i++)
	{
		if (mem[i] < threshold) continue;
		bool isLargeThanNearestNeighor=(mem[i] >=mem[i - 1]) && (mem[i] >=mem[i+1]);
		bool isLargeThanNearestTwoNeighors=(mem[i] * 4.0) > (mem[i+1]+mem[i+2]+mem[i - 1]+mem[i - 2]);
		if (!(isLargeThanNearestNeighor && isLargeThanNearestTwoNeighors)) continue;
		I32		upperIdx_1=i+w;
		I32		maxIdx=-999;
		F32		maxVal=-999;
		for (I32 j=i - w; j <=upperIdx_1; j++)
		{
			if ((prob[j] > prob[j - 1] && prob[j] >=prob[j+1])||(prob[j] >=prob[j - 1] && prob[j] > prob[j+1]))
			{
				if (prob[j] > maxVal) 	maxIdx=j,maxVal=prob[j];
			}			
		}		
		if ( maxVal < 0.f	)	continue;
		I32 diff_btw_twoNeighbors=maxIdx-cpfromProb_Pos[numCpt - 1]; 
		if ((numCpt==0)||diff_btw_twoNeighbors >=w2||diff_btw_twoNeighbors <=-w2)
		{
			cpfromSum_Pos[numCpt]=i;
			cpfromSum_Val[numCpt]=mem[i];
			cpfromProb_Pos[numCpt]=maxIdx;
			cpfromProb_Val[numCpt]=maxVal;
			numCpt++;
			continue;
		}
		else
		{
			if (maxVal >=cpfromProb_Val[numCpt - 1])
			{
				cpfromSum_Pos[numCpt - 1]=i;
				cpfromSum_Val[numCpt - 1]=mem[i];
				cpfromProb_Pos[numCpt - 1]=maxIdx;
				cpfromProb_Val[numCpt - 1]=maxVal;
				continue;
			}
		}		
	}
	if (numCpt==0) { return numCpt; }
	QuickSortD(cpfromProb_Val,cpfromProb_Pos,0,numCpt - 1);
	numCpt=min(numCpt,maxCptNumber);
	r_cblas_scopy(numCpt,(F32PTR)cpfromProb_Pos,1,(F32PTR) cpt,1);
	I32PTR INDEX=(I32PTR) mem;
	F32PTR CPT_float=mem+N;
	for (I32 i=0; i < numCpt; i++)
	{
		*INDEX++=i;
		*CPT_float++=(F32)cpt[i];
	}
	INDEX=INDEX - numCpt;
	CPT_float=CPT_float - numCpt;	
	QuickSortA(CPT_float,INDEX,0,numCpt - 1);
	for (I32 i=0; i < numCpt; i++)
	{
		cptCI[i]=-9999.f;
		cptCI[numCpt+i]=-9999.f;
	}
	F32 delta;
	delta=confidenceInterval(prob,((I32) CPT_float[0]-0+1),'L');
	cptCI[0]=delta;
	delta=confidenceInterval(prob+(I32)CPT_float[numCpt - 1],(N - (I32)CPT_float[numCpt - 1]+1),'R');
	cptCI[numCpt+numCpt - 1]=delta;
	if (numCpt==1) {
		cptCI[0]=CPT_float[0] - cptCI[0];
		cptCI[1]=CPT_float[0]+cptCI[1];
		return numCpt; 
	}
	for (I32 i=0; i < (numCpt-1); i++)
	{ 
		F32 del1,del2,del;
		del1=cptCI[numCpt+i] > 0 ? cptCI[numCpt+i] : cptCI[i];
		del2=cptCI[i+1] > 0 ? cptCI[i+1] : ((cptCI[numCpt+i+1] > 0) ? cptCI[numCpt+i+1] : -9999.f);
    	del=CPT_float[i+1] - CPT_float[i];
		if (del2 <=0)
		{
				del1=del1 * 2.f;
				del=(del1 > del) ? del/2 : del1;
		}else
		{
			del=del*del1/(del1+del2);
		}
		delta=confidenceInterval(prob+(I32)CPT_float[i],(I32) ceil(del),'R');
		cptCI[numCpt+i]=delta;
		del=CPT_float[i+1] - CPT_float[i];
		if (del2 <=0)
		{
			delta=del - delta * 2;
			del=delta <=0 ? del/2 : delta;
		}
		else
		{
			del2=del2 * 2.f;
			del=(del2 >=del) ? del/2 : del2;
		}
		I32 len=(I32)ceil(del);
		delta=confidenceInterval(prob+(I32)CPT_float[i+1]-(len-1),len,'L');
		cptCI[i+1]=delta;
	}
	F32PTR temp=mem+2 * N;
	r_cblas_scopy(2*numCpt,cptCI,1,temp,1);
	for (I32 i=0; i < numCpt; i++)
	{
		I32 idx  ;
		idx=INDEX[i];
		cptCI[idx]=CPT_float[i] - temp[i];
		cptCI[numCpt+idx]=CPT_float[i]+temp[numCpt+i];
	}
	return numCpt;
}
 I32 FindChangepoint(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber)
{
	if (maxCptNumber==0)	{ return maxCptNumber; }
	r_ippsSet_32f(0,mem,N);
	I32PTR cpfromSum_Pos=(I32PTR) mem+N;
	F32PTR cpfromSum_Val=(F32PTR) mem+N * 2;
	I32PTR cpfromProb_Pos=(I32PTR) mem+N * 3;
	F32PTR cpfromProb_Val=(F32PTR) mem+N * 4;
	I32 w0=minSepDist/2;   
	I32 w1=minSepDist - w0;  
	f32_sumfilter(prob,mem,N,minSepDist);
	I32  LOWERIDX=(minSepDist+1);
	I32  UPPERIDX=N - (minSepDist+1);
	I32  numCpt=0;
	for (I32 i=LOWERIDX; i < UPPERIDX; i++)
	{
		if (mem[i] < threshold) continue;
		bool isLargeThanNearestNeighor=(mem[i] >=mem[i - 1]) && (mem[i] >=mem[i+1]);
		bool isLargeThanNearestTwoNeighors=(mem[i] * 4.0) > (mem[i+1]+mem[i+2]+mem[i - 1]+mem[i - 2]);
		if ( isLargeThanNearestNeighor==0||isLargeThanNearestTwoNeighors==0 ) continue;
		I32		UPPERIDX_1=i+w1;
		I32		maxIdx=-9999;
		F32		maxVal=-9999.f;
		for (I32 j=i - w0; j <=UPPERIDX_1; j++) 	{
			if ((prob[j] > prob[j - 1] && prob[j] >=prob[j+1])||(prob[j] >=prob[j - 1] && prob[j] > prob[j+1]))			{
				if (prob[j] > maxVal) {
					maxIdx=j; maxVal=prob[j];
				}				
			}
		}
		if (maxVal < 0.f)	continue;
		I32 dist_to_prevCpt=maxIdx - cpfromProb_Pos[numCpt - 1];
		if ((numCpt==0)||dist_to_prevCpt > minSepDist||dist_to_prevCpt < -minSepDist)	{
			cpfromSum_Pos[numCpt]=i;
			cpfromSum_Val[numCpt]=mem[i];
			cpfromProb_Pos[numCpt]=maxIdx;
			cpfromProb_Val[numCpt]=maxVal;
			numCpt++;
			continue;
		} else	{  
			if (maxVal >=cpfromProb_Val[numCpt - 1]){
				cpfromSum_Pos[numCpt - 1]=i;
				cpfromSum_Val[numCpt - 1]=mem[i];
				cpfromProb_Pos[numCpt - 1]=maxIdx;
				cpfromProb_Val[numCpt - 1]=maxVal;
				continue;
			}
		}
	}
	if (numCpt==0) { return numCpt; }
	QuickSortD(cpfromSum_Val,cpfromProb_Pos,0,numCpt - 1);
	numCpt=min(numCpt,maxCptNumber);
	f32_copy( (F32PTR)cpfromProb_Pos,(F32PTR)cpt,numCpt);	
	I32PTR INDEX_timeToProbAmp=(I32 *)mem ;
	F32PTR cpt_f32=(F32 *)mem+N;	
	for (I32 i=0; i < numCpt; i++) {		
		cpt_f32[i]=(F32)cpt[i];
		INDEX_timeToProbAmp[i]=i;
	}
	QuickSortA(cpt_f32,INDEX_timeToProbAmp,0,numCpt - 1);
	f32_fill_val(-9999.f,cptCI,2*numCpt);
	F32PTR tmpSeg=(F32*) mem+3 * N;
	I32PTR nullSeg=(I32*) mem+4 * N;  
	for (I32 i=0; i < numCpt; i++)
	{
		I32 startIdx,endIdx,len;
		endIdx=(I32) cpt_f32[i];
		startIdx=i==0 ? 0 : (I32) cpt_f32[i-1];
		startIdx=(startIdx+endIdx)/2;
		len=endIdx-startIdx+1;		
		f32_copy(prob+startIdx,tmpSeg,len);		
		QuickSortA(tmpSeg,nullSeg,0,len - 1); 
		cptCI[i]=confidenceInterval(tmpSeg,len,'L');
		startIdx=(I32)cpt_f32[i];
		endIdx=i==(numCpt - 1) ? (N - 1) : (I32)cpt_f32[i+1];
		endIdx=(startIdx+endIdx)/2;
	    len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);		
		QuickSortD(tmpSeg,nullSeg,0,len - 1); 
		cptCI[numCpt+i]=confidenceInterval(tmpSeg,len,'R');
 	}
	F32PTR cptCI_backup=mem+3*N;
	f32_copy(cptCI,cptCI_backup,2 * numCpt);
	F32PTR cpt_summedProb=mem;
	for (I32 i=0; i < numCpt; i++)	{
		I32 idx=INDEX_timeToProbAmp[i];
		cptCI[idx]=cpt_f32[i] - cptCI_backup[i];
		cptCI[numCpt+idx]=cpt_f32[i]+cptCI_backup[numCpt+i];
		cpt_summedProb[i]=cpfromSum_Val[i]>1 ? 1.f : cpfromSum_Val[i];
	}
	return numCpt;
}
void WriteF32ArraryToStrideMEM(F32PTR src,VOID_PTR dst,I64 N,I64 stride,I64 dstOffset,DATA_TYPE dataType) 
{
	if ( dataType==DATA_FLOAT  )	{	  
		f32_to_strided_f32(src,dst,N,stride,dstOffset);
	}
	else if (dataType==DATA_DOUBLE) {
		f32_to_strided_f64(src,dst,N,stride,dstOffset);
	}
}
void CopyStrideMEMToF32Arr(F32PTR dst,VOID_PTR src,int N,int srcStride,int srcOffset,DATA_TYPE srcDataType)
{
	if      (srcDataType==DATA_FLOAT) 	{
		f32_from_strided_f32(dst,src,N,srcStride,srcOffset);
	}  
	else if (srcDataType==DATA_DOUBLE){
		f32_from_strided_f64(dst,src,N,srcStride,srcOffset);
	}  
	else if (srcDataType==DATA_INT32)	{
		f32_from_strided_i32(dst,src,N,srcStride,srcOffset);
	}  
	else if (srcDataType==DATA_INT16)	{
		f32_from_strided_i16(dst,src,N,srcStride,srcOffset);
	}  
}
#if defined(WIN64_OS)||defined(WIN32_OS) 
	#include "float.h"
	#if defined(MSVC_COMPILER)
	void EnableFloatExcetion() {
		unsigned int _oldState;
		errno_t err=_controlfp_s(&_oldState,EM_OVERFLOW|EM_UNDERFLOW|EM_ZERODIVIDE|EM_DENORMAL|EM_INVALID,MCW_EM);
	}
	#elif defined(GCC_COMPILER)
void EnableFloatExcetion() {
		unsigned int _oldState;
		errno_t err=_controlfp_s(&_oldState,_EM_OVERFLOW|_EM_UNDERFLOW|_EM_ZERODIVIDE|_EM_DENORMAL|_EM_INVALID,_MCW_EM);
	}
	#endif
#else
	#include "fenv.h" 
void EnableFloatExcetion() {
	#if defined(LINUX_OS) 
	feenableexcept(FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW); 
	#endif
}
#endif
#include "abc_000_warning.h"
