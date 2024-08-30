#include <math.h>
#include <string.h>
#include "abc_000_warning.h"
#include "abc_common.h"
#include "abc_001_config.h"
#include "abc_sort.h"
#include "abc_vec.h" 
#include "abc_blas_lapack_lib.h"
#include "abc_rand_pcg_local.h"
static INLINE int isSpace(char c) {return (c==' '||c=='\t'||c=='\n');}
int get_word(char* str) {
	int i=0,wordlen=0;
	while (isSpace(str[i])) {
		str[i]=' ';
		wordlen++;
		i++;
	}
	while (str[i] && !isSpace(str[i++])) { wordlen++;}
	return wordlen;}
char* word_wrap(char* str,int LINE_LENGTH) {
	int wordLen;                     
	int current_lineLen=0;             
	int start_curLen=0;              
	while ((wordLen=get_word(str+start_curLen+current_lineLen)) > 0) {
		if (current_lineLen+wordLen < LINE_LENGTH) {
			current_lineLen+=wordLen;
		} else {
			str[start_curLen+current_lineLen]='\n';
			start_curLen+=current_lineLen+1L;
			current_lineLen=0L;
		}
	}
	return str;
}
char* word_wrap_indented(char* str,int LINE_LENGTH,int nspace) {
	char* space="                      ";
	char newstr[2000];
	int  newlen=0;
	int  isfirstline=1;
	int wordLen;                     
	int current_lineLen=0;             
	int start_curLen=0;              
	while ((wordLen=get_word(str+start_curLen+current_lineLen)) > 0) {
		if (current_lineLen+wordLen < LINE_LENGTH) {
			current_lineLen+=wordLen;
		}
		else {
			str[start_curLen+current_lineLen]='\n';
			if (isfirstline) {
				isfirstline=0;
				memcpy(newstr+newlen,str+start_curLen,current_lineLen);
				newlen+=current_lineLen;
				newstr[newlen++]='\n';
				LINE_LENGTH -=nspace;
			}
			else {
				memcpy(newstr+newlen,space,nspace);
				newlen+=nspace;
				memcpy(newstr+newlen,str+start_curLen,current_lineLen);
				newlen+=current_lineLen;
				newstr[newlen++]='\n';
			}
			start_curLen+=current_lineLen+1L;
			current_lineLen=0;
		}
	}
	if (isfirstline) {
		memcpy(newstr+newlen,str+start_curLen,current_lineLen);
	} else {
		memcpy(newstr+newlen,space,nspace);
		newlen+=nspace;
		memcpy(newstr+newlen,str+start_curLen,current_lineLen);
	}
	newlen+=current_lineLen;
	if (newstr[newlen] !='\n') {
		newstr[newlen++]='\n';
	}
	newstr[newlen++]=0;
	strcpy(str,newstr);
	return str;
}
void ToUpper(char* s) { for (int i=0; s[i] !='\0'; i++) 	s[i]=s[i] >='a' && s[i] <='z' ? s[i] - 32 : s[i]; }
I32 strcicmp(char const * _restrict a,char const * _restrict b) {
	for (;; a++,b++) {
		I32 d=((*a)|(U08)32) - ((*b)|(U08)32);
		if (d !=0||!*a)	
			return d;
	}
}
I32 strcicmp_nfirst(char const* _restrict a,char const* _restrict b,int nfirst) {
	if (nfirst==0) {
		nfirst=(int) strlen(a)+1;
	}
	int i=0;
	for (;; a++,b++) {
		I32 d=((*a)|(U08)32) - ((*b)|(U08)32);
		i++;
		if (d !=0||!*a||i >=nfirst) {
			return d;
		}		
	}
}
I32 strmatch(char const* _restrict full,char const* _restrict part) {
	for (;; full++,part++) {
		I32 d=((*full)|(U08)32) - ((*part)|(U08)32);
		if (d !=0||!*part) {
			return (!*part)? 0: d;
		}
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
	memset(isNA,0,(size_t)(uint32_t)M);
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
		Bool isLargeThanNearestNeighor=(mem[i] >=mem[i - 1]) && (mem[i] >=mem[i+1]);
		Bool isLargeThanNearestTwoNeighors=(mem[i] * 4.0) > (mem[i+1]+mem[i+2]+mem[i - 1]+mem[i - 2]);
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
	f32d_introSort_index(cpfromProb_Val,0,numCpt - 1,cpfromProb_Pos);
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
	f32a_introSort_index(CPT_float,0,numCpt - 1,INDEX);
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
 I32 FindChangepointv1(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber)
{
	if (maxCptNumber==0)	{ return maxCptNumber; }
	r_ippsSet_32f(0,mem,N);
	I32PTR cpfromSumP_Pos=(I32PTR) mem+N;
	F32PTR cpfromSumP_Val=(F32PTR) mem+N * 2;
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
		Bool isLargeThanNearestNeighor=(mem[i] >=mem[i - 1]) && (mem[i] >=mem[i+1]);
		Bool isLargeThanNearestTwoNeighors=(mem[i] * 4.0) > (mem[i+1]+mem[i+2]+mem[i - 1]+mem[i - 2]);
		if ( isLargeThanNearestNeighor==0||isLargeThanNearestTwoNeighors==0 ) continue;
		I32		UPPERIDX_1=i+w1;
		I32		maxIdx=-9999;
		F32		maxVal=-9999.f;
		for (I32 j=i - w0; j <=UPPERIDX_1; j++) 	{
			if ((prob[j] > prob[j - 1] && prob[j] >=prob[j+1])||(prob[j] >=prob[j - 1] && prob[j] > prob[j+1]))			{
				if (prob[j] > maxVal) {
					maxIdx=j;
					maxVal=prob[j];
				}				
			}
		}
		if (maxVal <=0.f)	continue;
		I32 dist_to_prevCpt=maxIdx - cpfromProb_Pos[numCpt - 1];
		if ((numCpt==0)||dist_to_prevCpt > minSepDist||dist_to_prevCpt < -minSepDist)	{
			cpfromSumP_Pos[numCpt]=i;
			cpfromSumP_Val[numCpt]=mem[i];
			cpfromProb_Pos[numCpt]=maxIdx;
			cpfromProb_Val[numCpt]=maxVal;
			numCpt++;
			continue;
		} else	{  
			if (mem[i] >=cpfromSumP_Val[numCpt - 1]) {
				cpfromSumP_Pos[numCpt - 1]=i;
				cpfromSumP_Val[numCpt - 1]=mem[i];
				cpfromProb_Pos[numCpt - 1]=maxIdx;
				cpfromProb_Val[numCpt - 1]=maxVal;
				continue;
			}
		}
	}
	if (numCpt==0) { return numCpt; }
	f32d_introSort_index(cpfromSumP_Val,0,numCpt - 1,cpfromProb_Pos);
	numCpt=min(numCpt,maxCptNumber);
	f32_copy( (F32PTR)cpfromProb_Pos,(F32PTR)cpt,numCpt);	
	I32PTR INDEX_timeToProbAmp=(I32 *)mem ;
	F32PTR cpt_f32=(F32 *)mem+N;	
	for (I32 i=0; i < numCpt; i++) {		
		cpt_f32[i]=(F32)cpt[i];
		INDEX_timeToProbAmp[i]=i;
	}
	f32a_introSort_index(cpt_f32,0,numCpt - 1,INDEX_timeToProbAmp);
	f32_fill_val(-9999.f,cptCI,2*numCpt);
	F32PTR tmpSeg=(F32*) mem+3 * N;
	for (I32 i=0; i < numCpt; i++)
	{
		I32 startIdx,endIdx,len;
		endIdx=(I32) cpt_f32[i];
		startIdx=i==0 ? 0 : (I32) cpt_f32[i-1];
		startIdx=(startIdx+endIdx)/2;
		len=endIdx-startIdx+1;		
		f32_copy(prob+startIdx,tmpSeg,len);		
		f32a_introSort(tmpSeg,0,len - 1);  
		cptCI[i]=confidenceInterval(tmpSeg,len,'L');
		startIdx=(I32)cpt_f32[i];
		endIdx=i==(numCpt - 1) ? (N - 1) : (I32)cpt_f32[i+1];
		endIdx=(startIdx+endIdx)/2;
	    len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);		
		f32d_introSort(tmpSeg,0,len - 1); 
		cptCI[numCpt+i]=confidenceInterval(tmpSeg,len,'R');
 	}
	F32PTR cptCI_backup=mem+3*N;
	f32_copy(cptCI,cptCI_backup,2 * numCpt);
	F32PTR cpt_summedProb=mem;
	for (I32 i=0; i < numCpt; i++)	{
		I32 idx=INDEX_timeToProbAmp[i];
		cptCI[idx]=cpt_f32[i] - cptCI_backup[i];
		cptCI[numCpt+idx]=cpt_f32[i]+cptCI_backup[numCpt+i];
		cpt_summedProb[i]=cpfromSumP_Val[i]>1 ? 1.f : cpfromSumP_Val[i];
	}
	return numCpt;
}
static I32 FindChangepoint_minseg_is_1(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber) {
	F32PTR sump=mem;
	I32PTR cpfromSumP_Pos=(I32PTR)mem+N;
	F32PTR cpfromSumP_Val=(F32PTR)mem+N * 2;
	I32PTR cpfromProb_Pos=(I32PTR)mem+N * 3;
	F32PTR cpfromProb_Val=(F32PTR)mem+N * 4;
	r_ippsSet_32f(0,sump,N);        
	I32 minSegLen=minSepDist+1;
	I32 w0=minSegLen/2;       
	I32 w1=(minSegLen - w0)-1;  
	f32_sumfilter(prob,sump,N,minSegLen);
	I32  LOWERIDX=0;       
	I32  UPPERIDX=N - 1;   
	I32  numCpt=0;
	for (I32 i=LOWERIDX; i < UPPERIDX; i++) 	{
		if (sump[i] < threshold) continue;
		cpfromSumP_Pos[numCpt]=i;
		cpfromSumP_Val[numCpt]=sump[i];
		numCpt++;	 
	}
	for (I32 i=0; i < numCpt; i++) {
		I32     cpt=cpfromSumP_Pos[i];		
		I32     LOWERIDX=cpt-w0;
		I32		UPPERIDX=cpt+w1;
		cpfromProb_Pos[i]=cpt;
		cpfromProb_Val[i]=prob[cpt];
	}
	if (numCpt==0) { return numCpt; }
	f32d_introSort_index(cpfromSumP_Val,0,numCpt - 1,cpfromProb_Pos);
	numCpt=min(numCpt,maxCptNumber);
	f32_copy((F32PTR)cpfromProb_Pos,(F32PTR)cpt,numCpt);
	I32PTR INDEX_timeToProbAmp=(I32*)mem;
	F32PTR cpt_f32=(F32*)mem+N;
	for (I32 i=0; i < numCpt; i++) {
		cpt_f32[i]=(F32)cpt[i];
		INDEX_timeToProbAmp[i]=i;
	}
	f32a_introSort_index(cpt_f32,0,numCpt - 1,INDEX_timeToProbAmp);
	F32PTR cpt_summedProb=mem;
	for (I32 i=0; i < numCpt; i++) {
		I32 idx=INDEX_timeToProbAmp[i];
		cptCI[idx]=cpt_f32[i] - 0;
		cptCI[numCpt+idx]=cpt_f32[i]+0;
		cpt_summedProb[i]=cpfromSumP_Val[i] > 1 ? 1.f : cpfromSumP_Val[i];
	}
	return numCpt;
}
I32 FindChangepoint(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber)
{
	if (maxCptNumber==0) { return maxCptNumber; }
	if (minSepDist==0) {
		return FindChangepoint_minseg_is_1(prob,mem,threshold,cpt,cptCI,N,minSepDist,maxCptNumber);
	}
	F32PTR sump=mem;
	I32PTR cpfromSumP_Pos=(I32PTR)mem+N;
	F32PTR cpfromSumP_Val=(F32PTR)mem+N * 2;
	I32PTR cpfromProb_Pos=(I32PTR)mem+N * 3;
	F32PTR cpfromProb_Val=(F32PTR)mem+N * 4;
	r_ippsSet_32f(0,sump,N);        
	I32 minSegLen=minSepDist+1;
	I32 w0=minSegLen/2;       
	I32 w1=(minSegLen - w0)-1;  
	f32_sumfilter(prob,sump,N,minSegLen);
	I32  LOWERIDX=(minSepDist+1);
	I32  UPPERIDX=N - (minSepDist+1);
	I32  numCpt=0;
	for (I32 i=LOWERIDX; i < UPPERIDX; i++) 	{
		if (sump[i] < threshold) continue;
		Bool isLargeThanNearestNeighor=(sump[i] > sump[i - 1] && sump[i] >=sump[i+1])||(sump[i] >=sump[i - 1] && sump[i] >  sump[i+1]);
		Bool isLargeThanNearestTwoNeighors=(sump[i] * 4.0) >=(sump[i+1]+sump[i+2]+sump[i - 1]+sump[i - 2]);
		if (isLargeThanNearestNeighor==0||(minSegLen>4 && isLargeThanNearestTwoNeighors==0)  ) continue;
		I32 dist_to_prevCpt=i - cpfromSumP_Pos[numCpt - 1]; 
		if ((numCpt==0)||dist_to_prevCpt > minSepDist||dist_to_prevCpt < -minSepDist) {
			cpfromSumP_Pos[numCpt]=i;
			cpfromSumP_Val[numCpt]=sump[i];
			numCpt++;
			continue;
		} else {
			if (sump[i] > cpfromSumP_Val[numCpt - 1]) { 
				cpfromSumP_Pos[numCpt - 1]=i;
				cpfromSumP_Val[numCpt - 1]=sump[i];
				continue;
			}
		}
	}
	if (minSepDist <=4) {
		for (I32 i=0; i < numCpt; i++) {
			I32     cpt=cpfromSumP_Pos[i];
			I32     LOWERIDX=cpt - w0;
			I32		UPPERIDX=cpt+w1;
			I32		maxIdx=cpt;
			F32		maxVal=prob[cpt];
			for (I32 j=LOWERIDX; j <=UPPERIDX; j++) {
				F32 curProb=prob[j];
				if ((curProb > prob[j - 1] && curProb >=prob[j+1])||(curProb >=prob[j - 1] && curProb > prob[j+1])) {
					if (curProb > maxVal) {
						maxIdx=j;
						maxVal=curProb;
					}
				}
			}
			cpfromProb_Pos[i]=maxIdx;
			cpfromProb_Val[i]=maxVal;
		}
	} else {
		for (I32 i=0; i < numCpt; i++) {
			I32     cpt=cpfromSumP_Pos[i];
			I32     LOWERIDX=cpt - w0;
			I32		UPPERIDX=cpt+w1;
			I32 topTenPeaksLoc[10];
			F32 topTenPeaksPrb[10];
			I32 numTopTenCpt=0;
			for (I32 j=LOWERIDX; j <=UPPERIDX; j++) {
				F32 curProb=prob[j];
				if ((curProb > prob[j - 1] && curProb >=prob[j+1])||(curProb >=prob[j - 1] && curProb > prob[j+1])) {
					if (numTopTenCpt < 10) {
						topTenPeaksLoc[numTopTenCpt]=j;
						topTenPeaksPrb[numTopTenCpt]=curProb;
						numTopTenCpt++;
					}	else {
						F32 minProbInList=100.f;
						int minProbListIndex=0;
						for (int k=0; k < 10; k++) {
							if (minProbInList > topTenPeaksPrb[k]) {
								minProbListIndex=k;
								minProbInList=topTenPeaksPrb[k];
							}
						}
						if (curProb > minProbInList) {
							topTenPeaksLoc[minProbListIndex]=j;
							topTenPeaksPrb[minProbListIndex]=curProb;
						}
					}					
				} 
			} 
			if (numTopTenCpt==0) {
				cpfromProb_Pos[i]=cpt;
				cpfromProb_Val[i]=prob[cpt];
			}
			else if (numTopTenCpt==1) {
				cpfromProb_Pos[i]=topTenPeaksLoc[0];
				cpfromProb_Val[i]=topTenPeaksPrb[0];
			}
			else {
				f32d_introSort_index(topTenPeaksPrb,0,numTopTenCpt - 1,topTenPeaksLoc);
				if (topTenPeaksPrb[0] > 1.5 * topTenPeaksPrb[1]) {
					cpfromProb_Pos[i]=topTenPeaksLoc[0];
					cpfromProb_Val[i]=topTenPeaksPrb[0];
				}
				else {
					int bestK=0;
					F32 bestProb_3Neighors=-9999;
					for (int k=0; k < numTopTenCpt; k++) {
						int cpLoc=topTenPeaksLoc[k];
						F32 cpProb_3Neighors=prob[cpLoc]+prob[cpLoc+1]+prob[cpLoc - 1];
						if (cpProb_3Neighors > bestProb_3Neighors) {
							bestProb_3Neighors=cpProb_3Neighors;
							bestK=k;
						}
					}
					cpfromProb_Pos[i]=topTenPeaksLoc[bestK];
					cpfromProb_Val[i]=topTenPeaksPrb[bestK];
				}
			} 
		}
	}
	if (numCpt==0) { return numCpt; }
	f32d_introSort_index(cpfromSumP_Val,0,numCpt - 1,cpfromProb_Pos);
	numCpt=min(numCpt,maxCptNumber);
	f32_copy((F32PTR)cpfromProb_Pos,(F32PTR)cpt,numCpt);
	I32PTR INDEX_timeToProbAmp=(I32*)mem;
	F32PTR cpt_f32=(F32*)mem+N;
	for (I32 i=0; i < numCpt; i++) {
		cpt_f32[i]=(F32)cpt[i];
		INDEX_timeToProbAmp[i]=i;
	}
	f32a_introSort_index(cpt_f32,0,numCpt - 1,INDEX_timeToProbAmp);
	f32_fill_val(-9999.f,cptCI,2 * numCpt);
	F32PTR tmpSeg=(F32*)mem+3 * N;
	for (I32 i=0; i < numCpt; i++) 	{
		I32 startIdx,endIdx,len;
		endIdx=(I32)cpt_f32[i];
		startIdx=i==0 ? 0 : (I32)cpt_f32[i - 1];
		startIdx=(startIdx+endIdx)/2;
		len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);		
		f32a_introSort(tmpSeg,0,len - 1);             
		cptCI[i]=confidenceInterval(tmpSeg,len,'L');		
		startIdx=(I32)cpt_f32[i];
		endIdx=i==(numCpt - 1) ? (N - 1) : (I32)cpt_f32[i+1];
		endIdx=(startIdx+endIdx)/2;
		len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);
		f32d_introSort(tmpSeg,0,len - 1);       
		cptCI[numCpt+i]=confidenceInterval(tmpSeg,len,'R');
	}
	F32PTR cptCI_backup=mem+3 * N;
	f32_copy(cptCI,cptCI_backup,2 * numCpt);
	F32PTR cpt_summedProb=mem;
	for (I32 i=0; i < numCpt; i++) {
		I32 idx=INDEX_timeToProbAmp[i];
		cptCI[idx]=cpt_f32[i] - cptCI_backup[i];
		cptCI[numCpt+idx]=cpt_f32[i]+cptCI_backup[numCpt+i];
		cpt_summedProb[i]=cpfromSumP_Val[i] > 1 ? 1.f : cpfromSumP_Val[i];
	}
	return numCpt;
}
I32 FindChangepoint_LeftRightMargins(F32PTR prob,F32PTR mem,F32 threshold,I32PTR cpt,F32PTR cptCI,I32 N,I32 minSepDist,I32 maxCptNumber,I32 leftMargin,I32 rightMargin)
{
	if (maxCptNumber==0) { return maxCptNumber; }
	I32 minSegLen=minSepDist+1;
	if (minSepDist==0) { return FindChangepoint_minseg_is_1(prob,mem,threshold,cpt,cptCI,N,minSepDist,maxCptNumber);}
	F32PTR sump=mem;
	I32PTR cpfromSumP_Pos=(I32PTR)mem+N;
	F32PTR cpfromSumP_Val=(F32PTR)mem+N * 2;
	I32PTR cpfromProb_Pos=(I32PTR)mem+N * 3;
	F32PTR cpfromProb_Val=(F32PTR)mem+N * 4;
	I32 w0=minSegLen/2;         
	I32 w1=(minSegLen - w0) - 1;  
	f32_sumfilter(prob,sump,N,minSegLen);  
	sump[0]=0; 
   #define IsLocalPeak(j,p)      (  ( p[j]>p[j-1] && p[j]>=p[j+1] )||( p[j]>=p[j-1]&&p[j] > p[j+1]   ) )
   #define IsPeak_4Neigbor(j,p)  (  p[j]>=p[j-1] && p[j]>=p[j+1] && p[j]>=p[j-2] && p[j] >=p[j+2]   )
	I32  LOWERIDX=0+leftMargin+1; 
	I32  UPPERIDX=(N-1) - rightMargin;    
	I32  numCpt=0;
	for (I32 i=LOWERIDX; i <=UPPERIDX; i++) {
		if (sump[i] < threshold) continue;
		Bool hasRightNeighor=(i < N-1);
		Bool hasTwoNeighors=i > 1 && (i < N- 2);
		Bool isLargeThanNearestNeighor=IsLocalPeak(i,sump);
		Bool isLargeThanNearestTwoNeighors=IsPeak_4Neigbor(i,sump);
		Bool isLargeThanLeftNeighor=(sump[i] > sump[i - 1]);  
		if ( ( hasRightNeighor  && isLargeThanNearestNeighor==0 )||
			 ( minSegLen > 4    && hasTwoNeighors && isLargeThanNearestTwoNeighors==0)||
			 ( !hasRightNeighor && isLargeThanLeftNeighor==0) )
			continue;
		I32 dist_to_prevCpt=i - cpfromSumP_Pos[numCpt - 1]; 
		if ( (numCpt==0)||dist_to_prevCpt > minSepDist||dist_to_prevCpt < -minSepDist) {
			cpfromSumP_Pos[numCpt]=i;
			cpfromSumP_Val[numCpt]=sump[i];
			numCpt++;			
		} else {
			if (sump[i] > cpfromSumP_Val[numCpt - 1]) { 
				cpfromSumP_Pos[numCpt - 1]=i;
				cpfromSumP_Val[numCpt - 1]=sump[i];				
			}
		}
	}
	if (minSepDist <=4) {
		for (I32 i=0; i < numCpt; i++) {
			I32     cpt=cpfromSumP_Pos[i];
			I32     LOWERIDX=max(cpt - w0,1  );  
			I32		UPPERIDX=min(cpt+w1,N-1);  
			I32		maxIdx=cpt;
			F32		maxVal=prob[cpt];
			for (I32 j=LOWERIDX; j <=UPPERIDX; j++) {		 
				if (prob[j] > maxVal) {
						maxIdx=j;
						maxVal=prob[j];
				}
			}
			cpfromProb_Pos[i]=maxIdx;
			cpfromProb_Val[i]=maxVal;
		}
	}
	else {
		for (I32 i=0; i < numCpt; i++) {
			I32     cpt=cpfromSumP_Pos[i];
			I32     LOWERIDX=max(cpt - w0,1);     
			I32		UPPERIDX=min(cpt+w1,N - 1); 
			I32 topTenPeaksLoc[15];
			F32 topTenPeaksPrb[15];
			I32 numTopTenCpt=0;
			for (I32 j=LOWERIDX; j <=UPPERIDX; j++) {
				F32 curProb=prob[j];
				Bool hasRightNeighbor=(j < N - 1);
				if (  (hasRightNeighbor&&IsLocalPeak(j,prob) )||( !hasRightNeighbor && curProb>prob[j-1])    )
				{
					if (numTopTenCpt < 15) {
						topTenPeaksLoc[numTopTenCpt]=j;
						topTenPeaksPrb[numTopTenCpt]=curProb;
						numTopTenCpt++;
					} else {
						F32 minProbInList=100.f;
						int minProbListIndex=0;
						for (int k=0; k < 15; k++) {
							if (minProbInList > topTenPeaksPrb[k]) {
								minProbListIndex=k;
								minProbInList=topTenPeaksPrb[k];
							}
						}
						if (curProb > minProbInList) {
							topTenPeaksLoc[minProbListIndex]=j;
							topTenPeaksPrb[minProbListIndex]=curProb;
						}
					}
				} 
			} 
			if (numTopTenCpt==0) {
				cpfromProb_Pos[i]=cpt;
				cpfromProb_Val[i]=prob[cpt];
			} else if (numTopTenCpt==1) {
				cpfromProb_Pos[i]=topTenPeaksLoc[0];
				cpfromProb_Val[i]=topTenPeaksPrb[0];
			} else {
				f32d_introSort_index(topTenPeaksPrb,0,numTopTenCpt - 1,topTenPeaksLoc);
				if (topTenPeaksPrb[0] > 1.5 * topTenPeaksPrb[1]) {
					cpfromProb_Pos[i]=topTenPeaksLoc[0];
					cpfromProb_Val[i]=topTenPeaksPrb[0];
				} else {
					int bestK=0;
					F32 bestProb_3Neighors=-9999;
					for (int k=0; k < numTopTenCpt; k++) {
						int cpLoc=topTenPeaksLoc[k];
						F32 cpProb_3Neighors=cpLoc==N-1? (prob[cpLoc]+prob[cpLoc - 1]): (prob[cpLoc]+prob[cpLoc+1]+prob[cpLoc - 1]);
						if (cpProb_3Neighors > bestProb_3Neighors) {
							bestProb_3Neighors=cpProb_3Neighors;
							bestK=k;
						}
					}
					cpfromProb_Pos[i]=topTenPeaksLoc[bestK];
					cpfromProb_Val[i]=topTenPeaksPrb[bestK];
				}
			} 
		}
	}
	if (numCpt==0) { return numCpt; }
	f32d_introSort_index(cpfromSumP_Val,0,numCpt - 1,cpfromProb_Pos);
	numCpt=min(numCpt,maxCptNumber);
	f32_copy((F32PTR)cpfromProb_Pos,(F32PTR)cpt,numCpt);
	I32PTR INDEX_timeToProbAmp=(I32*)mem;
	F32PTR cpt_f32=(F32*)mem+N;
	for (I32 i=0; i < numCpt; i++) {
		cpt_f32[i]=(F32)cpt[i];
		INDEX_timeToProbAmp[i]=i;
	}
	f32a_introSort_index(cpt_f32,0,numCpt - 1,INDEX_timeToProbAmp);
	f32_fill_val(-9999.f,cptCI,2 * numCpt);
	F32PTR tmpSeg=(F32*)mem+3 * N;
	for (I32 i=0; i < numCpt; i++) {
		I32 startIdx,endIdx,len;
		endIdx=(I32)cpt_f32[i];
		startIdx=i==0 ? 0 : (I32)cpt_f32[i - 1];
		startIdx=(startIdx+endIdx)/2;
		len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);
		f32a_introSort(tmpSeg,0,len - 1);     
		cptCI[i]=confidenceInterval(tmpSeg,len,'L');
		startIdx=(I32)cpt_f32[i];
		endIdx=i==(numCpt - 1) ? (N - 1) : (I32)cpt_f32[i+1];
		endIdx=(startIdx+endIdx)/2;
		len=endIdx - startIdx+1;
		f32_copy(prob+startIdx,tmpSeg,len);
		f32d_introSort(tmpSeg,0,len - 1);  
		cptCI[numCpt+i]=confidenceInterval(tmpSeg,len,'R');
	}
	F32PTR cptCI_backup=mem+3 * N;
	f32_copy(cptCI,cptCI_backup,2 * numCpt);
	F32PTR cpt_summedProb=mem;
	for (I32 i=0; i < numCpt; i++) {
		I32 idx=INDEX_timeToProbAmp[i];
		cptCI[idx]=cpt_f32[i] - cptCI_backup[i];
		cptCI[numCpt+idx]=cpt_f32[i]+cptCI_backup[numCpt+i];
		cpt_summedProb[i]=min(1.0,cpfromSumP_Val[i]);
	}
	return numCpt;
}
#if defined(OS_WIN64)||defined(OS_WIN32) 
	#include "float.h"
	#if defined(COMPILER_MSVC)
	void EnableFloatExcetion(void) {
		unsigned int _oldState;
		errno_t err=_controlfp_s(&_oldState,EM_OVERFLOW|EM_UNDERFLOW|EM_ZERODIVIDE|EM_DENORMAL|EM_INVALID,MCW_EM);
	}
	#elif defined(COMPILER_GCC)
void EnableFloatExcetion(void) {
		unsigned int _oldState;
		errno_t err=_controlfp_s(&_oldState,_EM_OVERFLOW|_EM_UNDERFLOW|_EM_ZERODIVIDE|_EM_DENORMAL|_EM_INVALID,_MCW_EM);
	}
	#endif
#else
	#include "fenv.h" 
void EnableFloatExcetion(void) {
	#if defined(OS_LINUX) 
	#endif
}
#endif
#include "abc_cpu.h"
#include "abc_ide_util.h"
int GetNativeCPUType(void) {
	static int GLOBAL_CPU_NATIVE=0;
	if (GLOBAL_CPU_NATIVE) {
		return GLOBAL_CPU_NATIVE;
	}
	 struct cpu_x86   cpuinfo;
	 struct cpu_cache caches[8];
	 cpuinfo_detect(&cpuinfo,NULL);
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
	if (cpuinfo.HW_AVX512_F  && cpuinfo.HW_AVX512_BW && cpuinfo.HW_AVX512_DQ && cpuinfo.HW_AVX512_VL) {
		GLOBAL_CPU_NATIVE=3;
	}	else if (cpuinfo.HW_AVX &&cpuinfo.HW_AVX2 && cpuinfo.HW_FMA3) {
		GLOBAL_CPU_NATIVE=2;
	}	else {
		GLOBAL_CPU_NATIVE=1;
	}
#else
	 GLOBAL_CPU_NATIVE=1;
#endif
	 return GLOBAL_CPU_NATIVE;
}
void SetupRoutines_ByCPU(int cputype) {
#if !defined(COMPILER_SOLARIS) && defined(TARGET_64) && !defined(cpu_ARM64)
	if (cputype==2) {
		SetupVectorFunction_AVX2();
		SetupPCG_AVX2();
	}
	else if (cputype==3) {
		SetupVectorFunction_AVX512();
		SetupPCG_AVX512();
	} else {
		SetupVectorFunction_Generic();
		SetupPCG_GENERIC();	 
	} 
#else
	SetupVectorFunction_Generic();
	SetupPCG_GENERIC();
#endif
}
  const char *getBuild( void ) {
        #if defined(__x86_64__)||defined(_M_X64)
        return "x86_64";
        #elif defined(i386)||defined(__i386__)||defined(__i386)||defined(_M_IX86)
        return "x86_32";
        #elif defined(__ARM_ARCH_2__)
        return "ARM2";
        #elif defined(__ARM_ARCH_3__)||defined(__ARM_ARCH_3M__)
        return "ARM3";
        #elif defined(__ARM_ARCH_4T__)||defined(__TARGET_ARM_4T)
        return "ARM4T";
        #elif defined(__ARM_ARCH_5_)||defined(__ARM_ARCH_5E_)
        return "ARM5"
        #elif defined(__ARM_ARCH_6T2_)||defined(__ARM_ARCH_6T2_)
        return "ARM6T2";
        #elif defined(__ARM_ARCH_6__)||defined(__ARM_ARCH_6J__)||defined(__ARM_ARCH_6K__)||defined(__ARM_ARCH_6Z__)||defined(__ARM_ARCH_6ZK__)
        return "ARM6";
        #elif defined(__ARM_ARCH_7__)||defined(__ARM_ARCH_7A__)||defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
        return "ARM7";
        #elif defined(__ARM_ARCH_7A__)||defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
        return "ARM7A";
        #elif defined(__ARM_ARCH_7R__)||defined(__ARM_ARCH_7M__)||defined(__ARM_ARCH_7S__)
        return "ARM7R";
        #elif defined(__ARM_ARCH_7M__)
        return "ARM7M";
        #elif defined(__ARM_ARCH_7S__)
        return "ARM7S";
        #elif defined(__aarch64__)||defined(_M_ARM64)
        return "ARM64";
        #elif defined(mips)||defined(__mips__)||defined(__mips)
        return "MIPS";
        #elif defined(__sh__)
        return "SUPERH";
        #elif defined(__powerpc)||defined(__powerpc__)||defined(__powerpc64__)||defined(__POWERPC__)||defined(__ppc__)||defined(__PPC__)||defined(_ARCH_PPC)
        return "POWERPC";
        #elif defined(__PPC64__)||defined(__ppc64__)||defined(_ARCH_PPC64)
        return "POWERPC64";
        #elif defined(__sparc__)||defined(__sparc)
        return "SPARC";
        #elif defined(__m68k__)
        return "M68K";
        #else
        return "UNKNOWN";
        #endif
 }
#include "abc_000_warning.h"
