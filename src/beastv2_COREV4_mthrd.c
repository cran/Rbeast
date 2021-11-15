#include "abc_000_macro.h"
#include "abc_000_warning.h"
#if defined(MSVC_COMPILER)
#include "intrin.h"                
#endif
#include <stdio.h>	               
#include <string.h>	               
#include <time.h>
#include <math.h>
#include "abc_001_config.h"
#include "abc_mem.h"              
#include "abc_blas_lapack_lib.h"  
#include "abc_ide_util.h"
#include "abc_ts_func.h"
#include "abc_timer.h"
#include "abc_mcmc.h"
#include "abc_mat.h"
#include "abc_rand.h"
#include "abc_vec.h"   
#include "abc_math.h"  
#include "globalvars.h"  
#include "beastv2_header.h"
#include "beastv2_func.h" 
#include "beastv2_model_allocinit.h" 
#include "beastv2_prior_precfunc.h" 
#include "beastv2_xxyy_allocmem.h" 
#include "beastv2_io.h" 
#define LOCAL(...) do{ __VA_ARGS__ } while(0);
int beast2_main_corev4_mthrd(void* dummy)
{
	MemPointers MEM=(MemPointers){.init=mem_init,};
	MEM.init(&MEM);
	VSLStreamStatePtr stream;
	const BEAST2_OPTIONS_PTR	opt=GLOBAL_OPTIONS;
	const BEAST2_EXTRA          extra=opt->extra;
	typedef int QINT;
	const   QINT  q=1L;
	CI_PARAM     ciParam={0,};
	CI_RESULT    ci[MAX_NUM_BASIS];
	if (extra.computeCredible) {
		ConstructCIStruct(	opt->mcmc.credIntervalAlphaLevel,opt->mcmc.samples,opt->io.N*opt->io.q,  
							opt->prior.numBasis,&MEM,&extra.fastCIComputation,&ciParam,ci );  
	}
	BEAST2_MODEL  MODEL={0,};
	AllocInitModelMEM(&MODEL,opt,&MEM);
	LOCAL( 	
		U64 seed=(opt->mcmc.seed==0) ? TimerGetTickCount() : (opt->mcmc.seed+0x4f352a3dc);
		r_vslNewStream(&stream,VSL_BRNG_MT19937,seed);   	
	)
	const U32PTR  RND32=MyALLOC(MEM,MAX_RAND_NUM,U32,64);
	const U16PTR  RND16=MyALLOC(MEM,MAX_RAND_NUM * 2,U16,64);
	const U08PTR  RND08=MyALLOC(MEM,MAX_RAND_NUM * 4,U08,64);	
	const F32PTR  RNDGAMMA=MyALLOC(MEM,MAX_RAND_NUM,F32,64);	
	const U32PTR  RND32_END=RND32+MAX_RAND_NUM - 7;
	const U16PTR  RND16_END=RND16+MAX_RAND_NUM * 2 - 7;
	const U08PTR  RND08_END=RND08+MAX_RAND_NUM * 4 - 7 -3;     
	const F32PTR  RNDGAMMA_END=RNDGAMMA+MAX_RAND_NUM - MODEL.nPrec-1L;
	const F32PTR Xt_mars;
	const F32PTR Xnewterm;      
	const F32PTR Xt_zeroBackup; 
	AllocateXXXMEM(&Xt_mars,&Xnewterm,&Xt_zeroBackup,&MODEL,opt,&MEM);
	BEAST2_YINFO     yInfo;
	AllocateYinfoMEM(&yInfo,opt,&MEM);
	BEAST2_RESULT resultChain={ NULL,},result={ NULL,};
	BEAST2_Result_AllocMEM(&resultChain,opt,&MEM); 	
	BEAST2_Result_AllocMEM(&result,opt,&MEM);
	if (extra.computeCredible) {
		I32  Npad=(opt->io.N+7)/8 * 8;
		I32  XnewOffset=0;
		for (I32 i=0; i < MODEL.NUMBASIS; i++) {
			if (MODEL.b[i].type==SEASONID||MODEL.b[i].type==DUMMYID||MODEL.b[i].type==SVDID)
				ci[i].result=resultChain.sCI,		
			    ci[i].newDataRow=Xnewterm+XnewOffset;			
			else if (MODEL.b[i].type==TRENDID)
				ci[i].result=resultChain.tCI,     
			    ci[i].newDataRow=Xnewterm+XnewOffset;   
			else if (MODEL.b[i].type==OUTLIERID)
				ci[i].result=resultChain.oCI,      
			    ci[i].newDataRow=Xnewterm+XnewOffset; 
			XnewOffset+=Npad * q;   
		}
	} 
	const CORESULT coreResults[MAX_NUM_BASIS];
	SetupPointersForCoreResults(coreResults,MODEL.b,MODEL.NUMBASIS,&resultChain);
	const BEAST2_HyperPar  hyperPar={ .alpha_1=opt->prior.alpha1,.alpha_2=opt->prior.alpha2,
										.del_1=opt->prior.delta1,.del_2=opt->prior.delta2};
	InitTimerFunc();
	StartTimer();
	SetBreakPointForStartedTimer();
	const PREC_FUNCS precFunc;
	SetUpPrecFunctions(opt->prior.precPriorType,opt->io.q,&precFunc);
	if (extra.printProgressBar) {
		F32 frac=0.0; I32 firstTimeRun=1;
		printProgress2(frac,0,extra.consoleWidth,Xnewterm,firstTimeRun);
	}
	#undef  __DEBUG__ 
	#ifdef __DEBUG__
		I32    N=opt->io.N;
		I32    Npad=(N+7)/8 * 8;
		F32PTR flagSat=MyALLOC0(MEM,N,I32,64);
		F32PTR Xdebug=MyALLOC0(MEM,Npad*(opt->prior.K_MAX+opt->prior.K_MAX),I32,64); 
	#endif
	const U32  NUM_PIXELS=opt->io.numOfPixels;
	const U32  MCMC_SAMPLES=opt->mcmc.samples;
	const U32  MCMC_THINNING=opt->mcmc.thinningFactor;
	const U32  MCMC_BURNIN=opt->mcmc.burnin;
	const U32  MCMC_CHAINNUM=opt->mcmc.chainNumber;
	for (U32 pixelIndex=1; pixelIndex <=NUM_PIXELS; pixelIndex++)
	{
		pthread_mutex_lock(&mutex);
		pixelIndex=NEXT_PIXEL_INDEX++;
		if (pixelIndex > NUM_PIXELS) 	{
			pthread_mutex_unlock(&mutex);
			continue;
		}
		pthread_mutex_unlock(&mutex);
		F32PTR MEMBUF=Xnewterm; 
		BEAST2_fetch_next_timeSeries(&yInfo,pixelIndex,MEMBUF,&(opt->io));
		F32PTR  Xtmp=Xt_mars;
		U08     skipCurrentPixel=BEAST2_preprocess_timeSeries(&yInfo,MODEL.b,Xtmp,opt);		
		#ifdef __DEBUG__
			I32 accS[5]={ 0,0,0,0,0 },accT[5]={ 0,0,0,0,0 };
			I32 flagS[5]={ 0,0,0,0,0 },flagT[5]={ 0,0,0,0,0 };
			for (int i=0; i < yInfo.nMissing; i++) { flagSat[yInfo.rowsMissing[i]]=getNaN();}
		#endif
		#define __START_IF_NOT_SKIP_TIMESESIRIES__    
		#define __END_IF_NOT_SKIP_TIMESESIRIES__                        
		__START_IF_NOT_SKIP_TIMESESIRIES__  
		if (!skipCurrentPixel) {
		if (q==1) {
				yInfo.YtY_plus_alpha2Q[0]=yInfo.YtY_plus_alpha2Q[0]+2 *hyperPar.alpha_2;
				yInfo.alpha1_star=yInfo.n * 0.5+hyperPar.alpha_1;
		}	else {
				f32_add_val_matrixdiag(yInfo.YtY_plus_alpha2Q,hyperPar.alpha_2,q);
				yInfo.alpha1_star=yInfo.n * 0.5+(hyperPar.alpha_1+q - 1) * 0.5;
		}
		BEAST2_RNDSTREAM  RND;
		{
			RND.rnd32=RND32,RND.rnd16=RND16,RND.rnd08=RND08,RND.rndgamma=RNDGAMMA;
			r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,MAX_RAND_NUM,(U32PTR)RND32);
			r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,MAX_RAND_NUM,(U32PTR)RND16);
			r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,MAX_RAND_NUM,(U32PTR)RND08);
			r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,stream,MAX_RAND_NUM,RNDGAMMA,( hyperPar.alpha_1+yInfo.n * 0.5f),0,1);
		}
		BEAST2_Result_FillMEM(&result,opt,0);		
		for ( U32 chainNumber=0;  chainNumber < MCMC_CHAINNUM; chainNumber++)
		{
			const I32  N=opt->io.N;
			const I32  Npad=(N+7)/8 * 8;
			const I32  Npad16=(N+15)/16 * 16;	
			{   
				GenarateRandomBasis(MODEL.b,MODEL.NUMBASIS,N,&RND);
				if (q > 1) {
					MODEL.b[0].numKnot=0;
					MODEL.b[0].KNOT[0]=N+1;
					MODEL.b[0].ORDER[0]=3;
					MODEL.b[0].CalcBasisKsKeK_TermType(&MODEL.b[0]);
					MODEL.b[1].numKnot=0;
					MODEL.b[1].KNOT[0]=N+1;
					MODEL.b[1].ORDER[0]=1;
					MODEL.b[1].CalcBasisKsKeK_TermType(&MODEL.b[1]);
				}
				MODEL.b[0].Kbase=0;                           
				UpdateBasisKbase(MODEL.b,MODEL.NUMBASIS,0);	
				precFunc.GetNumTermsPerPrecGrp(&MODEL); 
				precFunc.GetXtXPrecDiag(&MODEL);        
				CvtKnotsToBinVec(MODEL.b,MODEL.NUMBASIS,N,&yInfo);
				if (q==1) {
					BEAST2_EvaluateModel(&MODEL.curr,MODEL.b,Xt_mars,N,MODEL.NUMBASIS,&yInfo,&hyperPar,opt->prior.precValue,&stream); 
				} else 	{
					MR_EvaluateModel(    &MODEL.curr,MODEL.b,Xt_mars,N,MODEL.NUMBASIS,&yInfo,&hyperPar,opt->prior.precValue,&stream);
				}
			}
			{
				BEAST2_Result_FillMEM(&resultChain,opt,0);
				memset(MODEL.extremePosVec,1,N);
				for (I32 i=0; i < yInfo.nMissing;++i) MODEL.extremePosVec[yInfo.rowsMissing[i]]=0;
				MODEL.extremPosNum=yInfo.n;
			}
			U32 ite=0;
			U32 sample=0;
			U32 subSampleIndex=0;
			PROP_DATA PROPINFO={.N=N,.Npad16=Npad16,.samples=&sample,
				                  .keyresult=coreResults,.mem=Xnewterm,.model=&MODEL, 
				                  .pRND=&RND,.sigFactor=1.8,.yInfo=&yInfo,
			                      .nSample_ExtremVecNeedUpdate=1L,}; 
			I32 numBadIterations=0;
			while (sample < MCMC_SAMPLES)
			{
				ite++;
				if (RND.rnd32    >=RND32_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,(RND.rnd32 - RND32),(U32PTR)RND32);	                               RND.rnd32=RND32;   }
				if (RND.rnd16    >=RND16_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,((char*)RND.rnd16 - (char*)RND16+3)/sizeof(U32),(U32PTR)RND16); RND.rnd16=RND16;   }
				if (RND.rnd08    >=RND08_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,((char*)RND.rnd08 - (char*)RND08+3)/sizeof(U32),(U32PTR)RND08); RND.rnd08=RND08;   }
				if (RND.rndgamma >=RNDGAMMA_END) {r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,stream,MAX_RAND_NUM,RNDGAMMA,(hyperPar.alpha_1+yInfo.n*0.5f),0.f,1.f);    RND.rndgamma=RNDGAMMA;}
				BEAST2_BASIS_PTR basis=MODEL.b+MODEL.PickBasisID(&PROPINFO); 
				NEWTERM    NEW;
				basis->Propose(basis,&NEW,&PROPINFO); 
				#ifdef __DEBUG__
					I32 basisIdx=basis - MODEL.b;
					if (basisIdx==0 && (NEW.flagMoveType==BIRTH||NEW.flagMoveType==MOVE)) {
						flagSat[NEW.newKnot - 1]+=1;
					}
				#endif
				I32 Knewterm=0;				
				for (I32 i=0; i < NEW.numSeg; i++) { 
					I32 kterms=basis->GenTerms(Xnewterm+Npad*Knewterm,N,&NEW.SEG[i],&(basis->bConst));
					NEW.SEG[i].K=kterms;
					Knewterm+=kterms;
				} 
				#ifdef SOLARIS_COMPILER
					NEW.k1=NEW.k1_old;
				#endif
				NEW.k2_new=NEW.k1+Knewterm - 1L;	
				NEW.k1+=basis->Kbase;			
				NEW.k2_old+=basis->Kbase;		
				NEW.k2_new+=basis->Kbase;
				I32 KOLD=MODEL.curr.K;                   
				I32 KNEW=KOLD+NEW.k2_new - NEW.k2_old; 
				MODEL.prop.K=KNEW;
				if (yInfo.nMissing > 0 && Knewterm > 0 )  
				f32_mat_multirows_extract_set_by_scalar(Xnewterm,Npad,Knewterm,Xt_zeroBackup,yInfo.rowsMissing,yInfo.nMissing,0.0f);
				for (I32 i=1; i<NEW.k1; i++) SCPY(i,MODEL.curr.XtX+(i-1L)*KOLD,MODEL.prop.XtX+(i-1L)*KNEW);
				if (Knewterm !=0) {
					FILL0(MODEL.prop.XtX+(NEW.k1-1)*KNEW,(KNEW-NEW.k1+1)*KNEW); 
					if (NEW.k1 > 1) {						
						BEAST2_BASESEG* _segInfo=(BEAST2_BASESEG *)(Xnewterm+Knewterm * Npad);
						I32             _numBands=GetInfoBandList(_segInfo,&MODEL,NEW.k1 - 1);
						MatxMat(_segInfo,_numBands,Xt_mars,
							    NEW.SEG,NEW.numSeg,Xnewterm,
							    MODEL.prop.XtX+(NEW.k1-1L)*KNEW,N,KNEW );
					}
					XtX_ByGroup(NEW.SEG,NEW.numSeg,Xnewterm,MODEL.prop.XtX+(NEW.k1 - 1) * KNEW+NEW.k1 - 1,N,KNEW);
					{
					}
				}
				if (NEW.k2_old !=KOLD) {
					for (I32 kold=NEW.k2_old+1,knew=NEW.k2_new+1; kold <=KOLD; kold++,knew++) {
						F32PTR ColStart_old=MODEL.curr.XtX+(kold - 1) * KOLD;
						F32PTR ColStart_new=MODEL.prop.XtX+(knew - 1) * KNEW;
						SCPY(NEW.k1 - 1,ColStart_old,ColStart_new); 
						SCPY(kold - NEW.k2_old,ColStart_old+(NEW.k2_old+1) - 1,ColStart_new+(NEW.k2_new+1) - 1); 
					}
					if (Knewterm !=0) { 						
						BEAST2_BASESEG* _segInfo=(BEAST2_BASESEG*) (Xnewterm+Knewterm * Npad);
						I32             _numBands=GetInfoBandList_post(_segInfo,&MODEL,NEW.k2_old+1);
						MatxMat(NEW.SEG,NEW.numSeg,Xnewterm,_segInfo,_numBands,Xt_mars+NEW.k2_old*Npad,
							    MODEL.prop.XtX+(NEW.k2_new+1 -1)*KNEW+NEW.k1- 1,N,KNEW);				
					} 
				}
				if (q==1) {
					if (NEW.k1 > 1)       SCPY(NEW.k1 - 1,MODEL.curr.XtY,MODEL.prop.XtY);
					if (Knewterm > 0)       MatxVec(NEW.SEG,NEW.numSeg,Xnewterm,yInfo.Y,MODEL.prop.XtY+NEW.k1 - 1,N);
					if (NEW.k2_old !=KOLD) SCPY(KNEW - NEW.k2_new,MODEL.curr.XtY+(NEW.k2_old+1L) - 1L,MODEL.prop.XtY+(NEW.k2_new+1) - 1);
				}	else {
					if (NEW.k1 > 1) {
						for (I32 c=0; c < q;++c) {
							SCPY( NEW.k1-1,MODEL.curr.XtY+MODEL.curr.K*c,MODEL.prop.XtY+KNEW * c);
						}						
					}
					if (Knewterm > 0) {
						MatxVec(NEW.SEG,NEW.numSeg,Xnewterm,yInfo.Y,MODEL.prop.XtY+NEW.k1 - 1,N);
						r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,Knewterm,q,N,1.f,Xnewterm,Npad,yInfo.Y,N,0.f,
							MODEL.prop.XtY+NEW.k1 - 1,KNEW);
					}
					if (NEW.k2_old !=KOLD) {
						for (I32 c=0; c < q;++c) {
							SCPY(KNEW - NEW.k2_new,MODEL.curr.XtY+(NEW.k2_old+1L) - 1L+MODEL.curr.K * c,MODEL.prop.XtY+(NEW.k2_new+1) - 1+KNEW * c);
						}
					}
				}
				if (1L) {		
					for (I32 i=1; i<NEW.k1; i++) 
						SCPY(i,MODEL.curr.cholXtX+(i-1)*KOLD,MODEL.prop.cholXtX+(i-1)*KNEW);
					precFunc.UpdateXtXPrec_nTermsPerGrp(&MODEL,basis,&NEW);
					precFunc.chol_addCol(  MODEL.prop.XtX+(NEW.k1-1)*KNEW,
							               MODEL.prop.cholXtX,
							               MODEL.prop.precXtXDiag,KNEW,NEW.k1,KNEW);
				   precFunc.ComputeMargLik(&MODEL.prop,&MODEL,&yInfo,&hyperPar);
				   if ( IsNaN(MODEL.prop.marg_lik)||IsInf(MODEL.prop.marg_lik ) ) {
					   if (++numBadIterations < 20) {
						   precFunc.IncreasePrecValues(&MODEL);
						   precFunc.GetXtXPrecDiag(&MODEL);
						   precFunc.chol_addCol(MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,MODEL.curr.K,1L,MODEL.curr.K);
						   precFunc.ComputeMargLik(&MODEL.curr,&MODEL,&yInfo,&hyperPar);
						   continue;
					   }  else {
						   skipCurrentPixel=2;
						   break;
					   }					   
				   }  else {
					   numBadIterations=0;
				   } 
				   if (q==1) {
					   MODEL.prop.alpha2_star=max(MODEL.prop.alpha2_star,MIN_ALPHA2_VALUE);
				   }
				}
				F32  factor;				
				if   ( NEW.flagMoveType==MOVE||basis->type==OUTLIERID) 	factor=0.;
				else { factor=basis->ModelPrior(basis,&NEW,N); }
				F32 delta_lik=MODEL.prop.marg_lik - MODEL.curr.marg_lik+factor;
				I08     acceptTheProposal;
				if      (delta_lik >   0)   acceptTheProposal=1;
				else if (delta_lik < -23) 	acceptTheProposal=0;				
				else {				 
					F32    expValue=fastexp(delta_lik);
					if     (delta_lik > -0.5) 	acceptTheProposal=*(RND.rnd08)++< expValue * 255.0f;
					else if(delta_lik > -5  )   acceptTheProposal=*(RND.rnd16)++< expValue * 65535.0f;
					else						acceptTheProposal=*(RND.rnd32)++< expValue * 4.294967296e+09;
				}
				#ifdef __DEBUG__
					if (basisIdx==0)++(flagS[NEW.flagMoveType]);
					else++(flagT[NEW.flagMoveType]);
				#endif
				if(acceptTheProposal)
				{
					#ifdef __DEBUG__
						if (basisIdx==0)++(accS[NEW.flagMoveType]);
						else++(accT[NEW.flagMoveType]);
					#endif
					if (yInfo.nMissing > 0 && Knewterm > 0 )  
						f32_mat_multirows_set_by_submat(Xnewterm,Npad,Knewterm,Xt_zeroBackup,yInfo.rowsMissing,yInfo.nMissing);
					if (NEW.k2_old !=KOLD && NEW.k2_new !=NEW.k2_old)
						MoveCOLsWithinMatrix(Xt_mars,Npad,NEW.k2_old+1,KOLD,NEW.k2_new+1);
					if (Knewterm !=0)
						SCPY(Knewterm*Npad,Xnewterm,Xt_mars+(NEW.k1-1) * Npad);
					basis->UpdateGoodVec(basis,&NEW,Npad16);
					basis->CalcBasisKsKeK_TermType(basis);
					UpdateBasisKbase(MODEL.b,MODEL.NUMBASIS,basis-MODEL.b);
					{
						#define Exchange(x,y)   {void * _restrict tmp; tmp=MODEL.x;  MODEL.x=MODEL.y; MODEL.y=tmp;}
						Exchange(curr.XtX,prop.XtX);
						Exchange(curr.XtY,prop.XtY);
						Exchange(curr.beta_mean,prop.beta_mean);
						Exchange(curr.cholXtX,prop.cholXtX);
						Exchange(curr.precXtXDiag,prop.precXtXDiag);
						Exchange(curr.nTermsPerPrecGrp,prop.nTermsPerPrecGrp);
						if (q==1) MODEL.curr.alpha2_star=MODEL.prop.alpha2_star; 
						else 	    Exchange(curr.alphaQ_star,prop.alphaQ_star);    
						MODEL.curr.marg_lik=MODEL.prop.marg_lik;
						MODEL.curr.K=MODEL.prop.K;  
						#undef Exchange											
					}
					#ifdef __DEBUG__	
					if (q==1) {
					}
					else {
						MR_EvaluateModel(&MODEL.prop,MODEL.b,Xdebug,N,MODEL.NUMBASIS,&yInfo,&hyperPar,MODEL.precVal,&stream);
					}
					#endif
				} 
				if (ite <=MCMC_BURNIN) continue;
				U08 bResampleParameter=(ite%20==0);
				U08 bStoreCurrentSample=(ite%MCMC_THINNING==0);
				if (bResampleParameter||bStoreCurrentSample)	{		
					if (q==1) {
						MODEL.sig2=(*RND.rndgamma++) * 1.f/MODEL.curr.alpha2_star;
						MODEL.sig2=1.0f/MODEL.sig2;
						MODEL.sig2=max(MODEL.sig2,MIN_SIG2_VALUE);
					}	else {
						F32PTR MEMBUF=Xnewterm;
						local_pcg_invwishart_upper( &stream,MODEL.SIG2,MODEL.SIG2+q*q,MEMBUF,q,
							                        MODEL.curr.alphaQ_star,hyperPar.alpha_1+yInfo.n+q - 1);					
					}
				}
				if (bResampleParameter||(bStoreCurrentSample && extra.useMeanOrRndBeta)) {
					if (q==1) {
						I32 K=MODEL.curr.K;
						r_vsRngGaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF,stream,K,MODEL.beta,0,1);
						solve_U_as_U_invdiag(MODEL.curr.cholXtX,MODEL.beta,K,K);
						r_ippsMulC_32f_I(fastsqrt(MODEL.sig2),MODEL.beta,K);
						r_ippsAdd_32f_I(MODEL.curr.beta_mean,MODEL.beta,K);
					} else {
						F32PTR MEMBUF=Xnewterm;
						I32    K=MODEL.curr.K;
						r_vsRngGaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF,stream,K * q,MEMBUF,0.,1.);
						r_cblas_sgemm(CblasColMajor,CblasNoTrans,CblasNoTrans,K,q,q,1.0,MEMBUF,K,MODEL.SIG2,q,0.f,MODEL.beta,K);
						solve_U_as_U_invdiag_multicols(MODEL.curr.cholXtX,MODEL.beta,K,K,q);
						r_ippsAdd_32f_I(MODEL.curr.beta_mean,MODEL.beta,K * q);
					}
				}
				if (bResampleParameter && q==1) 
				{
			       #ifdef DEBUG_BEAST2
					if (ite%10==0) {
						 float sum=0;
						 int   n=0;
							r_printf("ite%d: ",ite);
							for (int i=1; i <=MODEL.nPrec; i++) {
								if (opt->prior.precPriorType < 2) {
									sum+=MODEL.precVal;
									n++;
									r_printf("%7.4f|%7.4f ",MODEL.precVal,sum/n );
								}								
								else
									r_printf("%7.4f ",MODEL.precVec[i - 1]);
							}
							r_printf("\n ");
					}
				   #endif
					I32 ntries=0;
					do {
						if (ntries++==0)	precFunc.ResamplePrecValues(&MODEL,&hyperPar,&stream);							
						else				precFunc.IncreasePrecValues(&MODEL);											
						precFunc.GetXtXPrecDiag( &MODEL);
						precFunc.chol_addCol(    MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,MODEL.curr.K,1L,MODEL.curr.K);		
						precFunc.ComputeMargLik( &MODEL.curr,&MODEL,&yInfo,&hyperPar);
					} while (  IsNaN(MODEL.curr.marg_lik) && ntries < 20 );
					if ( IsNaN(MODEL.curr.marg_lik) ) {
						r_printf("skip3|prec:%.4f|marg_lik_cur:%.4f \n",MODEL.precVal,MODEL.curr.marg_lik);
						skipCurrentPixel=3;
						break;
					} 
				}
				if (bResampleParameter && q>1) 
				{
					F32PTR MEMBUF=Xnewterm;					
					I32    K=MODEL.curr.K;
					r_cblas_sgemm(CblasColMajor,CblasNoTrans,CblasNoTrans,K,q,q,1.0,MODEL.beta,K,MODEL.SIG2+q*q,q,0.f,MEMBUF,K);
					F32 sumq=DOT(K * q,MEMBUF,MEMBUF);
					r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,stream,1,&MODEL.precVal,(hyperPar.del_1+K * q * 0.5f),0.f,1.f);
					MODEL.precVal=MODEL.precVal/(hyperPar.del_2+0.5f * sumq);
					MODEL.logPrecVal=logf(MODEL.precVal);
					precFunc.GetXtXPrecDiag ( &MODEL);
					precFunc.chol_addCol(MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,K,1,K);
					precFunc.ComputeMargLik(&MODEL.curr,&MODEL,&yInfo,&hyperPar);
					if (IsNaN(MODEL.curr.marg_lik)) {
							skipCurrentPixel=3;
							break;
					} 
				}
				if (!bStoreCurrentSample) continue;
				sample++;
				*resultChain.marg_lik+=MODEL.curr.marg_lik;
				if (q==1) {
					*resultChain.sig2+=MODEL.sig2;
				}	else {
					F32PTR MEMBUF=Xnewterm;
					r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,q,1.f,MODEL.SIG2,q,MODEL.SIG2,q,0.f,MEMBUF,q);
					r_ippsAdd_32f_I(MEMBUF,resultChain.sig2,q*q);
				}
				F32PTR BETA=extra.useMeanOrRndBeta==0 ? MODEL.curr.beta_mean : MODEL.beta;
				{
					F32PTR MEMBUF1=Xnewterm;					
					for (I32 i=0; i < MODEL.NUMBASIS;++i) 
					{
						BEAST2_BASIS_PTR  basis=MODEL.b+i;
						CORESULT        * result=coreResults+i;
						I32        numKnot=basis->numKnot;
						TKNOT_PTR  KNOT=basis->KNOT;
						result->xNProb[numKnot]+=1L;
						for (I32 i=0; i < numKnot; i++) result->xProb[ KNOT[i]-1 ]+=1L;
						if (result->xorder !=NULL) {
							TORDER_PTR  orderList=basis->ORDER;
							for (I32 i=0; i <=numKnot;++i) {
								I16 r1=KNOT[i-1],r2=KNOT[i]-1;
								r_ippsAddC_32s_ISfs(orderList[i],result->xorder+r1 - 1,r2 - r1+1,0);							
							}
						}
						if (q==1) {
							basis->ComputeY(Xt_mars,BETA,MEMBUF1,basis,Npad);
							f32_add_v_v2_vec_inplace(MEMBUF1,result->x,result->xSD,N);
							MEMBUF1+=Npad;
						}	else {
							F32PTR 	X=Xt_mars+basis->Kbase * Npad;
							F32PTR  beta=BETA+basis->Kbase;
							I32     K=basis->K;
							r_cblas_sgemm(CblasColMajor,CblasNoTrans,CblasNoTrans,N,q,K,1.0f,
									     	      X,Npad,beta,MODEL.curr.K,0.f,MEMBUF1,N);
							f32_add_v_v2_vec_inplace(MEMBUF1,result->x,result->xSD,N*q);
							MEMBUF1+=Npad*q;
						}
					}
				}
				if(extra.computeSeasonAmp)
				{
					F32PTR           MEMBUF1=Xnewterm+3*Npad;
					F32PTR           MEMBUF2=MODEL.prop.beta_mean; 
					BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.sid];
					I32             knotNum=basis->numKnot;
					TKNOT_PTR       knotList=basis->KNOT;
					F32PTR       beta=BETA;
					TORDER_PTR   orderList=basis->ORDER;
					F32PTR       SEASON_SQR_CSUM=basis->bConst.season.SQR_CSUM;
					for (I32 i=0; i <=knotNum; i++) {
						I32 r1=knotList[i - 1];
						I32 r2=knotList[i] - 1;
						I32    segLength=r2-r1+1L;
						F32PTR seasonSqrCsum=SEASON_SQR_CSUM+1L;
						I32    order2=orderList[i] * 2L;
						F32  amp=0;
						for (I32 j=0; j < order2; j++) {
							F32 scalingFactor=N/(seasonSqrCsum[r2 - 1] - seasonSqrCsum[(r1 - 1) - 1]);
							amp+=(beta[j] * beta[j]) * scalingFactor;
							seasonSqrCsum+=(N+1LL);
						}						
						r_ippsSet_32f(amp,MEMBUF1+r1 - 1,segLength);
						beta+=order2;
					}
					r_ippsAdd_32f_I(MEMBUF1,resultChain.samp,N);
					r_ippsMul_32f_I(MEMBUF1,MEMBUF1,N);
					r_ippsAdd_32f_I(MEMBUF1,resultChain.sampSD,N); 
					if (extra.tallyPosNegSeasonJump)
					{
						I32  posKnotNum=0;
						for (I32 i=0; i < knotNum; i++) { 
							I64 knot=knotList[i];
							if (MEMBUF1[knot - 1] > MEMBUF1[knot - 1 - 1]) {
								resultChain.spos_cpOccPr[knot - 1]+=1;
								posKnotNum++;
							}
						}
						resultChain.spos_ncpPr[posKnotNum]+=1L;
						resultChain.sneg_ncpPr[knotNum - posKnotNum]+=1L;
					}
				}
				if(extra.computeTrendSlope)
				{
					BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.tid];
					I32             knotNum=basis->numKnot;
					TKNOT_PTR       knotList=basis->KNOT;
					F32PTR TREND=Xnewterm+Npad * MODEL.tid;     
					F32PTR SLP=Xnewterm+3 * Npad;				
					f32_diff_back(TREND,SLP,N);
					f32_add_v_v2_vec_inplace(SLP,resultChain.tslp,resultChain.tslpSD,N); 
					i32_increment_bycond_inplace(resultChain.tslpSignPr,SLP,N); 
					if (extra.tallyPosNegTrendJump ) {
						I32  posKnotNum=0;
						for (I32 i=0; i < knotNum; i++) {  
							I64 knot=knotList[i];
							if (SLP[knot - 1] > 0) {
								resultChain.tpos_cpOccPr[knot - 1]+=1;
								posKnotNum++;
							}
						}
						resultChain.tpos_ncpPr[posKnotNum]+=1L;
						resultChain.tneg_ncpPr[knotNum - posKnotNum]+=1L;
					}
					if (extra.tallyIncDecTrendJump ){
						I32  incKnotNum=0;
						for (I32 i=0; i < knotNum; i++) {  
							I64 knot=knotList[i];
							if (knot >=2 && SLP[(knot+1) - 1] > SLP[(knot - 1) - 1]) {
								resultChain.tinc_cpOccPr[knot - 1]+=1;
								incKnotNum++;
							}
						}
						resultChain.tinc_ncpPr[incKnotNum]+=1L;
						resultChain.tdec_ncpPr[knotNum-incKnotNum]+=1L;
					}
				}
				if(extra.tallyPosNegOutliers)
				{
					BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.oid];
					rI32             knotNum=basis->numKnot;
					rTKNOT_PTR       knotList=basis->KNOT;
					const F32PTR OUTLIIER=Xnewterm+Npad* MODEL.oid;
					I32  posKnotNum=0;
					for (I32 i=0; i < knotNum; i++) {  
						I64 knot=knotList[i];
						if (OUTLIIER[knot - 1] > 0) {
							resultChain.opos_cpOccPr[knot - 1]+=1;
							posKnotNum++;
						}							
					}
					resultChain.opos_ncpPr[posKnotNum]+=1L;
					resultChain.oneg_ncpPr[knotNum - posKnotNum]+=1L;								
				}
				if (extra.computeCredible)	{ 	
					if (extra.fastCIComputation) {
						if (*RND.rnd16++< ciParam.subsampleFraction_x_INT16MAX)
							++subSampleIndex;
						else 
							continue;
					} else {
						subSampleIndex=sample;
					}
					if (subSampleIndex <=ciParam.nSamples)	{	
						for (int i=0; i<MODEL.NUMBASIS;i++)      InsertInitialRows(&ciParam,&ci[i],subSampleIndex); 
					} else { 
						for (int i=0; i < MODEL.NUMBASIS; i++) InsertNewRowToUpdateCI(&ciParam,&ci[i]);						
					}
				} 
			}
			if (!skipCurrentPixel)
			{
				#define GetSum(arr) ( r_ippsSum_32s_Sfs(arr,N,&sum,0),sum)
				I32   sMAXNUMKNOT=MODEL.sid >=0? MODEL.b[MODEL.sid].prior.maxKnotNum:-9999999;
				I32   tMAXNUMKNOT=MODEL.tid>=0?  MODEL.b[MODEL.tid].prior.maxKnotNum:-9999999;
				I32   oMAXNUMKNOT=MODEL.oid>=0?  MODEL.b[MODEL.oid].prior.maxKnotNum:- 9999999;
				F32   inv_sample=1.f/sample;
				F32   sd=yInfo.sd[0];
				int   sum;
				*resultChain.marg_lik=*resultChain.marg_lik * inv_sample;
				*resultChain.sig2=*resultChain.sig2     * inv_sample * sd * sd;
				if (MODEL.sid >=0) {
						*resultChain.sncp=GetSum(resultChain.scpOccPr)* inv_sample;
						i32_to_f32_scaleby_inplace(resultChain.sncpPr,(sMAXNUMKNOT+1),inv_sample);						
						i32_to_f32_scaleby_inplace(resultChain.scpOccPr,N,inv_sample);					
						f32_sx_sxx_to_avgstd_inplace(resultChain.sY,resultChain.sSD,sample,sd,0,N);
						if (extra.computeSeasonOrder) i32_to_f32_scaleby_inplace(resultChain.sorder,N,inv_sample);
						if (extra.computeSeasonAmp)   f32_sx_sxx_to_avgstd_inplace(resultChain.samp,resultChain.sampSD,sample,sd,0,N);
						if (extra.computeCredible)    r_ippsMulC_32f_I(yInfo.sd[0],resultChain.sCI,N+N);
				}
				if (MODEL.tid >=0) {
						*resultChain.tncp=GetSum(resultChain.tcpOccPr)*inv_sample ;
						i32_to_f32_scaleby_inplace(resultChain.tncpPr,(tMAXNUMKNOT+1),inv_sample);
						i32_to_f32_scaleby_inplace(resultChain.tcpOccPr,N,inv_sample);
						f32_sx_sxx_to_avgstd_inplace(resultChain.tY,resultChain.tSD,sample,sd,yInfo.mean[0],N);
						if (extra.computeTrendOrder) 	i32_to_f32_scaleby_inplace(resultChain.torder,N,inv_sample);						
						if (extra.computeTrendSlope) {
							f32_sx_sxx_to_avgstd_inplace(resultChain.tslp,resultChain.tslpSD,sample,sd/opt->io.meta.deltaTime,0,N);
							i32_to_f32_scaleby_inplace(resultChain.tslpSignPr,N,inv_sample);
						}
						if (extra.computeCredible)
							r_ippsMulC_32f_I(yInfo.sd[0],resultChain.tCI,N+N),
							r_ippsSubC_32f_I(-yInfo.mean[0],resultChain.tCI,N+N); 
				}
				if (MODEL.oid >=0) {					
					 *resultChain.oncp=inv_sample * GetSum(resultChain.ocpOccPr);
					 i32_to_f32_scaleby_inplace(resultChain.oncpPr,(oMAXNUMKNOT+1),inv_sample);
					 i32_to_f32_scaleby_inplace(resultChain.ocpOccPr,N,inv_sample);
					 f32_sx_sxx_to_avgstd_inplace(resultChain.oY,resultChain.oSD,sample,sd,0,N);
					if (extra.computeCredible)	r_ippsMulC_32f_I(yInfo.sd[0],resultChain.oCI,N+N);
				}
				if (extra.tallyPosNegSeasonJump && MODEL.sid>=0) {
					GetSum(resultChain.spos_cpOccPr);	
					*resultChain.spos_ncp=inv_sample * sum; 
					*resultChain.sneg_ncp=*resultChain.sncp - *resultChain.spos_ncp; 
					i32_to_f32_scaleby_inplace(resultChain.spos_ncpPr,(sMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.sneg_ncpPr,(sMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.spos_cpOccPr,N,inv_sample);
					SCPY(N,resultChain.scpOccPr,resultChain.sneg_cpOccPr); 
					r_ippsSub_32f_I((F32PTR)resultChain.spos_cpOccPr,(F32PTR)resultChain.sneg_cpOccPr,N);   
				}
				if (extra.tallyPosNegTrendJump) {
					GetSum(resultChain.tpos_cpOccPr);	
					*resultChain.tpos_ncp=inv_sample * sum; 
					*resultChain.tneg_ncp=*resultChain.tncp - *resultChain.tpos_ncp; 
					i32_to_f32_scaleby_inplace(resultChain.tpos_ncpPr,(tMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.tneg_ncpPr,(tMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.tpos_cpOccPr,N,inv_sample);
					SCPY(N,resultChain.tcpOccPr,resultChain.tneg_cpOccPr); 
					r_ippsSub_32f_I((F32PTR)resultChain.tpos_cpOccPr,(F32PTR)resultChain.tneg_cpOccPr,N);   
				}
				if (extra.tallyIncDecTrendJump) {
					GetSum(resultChain.tinc_cpOccPr);	   *resultChain.tinc_ncp=inv_sample * sum; 
					*resultChain.tdec_ncp=*resultChain.tncp - *resultChain.tinc_ncp; 
					i32_to_f32_scaleby_inplace(resultChain.tinc_ncpPr,(tMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.tdec_ncpPr,(tMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.tinc_cpOccPr,N,inv_sample);
					SCPY(N,resultChain.tcpOccPr,resultChain.tdec_cpOccPr); 
					r_ippsSub_32f_I((F32PTR)resultChain.tinc_cpOccPr,(F32PTR)resultChain.tdec_cpOccPr,N);   
				}
				if (extra.tallyPosNegOutliers && MODEL.oid >=0) {
					GetSum(resultChain.opos_cpOccPr);	
					*resultChain.opos_ncp=inv_sample * sum; 
					*resultChain.oneg_ncp=*resultChain.oncp - *resultChain.opos_ncp; 
					i32_to_f32_scaleby_inplace(resultChain.opos_ncpPr,(oMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.oneg_ncpPr,(oMAXNUMKNOT+1),inv_sample);
					i32_to_f32_scaleby_inplace(resultChain.opos_cpOccPr,N,inv_sample);
					SCPY(N,resultChain.ocpOccPr,resultChain.oneg_cpOccPr); 
					r_ippsSub_32f_I((F32PTR)resultChain.opos_cpOccPr,(F32PTR)resultChain.oneg_cpOccPr,N);   
				}
			}
			if (!skipCurrentPixel) 
			{
				I32   sMAXNUMKNOT=MODEL.sid >=0? MODEL.b[MODEL.sid].prior.maxKnotNum:-9999999;
				I32   tMAXNUMKNOT=MODEL.tid>=0?  MODEL.b[MODEL.tid].prior.maxKnotNum:-9999999;
				I32   oMAXNUMKNOT=MODEL.oid>=0?  MODEL.b[MODEL.oid].prior.maxKnotNum:- 9999999;
				#define _1(x)      *(result.x)+=*(resultChain.x)
				#define _N(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N)
				#define _Nq(x)     r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N*q)
				#define _q(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,q)
				#define _q2(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,q*q)
				#define _2N(x)     r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N+N)
				#define _skn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,sMAXNUMKNOT+1)
				#define _tkn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,tMAXNUMKNOT+1)
				#define _okn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,oMAXNUMKNOT+1)
				_1(marg_lik);
				_q2(sig2);  
				if (MODEL.sid >=0) {					
					_1(sncp); _skn_1(sncpPr);	     _N(scpOccPr); _Nq(sY); _Nq(sSD);
					if (extra.computeSeasonOrder)    _N(sorder);
					if (extra.computeSeasonAmp)      _N(samp),_N(sampSD);
					if (extra.computeCredible)       _2N(sCI);
				}
				if (MODEL.tid >=0) {					
					_1(tncp); _tkn_1(tncpPr);	   _N(tcpOccPr); _Nq(tY); _Nq(tSD);
					if (extra.computeTrendOrder)   _N(torder);
					if (extra.computeTrendSlope)   _N(tslp),_N(tslpSD),_N(tslpSignPr);
					if (extra.computeCredible)     _2N(tCI);
				}
				if (MODEL.oid >=0) {
					_1(oncp); _tkn_1(oncpPr);	_N(ocpOccPr); _N(oY); _N(oSD);
					if (extra.computeCredible)   _2N(oCI);
				}
				if (extra.tallyPosNegSeasonJump && MODEL.sid >=0) {
					_1(spos_ncp);         _1(sneg_ncp); 
					_skn_1(spos_ncpPr);   _skn_1(sneg_ncpPr); 
					_N(spos_cpOccPr);     _N(sneg_cpOccPr); 
				}
				if (extra.tallyPosNegTrendJump ) {
					_1(tpos_ncp);            _1(tneg_ncp); 
				    _tkn_1(tpos_ncpPr); _tkn_1(tneg_ncpPr); 
					_N(tpos_cpOccPr);         _N(tneg_cpOccPr); 
				}
				if (extra.tallyIncDecTrendJump) {
					_1(tinc_ncp);         _1(tdec_ncp); 
					_tkn_1(tinc_ncpPr); _tkn_1(tdec_ncpPr); 
					_N(tinc_cpOccPr);     _N(tdec_cpOccPr); 
				}
				if (extra.tallyPosNegOutliers && MODEL.oid >=0) {
					_1(opos_ncp);            _1(oneg_ncp); 
					_tkn_1(opos_ncpPr); _tkn_1(oneg_ncpPr); 
					_N(opos_cpOccPr);         _N(oneg_cpOccPr); 
				}	
				#undef _1
				#undef _N
				#undef _Nq 
				#undef _q 
				#undef _q2
				#undef _2N
				#undef _skn_1
				#undef _tkn_1
			    #undef _okn_1
			}
			if (skipCurrentPixel) {
				break;
			}
		}
		__END_IF_NOT_SKIP_TIMESESIRIES__  
	}
		if (MCMC_CHAINNUM >=2 && !skipCurrentPixel)
		{
			I32  N=opt->io.N;			
			I32   sMAXNUMKNOT=MODEL.sid >=0 ? MODEL.b[MODEL.sid].prior.maxKnotNum : -9999999;
			I32   tMAXNUMKNOT=MODEL.tid >=0 ? MODEL.b[MODEL.tid].prior.maxKnotNum : -9999999;
			I32   oMAXNUMKNOT=MODEL.oid >=0 ? MODEL.b[MODEL.oid].prior.maxKnotNum : -9999999;
			const F32 invChainNumber=1.f/(F32)MCMC_CHAINNUM;
			#define _1(x)      *((F32PTR)result.x)*=invChainNumber
			#define _N(x)      r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N)
			#define _Nq(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N*q)
			#define _q(x)      r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,q)
			#define _q2(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,q*q)
			#define _2N(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N+N)
			#define _skn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,sMAXNUMKNOT+1)
			#define _tkn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,tMAXNUMKNOT+1)
			#define _okn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,oMAXNUMKNOT+1)
			_1(marg_lik);			
			_q2(sig2);
			if (MODEL.sid >=0) {
				_1(sncp); _skn_1(sncpPr);	     _N(scpOccPr); _Nq(sY); _Nq(sSD); 
				if (extra.computeSeasonOrder)    _N(sorder);
				if (extra.computeSeasonAmp)     {_N(samp),_N(sampSD);}
				if (extra.computeCredible)       _2N(sCI);
			}
			if (MODEL.tid >=0) {
				_1(tncp); _tkn_1(tncpPr);	     _N(tcpOccPr); _Nq(tY); _Nq(tSD); 
				if (extra.computeTrendOrder)     _N(torder);
				if (extra.computeTrendSlope)    { _N(tslp),_N(tslpSD),_N(tslpSignPr);}
				if (extra.computeCredible)       _2N(tCI);
			}
			if (MODEL.oid >=0) {
				_1(oncp); _tkn_1(oncpPr);	   
				_N(ocpOccPr); _Nq(oY); _Nq(oSD);
				if (extra.computeCredible)      _2N(oCI);
			}
			if (extra.tallyPosNegSeasonJump && MODEL.sid >=0) {
				_1(spos_ncp);             _1(sneg_ncp); 
				_skn_1(spos_ncpPr);    _skn_1(sneg_ncpPr); 
				_N(spos_cpOccPr);         _N(sneg_cpOccPr); 
			}
			if (extra.tallyPosNegTrendJump) {
				_1(tpos_ncp);            _1(tneg_ncp); 
				_tkn_1(tpos_ncpPr); _tkn_1(tneg_ncpPr); 
				_N(tpos_cpOccPr);         _N(tneg_cpOccPr); 
			}
			if (extra.tallyIncDecTrendJump) {
				_1(tinc_ncp);         _1(tdec_ncp); 
				_tkn_1(tinc_ncpPr); _tkn_1(tdec_ncpPr); 
				_N(tinc_cpOccPr);     _N(tdec_cpOccPr); 
			}
			if (extra.tallyPosNegOutliers && MODEL.oid >=0) {
				_1(opos_ncp);            _1(oneg_ncp); 
				_tkn_1(opos_ncpPr); _tkn_1(oneg_ncpPr); 
				_N(opos_cpOccPr);         _N(oneg_cpOccPr); 
			}
			#undef _1
			#undef _N
			#undef _2N
			#undef _skn_1
			#undef _tkn_1
			#undef _okn_1
		}
		if (!skipCurrentPixel) {		
			I32  N=opt->io.N;  
			if (MODEL.sid >=0) 							tsRemoveNaNs(result.sSD,N);
			if (MODEL.tid >=0)								tsRemoveNaNs(result.tSD,N);
			if (MODEL.tid >=0 && extra.computeTrendSlope)  tsRemoveNaNs(result.tslpSD,N);		 
			if (MODEL.oid >=0) 							tsRemoveNaNs(result.oSD,N);
		}
		if (!skipCurrentPixel)
		{
			I32     N=opt->io.N;
			F32     nan=getNaN();     
			F32  	threshold=0.001f;
			F32PTR	mem=Xnewterm;  
			I32PTR  cptList=(I32PTR)mem+5LL * N;
			F32PTR  cptCIList=(F32PTR)mem+6LL * N;
			I32  cptNumber;
			I32  trueCptNumber;
			const F32 T0=(F32)opt->io.meta.startTime;
			const F32 dT=(F32)opt->io.meta.deltaTime;
			I32   sMAXNUMKNOT=MODEL.sid >=0 ? MODEL.b[MODEL.sid].prior.maxKnotNum : -9999999;
			I32   tMAXNUMKNOT=MODEL.tid >=0 ? MODEL.b[MODEL.tid].prior.maxKnotNum : -9999999;
			I32   oMAXNUMKNOT=MODEL.oid >=0 ? MODEL.b[MODEL.oid].prior.maxKnotNum : -9999999;
			I32   sMINSEPDIST=MODEL.sid >=0 ? MODEL.b[MODEL.sid].prior.minSepDist : -9999999;
			I32   tMINSEPDIST=MODEL.tid >=0 ? MODEL.b[MODEL.tid].prior.minSepDist : -9999999;
			I32   oMINSEPDIST=MODEL.oid >=0 ? 1 : -9999999;
			if (extra.computeSeasonChngpt && MODEL.sid >=0) 
			{
				cptNumber=(I32)round((double)*result.sncp);
				trueCptNumber=FindChangepoint((F32PTR)result.scpOccPr,mem,threshold,cptList,cptCIList,N,sMINSEPDIST,cptNumber);
				for (int i=0; i < trueCptNumber; i++) {
					*(result.scp+i)=(F32)(*(cptList+i)) * dT+T0;
					*(result.scpPr+i)=(F32)mem[i];
					I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];
					*(result.scpAbruptChange+i)=result.sY[cptLoc] - result.sY[cptLoc - 1];
				}
				for (int i=trueCptNumber; i < sMAXNUMKNOT; i++)
					*(result.scp+i)=nan,
					* (result.scpPr+i)=nan,
					* (result.scpAbruptChange+i)=nan;
				for (int i=0; i < trueCptNumber; i++)
					*(result.scpCI+i)=(F32)(*(cptCIList+i)) * dT+T0,
					* (result.scpCI+sMAXNUMKNOT+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;
				for (int i=trueCptNumber; i < sMAXNUMKNOT; i++)
					*(result.scpCI+i)=nan,
					* (result.scpCI+sMAXNUMKNOT+i)=nan;
			}
			if (extra.computeTrendChngpt) {
				cptNumber=(I32)round((double)*result.tncp);
				trueCptNumber=FindChangepoint((F32PTR)result.tcpOccPr,mem,threshold,cptList,cptCIList,N,tMINSEPDIST,cptNumber);
				for (int i=0; i < trueCptNumber; i++) {
					*(result.tcp+i)=(F32)(*(cptList+i)) * dT+T0,
					*(result.tcpPr+i)=(F32)mem[i];
					I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];
					*(result.tcpAbruptChange+i)=result.tY[cptLoc] - result.tY[cptLoc - 1];;
				}
				for (int i=trueCptNumber; i < tMAXNUMKNOT; i++)
					*(result.tcp+i)=nan,
					*(result.tcpPr+i)=nan,
					*(result.tcpAbruptChange+i)=nan;
				for (int i=0; i < trueCptNumber; i++)
					*(result.tcpCI+i)=(F32)(*(cptCIList+i)) * dT+T0,
					*(result.tcpCI+tMAXNUMKNOT+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;
				for (int i=trueCptNumber; i < tMAXNUMKNOT; i++)
					*(result.tcpCI+i)=nan,
					*(result.tcpCI+tMAXNUMKNOT+i)=nan;
			}
#define GET_CHANGPOINTS(KNOTNUM,MINSEP,MAX_KNOTNUM,CHANGEINPUT,PROBCURVE,CP,CPPROB,CP_CHANGE,CP_CI)    \
			cptNumber=(I32)round((double)*KNOTNUM); \
			trueCptNumber=FindChangepoint((F32PTR)PROBCURVE,mem,threshold,cptList,cptCIList,N,MINSEP,cptNumber);\
			for (int i=0; i < trueCptNumber; i++) {\
				*(CP+i)=(F32) cptList[i]* dT+T0,\
				*(CPPROB+i)=(F32) mem[i];\
		         I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];\
				 *(CP_CHANGE+i)=CHANGEINPUT[cptLoc] - CHANGEINPUT[cptLoc - 1];\
			}\
			for (int i=trueCptNumber; i <MAX_KNOTNUM; i++) {\
				*(CP+i)=nan;\
				*(CPPROB+i)=nan;\
				  *(CP_CHANGE+i)=nan;\
			}	\
			for (int i=0; i < trueCptNumber; i++)\
				*(CP_CI+i)=(F32)cptCIList[i] * dT+T0,\
				*(CP_CI+MAX_KNOTNUM+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;\
			for (int i=trueCptNumber; i < MAX_KNOTNUM; i++)\
				*(CP_CI+i)=nan,\
				*(CP_CI+MAX_KNOTNUM+i)=nan;
#define OGET_CHANGPOINTS(KNOTNUM,MINSEP,MAX_KNOTNUM,PROBCURVE,CP,CPPROB,CP_CI)    \
			cptNumber=(I32)round((double)*KNOTNUM); \
			trueCptNumber=FindChangepoint((F32PTR)PROBCURVE,mem,threshold,cptList,cptCIList,N,MINSEP,cptNumber);\
			for (int i=0; i < trueCptNumber; i++) {\
				*(CP+i)=(F32) cptList[i]* dT+T0;\
				*(CPPROB+i)=(F32) mem[i];\
		         I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];\
			}\
			for (int i=trueCptNumber; i <MAX_KNOTNUM; i++) {\
				*(CP+i)=nan;\
				*(CPPROB+i)=nan;\
		    }\
			for (int i=0; i < trueCptNumber; i++) \
				*(CP_CI+i)=(F32)cptCIList[i] * dT+T0,\
				*(CP_CI+MAX_KNOTNUM+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;\
			for (int i=trueCptNumber; i < MAX_KNOTNUM; i++)\
				*(CP_CI+i)=nan,\
				*(CP_CI+MAX_KNOTNUM+i)=nan;
			if (extra.tallyPosNegSeasonJump && MODEL.sid >=0) {
				GET_CHANGPOINTS(result.spos_ncp,sMINSEPDIST,sMAXNUMKNOT,result.samp,
					result.spos_cpOccPr,result.spos_cp,result.spos_cpPr,result.spos_cpAbruptChange,result.spos_cpCI);
				GET_CHANGPOINTS(result.sneg_ncp,sMINSEPDIST,sMAXNUMKNOT,result.samp,
					result.sneg_cpOccPr,result.sneg_cp,result.sneg_cpPr,result.sneg_cpAbruptChange,result.sneg_cpCI);
			}
			if (extra.tallyPosNegTrendJump) {
				GET_CHANGPOINTS(result.tpos_ncp,tMINSEPDIST,tMAXNUMKNOT,result.tY,
					result.tpos_cpOccPr,result.tpos_cp,result.tpos_cpPr,result.tpos_cpAbruptChange,result.tpos_cpCI);
				GET_CHANGPOINTS(result.tneg_ncp,tMINSEPDIST,tMAXNUMKNOT,result.tY,
					result.tneg_cpOccPr,result.tneg_cp,result.tneg_cpPr,result.tneg_cpAbruptChange,result.tneg_cpCI);
			}
			if (extra.tallyIncDecTrendJump) {
				GET_CHANGPOINTS(result.tinc_ncp,tMINSEPDIST,tMAXNUMKNOT,result.tslp,
					result.tinc_cpOccPr,result.tinc_cp,result.tinc_cpPr,result.tinc_cpAbruptChange,result.tinc_cpCI);
				GET_CHANGPOINTS(result.tdec_ncp,tMINSEPDIST,tMAXNUMKNOT,result.tslp,
					result.tdec_cpOccPr,result.tdec_cp,result.tdec_cpPr,result.tdec_cpAbruptChange,result.tdec_cpCI);
			}
			if (extra.computeOutlierChngpt) {
				OGET_CHANGPOINTS(result.oncp,oMINSEPDIST,oMAXNUMKNOT,result.ocpOccPr,result.ocp,result.ocpPr,result.ocpCI);
			}
			if (extra.tallyPosNegOutliers && MODEL.oid >=0) {
				OGET_CHANGPOINTS(result.opos_ncp,oMINSEPDIST,oMAXNUMKNOT, 
					result.opos_cpOccPr,result.opos_cp,result.opos_cpPr,result.opos_cpCI);
				OGET_CHANGPOINTS(result.oneg_ncp,oMINSEPDIST,oMAXNUMKNOT, 
					result.oneg_cpOccPr,result.oneg_cp,result.oneg_cpPr,result.oneg_cpCI);
			}
		}
		if (opt->extra.dumpInputData||!skipCurrentPixel) {	
			I32  N=opt->io.N;	
			I32  Nq=N * q;  
			for (int i=0; i < q;++i) {
				memcpy(Xnewterm+N*i,yInfo.Y+N*i,sizeof(F32)* N);
				f32_scale_inplace(yInfo.sd[i],yInfo.mean[i],Xnewterm+N*i,N);
				F32PTR NULL_BUF_FOR_VALUES=(Xnewterm+N * i)+N;
				if (yInfo.nMissing > 0) {
					f32_gatherVec_scatterVal_byindex(Xnewterm+N*i,yInfo.rowsMissing,NULL_BUF_FOR_VALUES,getNaN(),yInfo.nMissing);
				}
			}			
		}
		if (opt->extra.dumpInputData && result.data !=NULL) {
			I32  N=opt->io.N;
			I32  Nq=N * q;  
			memcpy(result.data,Xnewterm,sizeof(F32)* Nq);
		}
  		if (!skipCurrentPixel) { 
			I32  N=opt->io.N ;
			I32  Nq=N * q;  
			I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
			I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
			I08 hasTrendCmpnt=1;
			F32PTR BUF=Xnewterm+Nq;
			f32_fill_val(0.,BUF,Nq);
			if (hasTrendCmpnt)   f32_add_vec_inplace(result.tY,BUF,Nq);
			if (hasSeasonCmpnt)  f32_add_vec_inplace(result.sY,BUF,Nq);
			if (hasOutlierCmpnt) f32_add_vec_inplace(result.oY,BUF,Nq);
			for (int j=0; j < q;++j) {
				F32 r=f32_corr_rmse_nan(BUF+N*j,Xnewterm+N*j,N,&result.RMSE[j]);
				result.R2[j]=r * r;
			}
		}
		if (!skipCurrentPixel) {
			I32  N=opt->io.N;
			I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
			I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
			I08 hasTrendCmpnt=1;
			F32PTR BUF=Xnewterm;
			if (hasTrendCmpnt && opt->extra.smoothCpOccPrCurve) {	
				memcpy(BUF,result.tcpOccPr,sizeof(F32)* N);
				f32_sumfilter(BUF,result.tcpOccPr,N,opt->prior.trendMinSepDist);
			}
			if (hasSeasonCmpnt && opt->extra.smoothCpOccPrCurve) {
				memcpy(BUF,result.scpOccPr,sizeof(F32)* N);
				f32_sumfilter(BUF,result.scpOccPr,N,opt->prior.seasonMinSepDist);
			}	
		}
	    if (skipCurrentPixel) BEAST2_Result_FillMEM(&result,opt,getNaN());
		if (!skipCurrentPixel) {
			int N=opt->io.N;
			for (int i=0; i < q;++i) {
				if (yInfo.Yseason) {
						r_ippsAdd_32f_I(yInfo.Yseason+N*i,result.sY+N* i,N);
					if (result.sCI) {
						r_ippsAdd_32f_I(yInfo.Yseason+N * i,result.sCI+2*N*i,N);
						r_ippsAdd_32f_I(yInfo.Yseason+N * i,result.sCI+2*N*i+N,N);
					}
					if (extra.dumpInputData)
						r_ippsAdd_32f_I(yInfo.Yseason+N * i,result.data+N*i,N);
				}
				if (yInfo.Ytrend) {
						r_ippsAdd_32f_I(yInfo.Ytrend+N * i,result.tY+N*i,N);
					if (result.tCI) {
						r_ippsAdd_32f_I(yInfo.Ytrend+N * i,result.tCI+2*N*i,N);
						r_ippsAdd_32f_I(yInfo.Ytrend+N * i,result.tCI+2*N*i+N,N);
					}
					if (extra.dumpInputData)
						r_ippsAdd_32f_I(yInfo.Ytrend+N * i,result.data+N * i,N);
				}
			} 
		}
		if(q==1)
			BEAST2_WriteOutput(opt,&result,pixelIndex);
		else
		    MR_WriteOutput(opt,&result,pixelIndex);
		pthread_mutex_lock(&mutex);
		NUM_OF_PROCESSED_GOOD_PIXELS+=!skipCurrentPixel;  
		NUM_OF_PROCESSED_PIXELS++;							
		pthread_mutex_unlock(&mutex);
		F32 elaspedTime=GetElaspedTimeFromBreakPoint();
		if (NUM_OF_PROCESSED_GOOD_PIXELS > 0 && NUM_PIXELS > 1 && (pixelIndex%50==0||elaspedTime > 1))  {
			PERCENT_COMPLETED=(F32)NUM_OF_PROCESSED_PIXELS/NUM_PIXELS;
			REMAINING_TIME=GetElapsedSecondsSinceStart()/NUM_OF_PROCESSED_GOOD_PIXELS * (NUM_PIXELS- NUM_OF_PROCESSED_PIXELS);
			if (elaspedTime > 1) {
				SetBreakPointForStartedTimer();
			}
		}
		if (IDE_USER_INTERRUPT)		{
			break;
		}
		#ifdef __DEBUG__
		r_printf("TREND: birth%4d/%-5d|death%4d/%-5d|merge%4d/%-5d|move%4d/%-5d|chorder%4d/%-5d\n", 
			      accT[0],flagT[0],accT[1],flagT[1],accT[2],flagT[2],accT[3],flagT[3],accT[4],flagT[4]);
		r_printf("SEASN: birth%4d/%-5d|death%4d/%-5d|merge%4d/%-5d|move%4d/%-5d|chorder%4d/%-5d\n",
			      accS[0],flagS[0],accS[1],flagS[1],accS[2],flagS[2],accS[3],flagS[3],accS[4],flagS[4]);
		#endif
	} 
	r_vslDeleteStream(&stream);
	MEM.free_all(&MEM);
	pthread_t  id=pthread_self();
	pthread_mutex_lock(&mutex);
	pthread_mutex_unlock(&mutex);
	return 1;
} 
#include "abc_000_warning.h"
