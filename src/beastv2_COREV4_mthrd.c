#include "abc_000_macro.h"
#include "abc_000_warning.h"
#if defined(COMPILER_MSVC)
#include "intrin.h"                
#endif
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
#include <stdio.h>	               
#include "globalvars.h"  
#include "beastv2_header.h"
#include "beastv2_func.h" 
#include "beastv2_model_allocinit.h" 
#include "beastv2_prior_precfunc.h" 
#include "beastv2_xxyy_allocmem.h" 
#include "beastv2_io.h" 
#define LOCAL(...) do{ __VA_ARGS__ } while(0);
#define  DEBUG_MODE  0
int beast2_main_corev4_mthrd(void* dummy) {
	MemPointers MEM=(MemPointers){.init=mem_init,};
	MEM.init(&MEM);
#if DEBUG_MODE==1
	MEM.checkHeader=1;
#endif
	VSLStreamStatePtr stream;
	const BEAST2_OPTIONS_PTR	opt=GLOBAL_OPTIONS;
	const BEAST2_EXTRA          extra=opt->extra;
	typedef int QINT;
	const QINT  q=opt->io.q;
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
	const F32PTR  RNDGAMMA_END=RNDGAMMA+MAX_RAND_NUM - MODEL.precState.nPrecGrp-1L;
	const F32PTR Xt_mars;
	const F32PTR Xnewterm;      
	const F32PTR Xt_zeroBackup; 
	AllocateXXXMEM(&Xt_mars,&Xnewterm,&Xt_zeroBackup,&MODEL,opt,&MEM);
	BEAST2_YINFO     yInfo;
	AllocateYinfoMEM(&yInfo,opt,&MEM);
	const BEAST2_RESULT resultChain={ NULL,},result={ NULL,};
	BEAST2_Result_AllocMEM(&resultChain,opt,&MEM); 	
	BEAST2_Result_AllocMEM(&result,opt,&MEM);
	const   I32  NumCIVars=MODEL.NUMBASIS+opt->extra.computeTrendSlope;
	 CI_PARAM     ciParam={ 0,};
	CI_RESULT    ci[MAX_NUM_BASIS+1];
	if (extra.computeCredible) {
		ConstructCIStruct(opt->mcmc.credIntervalAlphaLevel,opt->mcmc.samples,opt->io.N * opt->io.q,  
			              NumCIVars,&MEM,&extra.fastCIComputation,&ciParam,ci);
	}
	if (extra.computeCredible) {
		I32  Npad=(opt->io.N+7)/8 * 8;
		I32  XnewtermOffset=0;
		Npad=opt->io.N;    
		I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
		I08 hasTrendCmpnt=1;
		I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
		int numCIVars=0;
		if (hasSeasonCmpnt) {
			ci[numCIVars].result=resultChain.sCI;		         
			ci[numCIVars].newDataRow=Xnewterm+XnewtermOffset;	 
			numCIVars++;
			XnewtermOffset+=Npad * q;   
		}
		if (hasTrendCmpnt) {
			ci[numCIVars].result=resultChain.tCI;               
			ci[numCIVars].newDataRow=Xnewterm+XnewtermOffset;    
			numCIVars++;
			XnewtermOffset+=Npad * q;   
		}	 
		if (hasOutlierCmpnt) {
		  ci[numCIVars].result=resultChain.oCI,               
		  ci[numCIVars].newDataRow=Xnewterm+XnewtermOffset;     
		  numCIVars++;
		  XnewtermOffset+=Npad * q;   
		}
		if (opt->extra.computeTrendSlope) {
			ci[numCIVars].result=resultChain.tslpCI;           
			ci[numCIVars].newDataRow=Xnewterm+XnewtermOffset;    
			numCIVars++;
			XnewtermOffset+=Npad * q;   
		}
	} 
	const CORESULT coreResults[MAX_NUM_BASIS];
	SetupPointersForCoreResults(coreResults,MODEL.b,MODEL.NUMBASIS,&resultChain);
	const BEAST2_HyperPar  hyperPar={ .alpha_1=opt->prior.alpha1,.alpha_2=opt->prior.alpha2,.del_1=opt->prior.delta1,.del_2=opt->prior.delta2};
	InitTimerFunc();
	StartTimer();
	SetBreakPointForStartedTimer();
	const PREC_FUNCS precFunc;
	SetUpPrecFunctions(opt->prior.precPriorType,opt->io.q,&precFunc);
	if (extra.printProgress) {
		F32 frac=0.0; I32 firstTimeRun=1;
	}
	#if DEBUG_MODE==1
		I32    N=opt->io.N;
		I32    Npad=(N+7)/8 * 8; Npad=N;
		F32PTR flagSat=MyALLOC0(MEM,N,I32,64);
		F32PTR Xdebug=MyALLOC0(MEM,Npad*(opt->prior.K_MAX+opt->prior.K_MAX),I32,64); 
	#endif
	const U32  NUM_PIXELS=opt->io.numOfPixels;
	const U32  MCMC_SAMPLES=opt->mcmc.samples;
	const U32  MCMC_THINNING=opt->mcmc.thinningFactor;
	const U32  MCMC_BURNIN=opt->mcmc.burnin;
	const U32  MCMC_CHAINNUM=opt->mcmc.chainNumber;
	const U16  SEASON_BTYPE=opt->prior.seasonBasisFuncType;
	void (*Update_XtX_from_Xnewterm)(F32PTR X,F32PTR Xnewterm,F32PTR XtX,F32PTR XtXnew,NEWTERM * NEW,BEAST2_MODEL * MODEL);
	void (*Update_XtY_from_Xnewterm)(F32PTR Y,F32PTR Xnewterm,F32PTR XtY,F32PTR XtYnew,NEWTERM * new,I32 q);
	U16  GROUP_MatxMat=(MODEL.sid < 0||opt->prior.seasonBasisFuncType  !=3 ) && (MODEL.vid < 0||opt->prior.trendBasisFuncType   !=3 )
		              && (MODEL.tid < 0||opt->prior.trendBasisFuncType   !=2 ) && (MODEL.oid < 0||opt->prior.outlierBasisFuncType !=2 );
	Update_XtX_from_Xnewterm=GROUP_MatxMat ? Update_XtX_from_Xnewterm_ByGroup : Update_XtX_from_Xnewterm_NoGroup;
	Update_XtY_from_Xnewterm=GROUP_MatxMat ? Update_XtY_from_Xnewterm_ByGroup : Update_XtY_from_Xnewterm_NoGroup;
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
		BEAST2_fetch_timeSeries(&yInfo,pixelIndex,MEMBUF,&(opt->io));
		F32PTR  Xtmp=Xt_mars;
		U08     skipCurrentPixel=BEAST2_preprocess_timeSeries(&yInfo,MODEL.b,Xtmp,opt);		
        #if DEBUG_MODE==1
			I32 accS[5]={ 0,},accT[5]={ 0,};
			I32 flagS[5]={ 0,},flagT[5]={ 0,};
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
		ReInit_PrecValues(&MODEL,opt);
		for ( U32 chainNumber=0;  chainNumber < MCMC_CHAINNUM; chainNumber++)
		{
			const I32  N=opt->io.N; 
			const I32  Npad=N;  (N+7)/8 * 8; 
			const I32  Npad16=(N+15)/16 * 16;	
			{   
				GenarateRandomBasis(MODEL.b,MODEL.NUMBASIS,N,&RND,&yInfo);
				BEAST2_EvaluateModel(&MODEL.curr,MODEL.b,Xt_mars,N,MODEL.NUMBASIS,&yInfo,&hyperPar,&MODEL.precState,&precFunc); 
			}
			{
				CvtKnotsToBinVec(MODEL.b,MODEL.NUMBASIS,N,&yInfo);
				memset(MODEL.extremePosVec,1,N);
				for (I32 i=0; i < yInfo.nMissing;++i) MODEL.extremePosVec[yInfo.rowsMissing[i]]=0;
				MODEL.extremPosNum=yInfo.n;
				f32_fill_val(1e30,MODEL.deviation,N);
				for (I32 i=0; i < yInfo.nMissing;++i) MODEL.deviation[yInfo.rowsMissing[i]]=getNaN();
				MODEL.avgDeviation[0]=1.0;
				BEAST2_Result_FillMEM(&resultChain,opt,0);
				MODEL.b[0].Kbase=0;                           
				UpdateBasisKbase(MODEL.b,MODEL.NUMBASIS,0);	
			}
			U32 ite=0;
			U32 sample=0;
			if (extra.computeCredible) { 
				for (int i=0; i < NumCIVars; i++) {
					ci[i].samplesInserted=0;
				} 
			} 
			PROP_DATA PROPINFO={ .N=N,.Npad16=Npad16,.samples=&sample,.keyresult=coreResults,.mem=Xnewterm,.model=&MODEL,
							  .pRND=&RND,.yInfo=&yInfo,.sigFactor=opt->prior.sigFactor,.outlierSigFactor=opt->prior.outlierSigFactor,
							  .nSample_DeviationNeedUpdate=1L,.shallUpdateExtremVec=0L, 
				               .numBasisWithoutOutlier=MODEL.NUMBASIS - (opt->prior.basisType[MODEL.NUMBASIS - 1]==OUTLIERID),};
			NEWTERM   NEW={ .newcols={.N=N,.Nlda=Npad} };
			I32 numBadIterations=0;
			while (sample < MCMC_SAMPLES)
			{
				ite++;
				if (RND.rnd32    >=RND32_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,(RND.rnd32 - RND32),(U32PTR)RND32 ); RND.rnd32=RND32;   }
				if (RND.rnd16    >=RND16_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,((char*)RND.rnd16 - (char*)RND16+3)/sizeof(U32),(U32PTR)RND16 ); RND.rnd16=RND16;   }
				if (RND.rnd08    >=RND08_END)    {r_viRngUniformBits32(VSL_RNG_METHOD_UNIFORMBITS32_STD,stream,((char*)RND.rnd08 - (char*)RND08+3)/sizeof(U32),(U32PTR)RND08 ); RND.rnd08=RND08;   }
				if (RND.rndgamma >=RNDGAMMA_END) {r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,stream,MAX_RAND_NUM,RNDGAMMA,(hyperPar.alpha_1+yInfo.n*0.5f),0.f,1.f  ); RND.rndgamma=RNDGAMMA;}
				BEAST2_BASIS_PTR basis=MODEL.b+MODEL.PickBasisID(&PROPINFO); 
				basis->Propose(basis,&NEW,&PROPINFO);
				#if  DEBUG_MODE==1
					I32 basisIdx=basis - MODEL.b;		
					flagSat[NEW.newKnot - 1]+=basisIdx==0 && (NEW.jumpType==BIRTH||NEW.jumpType==MOVE);					 
					if (basisIdx==0)++(flagS[NEW.jumpType]);
					else++(flagT[NEW.jumpType]); 
					MEM.verify_header(&MEM);
				#endif
				I32 Knewterm=0;				
				for (I32 i=0; i < NEW.numSeg; i++) { 
					I32 kterms=NEW.SEG[i].K=basis->GenTerms(Xnewterm+Npad*Knewterm,N,&NEW.SEG[i],&(basis->bConst));					
					Knewterm+=kterms;
				} 
				NEW.newcols.k2_new=NEW.newcols.k1+Knewterm - 1L;	
				NEW.newcols.k1+=basis->Kbase;		
				NEW.newcols.k2_old+=basis->Kbase;
				NEW.newcols.k2_new+=basis->Kbase;
				int KOLD=MODEL.curr.K;
				int KNEW=MODEL.curr.K+NEW.newcols.k2_new - NEW.newcols.k2_old;
				NEW.newcols.Knewterm=Knewterm;
				NEW.newcols.KOLD=KOLD; 
				NEW.newcols.KNEW=KNEW; 
				if (yInfo.nMissing > 0 && Knewterm > 0 )  
				f32_mat_multirows_extract_set_by_scalar(Xnewterm,Npad,Knewterm,Xt_zeroBackup,yInfo.rowsMissing,yInfo.nMissing,0.0f);
			    #if DEBUG_MODE==1
					MEM.verify_header(&MEM);
				#endif
	 			Update_XtX_from_Xnewterm(Xt_mars,Xnewterm,MODEL.curr.XtX,MODEL.prop.XtX,&NEW,&MODEL); 
				Update_XtY_from_Xnewterm(yInfo.Y,Xnewterm,MODEL.curr.XtY,MODEL.prop.XtY,&NEW,q);
				if (1L) {
					for (I32 i=1; i < NEW.newcols.k1; i++) 	SCPY(i,MODEL.curr.cholXtX+(i - 1) * KOLD,MODEL.prop.cholXtX+(i - 1) * KNEW);
					precFunc.SetPropPrecXtXDiag_NtermsPerGrp(&MODEL,basis,&NEW); 
					precFunc.chol_addCol(MODEL.prop.XtX+(NEW.newcols.k1 - 1) * KNEW,MODEL.prop.cholXtX,MODEL.prop.precXtXDiag,KNEW,NEW.newcols.k1,KNEW);
				}
				MODEL.prop.K=KNEW;
				precFunc.ComputeMargLik(&MODEL.prop,&MODEL.precState,&yInfo,&hyperPar);
				if (IsNaN(MODEL.prop.marg_lik)||IsInf(MODEL.prop.marg_lik)) {
					if (++numBadIterations < 15) {
						precFunc.IncreasePrecValues(&MODEL);
						precFunc.SetPrecXtXDiag(MODEL.curr.precXtXDiag,MODEL.b,MODEL.NUMBASIS,&MODEL.precState);
						precFunc.chol_addCol(MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,MODEL.curr.K,1L,MODEL.curr.K);
						precFunc.ComputeMargLik(&MODEL.curr,&MODEL.precState,&yInfo,&hyperPar);
						continue;
					} else {
						skipCurrentPixel=2;
						break;
					}
				}	else {
					numBadIterations=0;
				} 
				F32 delta_lik=MODEL.prop.marg_lik - MODEL.curr.marg_lik;
				if   ( !(NEW.jumpType==MOVE||basis->type==OUTLIERID) ) {
					F32 factor=basis->ModelPrior(basis,&NEW.newcols,&NEW); 
					delta_lik+=factor;
				}
				I08     acceptTheProposal;
				if      (delta_lik >   0.0f)   acceptTheProposal=1;
				else if (delta_lik < -23.0f)   acceptTheProposal=0;				
				else {				 
					F32    expValue=fastexp(delta_lik);
					if     (delta_lik > -0.5f)    acceptTheProposal=*(RND.rnd08)++< expValue * 255.0f;
					else if(delta_lik > -5.0f  )  acceptTheProposal=*(RND.rnd16)++< expValue * 65535.0f;
					else                          acceptTheProposal=*(RND.rnd32)++< expValue * 4.294967296e+09;					 
				}
				#if DEBUG_MODE==1
                     if (basisIdx==0)++(flagS[NEW.jumpType]);
                     else++(flagT[NEW.jumpType]);
                     MEM.verify_header(&MEM);
				#endif
				if(acceptTheProposal)
				{
					#if DEBUG_MODE==1
						if (basisIdx==0)++(accS[NEW.jumpType]);
						else++(accT[NEW.jumpType]);
					#endif
					if (yInfo.nMissing > 0 && Knewterm > 0 )  
						f32_mat_multirows_set_by_submat(Xnewterm,Npad,Knewterm,Xt_zeroBackup,yInfo.rowsMissing,yInfo.nMissing);
					if (NEW.newcols.k2_old !=KOLD && NEW.newcols.k2_new !=NEW.newcols.k2_old)
						shift_lastcols_within_matrix(Xt_mars,Npad,NEW.newcols.k2_old+1,KOLD,NEW.newcols.k2_new+1);
					if (Knewterm !=0)
						SCPY(Knewterm*Npad,Xnewterm,Xt_mars+(NEW.newcols.k1-1) * Npad);
					 (basis->KNOT[-1]=basis->KNOT[INDEX_FakeStart],basis->KNOT[basis->nKnot]=basis->KNOT[INDEX_FakeEnd]);
					 basis->UpdateGoodVec_KnotList(basis,&NEW,Npad16);
					 (basis->KNOT[-1]=1,basis->KNOT[basis->nKnot]=N+1L);
					basis->CalcBasisKsKeK_TermType(basis);
					UpdateBasisKbase(MODEL.b,MODEL.NUMBASIS,basis-MODEL.b);
					{   
						#define Exchange(x)          {VOIDPTR tmp=MODEL.curr.x;  MODEL.curr.x=MODEL.prop.x; MODEL.prop.x=tmp;}
						#define Exchange2(x,y)       Exchange(x) Exchange(y) 
						#define Exchange4(x,y,z,k)   Exchange(x) Exchange(y) Exchange(z) Exchange(k)
						Exchange4(XtX,XtY,cholXtX,beta_mean);   
						Exchange2(precXtXDiag,nTermsPerPrecGrp);  
						Exchange(alpha2Q_star);                    
						MODEL.curr.marg_lik=MODEL.prop.marg_lik;
						MODEL.curr.K=MODEL.prop.K;     
					}
					#if  DEBUG_MODE==1 
					if (q==1) {
					}
					else {
						BEAST2_EvaluateModel(&MODEL.prop,MODEL.b,Xdebug,N,MODEL.NUMBASIS,&yInfo,&hyperPar,MODEL.precState.precVec,&stream);
					}
					#endif
				} 
				const int bResampleParameter=(ite >=100)         && (ite%20==0)     ;      
				const int bStoreCurrentSample=(ite > MCMC_BURNIN) && (ite%MCMC_THINNING==0);
				if (q==1) {
					if (bResampleParameter||bStoreCurrentSample)	{	 
						F32 sig2_inv=(*RND.rndgamma++) * 1.f/MODEL.curr.alpha2Q_star[0];
						F32 sig2=1.0f/sig2_inv;
						MODEL.sig2[0]=sig2 > MIN_SIG2_VALUE ? sig2 : MODEL.sig2[0];
					}	 
					if (bResampleParameter||(bStoreCurrentSample && extra.useRndBeta)) {
							I32 K=MODEL.curr.K;
							r_vsRngGaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF,stream,K,MODEL.beta,0,1);
							solve_U_as_U_invdiag(MODEL.curr.cholXtX,MODEL.beta,K,K);
							r_ippsMulC_32f_I(fastsqrt(MODEL.sig2[0]),MODEL.beta,K);
							r_ippsAdd_32f_I(MODEL.curr.beta_mean,MODEL.beta,K);
					}
					if (bResampleParameter ) {
						I32 ntries=0;
						do {
							if (ntries++==0)	precFunc.ResamplePrecValues(&MODEL,&hyperPar,&stream);
							else				precFunc.IncreasePrecValues(&MODEL);
							precFunc.SetPrecXtXDiag(MODEL.curr.precXtXDiag,MODEL.b,MODEL.NUMBASIS,&MODEL.precState);
							precFunc.chol_addCol(MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,MODEL.curr.K,1L,MODEL.curr.K);
							precFunc.ComputeMargLik(&MODEL.curr,&MODEL.precState,&yInfo,&hyperPar);
						} while (IsNaN(MODEL.curr.marg_lik) && ntries < 20);
						if (IsNaN(MODEL.curr.marg_lik)) {
                             #if !(defined(R_RELEASE)||defined(M_RELEASE)||defined(P_RELEASE)) 
							 r_printf("skip3|prec: %.4f|marg_lik_cur: %.4f \n",MODEL.precState.precVec[0],MODEL.curr.marg_lik);
                             #endif
							skipCurrentPixel=3;
							break;
						}
					}
				} 
				if (q !=1) {    
					F32PTR MEMBUF=Xnewterm;
					I32    K=MODEL.curr.K;
					if (bResampleParameter||bStoreCurrentSample) {						
						local_pcg_invwishart_upper(&stream,MODEL.sig2,MODEL.sig2+q * q,MEMBUF,q,MODEL.curr.alpha2Q_star,hyperPar.alpha_1+yInfo.n+q - 1);
					}
					if (bResampleParameter||(bStoreCurrentSample && extra.useRndBeta)) {
						r_vsRngGaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF,stream,K * q,MEMBUF,0.,1.);
						r_cblas_sgemm(CblasColMajor,CblasNoTrans,CblasNoTrans,K,q,q,1.0,MEMBUF,K,MODEL.sig2,q,0.f,MODEL.beta,K);
						solve_U_as_U_invdiag_multicols(MODEL.curr.cholXtX,MODEL.beta,K,K,q);
						r_ippsAdd_32f_I(MODEL.curr.beta_mean,MODEL.beta,K * q);
					}
					if (bResampleParameter )		{						
						I32 ntries=0;
						do {
							if (ntries++==0) {
								r_cblas_sgemm(CblasColMajor,CblasNoTrans,CblasNoTrans,K,q,q,1.0,MODEL.beta,K,MODEL.sig2+q * q,q,0.f,MEMBUF,K);
								F32 sumq=DOT(K * q,MEMBUF,MEMBUF);
								r_vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE,stream,1L,MODEL.precState.precVec,(hyperPar.del_1+K * q * 0.5f),0.f,1.f);
								MODEL.precState.precVec[0]=MODEL.precState.precVec[0]/(hyperPar.del_2+0.5f * sumq);
								MODEL.precState.logPrecVec[0]=logf(MODEL.precState.precVec[0]);
							}	else {
								precFunc.IncreasePrecValues(&MODEL);
							}
							precFunc.SetPrecXtXDiag(MODEL.curr.precXtXDiag,MODEL.b,MODEL.NUMBASIS,&MODEL.precState);
							precFunc.chol_addCol(MODEL.curr.XtX,MODEL.curr.cholXtX,MODEL.curr.precXtXDiag,K,1,K);
							precFunc.ComputeMargLik(&MODEL.curr,&MODEL.precState,&yInfo,&hyperPar);
						} while (IsNaN(MODEL.curr.marg_lik) && ntries < 20);
						if (IsNaN(MODEL.curr.marg_lik)) {
                            #if !(defined(R_RELEASE)||defined(M_RELEASE)||defined(P_RELEASE))
							 r_printf("skip4|prec: %.4f|marg_lik_cur: %.4f \n",MODEL.precState.precVec[0],MODEL.curr.marg_lik);
                            #endif
							skipCurrentPixel=3;
							break;
						}
					} 
				} 
				if (!bStoreCurrentSample) continue;
				#if DEBUG_MODE==1
					MEM.verify_header(&MEM);
				#endif
				sample++;
				*resultChain.marg_lik+=MODEL.curr.marg_lik;
				if (q==1) {
					resultChain.sig2[0]+=MODEL.sig2[0];
				}	else {
					F32PTR MEMBUF=Xnewterm;
					r_cblas_sgemm(CblasColMajor,CblasTrans,CblasNoTrans,q,q,q,1.f,MODEL.sig2,q,MODEL.sig2,q,0.f,MEMBUF,q);
					r_ippsAdd_32f_I(MEMBUF,resultChain.sig2,q*q);
				}
				const F32PTR BETA=(extra.useRndBeta==0) ? MODEL.curr.beta_mean : MODEL.beta;
				{
					 F32PTR MEMBUF1=Xnewterm;
					for (I32 i=0; i < MODEL.NUMBASIS;++i) 
					{
						const BEAST2_BASIS_PTR  basis=MODEL.b+i;
						const CORESULT        * result=coreResults+i;
						const I32        nKnot=basis->nKnot;
						const TKNOT_PTR  KNOT=basis->KNOT;
						result->xNProb[nKnot]+=1L;
						for (I32 j=0; j < nKnot; j++) result->xProb[ KNOT[j]-1 ]+=1L;
						if (result->xorder !=NULL) {
							for (I32 j=0; j <=nKnot;++j) {
								I32 r1=KNOT[j-1],r2=KNOT[j]-1;
								r_ippsAddC_32s_ISfs(basis->ORDER[j],result->xorder+r1 - 1,r2 - r1+1,0);
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
					const F32PTR           MEMBUF1=Xnewterm+3*Npad;
					const BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.sid];
					const I32              knotNum=basis->nKnot;
					const TKNOT_PTR        knotList=basis->KNOT;
					F32PTR             beta=BETA;
					const TORDER_PTR   orderList=basis->ORDER;
					const F32PTR       SEASON_SQR_CSUM=basis->bConst.season.SQR_CSUM+1L;  
					const F32PTR       SEASON_SCALE=basis->bConst.season.SCALE_FACTOR;
					if (SEASON_BTYPE==0) {
						for (I32 i=0; i <=knotNum; i++) {
							const I32	r1=knotList[i - 1];
							const I32	r2=knotList[i] - 1;							
							const I32   order2=orderList[i] * 2L;
							F32PTR seasonSqrCsum=SEASON_SQR_CSUM;							
							F32    amp=0;
							for (I32 j=0; j < order2; j++) {
								F32 scalingFactor=N/(seasonSqrCsum[r2 - 1] - seasonSqrCsum[(r1 - 1) - 1]);
								F32 beta0=beta[j]* SEASON_SCALE[j];
								amp=amp+(beta0 * beta0) * scalingFactor;
								seasonSqrCsum+=(N+1LL);
							}			 
							r_ippsSet_32f(amp,MEMBUF1+r1 - 1,r2 - r1+1L);
							beta+=order2;
						}
					} else {
						for (I32 i=0; i <=knotNum; i++) {
							const I32	r1=knotList[i - 1];
							const I32	r2=knotList[i] - 1;							
							const I32   order2=orderList[i] * 2L;			
							F32    amp=0;		 
							for (I32 j=0; j < order2;++j) {
								F32    beta0=beta[j] * SEASON_SCALE[j];
								amp+=beta0 * beta0;
							}					 
							r_ippsSet_32f(amp,MEMBUF1+r1 - 1,r2 - r1+1L);
							beta+=order2;
						}
					}
					r_ippsAdd_32f_I(MEMBUF1,resultChain.samp,N);
					r_ippsMul_32f_I(MEMBUF1,MEMBUF1,N);
					r_ippsAdd_32f_I(MEMBUF1,resultChain.sampSD,N); 
					if (extra.tallyPosNegSeasonJump) { 			 
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
					const BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.tid];
					const I32              knotNum=basis->nKnot;
					const TKNOT_PTR        knotList=basis->KNOT;
					const F32PTR TREND=Xnewterm+Npad * MODEL.tid;      
					const F32PTR SLP=Xnewterm+Npad * MODEL.NUMBASIS; 
					f32_diff_back(TREND,SLP,N);
					f32_add_v_v2_vec_inplace(SLP,resultChain.tslp,resultChain.tslpSD,N); 
					i32_increment_vec2_bycond_inplace(resultChain.tslpSgnPosPr,resultChain.tslpSgnZeroPr,SLP,N); 
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
				if(extra.tallyPosNegOutliers) {
					const BEAST2_BASIS_PTR basis=&MODEL.b[MODEL.oid];
					const I32              knotNum=basis->nKnot;
					const TKNOT_PTR        knotList=basis->KNOT;
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
					if ( !extra.fastCIComputation||*RND.rnd16++< ciParam.subsampleFraction_x_INT16MAX  ) {
						for (int i=0; i <  NumCIVars; i++) 
							InsertNewRowToUpdateCI(&ciParam,&ci[i]);						
					}					
				} 
			}
			if (!skipCurrentPixel)
			{
				int   sum;
				#define GetSum(arr) (r_ippsSum_32s_Sfs(arr,N,&sum,0),sum) 
				I32   sMAXNUMKNOT=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.maxKnotNum:-9999999; 
				I32   tMAXNUMKNOT=MODEL.tid>=0?  MODEL.b[MODEL.tid].prior.maxKnotNum:-9999999;
				I32   oMAXNUMKNOT=MODEL.oid>=0?  MODEL.b[MODEL.oid].prior.maxKnotNum:- 9999999;
				F32   inv_sample=1.f/sample;			
				*resultChain.marg_lik=*resultChain.marg_lik * inv_sample;
				for (int col=0; col < q; col++)	{ 
					for (int i=0; i < q; i++) {
						resultChain.sig2[col*q+i]=resultChain.sig2[col*q+i] * inv_sample * yInfo.sd[col] * yInfo.sd[i];
					}
				}
				if (MODEL.sid >=0||MODEL.vid >=0) {
						*resultChain.sncp=GetSum(resultChain.scpOccPr)* inv_sample; 
						i32_to_f32_scaleby_inplace(resultChain.sncpPr,(sMAXNUMKNOT+1),inv_sample);
						i32_to_f32_scaleby_inplace(resultChain.scpOccPr,N,inv_sample);	
						for (int i=0; i < q; i++) {
							F32 offset=0.0f;
							f32_sx_sxx_to_avgstd_inplace(resultChain.sY+i * N,resultChain.sSD+i * N,sample,yInfo.sd[i],offset,N);
						}
						if (extra.computeSeasonOrder) i32_to_f32_scaleby_inplace(resultChain.sorder,N,inv_sample);
						if (extra.computeSeasonAmp) {
							for (int i=0; i < q; i++) {
								F32 offset=0.0f;
								f32_sx_sxx_to_avgstd_inplace(resultChain.samp+i*N,resultChain.sampSD+i*N,sample,yInfo.sd[i]* yInfo.sd[i],offset,N);
							}							
						}
						if (extra.computeCredible) {
							for (int i=0; i < q; i++) {								
								r_ippsMulC_32f_I(yInfo.sd[i],resultChain.sCI+N*i,N);
								r_ippsMulC_32f_I(yInfo.sd[i],resultChain.sCI+N*q+N*i,N);
							}							
						} 
				}
				if (MODEL.tid >=0) {
						*resultChain.tncp=GetSum(resultChain.tcpOccPr)*inv_sample ;
						i32_to_f32_scaleby_inplace(resultChain.tncpPr,(tMAXNUMKNOT+1),inv_sample);
						i32_to_f32_scaleby_inplace(resultChain.tcpOccPr,N,inv_sample);
						for (int i=0; i < q; i++) {
							F32 offset=0.0f;
							f32_sx_sxx_to_avgstd_inplace(resultChain.tY+i * N,resultChain.tSD+i * N,sample,yInfo.sd[i],yInfo.mean[i],N);
						}
						if (extra.computeTrendOrder) 	i32_to_f32_scaleby_inplace(resultChain.torder,N,inv_sample);	
						if (extra.computeTrendSlope) {
							for (int i=0; i < q; i++) {
								f32_sx_sxx_to_avgstd_inplace(resultChain.tslp+i*N,resultChain.tslpSD+i*N,sample,yInfo.sd[i]/opt->io.meta.deltaTime,0,N);
							}							
							i32_to_f32_scaleby_inplace(resultChain.tslpSgnPosPr,N*q,inv_sample);
							i32_to_f32_scaleby_inplace(resultChain.tslpSgnZeroPr,N*q,inv_sample);
						}
						if (extra.computeCredible) {
							for (int i=0; i < q; i++) {
								f32_scale_inplace(yInfo.sd[i],yInfo.mean[i],resultChain.tCI+N * i,N);
								f32_scale_inplace(yInfo.sd[i],yInfo.mean[i],resultChain.tCI+N*q+N * i,N);
							}	
							if (extra.computeTrendSlope) {
								for (int i=0; i < q; i++) {
									f32_mul_val_inplace(yInfo.sd[i],resultChain.tslpCI+N * i,N);
									f32_mul_val_inplace(yInfo.sd[i],resultChain.tslpCI+N * q+N * i,N);
								}						
							}							
						}
				}
				if (MODEL.oid >=0) {					
					 *resultChain.oncp=inv_sample * GetSum(resultChain.ocpOccPr);
					 i32_to_f32_scaleby_inplace(resultChain.oncpPr,(oMAXNUMKNOT+1),inv_sample);
					 i32_to_f32_scaleby_inplace(resultChain.ocpOccPr,N,inv_sample);
					 for (int i=0; i < q; i++) {
						 f32_sx_sxx_to_avgstd_inplace(resultChain.oY+i*N,resultChain.oSD+i*N,sample,yInfo.sd[i],0,N);
					 }					 
					 if (extra.computeCredible) {
						 for (int i=0; i < q; i++) {
							 r_ippsMulC_32f_I(yInfo.sd[i],resultChain.oCI+i*N,N );
							 r_ippsMulC_32f_I(yInfo.sd[i],resultChain.oCI+N*q+i*N,N);
						 }						 
					 }	
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
					GetSum(resultChain.tinc_cpOccPr);	   
					*resultChain.tinc_ncp=inv_sample * sum; 
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
				I32   sMAXNUMKNOT=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.maxKnotNum : -9999999;
				I32   tMAXNUMKNOT=MODEL.tid>=0?  MODEL.b[MODEL.tid].prior.maxKnotNum:-9999999;
				I32   oMAXNUMKNOT=MODEL.oid>=0?  MODEL.b[MODEL.oid].prior.maxKnotNum:- 9999999;
				#define _1(x)      *(result.x)+=*(resultChain.x)
				#define _N(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N)
				#define _Nq(x)     r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N*q)
				#define _q(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,q)
				#define _q2(x)      r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,q*q)
				#define _2N(x)     r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N+N)
				#define _2Nq(x)     r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,N*q+N*q)
				#define _skn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,sMAXNUMKNOT+1)
				#define _tkn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,tMAXNUMKNOT+1)
				#define _okn_1(x)  r_ippsAdd_32f_I((F32PTR)resultChain.x,(F32PTR)result.x,oMAXNUMKNOT+1)
				_1(marg_lik);
				_q2(sig2);  
				if (MODEL.sid >=0||MODEL.vid >0) {
					_1(sncp); _skn_1(sncpPr);	     _N(scpOccPr); _Nq(sY); _Nq(sSD);
					if (extra.computeSeasonOrder)    _N(sorder);
					if (extra.computeSeasonAmp)      _N(samp),_N(sampSD);
					if (extra.computeCredible)       _2Nq(sCI);
				}
				if (MODEL.tid >=0) {					
					_1(tncp); _tkn_1(tncpPr);	   _N(tcpOccPr); _Nq(tY); _Nq(tSD);
					if (extra.computeTrendOrder)   _N(torder);
					if (extra.computeTrendSlope)   _N(tslp),_N(tslpSD),_N(tslpSgnPosPr),_N(tslpSgnZeroPr);
					if (extra.computeCredible)     _2Nq(tCI);
					if (extra.computeCredible && extra.computeTrendSlope ) _2Nq(tslpCI);
				}
				if (MODEL.oid >=0) {
					_1(oncp); _okn_1(oncpPr);	_N(ocpOccPr); _Nq(oY); _Nq(oSD);
					if (extra.computeCredible)   _2Nq(oCI);
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
					_okn_1(opos_ncpPr);      _okn_1(oneg_ncpPr); 
					_N(opos_cpOccPr);        _N(oneg_cpOccPr); 
				}	
				#undef _1
				#undef _N
				#undef _Nq 
				#undef _q 
				#undef _q2
				#undef _2N
				#undef _2Nq
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
			I32   sMAXNUMKNOT=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.maxKnotNum : -9999999;
			I32   tMAXNUMKNOT=MODEL.tid >=0 ? MODEL.b[MODEL.tid].prior.maxKnotNum : -9999999;
			I32   oMAXNUMKNOT=MODEL.oid >=0 ? MODEL.b[MODEL.oid].prior.maxKnotNum : -9999999;
			const F32 invChainNumber=1.f/(F32)MCMC_CHAINNUM;
			#define _1(x)      *((F32PTR)result.x)*=invChainNumber
			#define _N(x)      r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N)
			#define _Nq(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N*q)
			#define _q(x)      r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,q)
			#define _q2(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,q*q)
			#define _2N(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N+N)
			#define _2Nq(x)     r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,N*q+N*q)
			#define _skn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,sMAXNUMKNOT+1)
			#define _tkn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,tMAXNUMKNOT+1)
			#define _okn_1(x)  r_ippsMulC_32f_I(invChainNumber,(F32PTR)result.x,oMAXNUMKNOT+1)
			F32 maxncpProb;	 
			_1(marg_lik);			
			_q2(sig2);
			if (MODEL.sid >=0||MODEL.vid>0) {
				_1(sncp); _skn_1(sncpPr);	     _N(scpOccPr); _Nq(sY); _Nq(sSD); 
				if (extra.computeSeasonOrder)    _N(sorder);
				if (extra.computeSeasonAmp)     {_N(samp),_N(sampSD);}
				if (extra.computeCredible)       _2Nq(sCI);
				*result.sncp_mode=f32_maxidx(result.sncpPr,sMAXNUMKNOT+1,&maxncpProb);
				*result.sncp_median=GetPercentileNcp(result.sncpPr,sMAXNUMKNOT+1,0.5);
				*result.sncp_pct90=GetPercentileNcp(result.sncpPr,sMAXNUMKNOT+1,0.9);
				*result.sncp_pct10=GetPercentileNcp(result.sncpPr,sMAXNUMKNOT+1,0.1);
			}
			if (MODEL.tid >=0) {
				_1(tncp); _tkn_1(tncpPr);	     _N(tcpOccPr); _Nq(tY); _Nq(tSD); 
				if (extra.computeTrendOrder)     _N(torder);
				if (extra.computeTrendSlope)    { _N(tslp),_N(tslpSD),_N(tslpSgnPosPr),_N(tslpSgnZeroPr);}
				if (extra.computeCredible)       _2Nq(tCI);
				*result.tncp_mode=f32_maxidx(result.tncpPr,tMAXNUMKNOT+1,&maxncpProb);
				*result.tncp_median=GetPercentileNcp(result.tncpPr,tMAXNUMKNOT+1,0.5);
				*result.tncp_pct90=GetPercentileNcp(result.tncpPr,tMAXNUMKNOT+1,0.9);
				*result.tncp_pct10=GetPercentileNcp(result.tncpPr,tMAXNUMKNOT+1,0.1);
			}
			if (MODEL.oid >=0) {
				_1(oncp); _okn_1(oncpPr);	   
				_N(ocpOccPr); _Nq(oY); _Nq(oSD);
				if (extra.computeCredible)      _2Nq(oCI);
				*result.oncp_mode=f32_maxidx(result.oncpPr,oMAXNUMKNOT+1,&maxncpProb);
				*result.oncp_median=GetPercentileNcp(result.oncpPr,oMAXNUMKNOT+1,0.5);
				*result.oncp_pct90=GetPercentileNcp(result.oncpPr,oMAXNUMKNOT+1,0.9);
				*result.oncp_pct10=GetPercentileNcp(result.oncpPr,oMAXNUMKNOT+1,0.1);
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
				_okn_1(opos_ncpPr); _okn_1(oneg_ncpPr); 
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
			if (MODEL.sid >=0||MODEL.vid>0) 				tsRemoveNaNs(result.sSD,N);
			if (MODEL.tid >=0)								tsRemoveNaNs(result.tSD,N);
			if (MODEL.tid >=0 && extra.computeTrendSlope)  tsRemoveNaNs(result.tslpSD,N);		 
			if (MODEL.oid >=0) 							tsRemoveNaNs(result.oSD,N);
		}
		if (!skipCurrentPixel)
		{
			I32     N=opt->io.N;
			F32     nan=getNaN();     
			F32  	threshold=0.01f;
			F32PTR	mem=Xnewterm;  
			I32PTR  cptList=(I32PTR)mem+5LL * N;
			F32PTR  cptCIList=(F32PTR)mem+6LL * N;
			I32   cptNumber;
			I32   trueCptNumber;
			F32   maxncpProb;
			const F32 T0=(F32)opt->io.meta.startTime;
			const F32 dT=(F32)opt->io.meta.deltaTime;
			I32   sMAXNUMKNOT=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.maxKnotNum : -9999999;
			I32   tMAXNUMKNOT=MODEL.tid >=0 ? MODEL.b[MODEL.tid].prior.maxKnotNum : -9999999;
			I32   oMAXNUMKNOT=MODEL.oid >=0 ? MODEL.b[MODEL.oid].prior.maxKnotNum : -9999999;
			I32   sMINSEPDIST=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.minSepDist : -9999999;
			I32   tMINSEPDIST=MODEL.tid >=0                   ? MODEL.b[MODEL.tid].prior.minSepDist : -9999999;
			I32   oMINSEPDIST=MODEL.oid >=0                   ? 0 : -9999999;  
			I32   sLeftMargin=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.leftMargin : -9999999;
			I32   tLeftMargin=MODEL.tid >=0                   ? MODEL.b[MODEL.tid].prior.leftMargin : -9999999;
			I32   oLeftMargin=MODEL.oid >=0                   ? 0 : -9999999;  
			I32   sRightMargin=MODEL.sid >=0||MODEL.vid >=0 ? MODEL.b[0].prior.rightMargin : -9999999;
			I32   tRightMargin=MODEL.tid >=0                   ? MODEL.b[MODEL.tid].prior.rightMargin : -9999999;
			I32   oRightMargin=MODEL.oid >=0                   ? 0 : -9999999;  
			if (extra.computeSeasonChngpt && (MODEL.sid >=0||MODEL.vid>0 ))  	{
				cptNumber=sMAXNUMKNOT;
				trueCptNumber=FindChangepoint_LeftRightMargins((F32PTR)result.scpOccPr,mem,threshold,cptList,cptCIList,N,sMINSEPDIST,cptNumber,sLeftMargin,sRightMargin);
				for (int i=0; i < trueCptNumber; i++) {
					*(result.scp+i)=(F32)(*(cptList+i)) * dT+T0;
					*(result.scpPr+i)=(F32)mem[i];
					I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];
					for (int j=0; j < q;++j) {
						*(result.scpAbruptChange+j*sMAXNUMKNOT+i)=result.sY[j*N+cptLoc] - result.sY[j*N+cptLoc - 1];
					}					
				}
				for (int i=trueCptNumber; i < sMAXNUMKNOT; i++) {
					*(result.scp+i)=nan,
					*(result.scpPr+i)=nan;
					for (int j=0; j < q;++j) {
						*(result.scpAbruptChange+j * sMAXNUMKNOT+i)=nan;
					}					
				}
				for (int i=0; i < trueCptNumber; i++)
					*(result.scpCI+i)=(F32)(*(cptCIList+i)) * dT+T0,
					*(result.scpCI+sMAXNUMKNOT+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;
				for (int i=trueCptNumber; i < sMAXNUMKNOT; i++)
					*(result.scpCI+i)=nan,
					*(result.scpCI+sMAXNUMKNOT+i)=nan;
			}
			if (extra.computeTrendChngpt) {
				cptNumber=tMAXNUMKNOT;
				trueCptNumber=FindChangepoint_LeftRightMargins((F32PTR)result.tcpOccPr,mem,threshold,cptList,cptCIList,N,tMINSEPDIST,cptNumber,tLeftMargin,tRightMargin);
				for (int i=0; i < trueCptNumber; i++) {
					*(result.tcp+i)=(F32)(*(cptList+i)) * dT+T0,
					*(result.tcpPr+i)=(F32)mem[i];
					I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];
					for (int j=0; j < q;++j) {
						*(result.tcpAbruptChange+j*tMAXNUMKNOT+i)=result.tY[j * N+cptLoc] - result.tY[j * N+cptLoc - 1];
					}
				}
				for (int i=trueCptNumber; i < tMAXNUMKNOT; i++) {
					*(result.tcp+i)=nan,
					*(result.tcpPr+i)=nan;
					for (int j=0; j < q;++j) {
						*(result.tcpAbruptChange+j * tMAXNUMKNOT+i)=nan;
					}					
				}					
				for (int i=0; i < trueCptNumber; i++)
					*(result.tcpCI+i)=(F32)(*(cptCIList+i)) * dT+T0,
					*(result.tcpCI+tMAXNUMKNOT+i)=(F32)(*(cptCIList+trueCptNumber+i)) * dT+T0;
				for (int i=trueCptNumber; i < tMAXNUMKNOT; i++)
					*(result.tcpCI+i)=nan,
					*(result.tcpCI+tMAXNUMKNOT+i)=nan;
			}
			#define GET_CHANGPOINTS(NcpProb,KNOTNUM,MINSEP,LeftMargin,RightMargin,MAX_KNOTNUM,Y,CpOccPr,CP,CPPROB,CP_CHANGE,CP_CI)    \
			cptNumber=MAX_KNOTNUM;  \
			trueCptNumber=FindChangepoint_LeftRightMargins((F32PTR)CpOccPr,mem,threshold,cptList,cptCIList,N,MINSEP,cptNumber,LeftMargin,RightMargin);\
			for (int i=0; i < trueCptNumber; i++) {\
				*(CP+i)=(F32) cptList[i]* dT+T0,\
				*(CPPROB+i)=(F32) mem[i];\
		         I32 cptLoc=cptList[i]==0 ? 1 : cptList[i];\
				 *(CP_CHANGE+i)=Y[cptLoc] - Y[cptLoc - 1];\
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
			if (extra.tallyPosNegSeasonJump && MODEL.sid >=0) {
				GET_CHANGPOINTS(result.spos_ncpPr,result.spos_ncp,sMINSEPDIST,sLeftMargin,sRightMargin,sMAXNUMKNOT,result.samp,
					result.spos_cpOccPr,result.spos_cp,result.spos_cpPr,result.spos_cpAbruptChange,result.spos_cpCI);
				GET_CHANGPOINTS(result.sneg_ncpPr,result.sneg_ncp,sMINSEPDIST,sLeftMargin,sRightMargin,sMAXNUMKNOT,result.samp,
					result.sneg_cpOccPr,result.sneg_cp,result.sneg_cpPr,result.sneg_cpAbruptChange,result.sneg_cpCI);
			}			
			if (extra.tallyPosNegTrendJump) {
				GET_CHANGPOINTS(result.tpos_ncpPr,result.tpos_ncp,tMINSEPDIST,tLeftMargin,tRightMargin,tMAXNUMKNOT,result.tY,
					result.tpos_cpOccPr,result.tpos_cp,result.tpos_cpPr,result.tpos_cpAbruptChange,result.tpos_cpCI);
				GET_CHANGPOINTS(result.tneg_ncpPr,result.tneg_ncp,tMINSEPDIST,tLeftMargin,tRightMargin,tMAXNUMKNOT,result.tY,
					result.tneg_cpOccPr,result.tneg_cp,result.tneg_cpPr,result.tneg_cpAbruptChange,result.tneg_cpCI);
			}
			if (extra.tallyIncDecTrendJump) {
				GET_CHANGPOINTS(result.tinc_ncpPr,result.tinc_ncp,tMINSEPDIST,tLeftMargin,tRightMargin,tMAXNUMKNOT,result.tslp,
					result.tinc_cpOccPr,result.tinc_cp,result.tinc_cpPr,result.tinc_cpAbruptChange,result.tinc_cpCI);
				GET_CHANGPOINTS(result.tdec_ncpPr,result.tdec_ncp,tMINSEPDIST,tLeftMargin,tRightMargin,tMAXNUMKNOT,result.tslp,
					result.tdec_cpOccPr,result.tdec_cp,result.tdec_cpPr,result.tdec_cpAbruptChange,result.tdec_cpCI);
			}
			#define OGET_CHANGPOINTS(NcpProb,KNOTNUM,MINSEP,LeftMargin,RightMargin,MAX_KNOTNUM,PROBCURVE,CP,CPPROB,CP_CI)    \
			cptNumber=MAX_KNOTNUM;  \
			trueCptNumber=FindChangepoint_LeftRightMargins((F32PTR)PROBCURVE,mem,threshold,cptList,cptCIList,N,MINSEP,cptNumber,LeftMargin,RightMargin);\
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
			if (extra.computeOutlierChngpt) {
				OGET_CHANGPOINTS(result.oncpPr,result.oncp,oMINSEPDIST,oLeftMargin,oRightMargin,oMAXNUMKNOT,result.ocpOccPr,result.ocp,result.ocpPr,result.ocpCI);
			}
			if (extra.tallyPosNegOutliers && MODEL.oid >=0) {
				OGET_CHANGPOINTS(result.opos_ncpPr,result.opos_ncp,oMINSEPDIST,oLeftMargin,oRightMargin,oMAXNUMKNOT,
					result.opos_cpOccPr,result.opos_cp,result.opos_cpPr,result.opos_cpCI);
				OGET_CHANGPOINTS(result.oneg_ncpPr,result.oneg_ncp,oMINSEPDIST,oLeftMargin,oRightMargin,oMAXNUMKNOT,
					result.oneg_cpOccPr,result.oneg_cp,result.oneg_cpPr,result.oneg_cpCI);
			}
		}
		if ( !skipCurrentPixel) {	
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
			F32PTR BUF=yInfo.Y;
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
		BEAST2_WriteOutput(opt,&result,pixelIndex);
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
		#if DEBUG_MODE==1
		r_printf("TREND: birth%4d/%-5d|death%4d/%-5d|merge%4d/%-5d|move%4d/%-5d|chorder%4d/%-5d\n", 
			      accT[0],flagT[0],accT[1],flagT[1],accT[2],flagT[2],accT[3],flagT[3],accT[4],flagT[4]);
		r_printf("SEASN: birth%4d/%-5d|death%4d/%-5d|merge%4d/%-5d|move%4d/%-5d|chorder%4d/%-5d\n",
			      accS[0],flagS[0],accS[1],flagS[1],accS[2],flagS[2],accS[3],flagS[3],accS[4],flagS[4]);
		#endif
		if (IDE_USER_INTERRUPT)		{
			break;
		}
	} 
	r_vslDeleteStream(&stream);
	MEM.free_all(&MEM);
	pthread_t  id=pthread_self();
	pthread_mutex_lock(&mutex);
	pthread_mutex_unlock(&mutex);
	return 1;
} 
#include "abc_000_warning.h"
