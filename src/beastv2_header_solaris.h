#pragma once
#include "abc_000_macro.h"
#include "abc_datatype.h"   
#include "abc_ide_util.h"
#if  R_INTERFACE==1
#ifdef beta
#undef beta  
#endif
#endif
#define _IN_ 
#define _OUT_
#define MAX_RAND_NUM	  5000L
#define MAX_NUM_BASIS     3
#define MIN_PREC_VALUE    0.001
#define MIN_SIG2_VALUE    0.001
#define MIN_ALPHA2_VALUE  0.0001
#define PROB_SAMPLE_EXTREME_VECTOR 0.5
#define SEASONID   0
#define TRENDID    1
#define OUTLIERID  2 
#define DUMMYID    3
#define SVDID      4
#define A(xxx)     BEAST2_##xxx
typedef U32 TKNOT,*_restrict TKNOT_PTR;
typedef U08 TORDER,*_restrict TORDER_PTR;
#define rTKNOT_PTR   register  TKNOT_PTR
#define rTORDER_PTR  register  TORDER_PTR
#define rTKNOT       register  TKNOT
#define rTORDER      register  TORDER
typedef struct BEAST2_METADATA {
	VOID_PTR rawInput;
	VOID_PTR rawTimeVec;	
	I08      detrend;
	I08      deseasonalize;
	I08		 nrhs;
	I08		 isMetaStruct;
	I08		 isRegularOrdered;	
	I08      seasonForm;
	I08      hasSeasonCmpnt;	
	I08      hasOutlierCmpnt;	
	I08    whichDimIsTime;
	F32    period;
	F32    missingValue;
	F32    startTime,deltaTime;	
	F32    maxMissingRate;
	F32PTR svdTerms;
} BEAST2_METADATA,* _restrict BEAST2_METADATA_PTR;
typedef enum { ConstPrec,UniformPrec,ComponentWise,OrderWise} PRECPRIOR_TYPE;
typedef struct BEAST2_HyperPar {
	F32 alpha_1,alpha_2,del_1,del_2;
} BEAST2_HyperPar,* _restrict BEAST2_HyperPar_PTR;
typedef struct BEAST2_PRIOR {
	I08				basisType[MAX_NUM_BASIS];			
	I08				numBasis;
	U08				seasonBasisFuncType;
	U08				trendBasisFuncType;
	U08				outlierBasisFuncType;
	U08				modelPriorType;
	PRECPRIOR_TYPE  precPriorType;
	U08   seasonMinOrder,seasonMaxOrder,trendMinOrder,trendMaxOrder;
	I16   trendMinSepDist,seasonMinSepDist;
	U16   trendMinKnotNum,seasonMinKnotNum;
	U16   trendMaxKnotNum,seasonMaxKnotNum;
	I16   outlierMaxKnotNum;
	U16   K_MAX;
	F32   sig2;
	F32	  precValue;
	F32   alpha1,alpha2,delta1,delta2;
} BEAST2_PRIOR,* _restrict BEAST2_PRIOR_PTR;
typedef struct BEAST2_MCMC {
	U64   seed;	                  
	F32   credIntervalAlphaLevel;
	F32   ridgeFactor;
	F32   trendResamplingOrderProb;
	F32   seasonResamplingOrderProb;
	U16   maxMoveStepSize;
	U32   burnin,samples,chainNumber;
	U16   thinningFactor;
} BEAST2_MCMC,* _restrict BEAST2_MCMC_PTR;
typedef struct BEAST2_EXTRA {
	I08   smoothCpOccPrCurve;
	I08   useMeanOrRndBeta;
	I08   dumpInputData;
	U08   numThreadsPerCPU;
	U16   numParThreads;
	U16   numCPUCoresToUse;
	U16   consoleWidth;
	I08   whichOutputDimIsTime;
	bool  computeCredible;
	bool  fastCIComputation;
	bool  computeSeasonOrder;
	bool  computeTrendOrder;
	bool  computeSeasonChngpt,computeTrendChngpt,computeOutlierChngpt;
	bool  computeSeasonAmp;
	bool  computeTrendSlope;	
	bool tallyPosNegSeasonJump;
	bool tallyPosNegTrendJump;
	bool tallyIncDecTrendJump;
	bool tallyPosNegOutliers;
	bool  printOptions;
	bool  printProgressBar;
} BEAST2_EXTRA,* _restrict BEAST2_EXTRA_PTR;
typedef struct BEAST2_TIME {
	I32PTR     sortedTimeIdx;
	I32PTR     numPtsPerInterval;
	I32        startIdxOfFirsInterval;
} BEAST2_TIME;
struct BEAST2_RESULT;
typedef struct BEAST2_RESULT BEAST2_RESULT,* _restrict BEAST2_RESULT_PTR;
typedef struct BEAST2_IO {
	BEAST2_METADATA	meta;
	BEAST2_TIME		T;
	VOID_PTR		*pdata;
	DATA_TYPE		*dtype;
	I08				ndim;
	I32				dims[3];
	I32				numOfPixels;
	I32				N,q; 
	struct {
		BEAST2_RESULT* result;
		DATA_TYPE      dtype;
		U08            whichDimIsTime;
	} out;
} BEAST2_IO,* _restrict BEAST2_IO_PTR;
typedef struct BEAST2_OPTIONS {
	BEAST2_IO		    io;
	BEAST2_MCMC			mcmc;
	BEAST2_EXTRA		extra;
	BEAST2_PRIOR		prior;
} BEAST2_OPTIONS,* _restrict BEAST2_OPTIONS_PTR;
typedef struct BEAST2_YINFO {
	F32PTR    Yseason;
	F32PTR    Ytrend;
	F32PTR     mean,sd;	
	F32PTR     YtY_plus_alpha2Q;
	F32        alpha1_star;
	TKNOT      n,nMissing;		
	I32        q; 
	I32PTR     rowsMissing;
	F32PTR     Y;
} BEAST2_YINFO,* _restrict BEAST2_YINFO_PTR;
typedef struct BEAST2_RESULT {
	F32PTR  time;
	F32PTR  data;
	F32PTR  marg_lik,sig2,R2,RMSE;
	F32PTR  sncp;
	I32PTR  sncpPr,scpOccPr;
	F32PTR  sY,sSD;
	F32PTR  sCI;            
	I32PTR  sorder;         
	F32PTR  samp,sampSD;   
	F32PTR  scp,scpCI,scpPr,scpAbruptChange; 
	F32PTR  tncp;
	I32PTR  tncpPr,tcpOccPr;
	F32PTR  tY,tSD;
	F32PTR  tCI;            
	I32PTR  torder;         
	F32PTR  tslp,tslpSD;   
	I32PTR  tslpSignPr;   
	F32PTR  tcp,tcpCI,tcpPr,tcpAbruptChange; 
	F32PTR  oncp;
	I32PTR  oncpPr,ocpOccPr;
	F32PTR  oY,oSD;
	F32PTR  oCI;                 
	F32PTR  ocp,ocpCI,ocpPr; 
	F32PTR  spos_ncp,sneg_ncp;
	I32PTR  spos_ncpPr,sneg_ncpPr;
	I32PTR  spos_cpOccPr,sneg_cpOccPr;
	F32PTR  spos_cp,sneg_cp;
	F32PTR  spos_cpPr,sneg_cpPr;
	F32PTR  spos_cpAbruptChange,sneg_cpAbruptChange;
	F32PTR  spos_cpCI,sneg_cpCI;
	F32PTR  tpos_ncp,tneg_ncp;
	I32PTR  tpos_ncpPr,tneg_ncpPr;
	I32PTR  tpos_cpOccPr,tneg_cpOccPr;
	F32PTR  tpos_cp,tneg_cp;
	F32PTR  tpos_cpPr,tneg_cpPr;
	F32PTR  tpos_cpAbruptChange,tneg_cpAbruptChange;
	F32PTR  tpos_cpCI,tneg_cpCI;
	F32PTR  tinc_ncp,tdec_ncp;
	I32PTR  tinc_ncpPr,tdec_ncpPr;
	I32PTR  tinc_cpOccPr,tdec_cpOccPr;
	F32PTR  tinc_cp,tdec_cp;
	F32PTR  tinc_cpPr,tdec_cpPr;
	F32PTR  tinc_cpAbruptChange,tdec_cpAbruptChange;
	F32PTR  tinc_cpCI,tdec_cpCI;
	F32PTR  opos_ncp,oneg_ncp;
	I32PTR  opos_ncpPr,oneg_ncpPr;
	I32PTR  opos_cpOccPr,oneg_cpOccPr;
	F32PTR  opos_cp,oneg_cp;
	F32PTR  opos_cpPr,oneg_cpPr;	
	F32PTR  opos_cpCI,oneg_cpCI;
} BEAST2_RESULT,* _restrict BEAST2_RESULT_PTR;
typedef struct CORESULT {
	I32PTR xNProb,xProb,xorder;
	F32PTR x,xSD;
} CORESULT,* _restrict CORESULT_PTR;
typedef struct BEAST2_RNDSTREAM {
	U32PTR  rnd32;
	U16PTR  rnd16;
	U08PTR  rnd08;
	F32PTR  rndgamma;
} BEAST2_RNDSTREAM,* _restrict BEAST2_RANDSEEDPTR;
struct BEAST2_BASIS;
struct BEAST2_MODEL;
typedef struct BEAST2_BASIS  * _restrict BEAST2_BASIS_PTR;
typedef struct BEAST2_MODEL BEAST2_MODEL,* _restrict BEAST2_MODEL_PTR;
typedef struct PROPOSE_STRUCT {	
	I32PTR             samples;	
	CORESULT_PTR       keyresult;
	F32PTR             mem;
	BEAST2_MODEL_PTR   model;
	BEAST2_RANDSEEDPTR pRND;
	BEAST2_YINFO_PTR   yInfo;
	I32                nSample_ExtremVecNeedUpdate;
	I32                N,Npad16;
	F32                sigFactor;	
} PROP_DATA,*_restrict PROP_DATA_PTR;
typedef struct BEAST2_BASESEG {
	I32 R1,R2,K;
			I16 ORDER1,ORDER2; 
		I32     outlierKnot; 
} BEAST2_BASESEG,* _restrict BEAST2_BASESEG_PTR;
typedef struct _NEWTERM {
	BEAST2_BASESEG  SEG[2];
		TKNOT          newKnot;
			TORDER oldOrder,newOrder; 
	I16 nKnot_new;
	I16 newIdx;
		I16 k1;
		I16 k1_old;
		I16 k1_new;
	I16 k2_old,k2_new;
	U08 numSeg;
	I08 jumpType;
} NEWTERM,* _restrict NEWTERM_PTR;
typedef struct DUMMY_CONS { F32PTR TERMS; F32PTR SQRT_N_div_n; I32 period; }          DUMMY_CONST;
typedef struct SVD_CONS {  F32PTR TERMS; F32PTR SQR_CSUM;}          SVD_CONST;
typedef struct SEASON_CONST { F32PTR TERMS,SQR_CSUM;}				    SEASON_CONST;
typedef struct TREND_CONS   { F32PTR TERMS,COEFF_A,COEFF_B,INV_SQR;}	TREND_CONST;
typedef struct OUTLIER_CONS { F32    SQRTN,SQRTN_1; }		OUTLIER_CONST;
typedef union  {
	SVD_CONST     svd;
	DUMMY_CONST   dummy;
	SEASON_CONST  season;
	TREND_CONST   trend;
	OUTLIER_CONST outlier;
} BASIS_CONST;
typedef int  (*pfnGenTerms)(F32PTR X,I32 N,BEAST2_BASESEG*,BASIS_CONST * ptr);
typedef struct PROP_PROB_STRUCT {U08 birth,death,merge,move;} PROP_PROB_STRUCT;
typedef struct BEAST2_BASIS {
	BASIS_CONST  bConst;
	struct {
		void        (*Propose)(BEAST2_BASIS_PTR,NEWTERM_PTR,PROP_DATA*);
		pfnGenTerms GenTerms;
		int         (*CalcBasisKsKeK_TermType)(BEAST2_BASIS_PTR  basis);
		void        (*UpdateGoodVec)(BEAST2_BASIS_PTR basis,NEWTERM_PTR new,I32 Npad16_not_used);
		void        (*ComputeY)(F32PTR X,F32PTR beta,F32PTR Y,BEAST2_BASIS_PTR basis,I32 Npad);
		F32         (*ModelPrior)(BEAST2_BASIS_PTR basis,NEWTERM_PTR new,I32 N);
	};	
	F32PTR   scalingFactor;
	F64PTR   priorMat;
	F64PTR   priorVec;
	struct {
		TKNOT  minSepDist;
		I16    minKnotNum;
		I16    maxKnotNum;
		TORDER minOrder,maxOrder;
	} prior;
	PROP_PROB_STRUCT	propprob;
	I16					mcmc_Kstopping;
	I16					mcmc_MoveStep;
	TKNOT_PTR   KNOT;
	TORDER_PTR	ORDER;  
	I16PTR     ks,ke;
	U08PTR     termType;
	U08PTR     goodvec;
	I16      nPrec;
	I16      offsetPrec;
	I32      goodNum;
	I16      nKnot;
	I16      K,Kbase;
	U08      type;
} BEAST2_BASIS,* _restrict BEAST2_BASIS_PTR;
typedef struct {
	F32PTR XtX,XtY,cholXtX,beta_mean;
	F32PTR precXtXDiag;
	I08PTR nTermsPerPrecGrp;
	union {
		F32      alpha2_star;
		F32PTR   alphaQ_star; 
	};
	F32    marg_lik;
	I32    K;
} BEAST2_MODELDATA,*_restrict  BEAST2_MODELDATA_PTR;
typedef struct BEAST2_MODEL {
	I32 (*PickBasisID)(PROP_DATA_PTR );
	F32PTR beta;
	union {
		F32	    sig2;
		F32PTR  SIG2; 
	};
	I08PTR extremePosVec;
	F32PTR deviation;
	F32PTR avgDeviation;  
	I32    extremPosNum;
	F32    baseSig;
	F32PTR basePrec;
	I16    nPrec;	
	union {
		F32     precVal;
		F32PTR  precVec;	
	};
	union {
		F32     logPrecVal;
		F32PTR  logPrecVec;
	};
	BEAST2_MODELDATA curr,prop;
	I08          NUMBASIS;
	I08          vid,did,sid,tid,oid;
	BEAST2_BASIS b[MAX_NUM_BASIS];	
} BEAST2_MODEL,* _restrict BEAST2_MODEL_PTR;
typedef struct {
	I32              minSepDist;
	BEAST2_YINFO_PTR yInfo;
} KNOT2BINVEC,* _restrict KNOT2BINVEC_PTR;