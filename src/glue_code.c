#include "abc_000_warning.h"
#define IMPORT_NUMPY
#include "abc_001_config.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#if !defined(cpu_ARM64) && !defined(cpu_POWERPC)  
	#include <immintrin.h> 
    #include "abc_math_avx.h"
#endif
#include "abc_datatype.h"
#include "abc_blas_lapack_lib.h"
#include "abc_common.h"
#include "abc_ide_util.h"
#include "abc_pthread.h"
#include "abc_timer.h"
#include "abc_vec.h"
#include "abc_date.h"
#include "beastv2_io.h"
#if  ( !defined(R_RELEASE) && !defined(M_RELEASE) && !defined(P_RELEASE)  )||defined( PLAY_MODE)
	#include "mrbeast_header.h"
	#include "mrbeast_io.h" 
	#include "sbmfast.h"
	#include "sbmfast_io.h"
#endif
#include "globalvars.h"
#if defined(OS_WIN64) 
#endif
#include "abc_cpu.h"
#include "abc_timer.h"
#include "abc_rand.h" 
#define __IS_STRING_EQUAL(a,b)  (strcicmp(a,#b)==0)
static void  GetArg_GlobalFlagss(VOIDPTR prhs[],int nrhs) {
	 GLOBAL_PRNT_WARNING=1;
	 GLOBAL_PRNT_CPU=0;
	 GLOBAL_PRNT_PARAMETER=1;
	 GLOBAL_PRNT_PROGRESS=1;
	 GLOBAL_IS_QUIET_MODE=0;	 	 
	 GLOBAL_CPU_REQUEST=0;
 	if (nrhs >=6 && IsStruct(prhs[6L - 1L]) ) {
 		VOIDPTR tmp;
		VOIDPTR extra=prhs[6L - 1L];
		GLOBAL_IS_QUIET_MODE=(tmp=GetField123Check(extra,"quiet",3)) ? GetScalar(tmp) : 0L;
		if (GLOBAL_IS_QUIET_MODE) {
			GLOBAL_PRNT_WARNING=0;
			GLOBAL_PRNT_CPU=0;
			GLOBAL_PRNT_PARAMETER=0;
			GLOBAL_PRNT_PROGRESS=0;
		} else {
			GLOBAL_PRNT_WARNING=(tmp=GetField123Check(extra,"printWarning",7)) ? GetScalar(tmp) : 1L;
			GLOBAL_PRNT_CPU=(tmp=GetField123Check(extra,"printCpuInfo",7)) ? GetScalar(tmp) : 0L;
			GLOBAL_PRNT_PARAMETER=(tmp=GetField123Check(extra,"printParam",7)) ? GetScalar(tmp) : 1L;
			GLOBAL_PRNT_PROGRESS=(tmp=GetField123Check(extra,"printProgress",7)) ? GetScalar(tmp) : 1L;
		}
		tmp=GetField123Check(extra,"cputype",3);
		if (tmp && IsChar(tmp)) {
			char  str[10+1];
			GetCharArray(tmp,str,10);
			if (str[0]=='s'||str[0]=='S') {                            
				GLOBAL_CPU_REQUEST=1;
			} else if ((str[0]=='s'||str[0]=='A') && str[3]=='2' ) { 
				GLOBAL_CPU_REQUEST=2;
			} else if ((str[0]=='s'||str[0]=='A') && str[3]=='4') {  
				GLOBAL_CPU_REQUEST=3;
			} else {                                                          
				GLOBAL_CPU_REQUEST=0;
			}
		}
	} 
	if (GLOBAL_PRNT_CPU) {
		struct cpu_x86   cpuinfo;
		struct cpu_cache caches[8];
		cpuinfo_detect(&cpuinfo,caches);
		cpuinfo_print(&cpuinfo,caches);
	}
	if (GLOBAL_CPU_REQUEST==0) {
		GLOBAL_CPU_REQUEST=GetNativeCPUType();
	}
	return;
}
void * mainFunction(void *prhs[],int nrhs) {
	if (nrhs==0) {
		r_error("ERROR: Essential input paramaters are missing!\n");
		return IDE_NULL;
	}
	if (!IsChar(prhs[0])) {
		r_error("ERROR: The very first parameter must be a string specifying the algorithm name!\n");
		return IDE_NULL;
	}
	GetArg_GlobalFlagss(prhs,nrhs);
	if (GLOBAL_CPU_REQUEST !=GLOBAL_CPU_CURRENT) {
		GLOBAL_CPU_CURRENT=GLOBAL_CPU_REQUEST;
		SetupRoutines_ByCPU(GLOBAL_CPU_CURRENT);
	}
	#define __STRING_LEN__          20
	char  algorithm[__STRING_LEN__+1];
	GetCharArray(prhs[0],algorithm,__STRING_LEN__);
	void * ANS=NULL;
	int    nptr=0;
	if   (__IS_STRING_EQUAL(algorithm,beast_bayes))  	{
		BEAST2_OPTIONS      option={ {{0,},},}; 
		#if P_INTERFACE==1
			prhs[1]=CvtToPyArray_NewRef(prhs[1]);
		#endif
		if (  BEAST2_GetArgs(prhs,nrhs,&option)==0 ) {
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		    #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		    #endif
			return IDE_NULL;
		}		
		option.io.out.result=NULL;		 
		ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;
		GLOBAL_OPTIONS=(BEAST2_OPTIONS_PTR)&option;
		if (option.io.numOfPixels==1) {
			beast2_main_corev4();
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
			if (GLOBAL_PRNT_PROGRESS) {
				r_printf("\n");
			}			
		} else {
			I32 NUM_THREADS=option.extra.numParThreads;  
			I32 NUM_THREADS_PER_CPU=option.extra.numThreadsPerCPU;
			I32 NUM_CORES=GetNumCores();
			NUM_CORES=max(NUM_CORES,1L);			
			NUM_THREADS_PER_CPU=max(NUM_THREADS_PER_CPU,1L);
			NUM_THREADS=(NUM_THREADS <=0) ? NUM_CORES * NUM_THREADS_PER_CPU : NUM_THREADS;
			NUM_THREADS=min(NUM_THREADS,option.io.numOfPixels);
			NUM_OF_PROCESSED_PIXELS=0;
			NUM_OF_PROCESSED_GOOD_PIXELS=0;
			NEXT_PIXEL_INDEX=1;
			pthread_mutex_init(&mutex,NULL);
			pthread_cond_init(&condVar,NULL);
			pthread_attr_t attr;
			pthread_attr_init(&attr);
			pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_JOINABLE);
			thread_id=malloc(sizeof(pthread_t) * NUM_THREADS);
  		   int8_t *thread_stat=malloc(sizeof(int8_t) * NUM_THREADS);
			for (I32 i=0; i < NUM_THREADS; i++) {
             #if (defined(OS_LINUX)||defined (OS_WIN32)||defined (OS_WIN64) )  &&  USING_MUSL==0
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET( i%NUM_CORES,&cpuset);
				pthread_attr_setaffinity_np(&attr,sizeof(cpu_set_t),&cpuset);
				thread_stat[i]=pthread_create( &thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
             #elif (defined(OS_LINUX)||defined (OS_WIN32)||defined (OS_WIN64) )  &&  USING_MUSL==1
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET( i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				pthread_setaffinity_np(thread_id[i],sizeof(cpu_set_t),&cpuset);
			 #elif defined(OS_MAC)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				pthread_setaffinity_np(thread_id[i],sizeof(cpu_set_t),&cpuset);
		     #elif defined (OS_SOLARIS)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				sched_getaffinity(thread_id[i],sizeof(cpu_set_t),&cpuset);
			 #else
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
			 #endif
				if ( 0==thread_stat[i]) {
					if(GLOBAL_PRNT_PROGRESS) r_printf("Parallel computing: thread#%-2d generated ... \n",i+1);
			    } else {
					if (GLOBAL_PRNT_PROGRESS) r_printf("Parallel computing: thread#%-2d failed to generate ... \n",i+1);
				}
			}
			if (GLOBAL_PRNT_PROGRESS) r_printf("Rbeast: Waiting on %d threads...\n",NUM_THREADS);
			pthread_attr_destroy(&attr);
			IDE_USER_INTERRUPT=0;
			#if R_INTERACE==1
			if (GLOBAL_PRNT_PROGRESS)  r_printf("Press and hold the ESCAPE key or the STOP button to interrupt and quit while running.\n" );
			#elif M_INTERFACE==1
			if (GLOBAL_PRNT_PROGRESS)  r_printf("Press and hold CTR+C to interrupt and quit while running.\n");
			#endif
			if (option.extra.printProgress) {
				void* StrBuf=malloc(option.extra.consoleWidth* 3);
				PERCENT_COMPLETED=0;
				REMAINING_TIME=10000;
				printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,StrBuf,1);
				while (PERCENT_COMPLETED < 1.f && NEXT_PIXEL_INDEX < option.io.numOfPixels && IDE_USER_INTERRUPT==0) {
					printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,StrBuf,0);
					if (CheckInterrupt()) {
						ConsumeInterruptSignal();
						IDE_USER_INTERRUPT=1;
						r_printf("Quitting due to unexpected user interruption...\n");
					}					
					Sleep_ms(2 * 1000);
				}
				if (IDE_USER_INTERRUPT==0) {
					if (GLOBAL_PRNT_PROGRESS)	printProgress2(1.0,0,option.extra.consoleWidth,StrBuf,0);
				}
				free(StrBuf);
			}  
			if (GLOBAL_PRNT_PROGRESS) r_printf("\nFinalizing ... \n");
			for (I32 i=0; i < NUM_THREADS; i++) {
				if (thread_stat[0]==0) {
					I64 ret=0;
					pthread_join(thread_id[i],&ret); 
					if (GLOBAL_PRNT_PROGRESS) r_printf("Parallel computing : Thread # %-2d finished ... \n",i);
				}				
			}
			if (IDE_USER_INTERRUPT==0) {
				if (GLOBAL_PRNT_PROGRESS)  r_printf("\nRbeast: Waited on %d threads. Done.\n",NUM_THREADS); 
			}	else {
				if (GLOBAL_PRNT_PROGRESS)  r_printf("\nQuitted unexpectedly upon the user's interruption.\n");
			}
			pthread_mutex_destroy(&mutex);
			pthread_cond_destroy(&condVar);
			free(thread_id);
			free(thread_stat);
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		}
	     #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		 #endif
	}
	else if (__IS_STRING_EQUAL(algorithm,tsextract)) {
		extern void* BEAST2_TsExtract(void* o,void* pindex);
		ANS=PROTECT(BEAST2_TsExtract(prhs[1],prhs[2]));
		nptr++;
	}
	else if (__IS_STRING_EQUAL(algorithm,print)) {
		extern void* BEAST2_PrintResult(void* o,void* pindex);
		BEAST2_PrintResult(prhs[1],prhs[2]);
	}
	else if (__IS_STRING_EQUAL(algorithm,disp)) {
		IDEPrintObject(prhs[1]);
	}
	else if   (    __IS_STRING_EQUAL(algorithm,beast_bic)||__IS_STRING_EQUAL(algorithm,beast_aicc)
		   ||__IS_STRING_EQUAL(algorithm,beast_aic)||__IS_STRING_EQUAL(algorithm,beast_hic)
		   ||__IS_STRING_EQUAL(algorithm,beast_bic2)||__IS_STRING_EQUAL(algorithm,beast_bic1.5)
		   ||__IS_STRING_EQUAL(algorithm,beast_bic0.5)||__IS_STRING_EQUAL(algorithm,beast_bic0.25) )  	{
		int whichCritia=0;
		if      (__IS_STRING_EQUAL(algorithm,beast_bic))       whichCritia=1;
		else if (__IS_STRING_EQUAL(algorithm,beast_aic)) 	    whichCritia=2;
		else if (__IS_STRING_EQUAL(algorithm,beast_aicc)) 	    whichCritia=3;
		else if (__IS_STRING_EQUAL(algorithm,beast_hic)) 	    whichCritia=4;
		else if (__IS_STRING_EQUAL(algorithm,beast_bic0.25)) 	whichCritia=25;
		else if (__IS_STRING_EQUAL(algorithm,beast_bic0.5)) 	whichCritia=50;
		else if (__IS_STRING_EQUAL(algorithm,beast_bic1.5)) 	whichCritia=150;
		else if (__IS_STRING_EQUAL(algorithm,beast_bic2)) 	    whichCritia=200;
		BEAST2_OPTIONS      option={ {{0,},},}; 
		#if P_INTERFACE==1
			prhs[1]=CvtToPyArray_NewRef(prhs[1]);
		#endif
		if (BEAST2_GetArgs(prhs,nrhs,&option)==0)  {
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		    #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		    #endif
			return IDE_NULL;
		}		
		option.io.out.result=NULL;		 	
		ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;
		if (option.prior.precPriorType !=ConstPrec) {
			option.prior.precPriorType=UniformPrec;
			option.prior.precValue=0;
		}
		extern int beast2_main_corev4_bic(int whichCritia);
		extern int beast2_main_core_bic_mthrd(void* dummy);
		GLOBAL_OPTIONS=(BEAST2_OPTIONS_PTR)&option;
		if (option.io.numOfPixels==1) {
			beast2_main_corev4_bic(whichCritia);
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
			if (GLOBAL_PRNT_PROGRESS) {
				r_printf("\n");
			}
		} else {
			I32 NUM_THREADS=option.extra.numParThreads;  
			I32 NUM_THREADS_PER_CPU=option.extra.numThreadsPerCPU;
			I32 NUM_CORES=GetNumCores();
			NUM_CORES=max(NUM_CORES,1L);			
			NUM_THREADS_PER_CPU=max(NUM_THREADS_PER_CPU,1L);
			NUM_THREADS=(NUM_THREADS <=0) ? NUM_CORES * NUM_THREADS_PER_CPU : NUM_THREADS;
			NUM_THREADS=min(NUM_THREADS,option.io.numOfPixels);
			NUM_OF_PROCESSED_PIXELS=0;
			NUM_OF_PROCESSED_GOOD_PIXELS=0;
			NEXT_PIXEL_INDEX=1;
			pthread_mutex_init(&mutex,NULL);
			pthread_cond_init(&condVar,NULL);
			pthread_attr_t attr;
			pthread_attr_init(&attr);
			pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_JOINABLE);
			thread_id=malloc(sizeof(pthread_t) * NUM_THREADS);
  		   int8_t *thread_stat=malloc(sizeof(int8_t) * NUM_THREADS);
			for (I32 i=0; i < NUM_THREADS; i++) {
             #if (defined(OS_LINUX)||defined (OS_WIN32)||defined (OS_WIN64) )  &&  USING_MUSL==0
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET( i%NUM_CORES,&cpuset);
				pthread_attr_setaffinity_np(&attr,sizeof(cpu_set_t),&cpuset);
				thread_stat[i]=pthread_create( &thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
             #elif (defined(OS_LINUX)||defined (OS_WIN32)||defined (OS_WIN64) )  && USING_MUSL==1
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET( i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				pthread_setaffinity_np(thread_id[i],sizeof(cpu_set_t),&cpuset);
			 #elif defined(OS_MAC)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				pthread_setaffinity_np(thread_id[i],sizeof(cpu_set_t),&cpuset);
		     #elif defined (OS_SOLARIS)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i%NUM_CORES,&cpuset);
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				sched_getaffinity(thread_id[i],sizeof(cpu_set_t),&cpuset);
			 #else
				thread_stat[i]=pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
			 #endif
				if ( 0==thread_stat[i]) {
					if (GLOBAL_PRNT_PROGRESS)  r_printf("Parallel computing: thread#%-2d generated ... \n",i+1);
			    } else {
					if (GLOBAL_PRNT_PROGRESS)  r_printf("Parallel computing: thread#%-2d failed to generate ... \n",i+1);
				}
			}
			if (GLOBAL_PRNT_PROGRESS)  r_printf("Rbeast: Waiting on %d threads...\n",NUM_THREADS);
			pthread_attr_destroy(&attr);
			IDE_USER_INTERRUPT=0;
			#if R_INTERACE==1
			if (GLOBAL_PRNT_PROGRESS)  r_printf("Press and hold the ESCAPE key or the STOP button to interrupt and quit while running.\n" );
			#elif M_INTERFACE==1
			if (GLOBAL_PRNT_PROGRESS)  r_printf("Press and hold CTR+C to interrupt and quit while running.\n");
			#endif
			if (option.extra.printProgress) {
				void* StrBuf=malloc(option.extra.consoleWidth* 3);
				PERCENT_COMPLETED=0;
				REMAINING_TIME=10000;
				printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,StrBuf,1);
				while (PERCENT_COMPLETED < 1.f && NEXT_PIXEL_INDEX < option.io.numOfPixels && IDE_USER_INTERRUPT==0) {
					printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,StrBuf,0);
					if (CheckInterrupt()) {
						ConsumeInterruptSignal();
						IDE_USER_INTERRUPT=1;
						if (GLOBAL_PRNT_PROGRESS)  r_printf("Quitting due to unexpected user interruption...\n");
					}					
					Sleep_ms(2 * 1000);
				}
				if (IDE_USER_INTERRUPT==0) {
					if (GLOBAL_PRNT_PROGRESS) 	printProgress2(1.0,0,option.extra.consoleWidth,StrBuf,0);
				}
				free(StrBuf);
			}  
			if (GLOBAL_PRNT_PROGRESS)  r_printf("\nFinalizing ... \n");
			for (I32 i=0; i < NUM_THREADS; i++) {
				if (thread_stat[0]==0) {
					I64 ret=0;
					pthread_join(thread_id[i],&ret); 
					if (GLOBAL_PRNT_PROGRESS)  r_printf("Parallel computing : Thread # %-2d finished ... \n",i);
				}				
			}
			if (IDE_USER_INTERRUPT==0) {
				if (GLOBAL_PRNT_PROGRESS)  r_printf("\nRbeast: Waited on %d threads. Done.\n",NUM_THREADS); 
			}	else {
				if (GLOBAL_PRNT_PROGRESS)  r_printf("\nQuitted unexpectedly upon the user's interruption.\n"); 
			}
			pthread_mutex_destroy(&mutex);
			pthread_cond_destroy(&condVar);
			free(thread_id);
			free(thread_stat);
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		}
	     #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		 #endif
	}
	else if (__IS_STRING_EQUAL(algorithm,beastv4Demo)) 	{
		#ifdef OS_WIN64
			#if P_INTERFACE==1
					prhs[1]=CvtToPyArray_NewRef(prhs[1]);
			#endif
			BEAST2_OPTIONS      option={ {{0,},},}; 
			option.io.out.result=NULL;		
			GLOBAL_OPTIONS=(BEAST2_OPTIONS_PTR)&option;
			if ( BEAST2_GetArgs(prhs,nrhs,&option)==0 ) {
				BEAST2_DeallocateTimeSeriesIO(&(option.io));
				return IDE_NULL;
			}
			option.extra.computeCredible=TRUE;
		    option.extra.dumpMCMCSamples=FALSE;
			ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;
			void DllExport BEAST2_WinMain(VOID_PTR  option);
			BEAST2_WinMain((BEAST2_OPTIONS_PTR)GLOBAL_OPTIONS);
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
			#if P_INTERFACE==1
					Py_XDECREF(prhs[1]);
			#endif
		#else
			r_printf("WARNING: The GUI interface is supporte only on the Windows 64 operating system.\n");
		#endif	
	}
	#if !defined(R_RELEASE) &&  !defined(M_RELEASE) &&  !defined(P_RELEASE)
	else if  (__IS_STRING_EQUAL(algorithm,"mrbeast"))
	{
		MV_OPTIONS         option;
		MV_IO              io;
		MV_RESULT          result;
		memset(&io,0,sizeof(MV_IO));
		memset(&result,0,sizeof(MV_RESULT));
		option.io=&io;
		option.io->out.result=&result;
		option.io->isRegularOrdered=1;
		if (!MV_Get1stArg_Data(prhs,nrhs,&option)||
			!MV_Get2ndArg_MetaData(prhs,nrhs,&option)||
			!MV_Get3rdArg_Prior(prhs,nrhs,&option)||
			!MV_Get4thArg_MCMC(prhs,nrhs,&option)||
			!MV_Get5thArg_FLAGS(prhs,nrhs,&option)) {
			return IDE_NULL;
		}
		MV_print_options(&option);
		ANS=PROTECT(MV_AllocateOutput(&option)); nptr++;
		GLOBAL_OPTIONS=(MV_OPTIONS_PTR)&option;
		mrbeast_main_core();
		MV_DeallocateTimeSeriesIO(option.io);
	} 
	#endif
	else if (__IS_STRING_EQUAL(algorithm,svd)) {
		typedef struct SVDBasisMEM {
			int N,P,Ncycle;
			I32PTR nPtsPerTime; 
			F32PTR mean;       
			I32PTR goodTimeIndices; 
			F32PTR Ytrue;
			F32PTR Ycur;
			I32PTR Igood;
			I32PTR NgoodPerPeriod;
			F32PTR A,B; 
			F32PTR M; 
			F32PTR Mcopy; 
			F32PTR XtX; 
			F32PTR Bcoeff; 
		} SVDBasisMEM;
		void compute_seasonal_svdbasis(F32PTR y,F32PTR Yout,int  Kmax,SVDBasisMEM* mem);
		float Y[]={2.2859f,2.0691f,1.8224f,1.5187f,1.5328f,2.0847f,2.8942f,3.2902f,3.728f,5.8975f,6.5847f,10.2307f,10.9694,17.5811,14.1326,11.5202,10.2331,6.6177,5.0504,4.2145,1.8737,3.197,2.8468,2.5349,1.6543,1.3169,1.8825,2.2085,1.8276,2.5948,3.6769,4.822,4.8609,6.4938,6.9191,10.2873,11.2253,13.0343,14.1471,15.8413,11.9918,7.2993,5.5117,6.621,5.5967,2.8521,3.9991,3.2917,4.342,1.6677,1.9921,2.329,1.494,1.697,2.6342,2.5178,2.8399,4.4247,6.831,10.3966,13.3897,14.5965,14.5742,12.6073,11.3888,10.045,9.0508,5.148,4.2168,2.952,2.7271,2.3879,2.0889,1.5052,2.004,1.7263,1.4218,1.8645,3.7781,3.2067,2.5755,3.0108,5.7034,8.6385,9.9321,11.321,13.1164,13.2138,10.3973,8.117,6.3119,5.269,6.7344,3.2315,2.6521,2.6256,2.0983,1.844,1.9795,1.4476,1.8262,1.7339,2.4015,2.1856,3.1992,3.5943,5.1744,7.0701,10.6768,12.4415,15.6008,14.9724,10.063,8.6803,9.1394,6.0462,4.1622,2.6468,3.2652,2.0314,2.2326,1.8504,3.217,1.9163,1.1103,1.8004,2.5058,2.0672,3.3731,3.3932,6.4282,6.4809,10.5954,12.5961,15.711,14.2856,12.7219,12.4061,7.4394,9.5144,5.696,3.1871,3.2476,3.844,2.523,2.0841,2.1426,1.7278,2.0496,1.7432,1.8544,2.4466,3.3078,2.5855,4.2597,6.4083,7.4898,11.4539,12.9476,13.7927,15.2775,13.0988,8.255,7.3098,6.1615,3.3724,1.9019,2.6075,4.4399,2.2925,2.268,2.0642,1.9616,2.0934,2.0652,1.974,2.6556,3.2962,3.8939,4.9837,7.8963,11.7756,11.005,13.3557,11.9917,11.4185,9.9252,8.1113,5.6223,4.4981,3.5695,2.3075,2.5229,2.3264,1.9057,1.4906,1.5844,2.3672,2.0908,2.3647,3.0101,2.4715,4.5199,6.8071,7.7233,9.065,12.7859,14.8723,14.2513,16.931,12.1172,8.4426,6.9039,6.276,5.3299,3.2293};
		for (int i=0; i < 9*24; i++) {
			Y[i]=getNaN();
		}
		Y[10]=1;
		Y[20]=6;
		MemPointers MEM={ .init=mem_init };
		MEM.init(&MEM);
		SVDBasisMEM s={.N=24*9,.P=24};
		I64 Get_Alloc_SVDBasisMEM(int N,int P,SVDBasisMEM* s,VOID_PTR bufBase); 
		I32 totalSize=Get_Alloc_SVDBasisMEM(24*9,24,&s,NULL );
		VOIDPTR buf=malloc(totalSize);
		Get_Alloc_SVDBasisMEM(24 * 9,24,&s,buf);
		F32PTR Yout;
		ANS=CreateF64NumMatrix(24*9,24,&Yout);
		compute_seasonal_svdbasis(Y,Yout,9,&s);
		f32_to_f64_inplace(Yout,24 *9*24);
		free(buf);
		MEM.free_all(&MEM);
	}
	else if (__IS_STRING_EQUAL(algorithm,datenum)) {
		int y=GetScalar(prhs[1]);
		int m=GetScalar(prhs[2]);
		int d=GetScalar(prhs[3]);
		int yorg=GetScalar(prhs[4]);
		int morg=GetScalar(prhs[5]);
		int dorg=GetScalar(prhs[6]);
		int org=JulianDayNum_from_civil_ag1(yorg,morg,dorg);
		int JDN=JulianDayNum_from_civil_ag1(y,m,d);
		int dnum=JDN - org;
		r_printf("datenum: %d",dnum);		
	}
	else if (__IS_STRING_EQUAL(algorithm,cpu)) {
	   ANS=NULL;
       #if defined(OS_WIN64)||defined(OS_WIN32)
		  int GetCPUInfo(void);
		  GetCPUInfo();
        #endif
	}
	else if (__IS_STRING_EQUAL(algorithm,tofyear)) {
		void* to_fyear(void* TIMEobj);
		void* from_fyear(void* FYEARobj);
		ANS=to_fyear(prhs[1]);
	}
	else if (__IS_STRING_EQUAL(algorithm,fromfyear)) {	 
		void* from_fyear(void* FYEARobj);
		ANS=from_fyear(prhs[1]);
	}
#ifdef PLAY_MODE   
	else if (__IS_STRING_EQUAL(algorithm,SLIDE))
	{
		SOptions beastOption={ 0,};
		GLOBAL_OPTIONS=&beastOption;
		MemPointers MEM=(MemPointers){ .init=mem_init };
		MEM.init(&MEM);
		if (!GetArgs(prhs,nrhs,& beastOption,&MEM)) {
			MEM.free_all(&MEM);
			return IDE_NULL;
		}
		void SBM_print_options(SOptions * opt);
		SBM_print_options(&beastOption);
		DATAL0 data0;   GLOBAL_DATA0=&data0;
		AllocInitDataL0(&data0,&beastOption,&MEM);
		ANS=SBM_Output_AllocMEM(&beastOption);
		sbmfast_slide();
		free(beastOption.io);
		MEM.free_all(&MEM);
	} 
	else if (__IS_STRING_EQUAL(algorithm,QR))
	{
		SOptions beastOption={ 0,};
		GLOBAL_OPTIONS=&beastOption;
		MemPointers MEM=(MemPointers){ .init=mem_init };
		MEM.init(&MEM);
		if (!GetArgs(prhs,nrhs,&beastOption,&MEM)) {
			MEM.free_all(&MEM);
			return IDE_NULL;
		}
		void SBM_print_options(SOptions * opt);
		SBM_print_options(&beastOption);
		DATAL0 data0;   GLOBAL_DATA0=&data0;
		AllocInitDataL0(&data0,&beastOption,&MEM);
		ANS=SBM_Output_AllocMEM(&beastOption);
		sbmfast_slide_robust();
		free(beastOption.io);
		MEM.free_all(&MEM);
	}
	else if (__IS_STRING_EQUAL(algorithm,SWEEP))
	{
		SOptions beastOption={0,};  GLOBAL_OPTIONS=&beastOption;
		MemPointers MEM=(MemPointers){ .init=mem_init };
		MEM.init(&MEM);
		if (!GetArgs(prhs,nrhs,& beastOption,&MEM)) {
			MEM.free_all(&MEM);
			return IDE_NULL;
		}
		void SBM_print_options(SOptions * opt);
		SBM_print_options(&beastOption);
		DATAL0 data0; 
		GLOBAL_DATA0=&data0;
		AllocInitDataL0(&data0,&beastOption,&MEM);
		ANS=SBM_Output_AllocMEM(&beastOption);
		sbmfast_sweep();
		free(beastOption.io);
		MEM.free_all(&MEM);
	}
#endif
	UNPROTECT(nptr);
	return ANS==NULL ? IDE_NULL : ANS;	
}
#if R_INTERFACE==1
#include <R_ext/libextern.h>
#include "Rembedded.h"
#if defined(COMPILER_MSVC)
SEXP DllExport rexFunction1(SEXP rList,SEXP dummy) {
#else
SEXP DllExport rexFunction(SEXP rList,SEXP dummy) {
#endif
	if (!isNewList(rList)) 	return R_NilValue; 
	SEXP   prhs[10];
	int    nrhs=length(rList);
	nrhs=min(10L,nrhs);
	for (int i=0; i < nrhs; i++) 	
		prhs[i]=VECTOR_ELT(rList,i);
	SEXP ans;
	PROTECT(ans=mainFunction(prhs,nrhs));
	UNPROTECT(1);
	return ans==NULL ?  R_NilValue: ans;
}
#define CALLDEF(name,n) {#name,(DL_FUNC) &name,n}
#if (defined(OS_WIN64)||defined(OS_WIN32)) 
	SEXP TetrisSetTimer(SEXP action,SEXP seconds,SEXP envior);
	static const R_CallMethodDef CallEntries[]={
		#if defined(COMPILER_MSVC)
			CALLDEF(rexFunction1,2),
		#else
			CALLDEF(rexFunction,2),
		#endif
		CALLDEF(TetrisSetTimer,3),
		{ NULL,NULL,0 }
	};
#else
	static const R_CallMethodDef CallEntries[]={
					CALLDEF(rexFunction,2),
					{ NULL,NULL,0 }
			};
#endif
void  R_init_Rbeast(DllInfo *dll) {
	  R_registerRoutines(dll,NULL,CallEntries,NULL,NULL);
	  R_useDynamicSymbols(dll,FALSE);
}
#elif M_INTERFACE==1
#include "abc_date.h"
void DllExport mexFunction(int nlhs,mxArray* plhs[],int nrhs,const mxArray* prhs[]) {
	mxArray * ans=mainFunction(prhs,nrhs);
	plhs[0]=ans;
	return;
}
#elif P_INTERFACE==1
extern int import_array(void);
extern void** PyArray_API;
int import_array(void) {
	PyObject* numpy=PyImport_ImportModule("numpy.core._multiarray_umath");  
	if (numpy==NULL) {
		PyErr_SetString(PyExc_ImportError,"numpy.core.multiarray failed to import");
		return -1;
	}
	PyObject* c_api=PyObject_GetAttrString(numpy,"_ARRAY_API");
    Py_DECREF(numpy);
	if (c_api==NULL) {
		PyErr_SetString(PyExc_AttributeError,"_ARRAY_API not found");
		return -1;
	}
#if PY_VERSION_HEX >=0x02070000
	if (!PyCapsule_CheckExact(c_api)) {
		PyErr_SetString(PyExc_RuntimeError,"_ARRAY_API is not PyCapsule object");
		Py_DECREF(c_api);
		return -1;
	}
	PyArray_API=(void**)PyCapsule_GetPointer(c_api,NULL);
#else
	if (!PyCObject_Check(c_api)) {
		PyErr_SetString(PyExc_RuntimeError,"_ARRAY_API is not PyCObject object");
		Py_DECREF(c_api);
		return -1;
	}
	PyArray_API=(void**)PyCObject_AsVoidPtr(c_api);
#endif
	Py_DECREF(c_api);
	if (PyArray_API==NULL) {
		PyErr_SetString(PyExc_RuntimeError,"_ARRAY_API is NULL pointer. Failed to load Numpy functions!");
		return -1;
	}
	return 0;
}
DllExport PyObject* pexFunction(PyObject* self,PyObject* args,PyObject* kwds) {
	int nargs=PyTuple_Size(args);
	int nkwds=PyDict_Size(kwds);
	if (nargs==0) 	return Py_None;
	VOIDPTR   prhs[10]={NULL,};
	int       nrhs=nargs; 
	for (int i=0; i < nargs; i++) {
		prhs[i]=PyTuple_GetItem(args,i);
	}
	VOID_PTR ans=mainFunction(prhs,nrhs);
	return ans !=NULL ? ans : IDE_NULL;
}
static PyMethodDef methods[]={
  { "Rbeast",&pexFunction,METH_VARARGS|METH_KEYWORDS,"Hello world function" },
  { "setClassObjects",&setClassObjects,METH_VARARGS,"Hello world function" },
  { NULL,NULL,0,NULL }
};
static struct PyModuleDef module_def={
  PyModuleDef_HEAD_INIT, 
  "cbeast",               
  "Testing module",      
  -1,                    
  methods,               
};
PyMODINIT_FUNC PyInit_Rbeast() {
  PyObject *m=PyModule_Create(&module_def);
  BarObject_Type.tp_richcompare=PyBaseObject_Type.tp_richcompare;
  BarObject_Type.tp_hash=PyBaseObject_Type.tp_hash;
  if (PyType_Ready(&BarObject_Type) < 0)
      return NULL;
  Py_INCREF(&BarObject_Type);
  if (PyModule_AddObject(m,"pyobject",(PyObject*)&BarObject_Type)) {
      Py_DECREF(&BarObject_Type);
      Py_DECREF(m);
      return NULL;
  }
  import_array();  
  currentModule=m;
  return m;
}
#endif
#if R_INTERFACE==11111111111111
SEXP DllExport sbm2(SEXP Y,SEXP opt)
{
	if (!R_sbm_check_input(Y,opt))		 			return R_NilValue;
	char	missing[31];
	BEAST_OPTIONS beastOption;
	R_sbm_read_input(&beastOption,Y,opt,missing);
	if (!sbm_check_options(&beastOption,missing))	return R_NilValue;
	print_options(&beastOption);
	BEAST_RESULT	result;
	SEXP	ANS;
	PROTECT(ANS=R_allocate_output(&result,&beastOption));
	GLOBAL_OPTIONS=(BEAST_OPTIONS_PTR)&beastOption;
	GLOBAL_RESULT=(BEAST_RESULT_PTR)&result;
	sbm();
	UNPROTECT(1);
	return ANS;
}
#endif
#include "abc_000_warning.h"
