#include "abc_000_warning.h"
#define IMPORT_NUMPY
#include "abc_001_config.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#ifndef ARM64_OS
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
#include "beastv2_io.h"
#if  ( !defined(R_RELEASE) && !defined(M_RELEASE) && !defined(P_RELEASE)  )||defined( PLAY_MODE)
#include "mrbeast_header.h"
#include "mrbeast_io.h" 
#include "sbmfast.h"
#include "sbmfast_io.h"
#endif
#include "globalvars.h"
#if defined(WIN64_OS) 
#endif
#include "abc_cpu.h"
#include "abc_timer.h"
#include "abc_rand.h" 
#define __IS_STRING_EQUAL(a,b)  (strcicmp(a,#b)==0)
static void  GetArg_IsQuiteMode(VOIDPTR prhs[],int nrhs) {
 	if (nrhs >=6 && IsStruct( prhs[5L] ) ) {
 			VOIDPTR tmp;
			GLOBAL_QUIET_MODE=(tmp=GetField123Check(prhs[5L],"quiet",3)) ? GetScalar(tmp) : 0L;
			return;	 
	} 
	GLOBAL_QUIET_MODE=0;
	return;
}
void * mainFunction(void *prhs[],int nrhs) {
	if (nrhs >=7) {
		int avxOption=GetScalar(prhs[nrhs - 1]); 
		SetupRoutines_UserChoice(avxOption);
	}	else {
		GetArg_IsQuiteMode(prhs,nrhs);      
		SetupRoutines_ByCPU(1L);              
	}
	if (nrhs==0 ) 	{
		r_error("ERROR: Essential input paramaters are missing!\n");
		return IDE_NULL;
	}
	if ( !IsChar(prhs[0]) )	{
		r_error("ERROR: The very first parameter must be a string specifying the algorithm name!\n");
		return IDE_NULL;
	}
	#define __STRING_LEN__ 20
	char  algorithm[__STRING_LEN__+1];
	GetCharArray(prhs[0],algorithm,__STRING_LEN__);
	void * ANS=NULL;
	int    nptr=0;
	if      (__IS_STRING_EQUAL(algorithm,beastv4Demo)) 	{
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
		#ifdef WIN64_OS
			option.extra.computeCredible=TRUE;
			ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;
			void DllExport BEAST2_WinMain(VOID_PTR  option);
			BEAST2_WinMain((BEAST2_OPTIONS_PTR)GLOBAL_OPTIONS);
		#else
			r_printf("WARNING: The GUI interface is supporte only on the Windows 64 operating system.\n");
		#endif	
		BEAST2_DeallocateTimeSeriesIO(&(option.io));
        #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		#endif
	}
	else if (__IS_STRING_EQUAL(algorithm,beastv4))
	{
		#if P_INTERFACE==1
			prhs[1]=CvtToPyArray_NewRef(prhs[1]);
		#endif
		BEAST2_OPTIONS      option={ {{0,},},}; 
		if (BEAST2_GetArgs(prhs,nrhs,&option)==0) {
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		    #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
		    #endif
			return IDE_NULL;
		}		
		option.io.out.result=NULL;		 	
		if (option.io.q==1) {
			ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;	
		} else {			
			option.extra.computeSeasonAmp=0;
			option.extra.computeTrendSlope=0;
			option.extra.tallyIncDecTrendJump=0;
			option.extra.tallyPosNegTrendJump=0;
			option.extra.tallyPosNegOutliers=0;
			option.extra.tallyPosNegSeasonJump=0;
			option.extra.computeTrendChngpt=1;
			option.extra.computeSeasonChngpt=1;
			BEAST2_print_options(&option); 
			ANS=PROTECT(BEAST2_Output_AllocMEM(&option)); nptr++;
		}
		GLOBAL_OPTIONS=(BEAST2_OPTIONS_PTR)&option;
		if (option.io.numOfPixels==1) {
			beast2_main_corev4();
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
			r_printf("\n");
		} else {
			I32 NUM_THREADS=option.extra.numParThreads;  
			I32 NUM_CORES=GetNumCores();
			I32 NUM_THREADS_PER_CPU=option.extra.numThreadsPerCPU;
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
			for (I32 i=0; i < NUM_THREADS; i++) {
             #if defined(LINUX_OS)||defined (WIN32_OS)||defined (WIN64_OS) 
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i,&cpuset);
				pthread_attr_setaffinity_np(&attr,sizeof(cpu_set_t),&cpuset);
				pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
			 #elif defined(MAC_OS)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i,&cpuset);
				pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				pthread_setaffinity_np(thread_id[i],sizeof(cpu_set_t),&cpuset);
		     #elif defined (SOLARIS_OS)
				cpu_set_t cpuset;
				CPU_ZERO(&cpuset);
				CPU_SET(i,&cpuset);
				pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
				sched_getaffinity(thread_id[i],sizeof(cpu_set_t),&cpuset);
			 #else
				pthread_create(&thread_id[i],&attr,beast2_main_corev4_mthrd,(void*)NULL);
			 #endif
				r_printf("Parallel computing: thread#%-02d generated ... \n",i+1);
			}
			r_printf("Rbeast: Waiting on %d threads...\n",NUM_THREADS);
			pthread_attr_destroy(&attr);
			IDE_USER_INTERRUPT=0;
			#if R_INTERACE==1
				r_printf("Press and hold the ESCAPE key or the STOP button to interrupt and quit while running.\n" );
			#elif M_INTERFACE==1
				r_printf("Press and hold CTR+C to interrupt and quit while running.\n");
			#endif
			if (option.extra.printProgressBar) {
				void* BUF=malloc(option.extra.consoleWidth* 3);
				PERCENT_COMPLETED=0;
				REMAINING_TIME=10000;
				printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,BUF,1);				
				while (PERCENT_COMPLETED < 1.f && NEXT_PIXEL_INDEX < option.io.numOfPixels && IDE_USER_INTERRUPT==0) {
					printProgress2(PERCENT_COMPLETED,REMAINING_TIME,option.extra.consoleWidth,BUF,0);
					Sleep_ms(2 * 1000);
					if (CheckInterrupt()) {
						ConsumeInterruptSignal();
						IDE_USER_INTERRUPT=1;
						r_printf("Quitting due to unexpected user interruption...\n");
					}
				}
				if (IDE_USER_INTERRUPT==0) printProgress2(1.0,0,option.extra.consoleWidth,BUF,0);				
				free(BUF);
			}  
			r_printf("\nFinalizing ... \n");
			for (I32 i=0; i < NUM_THREADS; i++) {
				I64 ret=0;
				pthread_join(thread_id[i],&ret);
				r_printf("Rbeast: Thread #%-02d finished ... \n",i);
			}
			if (IDE_USER_INTERRUPT==0)
				r_printf("\nRbeast: Waited on %d threads. Done.\n",NUM_THREADS);
			else
				r_printf("\nQuitted unexpectedly upon the user's interruption.\n");
			pthread_mutex_destroy(&mutex);
			pthread_cond_destroy(&condVar);
			free(thread_id);
			BEAST2_DeallocateTimeSeriesIO(&(option.io));
		}
	     #if P_INTERFACE==1
				Py_XDECREF(prhs[1]);
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
#if defined(MSVC_COMPILER)
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
#if (defined(WIN64_OS)||defined(WIN32_OS)) 
	SEXP TetrisSetTimer(SEXP action,SEXP seconds,SEXP envior);
	static const R_CallMethodDef CallEntries[]={
		#if defined(MSVC_COMPILER)
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
