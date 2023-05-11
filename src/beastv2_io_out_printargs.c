#include "abc_000_warning.h"

#include "abc_001_config.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "abc_datatype.h"
#include "abc_ide_util.h"
#include "abc_ts_func.h"
#include "abc_date.h"

#include "beastv2_io.h"
#include "globalvars.h"

void BEAST2_print_options(A(OPTIONS_PTR)  opt)
{	
	if (opt->extra.printOptions== 0 || GLOBAL_QUIET_MODE) return;
	A(METADATA_PTR) meta = &(opt->io.meta);
	BEAST2_IO_PTR   io    = &opt->io;
	I32PTR          dims  = io->dims;
	
	char* yesno[] = { "Yes", "No" };

#if M_INTERFACE==1
	char comment	= '%';
	char filler		= '.';
	char* emptyList = "[]";
	char* logicals[] = { "false", "true" };
	char* nanstr = "NaN";
	char* em1 = ""; "<strong>";
	char* em2 = ""; "</strong>";
#elif R_INTERFACE==1
	char comment = '#';
	char filler  = '$';
	char* emptyList = "list()";
	char* logicals[] = {"FALSE", "TRUE" };
	char* nanstr = "NaN";
	char* em1 = "";
	char* em2 = "";
#elif P_INTERFACE==1
	char comment = '#';
	char filler = '.';
	char* emptyList  = " rb.args() ### or 'lambda: None': just get an empty object###";
	char* logicals[] = { "False", "True" };
	char* nanstr     ="float('nan')";
	char* em1 = "";
	char* em2 = "";
#endif

	I08 hasSeasonCmpnt  = opt->prior.basisType[0] == SEASONID || opt->prior.basisType[0] == DUMMYID || opt->prior.basisType[0] == SVDID;
	I08 hasOutlierCmpnt = opt->prior.basisType[opt->prior.numBasis - 1] == OUTLIERID;
	I08 hasTrendCmpnt   = 1;
	I08 hasAny       = 1;

	// ##__VA_ARGS__ to eat off the trainling ,
	// __VA_OPT__(,)
	//http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0306r2.html
	//https://stackoverflow.com/questions/52891546/what-does-va-args-mean
	//https://stackoverflow.com/questions/5891221/variadic-macros-with-zero-arguments
	//https://web.archive.org/web/20070616054343/http://developers.sun.com/sunstudio/support/CCcompare.html

	#define Print1(fmtstr,hasComponent,...) if(hasComponent)  r_printf(fmtstr, ##__VA_ARGS__);
	#define Print2(fmtstr,hasComponent,...) if(hasComponent)  r_printf(fmtstr __VA_OPT__(,)  __VA_ARGS__);
	#define Print(fmtstr,hasComponent,...) {if(hasComponent) r_printf(fmtstr,  __VA_ARGS__);}

	Print("%s", hasAny, "\n");
	Print("%sINFO%s: To supress printing the parameers in beast123(),   set extra%cprintOptions = 0  \n", hasAny,  em1, em2,filler);
	Print("%sINFO%s: To supress printing the parameers in beast(),      set print.options = 0 \n", hasAny, em1, em2, filler);
	Print("%sINFO%s: To supress printing the parameers in beast.irreg(),set print.options = 0 \n", hasAny, em1, em2, filler);
	Print("%sINFO%s: To supress warning messages in beast123(),         set extra%cquiet = 1  \n", hasAny, em1, em2, filler);
	Print("%sINFO%s: To supress warning messages in beast(),            set quiet = 1 \n", hasAny, em1, em2, filler);
	Print("%sINFO%s: To supress warning messages in beast.irreg(),      set quiet = 1 \n", hasAny, em1, em2, filler);

	Print("%s", hasAny, "\n");
	Print("%c--------------------------------------------------%c\n", hasAny, comment, comment);
	Print("%c       Brief summary of Input Data                %c\n", hasAny, comment, comment);
	Print("%c--------------------------------------------------%c\n", hasAny, comment, comment);	
	Print("%sData Dimension%s: One signal of length %d\n",                    hasAny && io->ndim == 1, em1, em2,    dims[0]); 
	Print("%sData Dimension%s: [%dx%d] - %d signals of length %d each\n",    hasAny && io->ndim == 2, em1, em2, dims[0], dims[1], io->numOfPixels, dims[io->meta.whichDimIsTime-1]);
	Print("%sData Dimension%s: [%dx%dx%d] - %d signals of length %d each\n", hasAny && io->ndim == 3, em1, em2, dims[0], dims[1], dims[2], io->numOfPixels, dims[io->meta.whichDimIsTime - 1]);
	Print("%sIsOrdered%s     : %s\n", hasAny, em1, em2, io->T.isOrderd ? "Yes, ordered in time": "No, unordered in time, to be sorted/ordered before running BEAST" );

	int unitFormat = 3;
	if (meta->isDate &&  io->T.asDailyTS) unitFormat = 1;  // analyazed as daily time series
	if (meta->isDate && !io->T.asDailyTS) unitFormat = 2; // unit is fractional year

	char* yesMsg = "Yes, evenly spaced at interval of ";
	char* noMsg  = "No, unevenly spaced at avg interval of ";
	char msg[100];
	F32  dT = io->T.data_dT;
	if (unitFormat == 1) snprintf(msg, 99,"%g days", dT);
	if (unitFormat == 2) snprintf(msg, 99, "%g year = %g months = %g days", dT, dT *12, dT *365);
	if (unitFormat == 3) snprintf(msg, 99, "%g (unknown unit)", dT);
	Print("%sIsRegular%s     : %s %s\n", hasAny, em1, em2, io->T.isRegular ? yesMsg : noMsg, msg);
	 	
	dT = io->meta.deltaTime;
	if (unitFormat == 1) snprintf(msg, 99, "%g days", dT);
	if (unitFormat == 2) snprintf(msg, 99, "%g year = %g months = %g days", dT, dT * 12, dT * 365);
	if (unitFormat == 3) snprintf(msg, 99, "%g (unknown unit)", dT);
	if (!io->T.needReordered && io->T.needAggregate && !io->meta.isRegular) {
	Print("%sPreprocessing%s : Aggregate irregular data into a regular interval of %s\n", 1L, em1, em2, msg );
	} else if (!io->T.needReordered && io->T.needAggregate && io->meta.isRegular) {
	Print("%sPreprocessing%s : Resample regular data to a regular interval of %s\n", 1L, em1, em2, msg);
	}

		
	if (meta->hasSeasonCmpnt) {
		F32 period = meta->period*meta->deltaTime;
		if (unitFormat == 1) snprintf(msg, 99, "%g days", period);
		if (unitFormat == 2) snprintf(msg, 99, "%g year = %g months = %g days", period, period * 12, period * 365);
		if (unitFormat == 3) snprintf(msg, 99, "%g (unknown unit)", period);

	Print("%sHasSeasonCmpnt%s: %-5s | period = %s. The model 'Y=Trend+Season+Error' is fitted.\n", hasAny, em1, em2, logicals[!!meta->hasSeasonCmpnt], msg);
	Print("              : Num_of_DataPoints_per_Period = period/deltaTime = %g/%g = %g\n", hasAny, meta->period* meta->deltaTime,meta->deltaTime, meta->period);
	Print("%sHasOutlierCmpt%s: %-5s | If true, Y=Trend+Season+Outlier+Error fitted instead of Y=Trend+Season+Error\n", hasAny, em1, em2, logicals[!!meta->hasOutlierCmpnt]);
	} else{
	Print("%shasSeasonCmpnt%s: %-5s | no periodic or seasonal component. The model Y=Trend+Error is fitted.\n", hasAny, em1, em2, logicals[!!meta->hasSeasonCmpnt]);
	Print("%sHasOutlierCmpt%s: %-5s | If true, Y=Trend+Outlier+Error (experimental) is fitted instead of Y=Trend+Error \n", hasAny, em1, em2, logicals[!!meta->hasOutlierCmpnt]);		
	}
	Print("%sDeseasonalize%s : %-5s | If true, remove a global seasonal  cmpnt before running BEAST & add it back after BEAST\n", hasSeasonCmpnt, em1, em2, logicals[!!meta->deseasonalize]);
	Print("%sDetrend%s       : %-5s | If true, remove a global trend component before running BEAST & add it back after BEAST\n", hasAny, em1, em2, logicals[!!meta->detrend]);

	if (IsNaN(meta->missingValue)) msg[0] = 0;
	else                           snprintf(msg, 99, " or %g ", meta->missingValue);
	Print("%sMissingValue%s  : NaN %s flagged as missing values \n", 1L, em1, em2, msg);
	Print("%sMissingRate%s   : if more than %g%% of data is missing, BEAST will skip it.\n", 1L, em1, em2, meta->maxMissingRate * 100);

	Print("%s", hasAny, "\n");

	 

	Print("%s", hasAny,"\n");
	Print("%c--------------------------------------------------%c\n", hasAny,comment,comment);
	Print("%c      OPTIONS used in the MCMC inference          %c\n", hasAny, comment, comment);
	//Print("%c--------------------------------------------------%c\n", hasAny, comment, comment);
	Print("%c--------------------------------------------------%c\n", hasAny, comment, comment);
	Print("%s", hasAny, "\n");	
	Print("%c......Start of displaying 'MetaData' ......\n", hasAny, comment); 
	Print("metadata                = %-10s %c metadata is used to interpret the input data\n",	hasAny, emptyList,comment);
	//Print("  metadata%cisRegular        = %s\n", hasAny, filler, logicals[!!meta->isRegular]);
	//Print("  metadata%cisOrdered        = %s\n", hasAny, filler, logicals[!!meta->isOrdered]);
	//Print("  metadata%cneedAggregate    = %s\n",	hasAny,		filler, logicals[!!meta->needAggregate]);
	Print("metadata%cseason         = 'none'     %c trend-only data with no periodic variation\n",     !hasSeasonCmpnt, filler, comment)                                //no seasonal cmpnt
	Print("metadata%cseason         = 'harmonic' %c fit a harmonic model to the periodic component\n", hasSeasonCmpnt&&meta->seasonForm =='S', filler, comment)  //haromic season basis
	Print("metadata%cseason         = 'dummy'    %c fit a dummy model to the periodic component\n",    hasSeasonCmpnt&&meta->seasonForm =='D', filler, comment)
	Print("metadata%cseason         = 'svd'      %c fit the periodic component with singlular-vector-decomposition bases \n",      hasSeasonCmpnt&&meta->seasonForm =='V', filler, comment)
	if (io->meta.isDate) {
		if (io->T.asDailyTS) {			
			F32 start = fractional_civil_from_days( (int) meta->startTime);
			Print("metadata%cstartTime      = %-10g %c unit: %g days lapsed since 1970-01-01\n", hasAny, filler, start, comment, meta->startTime);
			Print("metadata%cdeltaTime      = %-10g %c unit: days\n", hasAny, filler, meta->deltaTime, comment);
			Print("metadata%cperiod         = %-10g %c unit: days\n", hasSeasonCmpnt, filler, meta->period * meta->deltaTime, comment);
		}
		else {
			int mon, day;
			int yr = F32time2YMD(meta->startTime, &mon, &day);			
			Print("metadata%cstartTime      = %-10g %c %04d-%02d-%02d\n", hasAny, filler, meta->startTime, comment, yr, mon, day);
			Print("metadata%cdeltaTime      = %-10g %c %g year(s) = %g month(s) = %g day(s)\n", hasAny, filler, meta->deltaTime, comment, meta->deltaTime, meta->deltaTime * 12, meta->deltaTime * 365);
			Print("metadata%cperiod         = %-10g %c %g year(s) = %g month(s) = %g day(s)\n", hasSeasonCmpnt, filler, meta->period * meta->deltaTime, comment, meta->period * meta->deltaTime, meta->period * meta->deltaTime * 12, meta->period * meta->deltaTime * 365);
		}
	} else {			
			Print("metadata%cstartTime      = %-10g %c unknown unit\n", hasAny, filler, meta->startTime, comment);
			Print("metadata%cdeltaTime      = %-10g %c unknown unit\n", hasAny, filler, meta->deltaTime, comment);
			Print("metadata%cperiod         = %-10g %c unknown unit\n", hasSeasonCmpnt, filler, meta->period * meta->deltaTime, comment);
	}
	 
	Print("metadata%cwhichDimIsTime = %d\n",   hasAny && io->ndim > 1, filler, meta->whichDimIsTime); 
	Print("metadata%cmissingValue   = %g\n",   meta->missingValue==meta->missingValue, filler, meta->missingValue);	
	//Print("  metadata%cmissingValue   = %s\n",   meta->missingValue!=meta->missingValue, filler, nanstr);
	Print("metadata%cMissingRate    = %-10g %c if more than %g%% of data is missing, BEAST will skip it.\n", hasAny,	filler, meta->maxMissingRate, comment, meta->maxMissingRate * 100);

	Print("metadata%cdeseasonalize  = %-10s %c If true,remove a global seasonal cmpnt before running BEAST & add it back later\n",   hasSeasonCmpnt, filler, logicals[!!meta->deseasonalize],comment);
	Print("metadata%cdetrend        = %-10s %c If true,remove a global trend  cmpnt before running BEAST & add it back later\n",   hasAny,         filler, logicals[!!meta->detrend], comment);
	Print("%c........End of displaying MetaData ........\n\n",	hasAny,  comment);

	A(PRIOR_PTR) prior = &(opt->prior);

	Print("%c......Start of displaying 'prior' ......\n", hasAny, comment);
	Print("prior                  = %-10s %c prior is the key model parameters of BEAST\n", hasAny, emptyList, comment);
	Print("prior%cseasonMinOrder   = %-10d %c sorder.minmax[1]: min harmonic order alllowed\n", hasSeasonCmpnt, filler, prior->seasonMinOrder,comment );
	Print("prior%cseasonMaxOrder   = %-10d %c sorder.minmax[2]: max harmonic order alllowed\n", hasSeasonCmpnt, filler, prior->seasonMaxOrder,comment);
	Print("prior%cseasonMinKnotNum = %-10d %c scp.minmax[1]   : min num of seasonal chngpts\n", hasSeasonCmpnt, filler, prior->seasonMinKnotNum, comment);
	Print("prior%cseasonMaxKnotNum = %-10d %c scp.minmax[2]   : max num of seasonal chngpts\n", hasSeasonCmpnt, filler, prior->seasonMaxKnotNum, comment);
	Print("prior%cseasonMinSepDist = %-10d %c sseg.min        : min seasonal segment length in terms of datapoints\n", hasSeasonCmpnt, filler, prior->seasonMinSepDist,comment);
	Print("prior%ctrendMinOrder    = %-10d %c torder.minmax[1]: min trend polynomial order alllowed\n", hasTrendCmpnt,  filler, prior->trendMinOrder, comment);
	Print("prior%ctrendMaxOrder    = %-10d %c torder.minmax[2]: max trend polynomial order alllowed\n", hasTrendCmpnt,  filler, prior->trendMaxOrder, comment);
	Print("prior%ctrendMinKnotNum  = %-10d %c tcp.minmax[1]   : min num of chngpts in trend\n", hasTrendCmpnt,  filler, prior->trendMinKnotNum, comment);
	Print("prior%ctrendMaxKnotNum  = %-10d %c tcp.minmax[2]   : min num of chngpts in trend\n", hasTrendCmpnt,  filler, prior->trendMaxKnotNum, comment);
	Print("prior%ctrendMinSepDist  = %-10d %c tseg.min        : min trend segment length in terms of datapoints\n", hasTrendCmpnt,  filler, prior->trendMinSepDist, comment);
	Print("prior%coutlierMaxKnotNum= %-10d %c ocp             : max num of datapoints treated as outliers\n", hasOutlierCmpnt, filler, prior->outlierMaxKnotNum, comment);
	Print("prior%coutlierSigFactor = %-10g %c above which datapoints considered outliers\n", hasOutlierCmpnt, filler, prior->outlierSigFactor, comment);
	Print("prior%cK_MAX            = %-10d %c max number of terms in general linear model (useful only at small values)\n", hasAny,      filler, prior->K_MAX, comment);
	Print("prior%cprecValue        = %-10g %c useful mainly when precPriorType='constant'\n", hasAny,      filler, prior->precValue, comment);
	Print("prior%cmodelPriorType   = %-10d\n", hasAny, filler, prior->modelPriorType);
	//Print("   prior%calpha1           = %f\n", hasAny,      filler, prior->alpha1 );
	//Print("   prior%calpha2           = %f\n", hasAny,      filler, prior->alpha2);
	//Print("   prior%cdelta1           = %f\n", hasAny,      filler, prior->delta1);
	//Print("   prior%cdelta2           = %f\n", hasAny,      filler, prior->delta2);	
	
	if (prior->precPriorType == ConstPrec)
	Print("prior%cprecPriorType    = '%s'\n", hasAny, filler, "constant")
	else if (prior->precPriorType == UniformPrec)
	Print("prior%cprecPriorType    = '%s'\n", hasAny, filler, "uniform")
	else if (prior->precPriorType == ComponentWise)
	Print("prior%cprecPriorType    = '%s'\n", hasAny, filler, "componentwise")
	else if (prior->precPriorType == OrderWise)
	Print("prior%cprecPriorType    = '%s'\n", hasAny, filler, "orderwise");

	Print("%c......End of displaying pripr ......\n\n", hasAny, comment);

 
	A(MCMC_PTR) mcmc = &(opt->mcmc);

	Print("%c......Start of displaying 'mcmc' ......\n", hasAny, comment);
	Print("mcmc                           = %-10s %c mcmc is used to specify the inference algorithm\n",	 hasAny, emptyList,comment);
	Print("mcmc%cseed                      = %d\n", hasAny, filler, mcmc->seed);
	Print("mcmc%csamples                   = %d\n", hasAny, filler, mcmc->samples);
	Print("mcmc%cthinningFactor            = %d\n", hasAny, filler, mcmc->thinningFactor);
	Print("mcmc%cburnin                    = %d\n", hasAny, filler, mcmc->burnin);
	Print("mcmc%cchainNumber               = %d\n", hasAny, filler, mcmc->chainNumber);
	Print("mcmc%cmaxMoveStepSize           = %d\n", hasAny, filler, mcmc->maxMoveStepSize);
	Print("mcmc%ctrendResamplingOrderProb  = %g\n", hasTrendCmpnt, filler,  mcmc->trendResamplingOrderProb);
	Print("mcmc%cseasonResamplingOrderProb = %g\n", hasSeasonCmpnt,filler, mcmc->seasonResamplingOrderProb );
	Print("mcmc%ccredIntervalAlphaLevel    = %g\n", hasAny, filler, mcmc->credIntervalAlphaLevel);
	Print("%c......End of displaying mcmc ......\n\n", hasAny, comment);

 
	A(EXTRA_PTR) extra = &(opt->extra);

	Print("%c......Start of displaying 'extra' ......\n", hasAny, comment); 
	Print("extra                      = %-10s %c extra is used to configure output/computing options\n", hasAny, emptyList,comment);
	Print("extra%cdumpInputData        = %s\n", hasAny,      filler, logicals[!!extra->dumpInputData]);
	Print("extra%cwhichOutputDimIsTime = %d\n", hasAny,	   filler, extra->whichOutputDimIsTime );
	Print("extra%ccomputeCredible      = %s\n", hasAny,      filler, logicals[!!extra->computeCredible]);
	Print("extra%cfastCIComputation    = %s\n", hasAny,      filler, logicals[!!extra->fastCIComputation] );
	Print("extra%ccomputeSeasonOrder   = %s\n", hasSeasonCmpnt, filler, logicals[!!extra->computeSeasonOrder]);
	Print("extra%ccomputeTrendOrder    = %s\n", hasTrendCmpnt,  filler, logicals[!!extra->computeTrendOrder]  );
	Print("extra%ccomputeSeasonChngpt  = %s\n", hasSeasonCmpnt, filler, logicals[!!extra->computeSeasonChngpt]);
	Print("extra%ccomputeTrendChngpt   = %s\n", hasTrendCmpnt,  filler, logicals[!!extra->computeTrendChngpt] );
	Print("extra%ccomputeOutlierChngpt = %s\n", hasOutlierCmpnt,filler, logicals[!!extra->computeOutlierChngpt]  );
	Print("extra%ccomputeSeasonAmp     = %s\n", hasSeasonCmpnt, filler, logicals[!!extra->computeSeasonAmp]);
	Print("extra%ccomputeTrendSlope    = %s\n", hasTrendCmpnt,  filler, logicals[!!extra->computeTrendSlope]);
	
	Print("extra%ctallyPosNegSeasonJump= %s\n", hasSeasonCmpnt, filler, logicals[!!extra->tallyPosNegSeasonJump]);
	Print("extra%ctallyPosNegTrendJump = %s\n", hasTrendCmpnt,  filler, logicals[!!extra->tallyPosNegTrendJump]);
	Print("extra%ctallyIncDecTrendJump = %s\n", hasTrendCmpnt,  filler, logicals[!!extra->tallyIncDecTrendJump]);
	Print("extra%ctallyPosNegOutliers  = %s\n", hasOutlierCmpnt,filler, logicals[!!extra->tallyPosNegOutliers] );

	Print("extra%cprintProgressBar     = %s\n", hasAny, filler, logicals[!!extra->printProgressBar]);
	Print("extra%cprintOptions         = %s\n", hasAny, filler, logicals[!!extra->printOptions]);
	Print("extra%cconsoleWidth         = %d\n", hasAny, filler, extra->consoleWidth);
	//Print("  extra%cnumCPUCoresToUse     = %d\n", filler, extra->numCPUCoresToUse, hasAny);
	Print("extra%cnumThreadsPerCPU     = %d\n", hasAny, filler, extra->numThreadsPerCPU);
	Print("extra%cnumParThreads        = %d\n", hasAny, filler, extra->numParThreads);
	Print("%c......End of displaying extra ......\n\n", hasAny, comment);
 
#if M_INTERFACE==1
	extern void matlab_IOflush(void);
	matlab_IOflush();
#endif
}

#include "abc_000_warning.h"
