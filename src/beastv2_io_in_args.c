#include "abc_000_warning.h"
#include "abc_001_config.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "abc_datatype.h"
#include "abc_blas_lapack_lib.h"
#include "abc_ide_util.h"  
#include "abc_common.h"    
#include "abc_ts_func.h"
#include "abc_date.h"
#include "beastv2_func.h"    
#include "beastv2_io.h"
#define _IsAlmostInteger(x)  ( fabs(x-round(x)) <1e-3 )
static int  GetArg_0th_Data(VOIDPTR prhs[],int nrhs,BEAST2_IO_PTR _OUT_ io) {
	if (nrhs < 2L) {
		r_error("ERROR: At least one input argument is needed!\n");
		return 0;
	}
	VOIDPTR DATA=prhs[1];
	int     numel=GetNumberOfElements(DATA);
	if ( !(IsNumeric(DATA) && numel > 2)  && !(IsStruct(DATA) && numel >=1) && !IsCell(DATA)) {
		r_error("ERROR: The input data should be numeric and must be long enough.\n");
		return 0;
	}
	I32       q=0;
	VOID_PTR  Y=NULL;
	if ((IsStruct(DATA) && numel >=1)||IsCell(DATA)) {
		q=numel;
		io->pdata=malloc(sizeof(VOID_PTR) * q);
		io->dtype=malloc(sizeof(DATA_TYPE) * q);
		for (I32 i=0; i < q; i++) {
			Y=GetFieldByIdx(DATA,i);
			io->pdata[i]=GetData(Y);
			io->dtype[i]=GetDataType(Y);
		}
	} else {
		q=1;
		io->pdata=malloc(sizeof(VOID_PTR) * q);
		io->dtype=malloc(sizeof(DATA_TYPE) * q);
		io->pdata[0]=GetData(DATA);
		io->dtype[0]=GetDataType(DATA);
		Y=DATA;
	}
	for (int i=0; i < q; i++) {
		if (io->dtype[i]==DATA_UNKNOWN) {
			r_error("ERROR: The input data has an uknown numeric type!\n");
			return 0;
		}
	}
	io->q=q;
	io->timedim=-1L;   
	I32 ndims=GetNumOfDim(Y);	
	if (ndims==0||	 
		ndims==1)      
	{
		I32 N=GetNumberOfElements(Y);
		io->ndim=1L;
		io->dims[0]=N;
		io->dims[1]=1L;
		io->dims[2]=1L; 
		io->timedim=1L; 		
	}
	else if (ndims==2) {
		int N=GetDim1(Y);
		int M=GetDim2(Y);
		if (min(N,M)==1L) {
			N=max(N,M),
			io->ndim=1L;
			io->dims[0]=N;
			io->dims[1]=1L;
			io->dims[2]=1L;
			io->timedim=1L;
		} else {
			io->ndim=2L;
			io->dims[0]=N;
			io->dims[1]=M;
			io->dims[2]=1L; 
		}
	}
	else if (ndims==3) {
		io->ndim=3L;
		GetDimensions(Y,io->dims,3L);	
	}
	else {
		r_printf("ERROR: The maximum dimension allowed is 3 when data is a 3D stack of images over time,but the input has a dimension of %d.\n",ndims);
		return 0;
	}
	return 1;
}
static int  GetArg_1st_MetaData(VOIDPTR prhs[],int nrhs,BEAST2_IO_PTR _OUT_ io) {
	VOIDPTR  tmp;
	BEAST2_METADATA_PTR meta=&io->meta;
	int METADATA_NONE=0;
	int METADATA_NumericScalar=0;
	int METADATA_NumericVector=0;
	int METADATA_CharVector=0;
	int METADATA_CharScaler=0;
	int METADATA_Struct=0;
	int METADATA_OTHER=0;
	VOIDPTR pmeta=nrhs < 3 ? NULL : prhs[2L];
	VOIDPTR TIMEobj=NULL;
	meta->whichDimIsTime=-1L;	
	meta->startTime=getNaN();
	meta->deltaTime=getNaN();
	meta->isDate=0;
	if ( pmeta==NULL||IsEmpty(pmeta) ) {
		METADATA_NONE=1;
	} else if ( IsNumeric(pmeta) ) {
		int numel=GetNumberOfElements(pmeta);
		if      (numel==0) METADATA_NONE=1;
		else if (numel==1) METADATA_NumericScalar=1;
		else if (numel > 1)  METADATA_NumericVector=1,TIMEobj=pmeta;
		else 			     METADATA_OTHER=1;		
	} else if ( IsChar(pmeta) ) {
		int numel=GetNumberOfElements(pmeta);
		if      (numel==0) METADATA_NONE=1;
		else if (numel==1) METADATA_OTHER=1;
		else if (numel > 1)  METADATA_CharVector=1,TIMEobj=pmeta;
		else 			     METADATA_OTHER=1;
	} else if (IsStruct(pmeta) ) {
		METADATA_Struct=1;	
		TIMEobj=(tmp=GetField123Check(pmeta,"time",2))           ? tmp            : NULL;
		TIMEobj=tmp && IsEmpty(tmp)? NULL: tmp;  
		meta->whichDimIsTime=(tmp=GetField123Check(pmeta,"whichDimIsTime",2)) ? GetScalar(tmp) : -1;		
		if (io->ndim==1 && meta->whichDimIsTime !=-1) {			
			q_warning("WARNING: metadata$whichDimIsTime=%d is ignored because 'whichDimIsTime' is used only for 2D matrix or 3D array inputs "
					  "but your input is a 1D vector.\n",io->meta.whichDimIsTime);
		}
		I08 isDefaultStartTime=0;
		tmp=GetField123Check(pmeta,"startTime",2);
		tmp=tmp && IsEmpty(tmp) ? NULL : tmp;
		if (tmp && IsClass(tmp,"Date")) {
			int   days=GetScalar(tmp);
			meta->startTime=fractional_civil_from_days(days);
			meta->isDate=1;
		} else if (tmp && IsNumeric(tmp)) {
			char* msg="A valid startTime should be either a numeric scalar,a vector of two values (Year,Month),or a vector of three values (Year,Month,Day).";
			I32   n=GetNumberOfElements(tmp);
			if (n==1)
				meta->startTime=GetScalar(tmp);
			else if (n==2) {
				F32 Y=GetNumericElement(tmp,0),M=GetNumericElement(tmp,1);
				meta->startTime=Y+M/12-1/24.0;
				meta->isDate=1;
				q_warning("WARNING: metadata$startTime=[%g,%g] is interpreted as %04d/%02d/15 (Year/Month/Day) and converted to "
					      "a decimal year of %g. If not making sense,supply a correct start time: %s\n",Y,M,(int)Y,(int)M,meta->startTime,msg);
			}
			else if (n==3) {
				F32 Y=GetNumericElement(tmp,0),M=GetNumericElement(tmp,1),D=GetNumericElement(tmp,2);
				meta->startTime=(F32) YMDtoF32time((int)Y,(int)M,(int)D);
				meta->isDate=1;
				q_warning("WARNING: Your metadata$startTime=[%g,%g,%g] is interpreted as %04d/%02d/%02d (Year/Month/Day) and converted to "
					       "a decimal year of %g. If not making sense,supply a correct start time: %s\n",Y,M,D,(int)Y,(int)M,(int)D,meta->startTime,msg);
			}
			else {
				r_error("ERROR: Your metadata$startTime has more than three elements. %s\n",msg);
				return 0;
			}
		} else {
			meta->startTime=getNaN();
			isDefaultStartTime=1L;
		}
		tmp=GetField123Check(pmeta,"deltaTime",3);
		tmp=tmp && IsEmpty(tmp) ? NULL : tmp;
		if ( tmp==NULL  ) {
			meta->deltaTime=getNaN();
		} 
		else if (tmp && IsNumeric(tmp)) {
			meta->deltaTime=GetScalar(tmp);
		}
		else if (tmp && IsChar(tmp)) {
			char s[100+1];
			s[0]=0;
			int  slen=GetCharVecElem(tmp,0,s,100);
			F64  dt=extract_fyear(s);
			if (IsNaN(dt)) {
				r_error("ERROR: Unable to intepret metadata$deltaTime='%s'!\n",s);
				return 0;
			}
			meta->deltaTime=dt;
			meta->isDate=1;
		}
		else {
			r_error("ERROR: metadata$deltaTime is of unrecognizable type!\n");
			return 0;
		}
		if (meta->deltaTime <=0) {
			r_error("ERROR: metadata$deltaTime must be a positive time interval!\n");
			return 0;
		}
	} else {
		METADATA_OTHER=1;
	}
	if (METADATA_OTHER==1) {
		r_error("ERROR: The 'metadata' parameter given is of unsupported type.\n");
		return 0;
	}
	meta->hasSeasonCmpnt=-1;
	meta->period=getNaN();
	I08 metaProcessed=0;
	if (METADATA_NONE||METADATA_NumericScalar||METADATA_CharVector||METADATA_CharScaler||METADATA_NumericVector) {
		if (METADATA_NumericScalar) {
			meta->period=GetScalar(pmeta);
			if (meta->period <=0) {
				meta->hasSeasonCmpnt=0;
				q_warning("WARNING: A negative or zero value of period (%g) means no periodic/seasonal component in the input time series!\n",meta->period);
			} else {
				meta->hasSeasonCmpnt=1;
				if (!_IsAlmostInteger(meta->period)) {
					r_error("ERROR: When metadata is supplied as a single number %g,it must be an integer to specify the period of the regular time seires!\n",meta->period);
					return 0;
				}
				meta->period=round(meta->period);
			} 
		}
		else if (METADATA_CharScaler) {
			char period[100+1];
			GetCharArray(pmeta,period,100);
			if (strcicmp(period,"none")==0) {
				meta->hasSeasonCmpnt=0;
				meta->period=0;
			} else {
				r_error("ERROR: When metadata is supplied as a string to speciify period,it can only be 'none'!\n");
				return 0;
			}
		}
		else {
			meta->hasSeasonCmpnt=1;
		}
		meta->hasOutlierCmpnt=0;
		meta->seasonForm='S';
		meta->detrend=0;
		meta->deseasonalize=0;
		meta->missingValue=getNaN();
		meta->maxMissingRate=0.75;
		metaProcessed=1;
	} 
	if (METADATA_Struct) {
		tmp=GetField123Check(pmeta,"period",2);
		if (tmp==NULL||IsEmpty(tmp)) {
			meta->period=getNaN();
		}
		else if (tmp && IsNumeric(tmp)) {
			meta->period=GetScalar(tmp);
			if (meta->period <=0) {
				meta->hasSeasonCmpnt=0;
				q_warning("WARNING: A negative or zero value of period (%g) indicates no periodic/seasonal component in the input time series!\n",meta->period);
			}
			if (meta->period > 0) {
				if (meta->period/meta->deltaTime > 2.0) {
					meta->hasSeasonCmpnt=1;
				}				
			}
		}
		else if (tmp && IsChar(tmp)) {
			char s[100+1];
			s[0]=0;
			int  slen=GetCharVecElem(tmp,0,s,100);
			if (strcicmp(s,"none")==0) {
				meta->hasSeasonCmpnt=0;
				meta->period=0;
			}
			else {
				F64  dt=extract_fyear(s);
				if (IsNaN(dt)) {
					r_error("ERROR: Unable to intepret metadata$period='%s'!\n",s);
					return 0;
				}
				meta->period=dt;
				meta->isDate=1;
			}
		}
		else {
			r_error("ERROR: metadata$period is of unrecognizable type!\n");
			return 0;
		}
		tmp=GetField123Check(pmeta,"season",2);
		tmp=tmp && IsEmpty(tmp) && !IsChar(tmp) ? NULL : tmp;
		char season[20+1];
		season[0]=0;
		if (tmp) {
			GetCharArray(tmp,season,20);
			ToUpper(season);
		}
	   if (meta->hasSeasonCmpnt==0) {
		   if (tmp && season[0] !='N'){
			   q_warning("WARNING: A confilict found between metadata$season=%s and period=%g. '%s' suggests a time series with periodic variations but "
				         "period=%g or period='none' suggests a trend-only time series without any periodic variations. The season parameter is "
				         "ignored and no seasonal component is assumed for the input.\n",season,meta->period,season,meta->period);
	        }		
		} else if  (tmp !=NULL && IsChar(tmp) ) {			
			int  hasSeasonCmpt_season=1;
			char a=season[0],b=season[1];
			if      (a=='N' && b=='O') 		hasSeasonCmpt_season=0;							    
			else if (a=='H' && b=='A') 		hasSeasonCmpt_season=1,meta->seasonForm='S'; 	
			else if (a=='D' && b=='U')		hasSeasonCmpt_season=1,meta->seasonForm='D';			
			else if (a=='S' && b=='V')		hasSeasonCmpt_season=1,meta->seasonForm='V';			
			else {
				hasSeasonCmpt_season=1;
				meta->seasonForm='S';  
				q_warning("WARNING: metadata$season='%s' has an unrecongizable string. The default season='harmonic' is used instead.\n",season);
			}
			if (meta->hasSeasonCmpnt==1 && hasSeasonCmpt_season==0) {
				hasSeasonCmpt_season=1;
				meta->seasonForm='S';
				q_warning("WARNING: A confilict found between metadata$season='none' and period=%g. season='none' suggests a time series with "
					       "no periodic variations but period=%g suggests otherwise. The season='none' parameter is ignored and "
					      "the data is assumed to have a seasonal component.\n",meta->period,meta->period);
			}
			if (meta->seasonForm=='V') {
				meta->svdTerms=GetFieldCheck(pmeta,"svdTerms");
				if (meta->svdTerms==NULL) {
					meta->seasonForm='S';  
					q_warning("WARNING: metadata$svdTerms is missing. The default season='harmonic' is used instead.\n");
				}
			}	 
			meta->hasSeasonCmpnt=hasSeasonCmpt_season;
		} 	else	{
			if (meta->hasSeasonCmpnt==-1||meta->hasSeasonCmpnt==1) {
				meta->hasSeasonCmpnt=1;
				meta->seasonForm='S';
				q_warning("WARNING: metadata$season is either missing or not given as a valid specifier string (e.g.,none,harmonic,or dummy). A default season='harmonic' is assumed.\n");
			}
		}	
		if (meta->hasSeasonCmpnt==-1) {
			r_error("ERROR: hasSeasonCmpnt=-1; there must be somethong wrong!");
			return 0;
		}
		meta->hasOutlierCmpnt=(tmp=GetField123Check(pmeta,"hasOutlierCmpnt",2)) ? GetScalar(tmp) : 0L;
		meta->detrend=(tmp=GetField123Check(pmeta,"detrend",3)) ?  GetScalar(tmp) : 0L;
		meta->deseasonalize=(tmp=GetField123Check(pmeta,"deseasonalize",3)) ?  GetScalar(tmp) : 0L;
		meta->missingValue=(tmp=GetField123Check(pmeta,"missingValue",2))   ? GetScalar(tmp) : getNaN();
		meta->maxMissingRate=(tmp=GetField123Check(pmeta,"maxMissingRate",2)) ? GetScalar(tmp) : 0.75;
		metaProcessed=1;
	} 
    if (TIMEobj) { 
		I32    Nrawtime;
		meta->isDate=!IsNumeric(TIMEobj) ? 1 : meta->isDate;
		F32PTR f32time=CvtTimeObjToF32Arr(TIMEobj,&Nrawtime); 
		if (f32time==NULL) {
			r_error("ERROR: Unable to read and intepret 'time' or 'metadata$time'!\n");
			return 0;
		}
		int matcheNumDims=(Nrawtime==io->dims[0])+(Nrawtime==io->dims[1])+(Nrawtime==io->dims[2]);
		if (matcheNumDims==0) {
			free(f32time);
			r_error("ERROR: The input data must have the same length as the time in metadata.\n");
			return 0;
		} else if ( matcheNumDims==1) {
			int  timeDimMatched;
			if (Nrawtime==io->dims[0])  timeDimMatched=1;
			if (Nrawtime==io->dims[1])  timeDimMatched=2;
			if (Nrawtime==io->dims[2])  timeDimMatched=3;
			if ( io->meta.whichDimIsTime !=-1 && io->meta.whichDimIsTime !=timeDimMatched) {
				q_warning("WARNING: the specified metadata$whichDimIsTime=%d is ignored; 'whichDimIsTime=%d' is instead used based on the match between the input data and time.\n",io->meta.whichDimIsTime,timeDimMatched);
			}
			io->meta.whichDimIsTime=timeDimMatched;
			io->timedim=timeDimMatched;
		} else { 
			int whichDimIsTime=meta->whichDimIsTime;	
			if ( whichDimIsTime==-1||( io->ndim==2 && whichDimIsTime !=1 && whichDimIsTime !=2 ) ) {
				free(f32time);
				r_error("ERROR: For a 2D matrix input of size [%d x %d] (i.e.,multiple time series),metadata$whichDimIsTime must be given "
						"to tell which dim of the matrix  refers to time. It must take a value out of 1 or 2 only.\n",io->dims[0],io->dims[1]);
				return 0;
			}
			if (whichDimIsTime==-1||(io->ndim==3 && whichDimIsTime !=1 && whichDimIsTime !=2 && whichDimIsTime !=3)) {
				free(f32time);
				r_error("ERROR: For a 3D array input of size [%d x %d x %d] (i.e.,stacked time series images),metadata$whichDimIsTime must be given "
					"to tell which dim of the 3D array  refers to time. It must take a value out of 1,2 or 3 only.\n",io->dims[0],io->dims[1],io->dims[2]);
				return 0;
			}
			if ( io->dims[whichDimIsTime] !=Nrawtime ) {
				free(f32time);
				r_error("ERROR: The length of the time dimension of the input (whichDimIsTime=%d) doesn't match the length of time/metadata$time (i.e.,%d!=%d).\n",whichDimIsTime,io->dims[whichDimIsTime],Nrawtime);
				return 0;
			}
			io->timedim=whichDimIsTime;
		}
		meta->deltaTime=meta->deltaTime < 0 ? getNaN() : meta->deltaTime; 
		io->T.dT=meta->deltaTime;
		io->T.start=meta->startTime;
		io->N=TsAggegrationPrepare(f32time,Nrawtime,&io->T,meta->isDate,&meta->period);
		if (IsNaN(meta->deltaTime)) {
			if (io->T.isRegular) {
			}	else {
				if (meta->isDate && !io->T.asDailyTS) {
					q_warning("WARNING: The input time series is irregular (or may span across leap years) and BEAST needs to aggregate/resample it into regular data "
						"at a user-specified interval 'metadata$deltaTime/deltat' (for faster computation). But deltaTime is missing,a best guess of it %g year=%g months=%g days is used. "
						"If not making sense,please specify metadata$deltaTime/deltat explicitly. \n",io->T.dT,io->T.dT*12,io->T.dT*365.0);
				}	else {
					q_warning("WARNING: The input data is irregular and for faster computaiton,BEAST needs to aggregate/resample it into regular data "
						"at a user-specified interval 'metadata$deltaTime/deltat' But deltaTime is missing,a best guess of it %g is used. "
						"If not making sense,please specify metadata$deltaTime/deltat explicitly. \n",io->T.dT);
				}
			}
		}
		meta->deltaTime=io->T.dT;
		meta->startTime=io->T.start;
		meta->isOrdered=io->T.isOrderd;
		meta->isRegular=io->T.isRegular;
		meta->needAggregate=io->T.needAggregate;
		free(f32time);
	}
	else {
		meta->isOrdered=1;
		meta->isRegular=1;
		meta->needAggregate=0;
		int isDefaultDeltaTime=0;
		int isDefaultStartTime=0;
		if (IsNaN(meta->startTime)) {
			meta->startTime=1.f;
			isDefaultStartTime=1L;
		}
		if (IsNaN(meta->deltaTime)) {
			meta->deltaTime=1.f;
			isDefaultDeltaTime=1L;
		}
		char *warningMsg="WARNING: If the input data is regular and ordered in time,the times of individual datapoints are determined fully by 'metadata$startTime' and 'metadata$deltaTime'.";
		if (isDefaultDeltaTime && isDefaultStartTime) {
			q_warning("%s But startTime and deltaTime are missing and a default value 1 is used for both!\n",warningMsg);
		} else if (isDefaultStartTime) {
			q_warning("%s But startTime is missing and a default value 1 is used!\n",warningMsg);
		} else if (isDefaultDeltaTime) {
			q_warning("%s But deltaTime is missing and a default value 1 is used!\n",warningMsg);
		}
		if (io->timedim==-1) {
			int whichDimIsTime=meta->whichDimIsTime;
			if (whichDimIsTime==-1||(io->ndim==2 && whichDimIsTime !=1 && whichDimIsTime !=2)) {
				r_error("ERROR: For a 2D matrix input of size [%d x %d] (e.g.,multiple time series),metadata$whichDimIsTime must be given "
					"to tell which matrix dim refers to time. It must take a value out of 1 or 2 only.\n",io->dims[0],io->dims[1]);
				return 0;
			}
			if (whichDimIsTime==-1||(io->ndim==3 && whichDimIsTime !=1 && whichDimIsTime !=2 && whichDimIsTime !=3)) {
				r_error("ERROR: For a 3D array input of size [%d x %d x %d] (i.e.,stacked time series images),metadata$whichDimIsTime must be given "
					"to tell which aray dim refers to time. It must take a value out of 1,2 or 3 only.\n",io->dims[0],io->dims[1],io->dims[2]);
				return 0;
			}
			io->timedim=meta->whichDimIsTime;
		}
		meta->whichDimIsTime=io->timedim; 
		io->N=io->dims[io->timedim - 1L];
		io->T.isRegular=1;
		io->T.isOrderd=1;
		io->T.needAggregate=0;
		io->T.needReordered=0;
		io->T.asDailyTS=0;
		io->T.data_dT=io->T.dT=meta->deltaTime;
		io->T.data_start=io->T.start=meta->startTime;
        #define isEqaulOneDay(dT) ( fabs(dT-1./365.)<1e-3||fabs(dT - 1./366.) < 1e-3)
		F32 dT=io->T.dT;
		if (meta->isDate && isEqaulOneDay(dT)  ) {
			io->T.asDailyTS=1;
			io->T.data_dT=io->T.dT=meta->deltaTime=1L;
			io->T.data_start=io->T.start=meta->startTime=F32time2DateNum(meta->startTime);
			meta->period=meta->period * 365;
			if (meta->isDate && isEqaulOneDay(dT) && meta->hasSeasonCmpnt==1 && meta->period>=300.) {
				int Nrawtime=io-> N;
				F32PTR f32time=malloc(sizeof(F32) * Nrawtime);
				for (int i=0; i < Nrawtime; i++) {
					f32time[i]=fractional_civil_from_days((int)meta->startTime+i);
				}
				io->T.dT=1./365;
				io->T.start=f32time[0];
				meta->period=meta->period/365.0;
				io->N=TsAggegrationPrepare(f32time,Nrawtime,&io->T,meta->isDate,&meta->period);
				meta->deltaTime=io->T.dT;
				meta->startTime=io->T.start;
				meta->isOrdered=io->T.isOrderd;
				meta->isRegular=io->T.isRegular;
				meta->needAggregate=io->T.needAggregate;
				if (meta->needAggregate==1 && meta->isRegular==0) {
					r_printf("INFO: The regular daily time series will be treated as irregular time series because time is converted into "
						"fractional/decimal years and some years have 365 days but others have 366 days.\n");
				}
				free(f32time);
			}
		}
	}
	if (meta->whichDimIsTime==1) { io->rowdim=2,io->coldim=3,io->timedim=1; }
	if (meta->whichDimIsTime==2) { io->rowdim=1,io->coldim=3,io->timedim=2; }
	if (meta->whichDimIsTime==3) { io->rowdim=1,io->coldim=2,io->timedim=3; }
	io->imgdims[0]=io->dims[io->rowdim - 1];
	io->imgdims[1]=io->dims[io->coldim - 1];
	io->numOfPixels=(I64)io->dims[0] * io->dims[1] * io->dims[2]/io->dims[io->timedim - 1L];
	if (meta->hasSeasonCmpnt==1) {	
		F32 freq=meta->period=meta->period/meta->deltaTime;		   
		if (freq <=0) {
			r_error("ERROR: BEAST can't handle a time series with a periodic componnet (\"metadata$season='%s'\") but with metadata$period=%g or period='none' specified. If you mean to "
				    "handle trend-only time series,use the trend-only BEAST version by specifying metadata$season='none'.",meta->seasonForm=='S' ? "harmonic" : "dummy",meta->period* meta->deltaTime);
			return 0;
		}
		if (freq < 2.000) {
			r_error("ERROR: In metadata,season='%s' and period=%g suggests the presence of a periodic component but the ratio of period/deltaTime=%g is less than 2 (i.e.,%g data sample per period). If you mean to "
				    "handle trend-only time series,use the trend-only BEAST version by specifying metadata$season='none'.",meta->seasonForm=='S' ? "harmonic" : "dummy",meta->period * meta->deltaTime,freq,freq);
			return 0;
		}
		if (meta->seasonForm=='D' && !IsNaN(freq) && !_IsAlmostInteger(freq)  ) {
			r_error("ERROR: For a dummy seasonal component (\"metadata$season='dummy'\"),metadata$period=%g must be a multiple of metadata$deltaTime by an INTEGER number. "
				    "Your period/deltaTime ratio is %g.",freq*meta->deltaTime,freq);
			return 0;
		}
	}
	meta->nrhs=nrhs;
	return 1;
}
 static int ParsePeriod(BEAST2_IO_PTR _OUT_ io ) {
		if (!io->meta.hasSeasonCmpnt||   
			io->meta.period > 0        )  
		{
			return 1;
		}
		BEAST2_YINFO Yinfo;
		F32PTR       MEMBUF;
		int    N=io->N;		
		int    q=io->q;
		int    Nraw=io->dims[io->timedim - 1];
		F32PTR tmp=malloc(sizeof(F32)*(N*q+N+q+q+q*q+Nraw )); 
		Yinfo.Y=tmp;
		Yinfo.rowsMissing=tmp+N*q;
		Yinfo.mean=tmp+N * q+N;
		Yinfo.sd=tmp+N * q+N+q;
		Yinfo.YtY_plus_alpha2Q=tmp+N * q+N+q+q;
		MEMBUF=tmp+N * q+N+q+q+q*q; 
		F32 nan=getNaN();
		F32 period=nan;
		I32 goodPixelVisited=0;
		I32 MaxNumPixelVisisted=200;
		for (int i=1; i <=io->numOfPixels;++i) {
			BEAST2_fetch_timeSeries(&Yinfo,i,MEMBUF,io);
			Yinfo.nMissing=f32_normalize_multicols_zeroout_nans(Yinfo.Y,Yinfo.rowsMissing,N,N,q,Yinfo.mean,Yinfo.sd);
			U08 skipCurrentPixel=Yinfo.nMissing > (N * io->meta.maxMissingRate);
			if (skipCurrentPixel) {
				continue;
			}		
			for (int j=0; j < q;++j) {
				F32PTR y=Yinfo.Y+j * N;
				for (int k=0; k < Yinfo.nMissing;++k) {
					y[Yinfo.rowsMissing[k]]=nan;
				}
				F32 jthPeriod=DeterminePeriod(y,N);  
				if (j==0) { 
					period=jthPeriod;
					if (period < 0)	break;					
				} else {
					if (jthPeriod !=period ) {
						period=nan;
						break;
					}
				}
			}
			if (period > 0   )                           	break;
			if (++goodPixelVisited > MaxNumPixelVisisted) 	break;
		}
		free(tmp);
		if (period > 0) {
			q_warning("WARNING: metadata$season='%s' suggests that the time series has a periodic/seasonal component. \"metadata$period\" "
				"is needed but missing. A BEST GUESS of numbers of datapoints per period is %d,giving period=num_sample_per_period*deltaTime "
				"=%d*%g=%g. Please make sure this estimate makes sense; otherwise,the BEAST decomposition result will be incorrect.\n",
				io->meta.seasonForm=='S' ? "harmonic" : "dummy",(int)period,(int)period,io->meta.deltaTime,period * io->meta.deltaTime);
		} else {
			r_error("ERROR: metadata$season='%s' suggests that the time series has a periodic/seasonal component.  \"metadata$period\" "
				"is needed but missing. BEAST tried to estimate it via an auotcorrelation method but failed to "
				"get a reliable estimate. Please specify the period value EXPLICILTY. Or if your input has no periodic/seasonal component at all,"
				" set metadata$season='none' or period=0,which will fit a trend-only model.\n",io->meta.seasonForm=='S' ? "harmonic" : "dummy");
			return 0;
		}
		io->meta.period=period;
		return 1;
}
static int __GetPrecPriorType( VOID_PTR S ) {
	VOID_PTR  tmp=GetField123Check(S,"precPriorType",5);
	if (tmp==NULL)
		return UniformPrec;
	if (IsNumeric(tmp)) {
		I32 value=GetScalar(tmp);
		if (value==0) return ConstPrec;
		if (value==1) return UniformPrec;
		if (value==2) return ComponentWise;
		if (value==3) return OrderWise;
		q_warning("WARNING: The arg prior$precPriorType=(%d) is not a valid value; the default prior$precPriorType='%s' is assumed instread!",value,"uniform");
		return UniformPrec;
	} 
	else if (IsChar(tmp)) {
		char str[60+1];
		GetCharArray(tmp,str,60);
		ToUpper(str);
		char a=str[0];
		char c=str[2];
		if (a=='U')				return UniformPrec;
		if (a=='O')				return OrderWise;
		if (a=='C' && c=='M')	return ComponentWise;
		if (a=='C' && c=='N')	return ConstPrec;	
		q_warning("WARNING: The arg prior$precPriorType=(%s) is not recongizable; the default prior$precPriorType='%s' is assumed instread!",str,"uniform");
		return UniformPrec;
	}
	q_warning("WARNING: The arg prior$precPriorType has an supported format or value; the default prior$precPriorType='%s' is assumed instread!","uniform");
	return UniformPrec;
}
static int  GetArg_2nd_Prior__(VOIDPTR prhs[],int nrhs,BEAST2_PRIOR_PTR prior,BEAST2_IO_PTR io)
{	 
	struct PRIOR_MISSING {	
		U08   seasonMinOrder,seasonMaxOrder,trendMinOrder,trendMaxOrder;
		U08   trendMinSepDist,seasonMinSepDist;
		U08   trendMinKnotNum,seasonMinKnotNum;
		U08   trendMaxKnotNum,seasonMaxKnotNum,outlierMaxKnotNum;
		U08   seasonBasisFuncType,trendBasisFuncType,outlierBasisFuncType;
		U08   modelPriorType;
		U08   precPriorType;
		U08   K_MAX;
		U08   sigFactor;
		U08   outlierSigFactor;
		U08   sig2;
		U08   precValue;
		U08   alpha1,alpha2,delta1,delta2;
	} m={0,};
	#define o  (*prior)
	if (nrhs < 4) 	
		memset(&m,1L,sizeof(struct PRIOR_MISSING));
	if (nrhs >=4) {		 
		VOIDPTR S=prhs[3L];
		if (!IsStruct(S)) {
			q_warning("WARNING: The arg 'prior' is ignored because it is not a List/Struct variable.");
			memset(&m,1L,sizeof(struct PRIOR_MISSING));
		}
		else {
			VOIDPTR tmp;
			if (io->meta.hasSeasonCmpnt) {
				o.seasonMinOrder=(tmp=GetField123Check(S,"seasonMinOrder",10)) ? GetScalar(tmp) : (m.seasonMinOrder=1);
				o.seasonMaxOrder=(tmp=GetField123Check(S,"seasonMaxOrder",10)) ? GetScalar(tmp) : (m.seasonMaxOrder=1);
				o.seasonMinSepDist=(tmp=GetField123Check(S,"seasonMinSepDist",10)) ? GetScalar(tmp) : (m.seasonMinSepDist=1);
				o.seasonMinKnotNum=(tmp=GetField123Check(S,"seasonMinKnotNum",10)) ? GetScalar(tmp) : (m.seasonMinKnotNum=1);
				o.seasonMaxKnotNum=(tmp=GetField123Check(S,"seasonMaxKnotNum",10)) ? GetScalar(tmp) : (m.seasonMaxKnotNum=1);
			}
			o.trendMinOrder=(tmp=GetField123Check(S,"trendMinOrder",10)) ?   GetScalar(tmp) : (m.trendMinOrder=1);
			o.trendMaxOrder=(tmp=GetField123Check(S,"trendMaxOrder",10)) ?   GetScalar(tmp) : (m.trendMaxOrder=1);
			o.trendMinSepDist=(tmp=GetField123Check(S,"trendMinSepDist",10)) ?  GetScalar(tmp) : (m.trendMinSepDist=1);
			o.trendMinKnotNum=(tmp=GetField123Check(S,"trendMinKnotNum",10)) ?  GetScalar(tmp) : (m.trendMinKnotNum=1);			
			o.trendMaxKnotNum=(tmp=GetField123Check(S,"trendMaxKnotNum",10)) ?  GetScalar(tmp) : (m.trendMaxKnotNum=1);
			if (io->meta.hasOutlierCmpnt) {
				o.outlierMaxKnotNum=(tmp=GetField123Check(S,"outlierMaxKnotNum",10)) ? GetScalar(tmp) : (m.outlierMaxKnotNum=1);
				o.outlierSigFactor=(tmp=GetFieldCheck(S,"outlierSigFactor")) ? GetScalar(tmp) : (m.outlierSigFactor=1);
			}
			o.sigFactor=(tmp=GetFieldCheck(S,"sigFactor")) ?			GetScalar(tmp) : (m.sigFactor=1);
			o.sig2=(tmp=GetField123Check(S,"sig2",2)) ?				GetScalar(tmp) : (m.sig2=1);
			o.precValue=(tmp=GetField123Check(S,"precValue",5)) ?		GetScalar(tmp) : (m.precValue=1);
			o.alpha1=(tmp=GetField123Check(S,"alpha1",0)) ?			GetScalar(tmp) : (m.alpha1=1);
			o.alpha2=(tmp=GetField123Check(S,"alpha2",0)) ?			GetScalar(tmp) : (m.alpha2=1);
			o.delta1=(tmp=GetField123Check(S,"delta1",0)) ?			GetScalar(tmp) : (m.delta1=1);
			o.delta2=(tmp=GetField123Check(S,"delta2",0)) ?			GetScalar(tmp) : (m.delta2=1);
			o.K_MAX=(tmp=GetField123Check(S,"K_MAX",1)) ? GetScalar(tmp) : (m.K_MAX=1);
			if (io->meta.hasSeasonCmpnt)  o.seasonBasisFuncType=(tmp=GetField123Check(S,"seasonBasisFuncType",10)) ?  GetScalar(tmp) : (m.seasonBasisFuncType=1);
			if (1L)                       o.trendBasisFuncType=(tmp=GetField123Check(S,"trendBasisFuncType",10)) ?   GetScalar(tmp) : (m.trendBasisFuncType=1);
			if (io->meta.hasOutlierCmpnt) o.outlierBasisFuncType=(tmp=GetField123Check(S,"outlierBasisFuncType",10)) ? GetScalar(tmp) : (m.outlierBasisFuncType=1);
			o.modelPriorType=(tmp=GetField123Check(S,"modelPriorType",10)) ?		GetScalar(tmp) : (m.modelPriorType=1);
			o.precPriorType=__GetPrecPriorType(S);
		}
	} 
	o.numBasis=1L+io->meta.hasSeasonCmpnt+io->meta.hasOutlierCmpnt;
	I32  basisIdx=0;	
	if (io->meta.hasSeasonCmpnt) {
		I08      seasonFrom=io->meta.seasonForm;
		if      (seasonFrom=='S')	o.basisType[basisIdx++]=SEASONID;
		else if (seasonFrom=='D') o.basisType[basisIdx++]=DUMMYID;
		else if (seasonFrom=='V') o.basisType[basisIdx++]=SVDID;
		else {
			r_error("ERROR: the season character is unrecognized. Valid values are 'none','harmonic','dummy',and 'svd'. \n");
			return 0;
		}
	}
	o.basisType[basisIdx++]=TRENDID;
	if (io->meta.hasOutlierCmpnt) o.basisType[basisIdx++]=OUTLIERID;
	I32 period=io->meta.period;
	I32 N=io->N;
	if (io->meta.hasSeasonCmpnt) {
		if (m.seasonMinOrder)                                o.seasonMinOrder=1L;				   o.seasonMinOrder=min(o.seasonMinOrder,period/2 - 1);    o.seasonMinOrder=max(o.seasonMinOrder,1L);
		if (m.seasonMaxOrder)                                o.seasonMaxOrder=(period/2 - 1);      o.seasonMaxOrder=min(o.seasonMaxOrder,(period/2 - 1));  o.seasonMaxOrder=max(o.seasonMaxOrder,o.seasonMinOrder);
		if (m.seasonMinSepDist||o.seasonMinSepDist <=0)   o.seasonMinSepDist=period/2;          o.seasonMinSepDist=max(o.seasonMinSepDist,o.seasonMaxOrder);		 o.seasonMinSepDist=min(o.seasonMinSepDist,N/2 - 1); 
		if (m.seasonMinKnotNum)                              o.seasonMinKnotNum=0;                   o.seasonMinKnotNum=max(min(o.seasonMaxKnotNum,o.seasonMinKnotNum),0);
		if (m.seasonMaxKnotNum)                              o.seasonMaxKnotNum=min(floor(N/(o.seasonMinSepDist+1) - 1.f),5);  o.seasonMaxKnotNum=min(o.seasonMaxKnotNum,floor(N/(o.seasonMinSepDist+1) - 1.f));   o.seasonMaxKnotNum=max(o.seasonMaxKnotNum,o.seasonMinKnotNum);
	}
	if (m.trendMinOrder)                             o.trendMinOrder=0L;				                      o.trendMinOrder=max(o.trendMinOrder,0L);
	if (m.trendMaxOrder)                             o.trendMaxOrder=1L;				                      o.trendMaxOrder=max(o.trendMaxOrder,o.trendMinOrder);	
	if (m.trendMinSepDist||o.trendMinSepDist <=0) o.trendMinSepDist=io->meta.hasSeasonCmpnt? period/2: 3 ; o.trendMinSepDist=max(o.trendMinSepDist,o.trendMaxOrder+1);	o.trendMinSepDist=min(o.trendMinSepDist,N/2 - 1);
	if (m.trendMinKnotNum)                           o.trendMinKnotNum=0;	                                      o.trendMinKnotNum=max(min(o.trendMaxKnotNum,o.trendMinKnotNum),0);
	if (m.trendMaxKnotNum)                           o.trendMaxKnotNum=min(floor(N/(o.trendMinSepDist+1) - 1.f),10); o.trendMaxKnotNum=min(o.trendMaxKnotNum,floor(N/(o.trendMinSepDist+1) - 1.f)); o.trendMaxKnotNum=max(o.trendMaxKnotNum,o.trendMinKnotNum);
	if (m.outlierMaxKnotNum) o.outlierMaxKnotNum=o.trendMaxKnotNum;      o.outlierMaxKnotNum=max(o.outlierMaxKnotNum,1L); 
	if (m.K_MAX )            o.K_MAX=500;                  
	if (m.sigFactor)         o.sigFactor=1.8;            o.sigFactor=max(o.sigFactor,1.02);
	if (m.outlierSigFactor)  o.outlierSigFactor=2.5;            o.outlierSigFactor=max(o.outlierSigFactor,1.5);
	if (m.sig2 )             o.sig2=0.2f;				  o.sig2=max(o.sig2,0.01);
	if (m.precValue)         o.precValue=1.5f;				  o.precValue=max(o.precValue,0.01);
	if (m.alpha1)		     o.alpha1=0.00000001f;
	if (m.alpha2)		     o.alpha2=0.00000001f;
	if (m.delta1)		     o.delta1=0.00000001f;
	if (m.delta2)		     o.delta2=0.00000001f;
	if (m.precPriorType)			o.precPriorType=UniformPrec;
	if (m.seasonBasisFuncType) {
		if      (o.precPriorType==UniformPrec)		o.seasonBasisFuncType=0;
		else if (o.precPriorType==ConstPrec)          o.seasonBasisFuncType=0;
		else if (o.precPriorType==ComponentWise)      o.seasonBasisFuncType=1;
		else if (o.precPriorType==OrderWise)          o.seasonBasisFuncType=1;		
	}
	if (m.trendBasisFuncType) {
		if      (o.precPriorType==UniformPrec)		o.trendBasisFuncType=0;
		else if (o.precPriorType==ConstPrec)          o.trendBasisFuncType=0;
		else if (o.precPriorType==ComponentWise)      o.seasonBasisFuncType=1;
		else if (o.precPriorType==OrderWise)          o.trendBasisFuncType=1;
	}	 
	if (m.outlierBasisFuncType) {
		if      (o.precPriorType==UniformPrec)		o.outlierBasisFuncType=0;
		else if (o.precPriorType==ConstPrec)          o.outlierBasisFuncType=0;
		else if (o.precPriorType==ComponentWise)      o.outlierBasisFuncType=1;
		else if (o.precPriorType==OrderWise)          o.outlierBasisFuncType=1;
	}	 
	if (m.modelPriorType)			o.modelPriorType=1L;
	return 1;
#undef o
}
static int  GetArg_3rd_MCMC___(VOIDPTR prhs[],int nrhs,BEAST2_MCMC_PTR mcmc,BEAST2_OPTIONS_PTR opt)
{
	#define o (*mcmc)
	struct MCMC_MISSING {
		U08   seed;	                  
		U08   credIntervalAlphaLevel;
		U08   trendResamplingOrderProb;
		U08   seasonResamplingOrderProb;
		U08   ridgeFactor;
		U08   burnin,samples,chainNumber;
		U08   maxMoveStepSize;
		U08   thinningFactor;
	} m={0,};
	if (nrhs < 5) 
		memset(&m,1L,sizeof(struct MCMC_MISSING));
	if (nrhs >=5) {
		VOIDPTR S=prhs[4L];
		if (!IsStruct(S)) {
			q_warning("WARNING: The arg 'mcmc' is ignored because it is not a LIST variable.");
			memset(&m,1L,sizeof(struct MCMC_MISSING));
		} else {
			VOIDPTR tmp;
			o.maxMoveStepSize=(tmp=GetField123Check(S,"maxMoveStepSize",2))? GetScalar(tmp) : (m.maxMoveStepSize=1);
			o.samples=(tmp=GetField123Check(S,"samples",2)) ?        GetScalar(tmp) : (m.samples=1);
			o.thinningFactor=(tmp=GetField123Check(S,"thinningFactor",2)) ? GetScalar(tmp) : (m.thinningFactor=1);
			o.burnin=(tmp=GetField123Check(S,"burnin",2)) ?         GetScalar(tmp) : (m.burnin=1);
			o.chainNumber=(tmp=GetField123Check(S,"chainNumber",2)) ?    GetScalar(tmp) : (m.chainNumber=1);
			o.seed=(tmp=GetField123Check(S,"seed",2)) ?			GetScalar(tmp) : (m.seed=1);
			o.ridgeFactor=(tmp=GetField123Check(S,"ridgeFactor",2)) ?	GetScalar(tmp) : (m.ridgeFactor=1);
			o.trendResamplingOrderProb=(tmp=GetField123Check(S,"trendResamplingOrderProb",2)) ? GetScalar(tmp) : (m.trendResamplingOrderProb=1);
			o.seasonResamplingOrderProb=(tmp=GetField123Check(S,"seasonResamplingOrderProb",2)) ? GetScalar(tmp) : (m.seasonResamplingOrderProb=1);
			o.credIntervalAlphaLevel=(tmp=GetField123Check(S,"credIntervalAlphaLevel",2)) ? GetScalar(tmp) : (m.credIntervalAlphaLevel=1);
		}
	} 
	if (m.maxMoveStepSize||o.maxMoveStepSize==0) o.maxMoveStepSize=opt->io.meta.hasSeasonCmpnt? opt->io.meta.period: (opt->prior.trendMinSepDist+1);
	if (m.samples||o.samples==0)		   o.samples=3000;        o.samples=max(o.samples,800);
	if (m.thinningFactor||o.thinningFactor==0)  o.thinningFactor=1L;			o.thinningFactor=max(o.thinningFactor,1L);
	if (m.burnin||o.burnin==0)          o.burnin=150L;		o.burnin=max(o.burnin,150L);
	if (m.chainNumber||o.chainNumber==0)         o.chainNumber=3;			o.chainNumber=max(o.chainNumber,1L);
	if (m.seed)            o.seed=0L;
	if (m.credIntervalAlphaLevel)      o.credIntervalAlphaLevel=.95;
	if (m.ridgeFactor)     o.ridgeFactor=0.0001f;
	if (m.trendResamplingOrderProb)  o.trendResamplingOrderProb=.1f;
	if (m.seasonResamplingOrderProb) o.seasonResamplingOrderProb=.17f;
	return 1;
#undef o
}
static int  GetArg_4th_EXTRA__(VOIDPTR prhs[],int nrhs,BEAST2_EXTRA_PTR extra,I32 whichDimIsTime,I32 ndims)
{
	#define o (*extra)
	struct OUTFLAGS_MISSING {
		I08   numThreadsPerCPU;
		I08   numParThreads;
		I08   numCPUCoresToUse;		
		I08   consoleWidth;
		I08   whichOutputDimIsTime;
		I08   removeSingletonDims;
		I08   dumpInputData;
		I08   ncpStatMethod;
		I08  smoothCpOccPrCurve;
		I08  useMeanOrRndBeta;
		I08  computeCredible;
		I08  fastCIComputation;
		I08  computeSeasonOrder;
		I08  computeTrendOrder;
		I08  computeSeasonChngpt;
		I08  computeTrendChngpt;
		I08  computeOutlierChngpt;
		I08  computeSeasonAmp;
		I08  computeTrendSlope;
		I08 tallyPosNegSeasonJump;
		I08 tallyPosNegTrendJump;
		I08 tallyIncDecTrendJump;
		I08 tallyPosNegOutliers;
		I08  printOptions;
		I08  printProgressBar;
	} m={0,};
	if (nrhs < 6) 
		memset(&m,1L,sizeof(struct OUTFLAGS_MISSING));	
	if (nrhs >=6) {
		VOIDPTR S=prhs[5L];
		if (!IsStruct(S)) {
			q_warning("WARNING: The arg 'extra' is ignored because it is not a LIST variable.");
			memset(&m,1L,sizeof(struct OUTFLAGS_MISSING));
		}
		else {
			VOIDPTR tmp;
			o.whichOutputDimIsTime=(tmp=GetField123Check(S,"whichOutputDimIsTime",2)) ?	GetScalar(tmp) : (m.whichOutputDimIsTime=1);
			o.removeSingletonDims=(tmp=GetField123Check(S,"removeSingletonDims",8)) ? GetScalar(tmp) : (m.removeSingletonDims=1);			
			o.numThreadsPerCPU=(tmp=GetField123Check(S,"numThreadsPerCPU",4)) ? GetScalar(tmp) : (m.numThreadsPerCPU=1);
			o.numParThreads=(tmp=GetField123Check(S,"numParThreads",4)) ?			GetScalar(tmp) : (m.numParThreads=1);
			o.numCPUCoresToUse=(tmp=GetField123Check(S,"numCPUCoresToUse",4)) ?		GetScalar(tmp) : (m.numCPUCoresToUse=1);
			o.consoleWidth=(tmp=GetField123Check(S,"consoleWidth",2)) ?			GetScalar(tmp) : (m.consoleWidth=1);
			o.dumpInputData=(tmp=GetField123Check(S,"dumpInputData",2)) ? GetScalar(tmp) : (m.dumpInputData=1);
			o.smoothCpOccPrCurve=(tmp=GetField123Check(S,"smoothCpOccPrCurve",2)) ? GetScalar(tmp) : (m.smoothCpOccPrCurve=1);
			#define _1(x)       o.x=(tmp=GetFieldCheck(S,#x))? GetScalar(tmp): (m.x=1)
			#define _2(x,y)     _1(x);_1(y)
			#define _3(x,y,z)   _1(x);_2(y,z)
			#define _4(x,y,z,w) _2(y,z);_2(y,z)
			#define _5(x,y,z,w) _2(y,z);_3(y,z)
			_2(printProgressBar,printOptions);
			_2(computeCredible,fastCIComputation);
			_2(computeSeasonOrder,computeTrendOrder);
			_3(computeSeasonChngpt,computeTrendChngpt,computeOutlierChngpt);
			_2(computeSeasonAmp,computeTrendSlope);
			_4(tallyPosNegSeasonJump,tallyPosNegTrendJump,tallyIncDecTrendJump,tallyPosNegOutliers);
			_1(useMeanOrRndBeta);
		} 
	} 
	if (m.whichOutputDimIsTime)		o.whichOutputDimIsTime=whichDimIsTime;
	if (m.removeSingletonDims)		o.removeSingletonDims=1;
	if (o.whichOutputDimIsTime > ndims)		o.whichOutputDimIsTime=1L; 
	if (m.smoothCpOccPrCurve)    o.smoothCpOccPrCurve=0;
	if (m.dumpInputData)		 o.dumpInputData=0;
	if (m.numThreadsPerCPU)      o.numThreadsPerCPU=2;
	if (m.numParThreads)         o.numParThreads=0;
	if (m.numCPUCoresToUse)      o.numCPUCoresToUse=0;	
	if (m.consoleWidth||o.consoleWidth<=0)  o.consoleWidth=GetConsoleWidth(); 	o.consoleWidth=max(o.consoleWidth,40);
	if (m.printProgressBar)      o.printProgressBar=1;
	if (m.printOptions)          o.printOptions=1;
	if (m.computeCredible)       o.computeCredible=0L;
	if (m.fastCIComputation)     o.fastCIComputation=1L;
	if (m.computeSeasonOrder)    o.computeSeasonOrder=0L;
	if (m.computeTrendOrder)     o.computeTrendOrder=0L;
	if (m.computeSeasonChngpt)   o.computeSeasonChngpt=1L;
	if (m.computeTrendChngpt)    o.computeTrendChngpt=1L;
	if (m.computeOutlierChngpt)  o.computeOutlierChngpt=1L;
	if (m.computeSeasonAmp)       o.computeSeasonAmp=0L;
	if (m.computeTrendSlope)      o.computeTrendSlope=0L;
	if (m.tallyPosNegSeasonJump)     o.tallyPosNegSeasonJump=0L;
	if (m.tallyPosNegTrendJump)      o.tallyPosNegTrendJump=0L;
	if (m.tallyIncDecTrendJump)      o.tallyIncDecTrendJump=0L;
	if (m.tallyPosNegOutliers)       o.tallyPosNegOutliers=0L;
	if (o.tallyPosNegSeasonJump) o.computeSeasonChngpt=1,o.computeSeasonAmp=1;
	if (o.tallyPosNegTrendJump)  o.computeTrendChngpt=1,o.computeTrendSlope=1;
	if (o.tallyIncDecTrendJump)  o.computeTrendChngpt=1,o.computeTrendSlope=1;
	if (o.tallyPosNegOutliers)   o.computeOutlierChngpt=1;
	if (m.useMeanOrRndBeta)      o.useMeanOrRndBeta=0;
	return 1;
#undef o
}
I32 PostCheckArgs(A(OPTIONS_PTR) opt) {
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasHarmonicCmpnt=opt->prior.basisType[0]==SEASONID ;
	I08 hasDummyCmpnt=opt->prior.basisType[0]==DUMMYID;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I08 hasTrendCmpnt=1;
	I08 hasAlways=1;
	if (hasDummyCmpnt) opt->io.meta.period=ceil(opt->io.meta.period);
	if (opt->prior.trendMaxOrder==opt->prior.trendMinOrder)	opt->mcmc.trendResamplingOrderProb=0;
	if (opt->prior.seasonMaxOrder==opt->prior.seasonMinOrder)	opt->mcmc.seasonResamplingOrderProb=0;
	if (hasDummyCmpnt)											opt->mcmc.seasonResamplingOrderProb=0;
	if (opt->io.meta.deseasonalize && !hasSeasonCmpnt) {
		opt->io.meta.deseasonalize=0;		
	}
	if(opt->io.meta.period+3  > opt->io.N) {
		opt->io.meta.deseasonalize=0;
	}
	if (!hasSeasonCmpnt) {
		opt->extra.computeSeasonOrder=0;
		opt->extra.computeSeasonChngpt=0;
		opt->extra.computeSeasonAmp=0;
		opt->extra.tallyPosNegSeasonJump=0;
	}
	if (hasDummyCmpnt) {
		opt->extra.computeSeasonOrder=0;	
		opt->extra.computeSeasonAmp=0; 
		opt->mcmc.seasonResamplingOrderProb=0;
		opt->prior.seasonMinOrder=0;
		opt->prior.seasonMaxOrder=0;		
	}
	if (!hasOutlierCmpnt) {
		opt->extra.computeOutlierChngpt=0;
		opt->extra.tallyPosNegOutliers=0;
	}
	BEAST2_PRIOR_PTR PRIOR=&opt->prior;
	#define prior (*PRIOR)
	I08  isTrendCmpntFixed=prior.trendMaxOrder==prior.trendMinOrder  && prior.trendMaxKnotNum==0 && prior.trendMinKnotNum==0;
	I08  isSeasonCmpntFixed=prior.seasonMaxOrder==prior.seasonMinOrder && prior.seasonMaxKnotNum==0 && prior.seasonMinKnotNum==0;
	if (hasDummyCmpnt) isSeasonCmpntFixed=prior.seasonMaxKnotNum==0 && prior.seasonMinKnotNum==0;
	#undef prior
	if (opt->prior.numBasis==2 ) {
		if (hasTrendCmpnt  && hasOutlierCmpnt && isTrendCmpntFixed) {
			q_warning("WARNING: The options 'trendMaxOrder==trendMaxOrder && trendMaxKnotNum==0 && trendMinKnotNum==0' will"
				     " fix the trend to a global curve.\n");					 
		}
		if (hasHarmonicCmpnt && hasOutlierCmpnt && isSeasonCmpntFixed) {
			q_warning("WARNING: The options 'seasonMaxOrder==seasonMaxOrder && seasonMaxKnotNum==0 && seasonMinKnotNum==0' will"
				    " fix the season component to a global curve.\n");		 
		}
		if (hasDummyCmpnt && hasOutlierCmpnt && isSeasonCmpntFixed) {
			q_warning("WARNING: The options 'seasonMaxKnotNum==0 && seasonMinKnotNum==0' will"
				    " fix the dummy season component to a global curve.\n");			
		}
	}
	if (hasTrendCmpnt && hasDummyCmpnt & isTrendCmpntFixed && isSeasonCmpntFixed) {		
		q_warning("WARNING: The options 'trendMaxOrder==trendMaxOrder && trendMaxKnotNum==0 && trendMinKnotNum==0 && "
			    " seasonMaxKnotNum==0 && seasonMinKnotNum==0' will"
			    " fix the model structures of the trend and dummy season components.\n");
	}
	if (hasTrendCmpnt && hasHarmonicCmpnt & isTrendCmpntFixed && isSeasonCmpntFixed) {
		q_warning("WARNING: The options 'trendMaxOrder==trendMaxOrder && trendMaxKnotNum==0 && trendMinKnotNum==0 && "
			    "seasonMaxOrder==seasonMaxOrder && seasonMaxKnotNum==0 && seasonMinKnotNum==0' will"
			    " fix the model structures of the trend and harmonic season components.\n"); 
	}
	I32 KMAX=0;
	for (I32 i=0; i < PRIOR->numBasis; i++) {		
		I08 type=PRIOR->basisType[i];
		if (type==SEASONID)			KMAX+=(PRIOR->seasonMaxOrder*2) * (PRIOR->seasonMaxKnotNum+1);
		if (type==DUMMYID)			KMAX+=(opt->io.meta.period)    * (PRIOR->seasonMaxKnotNum+1);
		if (type==SVDID)			    KMAX+=(opt->io.meta.period) * (PRIOR->seasonMaxKnotNum+1);
		if (type==TRENDID)			KMAX+=(PRIOR->trendMaxOrder+1) * (PRIOR->trendMaxKnotNum+1);
		if (type==OUTLIERID)			KMAX+=PRIOR->outlierMaxKnotNum;
	}
	PRIOR->K_MAX=min(PRIOR->K_MAX,KMAX);
	I32 K_INITIAL_MODEL=0;
	for (I32 i=0; i < PRIOR->numBasis; i++) {
		I08 type=PRIOR->basisType[i];
		if (type==SEASONID) {
			I32 order=ceil((PRIOR->seasonMaxOrder+PRIOR->seasonMinOrder)/2.0);
			I32 nKnot=ceil((PRIOR->seasonMinKnotNum+PRIOR->seasonMaxKnotNum)/2.0);   
			K_INITIAL_MODEL+=(order * 2) * (nKnot+1);
		}
		if (type==DUMMYID) {
			I32 order=ceil(opt->io.meta.period);
			I32 nKnot=ceil((PRIOR->seasonMinKnotNum+PRIOR->seasonMaxKnotNum)/2.0);   
			K_INITIAL_MODEL+=(order ) * (nKnot+1);
		}
		if (type==SVDID) {
			I32 order=ceil(opt->io.meta.period);
			I32 nKnot=ceil((PRIOR->seasonMinKnotNum+PRIOR->seasonMaxKnotNum)/2.0);   
			K_INITIAL_MODEL+=(order) * (nKnot+1);
		}
		if (type==TRENDID)		{
			I32 order=ceil((PRIOR->trendMaxOrder+PRIOR->trendMinOrder)/2.0);
			I32 nKnot=ceil((PRIOR->trendMinKnotNum+PRIOR->trendMaxKnotNum)/2.0);   
			K_INITIAL_MODEL+=(order+1L) * (nKnot+1);
		}
		if (type==OUTLIERID) {
			K_INITIAL_MODEL+=0;
		}
	}
	PRIOR->K_MAX=max(PRIOR->K_MAX,K_INITIAL_MODEL);
	opt->io.out.dtype=IsRinterface() ? DATA_DOUBLE : DATA_FLOAT;
	opt->io.out.whichDimIsTime=opt->extra.whichOutputDimIsTime;
	if (opt->prior.precPriorType==ComponentWise && opt->prior.numBasis==1) {
		opt->prior.precPriorType=UniformPrec;
	}
	return 1;
}
int BEAST2_GetArgs(VOIDPTR prhs[],int nrhs,A(OPTIONS_PTR) opt) {
	int  failed=!GetArg_0th_Data(prhs,nrhs,&opt->io)||
		          !GetArg_1st_MetaData(prhs,nrhs,&opt->io)|| 				  
		          !ParsePeriod(&opt->io)||
			      !GetArg_2nd_Prior__(prhs,nrhs,&opt->prior,&opt->io)||
			      !GetArg_3rd_MCMC___(prhs,nrhs,&opt->mcmc,opt)||
			      !GetArg_4th_EXTRA__(prhs,nrhs,&opt->extra,opt->io.meta.whichDimIsTime,opt->io.ndim) ;
	int success=!failed;	
	if (success) 	success=PostCheckArgs(opt); 	
	if (success) 	BEAST2_print_options(opt);	
	if (opt->io.T.asDailyTS) {
		opt->io.meta.deltaTime=1./365;
		opt->io.meta.startTime=fractional_civil_from_days((int)(opt->io.meta.startTime));
	}
	return success;
}
void BEAST2_DeallocateTimeSeriesIO(BEAST2_IO_PTR  o) {
	if (o->T.numPtsPerInterval !=NULL) {
		free(o->T.numPtsPerInterval);
		o->T.numPtsPerInterval=NULL;
	}
	if (o->T.sortedTimeIdx !=NULL) {
		free(o->T.sortedTimeIdx);
		o->T.sortedTimeIdx=NULL;
	}
	if (o->pdata !=NULL) {
		free(o->pdata);
		o->pdata=NULL;
	}
	if (o->dtype !=NULL) {
		free(o->dtype);
		o->dtype=NULL;
	}
	if (o->out.result !=NULL) {
		free(o->out.result);
		o->out.result=NULL;
	}
}
#include "abc_000_warning.h"
