#include "abc_000_warning.h"
#include "abc_001_config.h"
#include "abc_ide_util.h"
#include "abc_vec.h"
#include "abc_mem.h"
#include "beastv2_io.h"
#define BEGIN {
#define END   }
static void __ChangeFieldsTimeDimFrm1to2_2D(FIELD_ITEM *flist,int n) {
	for (int i=0; i < n; i++) {
		int d1=flist[i].dims[0],d2=flist[i].dims[1],d3=flist[i].dims[2];
		if (flist[i].ndim==2) {
			flist[i].dims[1 - 1]=d2;
			flist[i].dims[2 - 1]=d1;
		} else if (flist[i].ndim==3) {
			if (d2 !=2)  r_printf("There must be something wrong!");			
			flist[i].dims[1 - 1]=d3;
			flist[i].dims[2 - 1]=d1;
			flist[i].dims[3 - 1]=d2=2;
		} else {
			r_printf("There must be something wrong!");
		}
	}
}
static void __ChangeFieldsTimeDimFrm1to_3D(FIELD_ITEM* flist,int n,int newDim) {
	for (int i=0; i < n; i++) {
		int d1=flist[i].dims[0],d2=flist[i].dims[1],d3=flist[i].dims[2],d4=flist[i].dims[3];
		if (flist[i].ndim==2) { } 
		else if (flist[i].ndim==3) {
			if (newDim==2)
				flist[i].dims[1 - 1]=d2,
				flist[i].dims[2 - 1]=d1,
				flist[i].dims[3 - 1]=d3;
			else 
				flist[i].dims[1 - 1]=d2,
				flist[i].dims[2 - 1]=d3,
				flist[i].dims[3 - 1]=d1;	}
		else if (flist[i].ndim==4) {
			if (newDim==2)
				flist[i].dims[1 - 1]=d3,
				flist[i].dims[2 - 1]=d1,
				flist[i].dims[3 - 1]=2,
				flist[i].dims[4 - 1]=d4;
			else 
				flist[i].dims[1 - 1]=d3,
				flist[i].dims[2 - 1]=d4,
				flist[i].dims[3 - 1]=d1,
				flist[i].dims[4 - 1]=2;		}
		else {
			r_printf("There must be something wrong!");
		}
	}
}
static void __RemoveFieldsGivenFlags_Trend(A(OPTIONS_PTR)  opt,FIELD_ITEM * fieldList,int nfields) {
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I08 hasTrendCmpnt=1;
	I08 hasAlways=1;
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	const A(EXTRA_PTR)   flag=&(opt->extra);
	if (!flag->computeCredible)		RemoveField(fieldList,nfields,"CI"),mat->tCI=NULL;	
	if (!flag->computeTrendOrder) 	RemoveField(fieldList,nfields,"order"),mat->torder=NULL;	
	if (!flag->computeTrendSlope)
		RemoveField(fieldList,nfields,"slp"),mat->tslp=NULL,
		RemoveField(fieldList,nfields,"slpSD"),mat->tslpSD=NULL,
		RemoveField(fieldList,nfields,"slpSignPr"),mat->tslpSignPr=NULL;	
	if (!flag->computeTrendChngpt)
		RemoveField(fieldList,nfields,"cp"),mat->tcp=NULL,
		RemoveField(fieldList,nfields,"cpPr"),mat->tcpPr=NULL,
		RemoveField(fieldList,nfields,"cpCI"),mat->tcpCI=NULL,
		RemoveField(fieldList,nfields,"cpAbruptChange"),mat->tcpAbruptChange=NULL;
	#define  _(x)						RemoveField(fieldList,nfields,#x),mat->t##x=NULL
	#define _2(x,y)						_(x),_(y)
	#define _3(x,y,z)					_2(x,y),_(z)
	#define _4(x,y,z,v)					_3(x,y,z),_(v)
	#define _6(x,y,z,v1,v2,v3)			_3(x,y,z),_3(v1,v2,v3)
	#define _7(x,y,z,v1,v2,v3,v4)		_6(x,y,z,v1,v2,v3),_(v4)
	if (!flag->tallyPosNegTrendJump) {
		_4(pos_ncp,neg_ncp,pos_ncpPr,neg_ncpPr),
		_2(pos_cpOccPr,neg_cpOccPr),
		_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr),
		_2(pos_cpAbruptChange,neg_cpAbruptChange),
		_2(pos_cpCI,neg_cpCI);
	}
	if (!flag->tallyIncDecTrendJump) {
		_4(inc_ncp,dec_ncp,inc_ncpPr,dec_ncpPr),
		_2(inc_cpOccPr,dec_cpOccPr),
		_4(inc_cp,dec_cp,inc_cpPr,dec_cpPr),
		_2(inc_cpAbruptChange,dec_cpAbruptChange),
		_2(inc_cpCI,dec_cpCI);
	}
	#undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
static void __RemoveFieldsGivenFlags_Season(A(OPTIONS_PTR)  opt,FIELD_ITEM * fieldList,int nfields) {
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I08 hasTrendCmpnt=1;
	I08 hasAlways=1;
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	const A(EXTRA_PTR)   flag=&(opt->extra);
	#define  _(x)						RemoveField(fieldList,nfields,#x),mat->s##x=NULL
	#define _2(x,y)						_(x),_(y)
	#define _3(x,y,z)					_2(x,y),_(z)
	#define _4(x,y,z,v)					_3(x,y,z),_(v)
	#define _6(x,y,z,v1,v2,v3)			_3(x,y,z),_3(v1,v2,v3)
	#define _5(x,y,z,v1,v2)				_4(x,y,z,v1),_(v2)
	#define _7(x,y,z,v1,v2,v3,v4)		_6(x,y,z,v1,v2,v3),_(v4)
	if (!hasSeasonCmpnt) {
		_6(ncp,ncpPr,cpOccPr,Y,SD,CI);
		_7(order,amp,ampSD,cp,cpPr,cpCI,cpAbruptChange);
		_6(pos_ncp,neg_ncp,pos_ncpPr,neg_ncpPr,pos_cpOccPr,neg_cpOccPr);
		_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr  );
		_4(pos_cpAbruptChange,neg_cpAbruptChange,pos_cpCI,neg_cpCI);
		return;
	}
	if (!flag->computeCredible)		RemoveField(fieldList,nfields,"CI"),mat->sCI=NULL;		
	if (!flag->computeSeasonOrder) 	RemoveField(fieldList,nfields,"order"),mat->sorder=NULL;
	if (!flag->computeSeasonAmp)
		RemoveField(fieldList,nfields,"amp"),mat->samp=NULL,
		RemoveField(fieldList,nfields,"ampSD"),mat->sampSD=NULL;
	if (!flag->computeSeasonChngpt)
		RemoveField(fieldList,nfields,"scp"),mat->scp=NULL,
		RemoveField(fieldList,nfields,"scpPr"),mat->scpPr=NULL,
		RemoveField(fieldList,nfields,"scpCI"),mat->scpCI=NULL,
		RemoveField(fieldList,nfields,"scpAbruptChange"),mat->scpAbruptChange=NULL;
	if (!flag->tallyPosNegSeasonJump) {
		_4(pos_ncp,neg_ncp,pos_ncpPr,neg_ncpPr),
		_2(pos_cpOccPr,neg_cpOccPr),
		_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr),
		_2(pos_cpAbruptChange,neg_cpAbruptChange),
		_2(pos_cpCI,neg_cpCI);
	}
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
static void __RemoveFieldsGivenFlags_Outlier(A(OPTIONS_PTR)  opt,FIELD_ITEM * fieldList,int nfields) {
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I08 hasTrendCmpnt=1;
	I08 hasAlways=1;
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	const A(EXTRA_PTR)   flag=&(opt->extra);
	#define  _(x)						RemoveField(fieldList,nfields,#x),mat->o##x=NULL
	#define _2(x,y)						_(x),_(y)
	#define _3(x,y,z)					_2(x,y),_(z)
	#define _4(x,y,z,v)					_3(x,y,z),_(v)
	#define _6(x,y,z,v1,v2,v3)			_3(x,y,z),_3(v1,v2,v3)
	#define _5(x,y,z,v1,v2)				_4(x,y,z,v1),_(v2)
	#define _7(x,y,z,v1,v2,v3,v4)		_6(x,y,z,v1,v2,v3),_(v4)
	if (!hasOutlierCmpnt) {				
		_6(ncp,ncpPr,cpOccPr,Y,SD,CI);
		_3(cp,cpPr,cpCI);
		_6(pos_ncp,neg_ncp,pos_ncpPr,neg_ncpPr,pos_cpOccPr,neg_cpOccPr);
		_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr);
		_2( pos_cpCI,neg_cpCI);		
		return;
	}
	if (!flag->computeCredible)		RemoveField(fieldList,nfields,"oCI"),mat->oCI=NULL;
	if (!flag->computeOutlierChngpt)
		RemoveField(fieldList,nfields,"ocp"),mat->ocp=NULL,
		RemoveField(fieldList,nfields,"ocpPr"),mat->ocpPr=NULL,
		RemoveField(fieldList,nfields,"ocpCI"),mat->ocpCI=NULL;	
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
static void* __BEAST2_Output_AllocMEM_Trend(A(OPTIONS_PTR)  opt) {
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE  dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.trendMaxKnotNum;
	#define NUMARGS(...)                 (sizeof((int[]){__VA_ARGS__})/sizeof(int))
	#define NARGS(...)                   (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
	#define _(name,...)                 {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->t##name }
	#define _1(name,...)                _(name,__VA_ARGS__)  
	#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
	#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
	#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
	#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
	#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),		    
			_(ncpPr,mxKnotNum+1,M),				
			_(cpOccPr,N,M),			
			_(order,N,M),						
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,M    ),
			_(cpCI,mxKnotNum,2,M ),
			_2(Y,SD,N,M),
			_(CI,N,2,M),
			_2(pos_ncp,neg_ncp,1,M),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_2(pos_cpOccPr,neg_cpOccPr,N,M),
			_6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,M),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
			_3(slp,slpSD,slpSignPr,N,M),
			_2(inc_ncp,dec_ncp,1,M),
			_2(inc_ncpPr,dec_ncpPr,mxKnotNum+1,M),
			_2(inc_cpOccPr,dec_cpOccPr,N,M),
			_6(inc_cp,dec_cp,inc_cpPr,dec_cpPr,inc_cpAbruptChange,dec_cpAbruptChange,mxKnotNum,M),
			_2(inc_cpCI,dec_cpCI,mxKnotNum,2,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3)
	{
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
		case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_(cpOccPr,N,ROW,COL),
			_(order,N,ROW,COL),
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_2(Y,SD,N,ROW,COL),
			_(CI,N,2,ROW,COL),
			_2(pos_ncp,neg_ncp,ROW,COL),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,ROW,COL),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
			_3(slp,slpSD,slpSignPr,N,ROW,COL),
			_2(inc_ncp,dec_ncp,ROW,COL),
			_2(inc_ncpPr,dec_ncpPr,mxKnotNum+1,ROW,COL),
			_2(inc_cpOccPr,dec_cpOccPr,N,ROW,COL),
			_6(inc_cp,dec_cp,inc_cpPr,dec_cpPr,inc_cpAbruptChange,dec_cpAbruptChange,mxKnotNum,ROW,COL),
			_2(inc_cpCI,dec_cpCI,mxKnotNum,2,ROW,COL),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Trend(opt,fieldList,nfields);
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));
	UNPROTECT(1L);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
static void* __BEAST2_Output_AllocMEM_Season(A(OPTIONS_PTR)  opt)
{
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE   dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.seasonMaxKnotNum;
#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->s##name }
#define _1(name,...)               _(name,__VA_ARGS__)  
#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),		    
			_(ncpPr,mxKnotNum+1,M),				
			_(cpOccPr,N,M),				
			_(order,N,M),						
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,M    ),
			_(cpCI,mxKnotNum,2,M ),
			_2(Y,SD,N,M),
			_(CI,N,2,M),
			_2(pos_ncp,neg_ncp,1,M),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_2(pos_cpOccPr,neg_cpOccPr,N,M),
			_6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,M),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
			_2(amp,ampSD,N,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3) {
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
		case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_(cpOccPr,N,ROW,COL),			
			_(order,N,ROW,COL),
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_2(Y,SD,N,ROW,COL),
			_(CI,N,2,ROW,COL),
			_2(pos_ncp,neg_ncp,ROW,COL),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,ROW,COL),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
			_2(amp,ampSD,N,ROW,COL),			
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Season(opt,fieldList,nfields);
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));
	UNPROTECT(1L);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
static void* __BEAST2_Output_AllocMEM_Outlier(A(OPTIONS_PTR)  opt)
{
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE   dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.outlierMaxKnotNum;	
#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->o##name }
#define _1(name,...)               _(name,__VA_ARGS__)  
#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),
			_(ncpPr,mxKnotNum+1,M),
			_(cpOccPr,N,M),
			_2(cp,cpPr,mxKnotNum,M),
			_(cpCI,mxKnotNum,2,M),
			_2(Y,SD,N,M),
			_(CI,N,2,M),
			_2(pos_ncp,neg_ncp,1,M),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_2(pos_cpOccPr,neg_cpOccPr,N,M),
			_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr,mxKnotNum,M),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3) {
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
			case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
			case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
			case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={		
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_3(cpOccPr,Y,SD,N,ROW,COL),
			_(CI,N,2,ROW,COL),
			_2(cp,cpPr,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_2(pos_ncp,neg_ncp,ROW,COL),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr,mxKnotNum,ROW,COL),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Outlier(opt,fieldList,nfields);
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));
	UNPROTECT(1L);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
void*  BEAST2_Output_AllocMEM(A(OPTIONS_PTR)  opt) 
{	
	if (opt->io.out.result) {
		free(opt->io.out.result);
	}
	opt->io.out.result=malloc(sizeof(BEAST2_RESULT) * opt->io.q);
	memset(opt->io.out.result,0,sizeof(BEAST2_RESULT) * opt->io.q);
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE  dtype=io->out.dtype; 
#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->tY##name }
#define _1(name,...)               _(name,__VA_ARGS__)  
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasDummyCmpnt=opt->prior.basisType[0]==DUMMYID;
	I08 hasTrendCmpnt=1;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I32 nprt=0;
	VOID_PTR  trend=NULL,season=NULL,outlier=NULL;
	if (hasTrendCmpnt)   { trend=PROTECT(__BEAST2_Output_AllocMEM_Trend(opt));    nprt++; }
	if (hasSeasonCmpnt)  { season=PROTECT(__BEAST2_Output_AllocMEM_Season(opt));   nprt++; }
	if (hasOutlierCmpnt) { outlier=PROTECT(__BEAST2_Output_AllocMEM_Outlier(opt));  nprt++;}
	int   M=io->numOfPixels;
	int   ROW,COL;
	if (io->ndim==1||io->ndim==2) {
		if (io->ndim==1||(io->ndim==2 && io->out.whichDimIsTime==1)) 			
			ROW=1,COL=M;
		else
			ROW=M,COL=1;
	} else {
		switch (io->meta.whichDimIsTime) {
		case 1:		ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:		ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:		ROW=io->dims[0],COL=io->dims[1]; break;
		}	
	}
	const  int  N=io->N;
	const  int  q=io->q;
	const  int  Nq=N * q;
	FIELD_ITEM  fieldList[ ]={
		{"time",dtype,2,{N,1,},&mat->time},
		{"data",dtype,-1,{-1,-1,},&mat->data}, 
		{"marg_lik",dtype,2,{ROW,COL,},&mat->marg_lik},
		#ifdef __DEBUG__
        {"R2",dtype,2,{(N+7)/8 * 8,300,},&mat->R2},
		{"RMSE",dtype,2,{(N+7)/8 * 8,300,},&mat->RMSE},
	    #endif
		{"R2",dtype,2,{ROW,COL,},&mat->R2},
		{"RMSE",dtype,2,{ROW,COL,},&mat->RMSE},
	    {"sig2",dtype,2,{ROW,COL,},&mat->sig2},
		{"trend",DATA_STRUCT,0,{0,},(void**)trend},
		{"season",DATA_STRUCT,0,{0,},(void**)season},
		{"outlier",DATA_STRUCT,0,{0,},(void**)outlier},
	};
	I32    nfields=sizeof(fieldList)/sizeof(FIELD_ITEM);
	if (opt->extra.dumpInputData) {
		if (io->ndim==1||io->ndim==2) {
			fieldList[1].ndim=2;
			fieldList[1].dims[0]=N;	fieldList[1].dims[1]=M;
			if (io->ndim==2 && io->out.whichDimIsTime==2) 
				__ChangeFieldsTimeDimFrm1to2_2D(fieldList+1,1L);
		}
		if (io->ndim==3) {
			int   ROW,COL;
			switch (io->meta.whichDimIsTime) {
			case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
			case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
			case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
			}
			fieldList[1].ndim=3;
			fieldList[1].dims[0]=N;	fieldList[1].dims[1]=ROW; fieldList[1].dims[2]=COL;
			if (io->out.whichDimIsTime > 1) 
				__ChangeFieldsTimeDimFrm1to_3D(fieldList+1,1L,io->out.whichDimIsTime);		
		}
	}
	else {
		RemoveField(fieldList,nfields,"data");
		mat->data=NULL;
	}
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields)); nprt++;
	AddStringAttribute(out,"class","beast");	
	if (hasSeasonCmpnt && !hasDummyCmpnt) AddStringAttribute(out,"season_type","harmonic");
	if (hasSeasonCmpnt &&  hasDummyCmpnt) AddStringAttribute(out,"season_type","dummy");
	if (!hasSeasonCmpnt )                 AddStringAttribute(out,"season_type","none");
	AddIntegerAttribute(out,"hasOutlier",hasOutlierCmpnt);	
	AddIntegerAttribute(out,"whichOutputDimIsTime",opt->io.out.whichDimIsTime);
	f32_seq(mat->time,io->meta.startTime,io->meta.deltaTime,N);
	if (dtype==DATA_DOUBLE)  f32_to_f64_inplace(mat->time,N);	
	UNPROTECT(nprt);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
}
void BEAST2_Result_AllocMEM(A(RESULT_PTR)  result,A(OPTIONS_PTR)  opt,MemPointers* _restrict MEM)
{	
	const I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	const I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	const I08 hasTrendCmpnt=1;
	const I08 hasAlways=1;
	const I32 N=opt->io.N;
	const I32 q=opt->io.q;
	const I32 Nq=N * q;
	const I32 seasonMaxKnotNum=opt->prior.seasonMaxKnotNum;
	const I32 trendMaxKnotNum=opt->prior.trendMaxKnotNum;
	const I32 outlierMaxKnotNum=opt->prior.outlierMaxKnotNum;
	*result=(BEAST2_RESULT){0,}; 
	result->time=NULL; 
	if (opt->extra.dumpInputData) {
		result->data=MEM->alloc(MEM,sizeof(F32) * Nq,0);
	}
	result->marg_lik=MEM->alloc(MEM,sizeof(F32) * 1,0);
	result->sig2=MEM->alloc(MEM,sizeof(F32) * q*q,0);
	result->R2=MEM->alloc(MEM,sizeof(F32) * q,0);
	result->RMSE=MEM->alloc(MEM,sizeof(F32) * q,0);
	if (hasSeasonCmpnt) {
		result->sncp=MEM->alloc(MEM,sizeof(F32) * 1,0);
		result->sncpPr=MEM->alloc(MEM,sizeof(I32) * (seasonMaxKnotNum+1),64);
		result->scpOccPr=MEM->alloc(MEM,sizeof(I32) * N,64);
		result->sY=MEM->alloc(MEM,sizeof(F32) * Nq,64);
		result->sSD=MEM->alloc(MEM,sizeof(F32) * Nq,64);
		if (opt->extra.computeSeasonOrder)
			result->sorder=MEM->alloc(MEM,sizeof(U32) * N,64);
		if (opt->extra.computeSeasonAmp)
			result->samp=MEM->alloc(MEM,sizeof(F32) * Nq,64),  
			result->sampSD=MEM->alloc(MEM,sizeof(F32) * Nq,64);  
	}
	if (hasTrendCmpnt) {
		result->tncp=MEM->alloc(MEM,sizeof(F32) * 1,0);
		result->tncpPr=MEM->alloc(MEM,sizeof(I32) * (trendMaxKnotNum+1),64);
		result->tcpOccPr=MEM->alloc(MEM,sizeof(I32) * N,64);
		result->tY=MEM->alloc(MEM,sizeof(F32) * Nq,64);
		result->tSD=MEM->alloc(MEM,sizeof(F32) * Nq,64);
		if (opt->extra.computeTrendOrder)
			result->torder=MEM->alloc(MEM,sizeof(U32) * N,64);
		if (opt->extra.computeTrendSlope)
			result->tslp=MEM->alloc(MEM,sizeof(F32) * Nq,64),
			result->tslpSD=MEM->alloc(MEM,sizeof(F32) * Nq,64),
			result->tslpSignPr=MEM->alloc(MEM,sizeof(I32) * Nq,64);
	}
	if (hasOutlierCmpnt) {
		result->oncp=MEM->alloc(MEM,sizeof(F32) * 1,0);
		result->oncpPr=MEM->alloc(MEM,sizeof(I32) * (outlierMaxKnotNum+1),64);
		result->ocpOccPr=MEM->alloc(MEM,sizeof(I32) * N,64);
		result->oY=MEM->alloc(MEM,sizeof(F32) * Nq,64);
		result->oSD=MEM->alloc(MEM,sizeof(F32) * Nq,64);
	}
	if (opt->extra.computeCredible){
		if (hasSeasonCmpnt) result->sCI=MEM->alloc(MEM,sizeof(F32) * Nq * 2,64);
		if (hasTrendCmpnt)  result->tCI=MEM->alloc(MEM,sizeof(F32) * Nq * 2,64);
		if (hasOutlierCmpnt) result->oCI=MEM->alloc(MEM,sizeof(F32) * Nq * 2,64);
	}
	if (opt->extra.computeSeasonChngpt && hasSeasonCmpnt) {
		result->scp=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum,64),
		result->scpPr=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum,64),
		result->scpAbruptChange=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum,64),
		result->scpCI=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * 2,64);	}	
	if (opt->extra.computeTrendChngpt && hasTrendCmpnt)
		result->tcp=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum,64),
		result->tcpPr=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum,64),
		result->tcpAbruptChange=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum,64),
		result->tcpCI=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * 2,64);
	if (opt->extra.computeOutlierChngpt && hasOutlierCmpnt)
		result->ocp=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum,64),
		result->ocpPr=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum,64),
		result->ocpCI=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * 2,64);
	if (opt->extra.tallyPosNegSeasonJump && hasSeasonCmpnt)
		result->spos_ncp=MEM->alloc(MEM,sizeof(F32) * 1*q,0),
		result->sneg_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->spos_ncpPr=MEM->alloc(MEM,sizeof(I32) * (seasonMaxKnotNum+1) * q,64),
		result->sneg_ncpPr=MEM->alloc(MEM,sizeof(I32) * (seasonMaxKnotNum+1) * q,64),
		result->spos_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->sneg_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->spos_cp=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->sneg_cp=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->spos_cpPr=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->sneg_cpPr=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->spos_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->sneg_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * q,64),
		result->spos_cpCI=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * 2 * q,64),
		result->sneg_cpCI=MEM->alloc(MEM,sizeof(U32) * seasonMaxKnotNum * 2 * q,64);
	if (opt->extra.tallyPosNegTrendJump && hasTrendCmpnt)
		result->tpos_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->tneg_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->tpos_ncpPr=MEM->alloc(MEM,sizeof(I32) * (trendMaxKnotNum+1) * q,64),
		result->tneg_ncpPr=MEM->alloc(MEM,sizeof(I32) * (trendMaxKnotNum+1) * q,64),
		result->tpos_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->tneg_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->tpos_cp=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tneg_cp=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tpos_cpPr=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tneg_cpPr=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tpos_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tneg_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tpos_cpCI=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * 2 * q,64),
		result->tneg_cpCI=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * 2 * q,64);
	if (opt->extra.tallyIncDecTrendJump && hasTrendCmpnt)
		result->tinc_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->tdec_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->tinc_ncpPr=MEM->alloc(MEM,sizeof(I32) * (trendMaxKnotNum+1) * q,64),
		result->tdec_ncpPr=MEM->alloc(MEM,sizeof(I32) * (trendMaxKnotNum+1) * q,64),
		result->tinc_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->tdec_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->tinc_cp=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tdec_cp=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tinc_cpPr=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tdec_cpPr=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tinc_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tdec_cpAbruptChange=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * q,64),
		result->tinc_cpCI=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * 2 * q,64),
		result->tdec_cpCI=MEM->alloc(MEM,sizeof(U32) * trendMaxKnotNum * 2 * q,64);
	if (opt->extra.tallyPosNegOutliers && hasOutlierCmpnt)
		result->opos_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->oneg_ncp=MEM->alloc(MEM,sizeof(F32) * 1 * q,0),
		result->opos_ncpPr=MEM->alloc(MEM,sizeof(I32) * (outlierMaxKnotNum+1) * q,64),
		result->oneg_ncpPr=MEM->alloc(MEM,sizeof(I32) * (outlierMaxKnotNum+1) * q,64),
		result->opos_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->oneg_cpOccPr=MEM->alloc(MEM,sizeof(I32) * Nq,64),
		result->opos_cp=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * q,64),
		result->oneg_cp=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * q,64),
		result->opos_cpPr=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * q,64),
		result->oneg_cpPr=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * q,64),
		result->opos_cpCI=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * 2 * q,64),
		result->oneg_cpCI=MEM->alloc(MEM,sizeof(U32) * outlierMaxKnotNum * 2 * q,64);
}
void BEAST2_Result_FillMEM(A(RESULT_PTR)  result,A(OPTIONS_PTR)  opt,const F32 nan)
{
	const I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	const I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	const I08 hasTrendCmpnt=1;
	const I08 hasAlways=1;
	const I32 N=opt->io.N;
	const I32 q=opt->io.q;
	const I32 Nq=N * q;
	const I32 seasonMaxKnotNum=opt->prior.seasonMaxKnotNum;
	const I32 trendMaxKnotNum=opt->prior.trendMaxKnotNum;
	const I32 outlierMaxKnotNum=opt->prior.outlierMaxKnotNum;
	A(EXTRA_PTR) flag=&(opt->extra);
	*result->marg_lik=nan;
	f32_fill_val(nan,result->sig2,q*q);
	f32_fill_val(nan,result->R2,q);
	f32_fill_val(nan,result->RMSE,q);
	if (hasSeasonCmpnt) {
			*result->sncp=nan;
			f32_fill_val(nan,result->sncpPr,seasonMaxKnotNum+1);
			f32_fill_val(nan,result->scpOccPr,N);
			f32_fill_val(nan,result->sY,Nq);
			f32_fill_val(nan,result->sSD,Nq);
			if (flag->computeSeasonOrder)  
				f32_fill_val(nan,result->sorder,N); 
			if (flag->computeSeasonAmp) {
				f32_fill_val(nan,result->samp,Nq);
				f32_fill_val(nan,result->sampSD,Nq);
			}
	}
	if (hasTrendCmpnt) {
			*result->tncp=nan;
			f32_fill_val(nan,result->tncpPr,trendMaxKnotNum+1);
			f32_fill_val(nan,result->tcpOccPr,N);
			f32_fill_val(nan,result->tY,Nq);
			f32_fill_val(nan,result->tSD,Nq);
			if (flag->computeTrendOrder)
				f32_fill_val(nan,result->torder,N);
			if (flag->computeTrendSlope) {
				f32_fill_val(nan,result->tslp,Nq);
				f32_fill_val(nan,result->tslpSD,Nq);
				f32_fill_val(nan,result->tslpSignPr,Nq);
			}
	}
	if (hasOutlierCmpnt) {
		*result->oncp=nan;
		f32_fill_val(nan,result->oncpPr,outlierMaxKnotNum+1);
		f32_fill_val(nan,result->ocpOccPr,N);
		f32_fill_val(nan,result->oY,Nq);
		f32_fill_val(nan,result->oSD,Nq);
	}
	if (flag->computeCredible) 	{
		if (hasSeasonCmpnt)  f32_fill_val(nan,result->sCI,2*Nq);
		if (hasTrendCmpnt)   f32_fill_val(nan,result->tCI,2*Nq);
		if (hasOutlierCmpnt) f32_fill_val(nan,result->oCI,2*Nq);
	}
	if (flag->computeSeasonChngpt && hasSeasonCmpnt) 	{
		f32_fill_val(nan,result->scp,seasonMaxKnotNum);
		f32_fill_val(nan,result->scpPr,seasonMaxKnotNum);
		f32_fill_val(nan,result->scpAbruptChange,seasonMaxKnotNum);
		f32_fill_val(nan,result->scpCI,2*seasonMaxKnotNum); 
	}
	if (flag->computeTrendChngpt && hasTrendCmpnt) {
		f32_fill_val(nan,result->tcp,trendMaxKnotNum * q);
		f32_fill_val(nan,result->tcpPr,trendMaxKnotNum * q);
		f32_fill_val(nan,result->tcpAbruptChange,trendMaxKnotNum * q);
		f32_fill_val(nan,result->tcpCI,2* trendMaxKnotNum * q);
	}
	if (flag->computeOutlierChngpt&& hasOutlierCmpnt) {
		f32_fill_val(nan,result->ocp,outlierMaxKnotNum * q);
		f32_fill_val(nan,result->ocpPr,outlierMaxKnotNum * q);
		f32_fill_val(nan,result->ocpCI,2* outlierMaxKnotNum * q);
	}
	if (flag->tallyPosNegSeasonJump && hasSeasonCmpnt)  {
		f32_fill_val(nan,result->spos_ncp,1*q);
		f32_fill_val(nan,result->sneg_ncp,1 * q);
		f32_fill_val(nan,result->spos_ncpPr,(seasonMaxKnotNum+1) * q);
		f32_fill_val(nan,result->sneg_ncpPr,(seasonMaxKnotNum+1) * q);
		f32_fill_val(nan,result->spos_cpOccPr,Nq);
		f32_fill_val(nan,result->sneg_cpOccPr,Nq);
		f32_fill_val(nan,result->spos_cp,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->sneg_cp,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->spos_cpPr,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->sneg_cpPr,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->spos_cpAbruptChange,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->sneg_cpAbruptChange,seasonMaxKnotNum * q);
		f32_fill_val(nan,result->spos_cpCI,2*seasonMaxKnotNum * q);
		f32_fill_val(nan,result->sneg_cpCI,2 * seasonMaxKnotNum * q);
	}
	if (flag->tallyPosNegTrendJump && hasTrendCmpnt) {
		f32_fill_val(nan,result->tpos_ncp,1 * q);
		f32_fill_val(nan,result->tneg_ncp,1 * q);
		f32_fill_val(nan,result->tpos_ncpPr,(trendMaxKnotNum+1) * q);
		f32_fill_val(nan,result->tneg_ncpPr,(trendMaxKnotNum+1) * q);
		f32_fill_val(nan,result->tpos_cpOccPr,Nq);
		f32_fill_val(nan,result->tneg_cpOccPr,Nq);
		f32_fill_val(nan,result->tpos_cp,trendMaxKnotNum * q);
		f32_fill_val(nan,result->tneg_cp,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tpos_cpPr,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tneg_cpPr,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tpos_cpAbruptChange,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tneg_cpAbruptChange,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tpos_cpCI,2* trendMaxKnotNum * q);
		f32_fill_val(nan,result->tneg_cpCI,2 * trendMaxKnotNum * q);
	}
	if (flag->tallyIncDecTrendJump && hasTrendCmpnt) {
		f32_fill_val(nan,result->tinc_ncp,1 * q);
		f32_fill_val(nan,result->tdec_ncp,1 * q);
		f32_fill_val(nan,result->tinc_ncpPr,(trendMaxKnotNum+1) * q);
		f32_fill_val(nan,result->tdec_ncpPr,(trendMaxKnotNum+1)* q);
		f32_fill_val(nan,result->tinc_cpOccPr,Nq);
		f32_fill_val(nan,result->tdec_cpOccPr,Nq);
		f32_fill_val(nan,result->tinc_cp,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tdec_cp,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tinc_cpPr,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tdec_cpPr,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tinc_cpAbruptChange,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tdec_cpAbruptChange,trendMaxKnotNum* q);
		f32_fill_val(nan,result->tinc_cpCI,2* trendMaxKnotNum);
		f32_fill_val(nan,result->tdec_cpCI,2 * trendMaxKnotNum); 
	}
	if (flag->tallyPosNegOutliers && hasOutlierCmpnt) {
		f32_fill_val(nan,result->opos_ncp,1 * q);
		f32_fill_val(nan,result->oneg_ncp,1 * q);
		f32_fill_val(nan,result->opos_ncpPr,(outlierMaxKnotNum+1)* q);
		f32_fill_val(nan,result->opos_ncpPr,(outlierMaxKnotNum+1) * q);
		f32_fill_val(nan,result->opos_cpOccPr,Nq);
		f32_fill_val(nan,result->oneg_cpOccPr,Nq);
		f32_fill_val(nan,result->opos_cp,outlierMaxKnotNum* q);
		f32_fill_val(nan,result->oneg_cp,outlierMaxKnotNum* q);
		f32_fill_val(nan,result->opos_cpPr,outlierMaxKnotNum* q);
		f32_fill_val(nan,result->oneg_cpPr,outlierMaxKnotNum * q);
		f32_fill_val(nan,result->opos_cpCI,2 * outlierMaxKnotNum * q);
		f32_fill_val(nan,result->oneg_cpCI,2 * outlierMaxKnotNum * q);
	}
}
static INLINE I32 __MR_ExtendFieldsToMultiVaraiteTS(FIELD_ITEM *flist,I32 N,I32 q) {
	static char* nms[]={ "Y1","Y2","Y3","Y4","Y5","Y6","Y7",
					   "Y8","Y9","Y10","Y11","Y12","Y13","Y14",
						"Y15","Y16","Y17","Y18","Y19","Y20" };
	I32 nptr=0;
	I32 nptr_dummy=0;
	for (int i=0; i < N; i++) {
		if (flist[i].extra==0)	continue;
		nptr_dummy++;
		FIELD_ITEM qList[100]={ 0,};
		for (int j=0; j < q; j++) {
			strcpy(qList[j].name,nms[j]);
			qList[j].type=flist[i].type;
			qList[j].ndim=flist[i].ndim;
			memcpy(&qList[j].dims,&flist[i].dims,sizeof(I32) * 5);
			qList[j].extra=0;
			if (flist[i].ptr !=NULL)
				qList[j].ptr=(char*)(flist[i].ptr)+sizeof(BEAST2_RESULT) * j;
			else
				qList[j].ptr=NULL;
		}
		nptr_dummy--;
		VOID_PTR  out;
		PROTECT(out=CreateStructVar(qList,q));		nptr++;
		flist[i].extra=0;
		flist[i].ndim=0;
		flist[i].type=DATA_STRUCT;
		flist[i].ptr=out;
	}
	 UNPROTECT(nptr_dummy); 
	 return nptr;
}
static void* __MR_Output_AllocMEM_Trend(A(OPTIONS_PTR)  opt) {
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE  dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.trendMaxKnotNum;
	#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
	#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
	#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->t##name,0 }
	#define _1(name,...)               _(name,__VA_ARGS__)  
	#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
	#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
	#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
	#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
	#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	#define _q(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->t##name,1 }
	#define _q1(name,...)               _q(name,__VA_ARGS__)  
	#define _q2(name1,name2,...)         _q(name1,__VA_ARGS__),_q(name2,__VA_ARGS__)   
	#define _q3(n1,n2,n3,...)            _q2(n1,n2,__VA_ARGS__),_q(n3,__VA_ARGS__)  
	#define _q4(n1,n2,n3,n4,...)         _q3(n1,n2,n3,__VA_ARGS__),_q(n4,__VA_ARGS__)  
	#define _q5(n1,n2,n3,n4,n5,...)      _q4(n1,n2,n3,n4,__VA_ARGS__),_q(n5,__VA_ARGS__)  
	#define _q6(n1,n2,n3,n4,n5,n6,...)   _q5(n1,n2,n3,n4,n5,__VA_ARGS__),_q(n6,__VA_ARGS__) 
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),		    
			_(ncpPr,mxKnotNum+1,M),				
			_(cpOccPr,N,M),				
			_(order,N,M),						
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,M    ),
			_(cpCI,mxKnotNum,2,M ),
			_q2(Y,SD,N,M),
			_q(CI,N,2,M),
			_q2(pos_ncp,neg_ncp,1,M),
			_q2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_q2(pos_cpOccPr,neg_cpOccPr,N,M),
			_q6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,M),
			_q2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
			_q3(slp,slpSD,slpSignPr,N,M),
			_q2(inc_ncp,dec_ncp,1,M),
			_q2(inc_ncpPr,dec_ncpPr,mxKnotNum+1,M),
			_q2(inc_cpOccPr,dec_cpOccPr,N,M),
			_q6(inc_cp,dec_cp,inc_cpPr,dec_cpPr,inc_cpAbruptChange,dec_cpAbruptChange,mxKnotNum,M),
			_q2(inc_cpCI,dec_cpCI,mxKnotNum,2,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3)
	{
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
		case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_(cpOccPr,N,ROW,COL),
			_(order,N,ROW,COL),
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_q2(Y,SD,N,ROW,COL),
			_q(CI,N,2,ROW,COL),
			_q2(pos_ncp,neg_ncp,ROW,COL),
			_q2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_q2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_q6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,ROW,COL),
			_q2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
			_q3(slp,slpSD,slpSignPr,N,ROW,COL),
			_q2(inc_ncp,dec_ncp,ROW,COL),
			_q2(inc_ncpPr,dec_ncpPr,mxKnotNum+1,ROW,COL),
			_q2(inc_cpOccPr,dec_cpOccPr,N,ROW,COL),
			_q6(inc_cp,dec_cp,inc_cpPr,dec_cpPr,inc_cpAbruptChange,dec_cpAbruptChange,mxKnotNum,ROW,COL),
			_q2(inc_cpCI,dec_cpCI,mxKnotNum,2,ROW,COL),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Trend(opt,fieldList,nfields);
	I32       nptr=__MR_ExtendFieldsToMultiVaraiteTS(fieldList,nfields,io->q);
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));
	UNPROTECT(1L);
	UNPROTECT(nptr);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
	#undef _q
	#undef _q2
	#undef _q3
	#undef _q4
    #undef _q5
	#undef _q6
	#undef _q7
}
static void* __MR_Output_AllocMEM_Season(A(OPTIONS_PTR)  opt)
{
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE   dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.seasonMaxKnotNum;
	#define NUMARGS(...)                 (sizeof((int[]){__VA_ARGS__})/sizeof(int))
	#define NARGS(...)                   (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
	#define _(name,...)                 {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->s##name }
	#define _1(name,...)                _(name,__VA_ARGS__)  
	#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
	#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
	#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
	#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
	#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	#define _q(name,...)                 {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->s##name,1 }
	#define _q1(name,...)                _q(name,__VA_ARGS__)  
	#define _q2(name1,name2,...)         _q(name1,__VA_ARGS__),_q(name2,__VA_ARGS__)   
	#define _q3(n1,n2,n3,...)            _q2(n1,n2,__VA_ARGS__),_q(n3,__VA_ARGS__)  
	#define _q4(n1,n2,n3,n4,...)         _q3(n1,n2,n3,__VA_ARGS__),_q(n4,__VA_ARGS__)  
	#define _q5(n1,n2,n3,n4,n5,...)      _q4(n1,n2,n3,n4,__VA_ARGS__),_q(n5,__VA_ARGS__)  
	#define _q6(n1,n2,n3,n4,n5,n6,...)   _q5(n1,n2,n3,n4,n5,__VA_ARGS__),_q(n6,__VA_ARGS__) 
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),		    
			_(ncpPr,mxKnotNum+1,M),				
			_(cpOccPr,N,M),				
			_(order,N,M),						
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,M    ),
			_(cpCI,mxKnotNum,2,M ),
			_q2(Y,SD,N,M),
			_q(CI,N,2,M),
			_q2(pos_ncp,neg_ncp,1,M),
			_q2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_q2(pos_cpOccPr,neg_cpOccPr,N,M),
			_q6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,M),
			_q2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
			_q2(amp,ampSD,N,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3) {
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
		case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_(cpOccPr,N,ROW,COL),			
			_(order,N,ROW,COL),
			_3(cp,cpPr,cpAbruptChange,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_q2(Y,SD,N,ROW,COL),
			_q(CI,N,2,ROW,COL),
			_q2(pos_ncp,neg_ncp,ROW,COL),
			_q2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_q2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_q6(pos_cp,neg_cp,pos_cpPr,neg_cpPr,pos_cpAbruptChange,neg_cpAbruptChange,mxKnotNum,ROW,COL),
			_q2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
			_q2(amp,ampSD,N,ROW,COL),			
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Season(opt,fieldList,nfields);
	I32       nptr=__MR_ExtendFieldsToMultiVaraiteTS(fieldList,nfields,io->q);
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));
	UNPROTECT(1L);
	UNPROTECT(nptr);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
	#undef _q
	#undef _q2
	#undef _q3
	#undef _q4
    #undef _q5
	#undef _q6
	#undef _q7
}
static void* __MR_Output_AllocMEM_Outlier(A(OPTIONS_PTR)  opt)
{
	const A(IO_PTR)      io=&opt->io;
	const A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE   dtype=io->out.dtype; 
	const int   N=io->N;
	const int   M=io->numOfPixels;	
	const int   mxKnotNum=opt->prior.outlierMaxKnotNum;	
	#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
	#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
	#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->o##name }
	#define _1(name,...)               _(name,__VA_ARGS__)  
	#define _2(name1,name2,...)         _(name1,__VA_ARGS__),_(name2,__VA_ARGS__)   
	#define _3(n1,n2,n3,...)            _2(n1,n2,__VA_ARGS__),_(n3,__VA_ARGS__)  
	#define _4(n1,n2,n3,n4,...)         _3(n1,n2,n3,__VA_ARGS__),_(n4,__VA_ARGS__)  
	#define _5(n1,n2,n3,n4,n5,...)      _4(n1,n2,n3,n4,__VA_ARGS__),_(n5,__VA_ARGS__)  
	#define _6(n1,n2,n3,n4,n5,n6,...)   _5(n1,n2,n3,n4,n5,__VA_ARGS__),_(n6,__VA_ARGS__)  
	#define _q(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->o##name,1 }
	#define _q1(name,...)                _q(name,__VA_ARGS__)  
	#define _q2(name1,name2,...)         _q(name1,__VA_ARGS__),_q(name2,__VA_ARGS__)   
	#define _q3(n1,n2,n3,...)            _q2(n1,n2,__VA_ARGS__),_q(n3,__VA_ARGS__)  
	#define _q4(n1,n2,n3,n4,...)         _q3(n1,n2,n3,__VA_ARGS__),_q(n4,__VA_ARGS__)  
	#define _q5(n1,n2,n3,n4,n5,...)      _q4(n1,n2,n3,n4,__VA_ARGS__),_q(n5,__VA_ARGS__)  
	#define _q6(n1,n2,n3,n4,n5,n6,...)   _q5(n1,n2,n3,n4,n5,__VA_ARGS__),_q(n6,__VA_ARGS__) 
	FIELD_ITEM  fieldList[93+2];
	I32         nfields=0;
	if (io->ndim==1||io->ndim==2  ) { 
		FIELD_ITEM fldList[]={
			_(ncp,1,M),
			_(ncpPr,mxKnotNum+1,M),
			_(cpOccPr,N,M),
			_2(cp,cpPr,mxKnotNum,M),
			_(cpCI,mxKnotNum,2,M),
			_q2(Y,SD,N,M),
			_q(CI,N,2,M),
			_q2(pos_ncp,neg_ncp,1,M),
			_q2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,M),
			_q2(pos_cpOccPr,neg_cpOccPr,N,M),
			_q4(pos_cp,neg_cp,pos_cpPr,neg_cpPr,mxKnotNum,M),
			_q2(pos_cpCI,neg_cpCI,mxKnotNum,2,M),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->ndim==2 && io->out.whichDimIsTime==2) {
			__ChangeFieldsTimeDimFrm1to2_2D(fldList,nfields);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	if (io->ndim==3) {
		int   ROW,COL;
		switch (io->meta.whichDimIsTime) {
			case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
			case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
			case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
		}
		FIELD_ITEM fldList[]={		
			_(ncp,ROW,COL),
			_(ncpPr,mxKnotNum+1,ROW,COL),
			_3(cpOccPr,Y,SD,N,ROW,COL),
			_(CI,N,2,ROW,COL),
			_2(cp,cpPr,mxKnotNum,ROW,COL),
			_(cpCI,mxKnotNum,2,ROW,COL),
			_2(pos_ncp,neg_ncp,ROW,COL),
			_2(pos_ncpPr,neg_ncpPr,mxKnotNum+1,ROW,COL),
			_2(pos_cpOccPr,neg_cpOccPr,N,ROW,COL),
			_4(pos_cp,neg_cp,pos_cpPr,neg_cpPr,mxKnotNum,ROW,COL),
			_2(pos_cpCI,neg_cpCI,mxKnotNum,2,ROW,COL),
		};
		nfields=sizeof(fldList)/sizeof(FIELD_ITEM);
		if (io->out.whichDimIsTime > 1) {
			__ChangeFieldsTimeDimFrm1to_3D(fldList,nfields,io->out.whichDimIsTime);
		}
		memcpy(fieldList,fldList,nfields * sizeof(FIELD_ITEM));
	}
	__RemoveFieldsGivenFlags_Outlier(opt,fieldList,nfields);
	I32       nptr=__MR_ExtendFieldsToMultiVaraiteTS(fieldList,nfields,io->q);	 
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));            nptr++;	
	UNPROTECT(nptr);
	return out;
	#undef NUMARGS
    #undef NARGS
    #undef _
	#undef _2
	#undef _3
	#undef _4
    #undef _5
	#undef _6
	#undef _7
	#undef _q
	#undef _q2
	#undef _q3
	#undef _q4
    #undef _q5
	#undef _q6
	#undef _q7
}
void *  MR_Output_AllocMEM(BEAST2_OPTIONS_PTR  opt) 
{	
	if (opt->io.out.result) {
		free(opt->io.out.result);
	}
	opt->io.out.result=malloc(sizeof(BEAST2_RESULT) * opt->io.q);
	memset(opt->io.out.result,0,sizeof(BEAST2_RESULT) * opt->io.q);
	const     A(IO_PTR)      io=&opt->io;
	const     A(RESULT_PTR)  mat=io->out.result;
	DATA_TYPE  dtype=io->out.dtype; 
	#define NUMARGS(...)                (sizeof((int[]){__VA_ARGS__})/sizeof(int))
	#define NARGS(...)                  (sizeof((int[]){0,##__VA_ARGS__})/sizeof(int)-1)
	#define _(name,...)                {#name,dtype,NUMARGS(__VA_ARGS__),{__VA_ARGS__},(void ** )&mat->tY##name }
	#define _1(name,...)               _(name,__VA_ARGS__)  
	I08 hasSeasonCmpnt=opt->prior.basisType[0]==SEASONID||opt->prior.basisType[0]==DUMMYID||opt->prior.basisType[0]==SVDID;
	I08 hasTrendCmpnt=1;
	I08 hasOutlierCmpnt=opt->prior.basisType[opt->prior.numBasis - 1]==OUTLIERID;
	I32 nprt=0;
	VOID_PTR  trend=NULL,season=NULL,outlier=NULL;
	if (hasTrendCmpnt)   { trend=PROTECT(__MR_Output_AllocMEM_Trend(opt));    nprt++; }
	if (hasSeasonCmpnt)  { season=PROTECT(__MR_Output_AllocMEM_Season(opt));   nprt++; }
	if (hasOutlierCmpnt) { outlier=PROTECT(__MR_Output_AllocMEM_Outlier(opt));   nprt++;}
	int   M=io->numOfPixels;
	int   ROW,COL;
	if (io->ndim==1||io->ndim==2) {
		if (io->ndim==1||(io->ndim==2 && io->out.whichDimIsTime==1)) 			
			ROW=1,COL=M;
		else
			ROW=M,COL=1;
	} else {
		switch (io->meta.whichDimIsTime) {
		case 1:		ROW=io->dims[1],COL=io->dims[2]; break;
		case 2:		ROW=io->dims[0],COL=io->dims[2]; break;
		case 3:		ROW=io->dims[0],COL=io->dims[1]; break;
		}	
	}
	const  int  N=io->N;
	const  int  q=io->q;
	const  int  Nq=N * q;
    FIELD_ITEM  fieldList[ ]={
		{"time",dtype,2,{N,1,},&mat->time},
		{"data",dtype,-1,{-1,-1,},&mat->data,.extra=1}, 
		{"marg_lik",dtype,2,{ROW,COL,},&mat->marg_lik },
		#ifdef __DEBUG__
        {"R2",dtype,2,{(N+7)/8 * 8,300,},&mat->R2},
		{"RMSE",dtype,2,{(N+7)/8 * 8,300,},&mat->RMSE},
	    #else
		{"R2",dtype,2,{ROW,COL,},&mat->R2,.extra=1},
		{"RMSE",dtype,2,{ROW,COL,},&mat->RMSE,.extra=1},
		#endif
	    {"sig2",dtype,4,{ROW,COL,q,q},&mat->sig2},
		{"trend",DATA_STRUCT,0,{0,},(void**)trend},
		{"season",DATA_STRUCT,0,{0,},(void**)season},
		{"outlier",DATA_STRUCT,0,{0,},(void**)outlier},
	};
	I32    nfields=sizeof(fieldList)/sizeof(FIELD_ITEM);
	if (opt->extra.dumpInputData) {
		if (io->ndim==1||io->ndim==2) {
			fieldList[1].ndim=2;
			fieldList[1].dims[0]=N;	fieldList[1].dims[1]=M;
			if (io->ndim==2 && io->out.whichDimIsTime==2) 
				__ChangeFieldsTimeDimFrm1to2_2D(fieldList+1,1L);
		}
		if (io->ndim==3) {
			int   ROW,COL;
			switch (io->meta.whichDimIsTime) {
			case 1:	ROW=io->dims[1],COL=io->dims[2]; break;
			case 2:	ROW=io->dims[0],COL=io->dims[2]; break;
			case 3:	ROW=io->dims[0],COL=io->dims[1]; break;
			}
			fieldList[1].ndim=3;
			fieldList[1].dims[0]=N;	fieldList[1].dims[1]=ROW; fieldList[1].dims[2]=COL;
			if (io->out.whichDimIsTime > 1) 
				__ChangeFieldsTimeDimFrm1to_3D(fieldList+1,1L,io->out.whichDimIsTime);		
		}
	}
	else {
		RemoveField(fieldList,nfields,"data");
		mat->data=NULL;
	}
	I32       nptr1=__MR_ExtendFieldsToMultiVaraiteTS(fieldList,nfields,io->q);  nprt+=nptr1;
	VOID_PTR  out=PROTECT(CreateStructVar(fieldList,nfields));                   nprt++;
	AddStringAttribute(out,"class","beast");
	AddStringAttribute(out,"algorithm","beastv4");
	AddIntegerAttribute(out,"whichOutputDimIsTime",opt->io.out.whichDimIsTime);
	f32_seq(mat->time,io->meta.startTime,io->meta.deltaTime,N);
	if (dtype==DATA_DOUBLE)  f32_to_f64_inplace(mat->time,N);	
	UNPROTECT(nprt);
	return out;
}
#include "abc_000_warning.h"
