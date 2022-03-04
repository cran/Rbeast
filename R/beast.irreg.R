beast.irreg <- function(  
                    y,    
                    time, deltat, freq = NA,                  
                    season=c('harmonic','dummy','none'),
                    scp.minmax=c(0,10), sorder.minmax=c(0,5), sseg.min=NULL,
                    tcp.minmax=c(0,10), torder.minmax=c(0,1), tseg.min=NULL,
                    detrend=FALSE,
                    deseasonalize=FALSE,
                    mcmc.seed=0,  mcmc.burnin=200, mcmc.chains=3, mcmc.thin=5,mcmc.samples=8000,
					ci             = FALSE,
                    print.options=TRUE,
                    print.progress  =TRUE,					
                    gui=FALSE,...)
{

  #time=df$date
  #deltat=1/24
  #freq   = 24
  #season ='harmonic';
  #scp.minmax=c(0,10); sorder.minmax=c(0,5); sseg.min=3  
  #tcp.minmax=c(0,10); torder.minmax=c(0,1); tseg.min=3 
  #detrend = FALSE; deseasonalize=FALSE
  #mcmc.seed=0;  mcmc.bunrin=200; mcmc.chains=3; mcmc.thin=5; mcmc.samples=8000
  #print.options=TRUE
  #print.progress  =TRUE
  #gui=FALSE
  
  if ( !hasArg("y") || is.list(y) )  {
    stop("Something is wrong with the input 'y'. Make sure that y is a vector")
    invisible(return(NULL))
  }  
  if ( is.matrix(y) )  {
    dims=dim(y);
	if (dims[1]>1 && dims[2]>1) {	
		stop("If there are multiple time series to process (e.g., stacked images), pls use the beast123() function. Type ?beast123 for more information!")
		invisible(return(NULL))
	}	
	y=as.vector(y);
  }  
  c=class(y); 
  if ( sum(c=='ts')>0 || sum(c=='zoo')>0 || sum(c=='xts')>0 )  {    
	y=as.vector(y);
  }  
  if (length(y)==1) {
  	stop("Something is wrong with the input 'y'. Make sure that y is a vector")
	invisible(return(NULL))
  }

 
 season=match.arg(season)
 # tmplist=list(...)
#......Start of displaying 'MetaData' ......
   metadata = list()
   metadata$isRegularOrdered = FALSE
   metadata$season           = season   
   metadata$time             = time
   #metadata$startTime       = start
   metadata$deltaTime        = deltat
   if ( season!='none'){
	metadata$period           = deltat*freq;
   }   
   #metadata$whichDimIsTime   = 1
   metadata$deseasonalize     =deseasonalize
   metadata$detrend           =detrend
   metadata$missingValue      = NaN
   metadata$maxMissingRate    = 0.7500
   if ( hasArg('hasOutlier') ) {   
        hasOutlier =list(...)[['hasOutlier']]
		metadata$hasOutlierCmpnt=as.logical(hasOutlier)		           
   }
		  
#......End of displaying MetaData ......
   prior = list()
   prior$modelPriorType	  = 1   
   if (!(season=='none')){
    prior$seasonMinOrder   = sorder.minmax[1]
	  prior$seasonMaxOrder   = sorder.minmax[2]
    prior$seasonMinKnotNum = scp.minmax[1]
    prior$seasonMaxKnotNum = scp.minmax[2]
    if (!is.null(sseg.min) && !is.na(sseg.min))   prior$seasonMinSepDist = sseg.min
   }   
   prior$trendMinOrder	  = torder.minmax[1]
   prior$trendMaxOrder	  = torder.minmax[2]
   prior$trendMinKnotNum  = tcp.minmax[1]
   prior$trendMaxKnotNum  = tcp.minmax[2]
   if (!is.null(tseg.min) && !is.na(tseg.min))   prior$trendMinSepDist = tseg.min
   if ( hasArg('ocp') ) {    
        
		metadata$hasOutlierCmpnt = TRUE	
        prior$outlierMaxKnotNum	 =list(...)[['ocp']] 
   }  
   prior$K_MAX            = 300
   prior$precValue        = 1.500000
   prior$precPriorType    = 'uniform'
#......End of displaying pripr ......

#......Start of displaying 'mcmc' ......
   mcmc = list()
   mcmc$seed            = mcmc.seed
   mcmc$samples         = mcmc.samples
   mcmc$thinningFactor  = mcmc.thin
   mcmc$burnin          = mcmc.burnin
   mcmc$chainNumber     = mcmc.chains
   mcmc$maxMoveStepSize = 0;
   mcmc$trendResamplingOrderProb  = 0.1000
   mcmc$seasonResamplingOrderProb = 0.1700
   mcmc$credIntervalAlphaLevel    = 0.950
#......End of displaying mcmc ......

#......Start of displaying 'extra' ......
   extra = list()
   extra$dumpInputData        = TRUE
   #extra$whichOutputDimIsTime = 1
   extra$computeCredible      = ci
   extra$fastCIComputation    = TRUE
   extra$computeSeasonOrder   = TRUE
   extra$computeTrendOrder    = TRUE
   extra$computeSeasonChngpt  = TRUE
   extra$computeTrendChngpt   = TRUE
   extra$computeSeasonAmp     = TRUE
   extra$computeTrendSlope    = TRUE
   extra$tallyPosNegSeasonJump= TRUE
   extra$tallyPosNegTrendJump = TRUE
   extra$tallyIncDecTrendJump = TRUE
   extra$printProgressBar     = print.progress
   extra$printOptions         = print.options
   extra$consoleWidth         = 0
   #extra$numThreadsPerCPU     = 2
   #extra$numParThreads        = 0
 
  if (gui && !base::interactive()) {
	warning('R is not running in the inteactive mode. Resetting gui to FALSE.');
	gui = FALSE
 }
 
 funstr=ifelse(!gui,"beastv4","beastv4demo")  
  if ( hasArg("cputype") )  {
    cputype = list(...)[['cputype']]  
	cputype = switch(cputype, sse=1, avx2=2, avx512=3);	
	ANS    = .Call( BEASTV4_rexFunction, list(funstr,y,metadata,prior,mcmc,extra,cputype),   212345)   		   
 } else {
	ANS    = .Call( BEASTV4_rexFunction, list(funstr,y,metadata,prior,mcmc,extra),   212345)   		   
 }
 		   
 invisible(return(ANS))
    
}