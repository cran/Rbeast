beast <- function(  y,                        
				    start  = 1, deltat = 1, 
					season = c('harmonic','dummy', 'svd','none'),
					freq   = NA,                  
					scp.minmax=c(0,10), sorder.minmax=c(0,5), sseg.min=NULL,
					tcp.minmax=c(0,10), torder.minmax=c(0,1), tseg.min=NULL,
					detrend   =FALSE, deseasonalize=FALSE,
					mcmc.seed =0,  mcmc.burnin=200, mcmc.chains=3, mcmc.thin=5,mcmc.samples=8000,
					ci             = TRUE,
					print.options  =TRUE,
					print.progress =TRUE,
 					gui = FALSE,...)
{

  #start  = 1; deltat = 1 
  #season ='harmonic';
  #freq   = NA                  
  #scp.minmax=c(0,10); sorder.minmax=c(0,5); sseg.min=3  
  #tcp.minmax=c(0,10); torder.minmax=c(0,1); tseg.min=3 
  #detrend = FALSE; deseasonalize=FALSE
  #mcmc.seed=0;  mcmc.burin=200; mcmc.chains=3; mcmc.thin=5; mcmc.samples=8000
  #gui=FALSE
  season=match.arg(season)
  
  if ( !hasArg("y") || is.list(y) )  {
    stop("Something is wrong with the input 'y'. Make sure that y is a vector")
    invisible(return(NULL))         
  }  
  if ( is.matrix(y) )  {
    dims=dim(y)
	if (dims[1]>1 && dims[2]>1) {	
		stop("If there are multiple time series to process (e.g., stacked images), pls use the beast123() function. Type ?beast123 for more information!")
		invisible(return(NULL))
	}	
	y=as.vector(y);
  }  
   
  c=class(y); 
  if (  sum(c=='ts')>0 ){
      tsp   =attributes(y)$tsp
	  start =tsp[1]
	  end   =tsp[2]
	  deltat=(end-start)/(length(y)-1)
	  freq=tsp[3]
	  if (  freq==1 && season!='none'){
	   warning("The input is a object of class ts with a frequency of 1 (trend-only data), season='none' is used instead.");
	   season='none'
	  }
	  else if (  freq >1 && season=='none'){
	   s=sprintf("The input is a object of class ts with a frequency of %d (with a periodic component), season='harmonic' is used instead.", freq)
  	   warning(s);
	   season='harmonic'
	  }
  }
  if (sum(c=='zoo')>0 || sum(c=='xts')>0 )  {  
        warning("The input is a object of class 'zoo' or 'xts'. Its time attributes are not ignored, and only the data vector is used.");  
    	y=as.vector(y);
  }  
  if (length(y)==1) {
  	stop("Something is wrong with the input 'y'. Make sure that y is a vector")
	invisible(return(NULL))
  }

#################################################################################
#################################################################################
  syscall = sys.call()
  if( length(syscall)==3 ) {
       call3    = syscall[[3]]
       arg2     = eval(call3,envir = parent.frame())
	   argname2 = names(syscall)[[3]]
	   if( is.null(argname2) ){ argname2=''}
       if (is.numeric(arg2) && length(arg2)==1 && argname2=='') {
          s=sprintf('Switching to the old interface of Rbeast v0.2: beast(Y,freq=%d)\n', arg2);   warning(s);
		  freq=arg2;
          invisible( return( beast.old(y,freq) )  )
       }
     
       if (is.list(arg2)) {
        
          valname2 = deparse(syscall[[3]])
          if (  argname2 == '' && valname2 =='opt' )     {
              s=sprintf('Switching to the old interface of Rbeast v0.2: beast(Y,opt)\n');  warning(s);
			  opt=arg2;
			  invisible( return( beast.old(y,opt) )  )
          } else if (argname2 == 'option' )    {
              s  =sprintf('Switching to the old interace of Rbeast v0.2: beast(Y,option=)\n');  warning(s);
			  opt=arg2;
			  invisible( return( beast.old(y,option=opt) )  )
          }
        } 
        
     }
   # eval(substitute(alist(...)))
   # substitute(alist(...))
   # ...names()
   # ...length()
   # ...1
 #################################################################################
 #################################################################################  

 # tmplist=list(...)
 #......Start of displaying 'MetaData' ......
   metadata = list()
   metadata$isRegularOrdered = TRUE
   metadata$season           = season   
   metadata$startTime        = start
   metadata$deltaTime        = deltat
   if ( season!='none'){
     metadata$period           = deltat*freq;
   }   
   if ( season=='svd' ){
		if (freq<=1.1 || is.na(freq) ) {
	     	stop('When season=svd, freq must be specified and larger than 1.')
		    invisible(return(NULL))
		}
		metadata$svdTerms=svdbasis(y,freq,deseasonalize)
   }
   metadata$deseasonalize     =deseasonalize
   metadata$detrend           =detrend
   #metadata$whichDimIsTime   = 1
   metadata$missingValue     = NaN
   metadata$maxMissingRate   = 0.7500
   
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
   if (!is.null(tseg.min) && !is.na(tseg.min))  prior$trendMinSepDist = tseg.min
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
   mcmc$maxMoveStepSize = 0    # if set to zero, a default value in reference to freq will be used
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
   extra$printProgressBar     = TRUE
   extra$printOptions         = TRUE
   extra$consoleWidth         = 0
   #extra$numThreadsPerCPU     = 2
   #extra$numParThreads        = 0
 
 if (gui && base::interactive()) {
  warning('R is not running in the inteactive mode. Resetting gui to FALSE.');
  gui = FALSE
 }
 
 funstr = ifelse(!gui,"beastv4","beastv4demo") 
 ANS    = .Call( BEASTV4_rexFunction, list(funstr,y,metadata,prior,mcmc,extra),   212345)   		   
 invisible(return(ANS))    
}

meanfilter <- function(x,n=5){filter(x,rep(1,n), sides=2)}

 
 
 
