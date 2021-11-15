printf=function(...){
   s =sprintf(...)
   cat(s)
}
  
print.beast <- function (x,index=1,...) {
  
  call  = match.call()
  oName = as.character(call$x)
 
  #https://stackoverflow.com/questions/50561768/r-get-argument-names-from-function-call
  #argName=formalArgs(deparse(substitute(x))[[1]])
  #https://stackoverflow.com/questions/4959463/getting-the-object-name-for-s3-print-method-failing
  #argName=deparse(substitute(x))
  
  #argName=strtrim(argName)
  if (is.null(x)) {  return( invisible(NULL))  }
  
  xdim  =dim(x$R2)
  n     =xdim[1]
  m     =xdim[2]
  nTS   = n*m
  is3D  = (m>1)
 
  if ( is.null( attributes(x)$tsextract ) ) {
		x=tsextract(x,index)
  }  
   

  if (is3D & length(index)>1)
   printf("\nResult for time series #(%d,%d) (total number of time series in '%s': %d)\n\n", index[1],index[2],oName, nTS)
  else
   printf("\nResult for time series #%d (total number of time series in '%s': %d)\n\n", index, oName, nTS)
 
  
  hasSeason   =!is.null(x$season)
  hasOutlier  =!is.null(x$outlier)
  hasHarmonic =!is.null(x$season$order)
  
  s1    = '                                             ';
  s2    = '*********************************************';
	
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("+                     SEASONAL CHANGEPOINTS                    +\n")
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n\n")
   
  if (!hasSeason) {
    cat(" No seasonal/periodic component present (i.e., season='none')\n")
  } else {
    y     = x$season
    n     = length(y$ncpPr)
    maxPr = max(y$ncpPr)
    maxIx = which(y$ncpPr==maxPr)[1]
    n     = min(n,99)
    printf('An ascii plot of the probability dist for number of chgpts(ncp)\n');
    printf('---------------------------------------------------------------\n');
    for (i in 1:n){
	  p=max(1,ceiling( y$ncpPr[i]/maxPr*(nchar(s1)-1)) )
      s=s1; substr(s,1,p)=s2	  
      printf("Pr(ncp=%-2d)=%.3f|%s|\n",i-1, y$ncpPr[i],s)      
    }
    printf('---------------------------------------------------------------\n')
    printf('Max ncp : %-4d | A parameter you set (e.g., maxSeasonKnotNum) |\n',length(y$ncpPr)-1 );
    printf('Mode ncp: %-4d | Pr(ncp=%2d)=%3.2f; there is a %3.1f%% probability|\n',maxIx-1,min(maxIx-1,99),maxPr,maxPr*100)
    printf('	       | that the seasonal componet has %2d chngept(s).|\n', min(maxIx-1,99) );
    printf('Avg ncp : %-4.2f | Sum[ncp*Pr(ncp)]                             |\n',          y$ncp);
    printf('---------------------------------------------------------------\n');
    cat('\n')
    
    ###################################################################################
    printf("List of most probable seasonal changepnts (avg number of changpts: %.2f) \n", y$ncp)
    printf("--------------------------------.\n")
    printf("scp# |time (cp)      |prob(cpPr)|\n")
    printf("-----|---------------|----------|\n")
    ncp = sum( !is.na (x$season$cp) )
    for (i in 1:ncp){
      printf("%-5d|%-15.4f|%10.5f|\n", i,y$cp[i],y$cpPr[i] )      
    }
    printf("--------------------------------'\n")
      
    
    
  }  
  cat('\n\n')
  
 
  
  ###################################################################################
  ###################################################################################
  ###################################################################################  
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("+                     TREND CHANGEPOINTS                       +\n")
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n\n")

  y=x$trend
  n=length(y$ncpPr)
  maxPr=max(y$ncpPr)
  maxIx=which(y$ncpPr==maxPr)[1]
  n=min(n,99)
  printf('An ascii plot of the probability dist for number of chgpts(ncp)\n');
  printf('---------------------------------------------------------------\n');
  for (i in 1:n){
	  p=max(1,ceiling( y$ncpPr[i]/maxPr*(nchar(s1)-1)) )
      s=s1; substr(s,1,p)=s2	  
      printf("Pr(ncp=%-2d)=%.3f|%s|\n",i-1, y$ncpPr[i],s)   
  }
  printf('---------------------------------------------------------------\n')
  printf('Max ncp : %-4d | A parameter you set (e.g., maxTrendKnotNum)  |\n',length(y$ncpPr)-1 );
  printf('Mode ncp: %-4d | Pr(ncp=%2d)=%3.2f; there is a %3.1f%% probability|\n',maxIx-1,min(maxIx-1,99),maxPr,maxPr*100)
  printf('	       | that the trend componet has %2d chngept(s).   |\n', min(maxIx-1,99) );
  printf('Avg ncp : %-4.2f | Sum[ncp*Pr(ncp)]                             |\n',          y$ncp);
  printf('---------------------------------------------------------------\n');  
  cat('\n')
  
  printf("List of most probable trend changepoints (avg number of changpts: %.2f) \n", y$ncp)
  printf("--------------------------------.\n")
  printf("tcp# |time (cp)      |prob(cpPr)|\n")
  printf("-----|---------------|----------|\n")
  ncp = sum( !is.na (y$cp) )
  for (i in 1:ncp){
   printf("%-5d|%-15.4f|%10.5f|\n", i,y$cp[i], y$cpPr[i] )  
  }
  printf("--------------------------------'\n")   
  cat('\n') 
  
  printf("Note: the beast object '%s' is a LIST. Type 'str(%s)' to see all the elements in it. Or use 'plot(%s)' or 'plot(%s,interactive=TRUE)' to plot the model output.\n",oName,oName,oName,oName)

  
}
 