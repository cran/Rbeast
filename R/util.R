get.names  = function(o){
  
  TAG  = list()
  LEN  = c()
  TAGs = c()
  
  NAMES = names(o)
  i=0
  for (name in NAMES){
    e=o[[name]]
    if (is.list(e)){
      NAMES1 = names(e)
      for (name1 in NAMES1){
        e1=e[[name1]]
        i=i+1
        TAG[[i]]= c(name,name1)
        TAGs[i] = paste(name,name1,sep='$')
        LEN[i]  = nchar(TAGs[i])
      }
    } else {
      i=i+1
      TAG[[i]]= name
      TAGs[i] = name
      LEN[i]  = nchar(name)
    }
  }
  
  idx = which(LEN==max(LEN))[[1]]

  x     =list()
  x$tag =TAG
  x$tags=TAGs
  x$maxstr=TAGs[idx]
  invisible(x)
}

tsextract = function(x, index=1){
  
  if (class(x)!='beast'){
    stop("The input x must has a class type of 'beast' (i.e., an output from the BEAST function).")
  }
  if ( !is.null( attributes(x)$tsextract ) ) {
		return(x)
  }  
  
  hasSeason   =!is.null(x$season)
  hasOutlier  =!is.null(x$outlier)
  hasHarmonic =!is.null(x$season$order)
  hasData     =!is.null(x$data)
  dimT        = attributes(x)$whichOutputDimIsTime
  
  tag         = get.names(x)
  
  xdim  = dim(x$R2)
  n     = xdim[1]
  m     = xdim[2]
  nts   = n*m
  is3D  = (m>1) && (n>1)
  nPixels = nts
  
  o = list( time=x$time )
  
  tag=tag$tag
  
  if (is3D){
  
    if (length(index)==1){
      
    	  if (index > nPixels){
    		   row=1  
    		   col=1
    		   warning("The input x contains a 3D array of time series and the index should be a vector of two integers, specifying the row and column of the selected pixel.")
    	  }  else {
    	     col = ceiling(index/n)
    		   row = index-(col-1)*n
    	  }      
    } else{
      row=min(index[1],n)
      col=min(index[2],m)
    }
    
   
    for (i in 2:length(tag)){
      tags = tag[[i]]
	  
	    if(length(tags) == 1)	 tmp  = x[[ tags[1] ]]
	    else             	     tmp  =x[[ tags[1] ]] [[tags[2] ]]
	  
	    dims = (dim(tmp))
      ndim = length(dims)
      if (     ndim==2)  {
        tmp = tmp[row,col]
      }
      else if ( ndim==3)    {
  			if (dimT==1) tmp = tmp[,row,col]
  			if (dimT==2) tmp = tmp[row,,col]
  			if (dimT==3) tmp = tmp[row,col,]
	    }	else if (ndim==4)   { 
  			if (dimT==1) tmp = tmp[,,row,col]
  			if (dimT==2) tmp = tmp[row,,,col]
  			if (dimT==3) tmp = tmp[row,col,,]	
	    }	   
	 
	 
 	  if(length(tags) == 1)	{
	  
	     if (is.null(tmp))   o[ tags[1] ]   = list(NULL)
       else                o[[ tags[1] ]] = tmp 
	  
	  }  else  {          	    
        if (is.null(tmp))   o[[ tags[1] ]][ tags[2] ]   = list(NULL)
         else                o[[ tags[1] ]][[ tags[2] ]] = tmp
	  }  
      
      
    }#for (i in 2:length(tag))
    
    
  } 
  else {
    
    
    for (i in 2:length(tag)){
	
      tags=tag[[i]]  

	    if(length(tags) == 1)	 tmp  = x[[ tags[1] ]]
	    else             	     tmp  = x[[ tags[1] ]] [[tags[2] ]]
       
	    dims=(dim(tmp))
      ndim=length(dims)
	
	    if (       ndim==2 )    {
	      if (  sum(abs(dims-xdim)) ==0 ){
	        tmp = tmp[index]
	      } else {
	        if (dimT==1) tmp = tmp[,index]
	        if (dimT==2) tmp = tmp[index,]  
	      }
  			
 
	    }	else if (ndim==3)   { 
  			if (dimT==1) tmp = tmp[,,index]
  			if (dimT==2) tmp = tmp[index,,] # this should never occur for a single ts ouput
	    }	   
	 
	 
	    if(length(tags) == 1)	{	  
	            if (is.null(tmp))   o[ tags[1] ]   = list(NULL)
              else                o[[ tags[1] ]] = tmp 
	  
	     }  else  {          	    
             if (is.null(tmp))   o[[ tags[1] ]][ tags[2] ]   = list(NULL)
             else                o[[ tags[1] ]][[ tags[2] ]] = tmp
       }  
 
    }# for (i in 2:length(tag))
    
  }
  
  attributes(x)$tsextract=TRUE  
  invisible(o)
}
 