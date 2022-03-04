get.names  = function(o){  
  TAG    = list()
  TAGSTR = c()
    
  i      = 1
  NAMES  = names(o)
  for (name in NAMES) {
    e = o[[name]]
    if (is.list(e)){
      subNAMES = names(e)
      for (subname in subNAMES){
        #e1        = e[[subname]]     
        TAG[[i]]   = c(name,subname)
        TAGSTR[i]  = paste(name,subname,sep='$')   
        i  = i+1		
      }
    } else {      
      TAG[[i]]  = name
      TAGSTR[i] = name  
      i  = i+1	  
    }	
	
  }
  
  len = nchar(TAGSTR)
  idx = which(len==max(len))[[1]]
  x   = list(tag=TAG, tagstr=TAGSTR, maxstr=TAGSTR[idx])  
  invisible(x)
}

tsextract = function(x, index=1){
 
  if ( sum(class(x) == 'beast') == 0 ){
    stop("The input x must has a class type of 'beast' (i.e., an output from the BEAST function).")
  }
  if ( !is.null( attributes(x)$tsextract ) ) {
		return(invisible(x))
  }  
  
  hasSeason   =!is.null(x$season)
  hasOutlier  =!is.null(x$outlier)
  hasHarmonic =!is.null(x$season$order)
  hasData     =!is.null(x$data)
  dimT        = attributes(x)$whichOutputDimIsTime
  
  tag   = get.names(x)
  tag   = tag$tag
  
  dimx  = dim(x$R2)
  n       = dimx[1]
  m       = dimx[2]
  nts     = n*m
  is3D    = (m>1) && (n>1)
  nPixels = nts
  
  o      = list( )
  o$time = x$time
  
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
	  if(length(tags) == 1)	 tmp = x[[ tags ]]
	  else             	     tmp = x[[ tags[1] ]] [[tags[2] ]]
	  
	  dims = dim(tmp)
      ndim = length(dims)
      if ( ndim==2)  {
         tmp = tmp[row,col]
      } else if ( ndim==3)    {
  		if (dimT==1) tmp = tmp[,row,col]
  		if (dimT==2) tmp = tmp[row,,col]
  		if (dimT==3) tmp = tmp[row,col,]
	  }	else if (ndim==4)   { 
  		if (dimT==1) tmp = tmp[,,row,col]
  		if (dimT==2) tmp = tmp[row,,,col]
  		if (dimT==3) tmp = tmp[row,col,,]	
	  }	   
	 
	 
 	  if(length(tags) == 1)	{	  
	    if (is.null(tmp))   o[  tags ]  = list(NULL)
        else                o[[ tags ]] = tmp 	  
	  }  else  {          	    
        if (is.null(tmp))   o[[ tags[1] ]][ tags[2] ]   = list(NULL)
        else                o[[ tags[1] ]][[ tags[2] ]] = tmp
	  }       
      
    }#for (i in 2:length(tag))
    
    
  } 
  else {
    
    
    for (i in 2:length(tag)){	
	
        tags = tag[[i]]  
	    if(length(tags) == 1)	 tmp  = x[[ tags ]]
	    else             	     tmp  = x[[ tags[1] ]] [[tags[2] ]]
       
	    dims = dim(tmp)
        ndim = length(dims)	
	    if ( ndim==2 )    {
	      if ( sum(abs(dims-dimx)) ==0 ){
	        tmp = tmp[index]
	      } else {
	        if (dimT==1) tmp = tmp[,index]
	        if (dimT==2) tmp = tmp[index,]  
	      }
	    } else if (ndim==3)   { 
  			if (dimT==1) tmp = tmp[,,index]
  			if (dimT==2) tmp = tmp[index,,] # this should never occur for a single ts ouput
	    }	   

	    if(length(tags) == 1)	{	  
	       if (is.null(tmp))   o[  tags ]     = list(NULL)
           else                o[[ tags[1] ]] = tmp 	  
	    }  else  {          	    
           if (is.null(tmp))   o[[ tags[1] ]][ tags[2] ]   = list(NULL)
           else                o[[ tags[1] ]][[ tags[2] ]] = tmp
  	    }  
 
    }# for (i in 2:length(tag))
    
  }
  
  attributes(o)$tsextract = TRUE  
  class(o)                = 'beast'
  invisible(o)
}
 
 "[.beast" = function(x, index1=1, index2=NULL, ...){
        if (is.character(index1))
			return(x[index1])
		else {
		  if (is.null(index2)) 
		    return(tsextract(x,index1))
		  else
		    return( tsextract( x, c(index1,index2 ) ) )		
		}
 }