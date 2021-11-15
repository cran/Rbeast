\name{beast.irreg}
\docType{methods}
\alias{beast.irreg}
\alias{BEAST.irreg} 

\title{Bayesian time series decomposition for changepoint, trend, and periodicity or seasonality}
\description{
   A Bayesian model averaging algorithm called BEAST to decompose time series or 1D sequential data into individual components: Abrupt changes, trends, or periodic/seasonal variations. As a multi-purpose tool, it is useful for nonlinear trend analysis, time series decomposition, time series segmentation, changepoint detection, and detection of breakpoints or structural breaks.
  }
\usage{
  beast.irreg(   
       y, 
       time, deltat,
       freq         = NA,  		   
       season       = c('harmonic','dummy','none'),
       scp.minmax   = c(0,10),  sorder.minmax = c(0,5),  sseg.min = NULL,
       tcp.minmax   = c(0,10),  torder.minmax = c(0,1),  tseg.min = NULL,
       detrend      = FALSE, 
       deseasonalize= FALSE,
       mcmc.seed    = 0,      
       mcmc.burnin   = 200, 
       mcmc.chains  = 3,
       mcmc.thin    = 5,
       mcmc.samples = 8000,
       print.options= TRUE,
       progressbar  = TRUE,
       gui          = FALSE,
       ...
      )
}
\arguments{

  \item{y}{ a vector for an irregular or unordered time series. Missing values such as NA and NaN are allowed.
			\itemize{
				\item  If \code{y} is regular (i.e., evenly-spaced in time), use the \code{\link[Rbeast]{beast}}function instead.
				\item  If \code{y} is a matrix or 3D array consisting of multiple regular or irregular time series (e.g., stacked images), use \code{\link[Rbeast]{beast123}} instead.
            }  
  }

  \item{time}{ a vector of the same length as \code{y}; the time vector of datapoints of \code{y}. It can be a numeric vector or a vector of Dates. If your time is formated as strings or vectors of year, months, and days, use \code{beast123()} instead where more formats are supported.}
  
  \item{deltat}{ numeric; a user-specified time interval to aggregate \code{y} based on \code{time} into a regular time series. The BEAST model is currently formulated for regular data only, so internally, the \code{beast.irreg} function will aggregate/re-bin irregular data into regular ones; for the aggregation, the  \code{deltat} MUST be provided to specify the desired bin size or time interval. The unit of delta needs to be consisent with \code{time}. If \code{time} takes a numeric vector, the unit is arbitrary and irrelevant to beast. If \code{time} takes a vector of Dates, the unit  is assumed to YEAR; for example, if the desired time interval is 1 month (or 1 day), deltat should be 1/12 (or 1/365).}  

  \item{freq}{ numeric. Needed only for data with a periodic/cyclic component (i.e., season=\code{'harmonic'} or \code{'dummy'}) and ignored for trend-only data (i.e., \code{season='none'}). It specifies the number of samples/datapoints per cycle (e.g, a time series of monthly observations with an annual period has a frequency of 12); it may be a decimal real number (e.g., a time series of bi-hourly observations with a period of 37.5 hrs has a freq of 37.5/2=18.75). The period of the cyclic component in the unit of time is \code{period=deltat*freq}. \code{freq} is not a model parameter of BEAST and it has to be specified by the user. If \code{freq} is missing, BEAST first attempts to guess its value via auto-correlation before fitting the model, but in this case, \code{freq} is assumed to be an integer. }
  
  \item{season}{ characters (default to 'harmonic'); speficy if \code{y} has a periodic component or not. Three strings are possible.
				\itemize{
					\item  \code{'none'}: \code{y} is trend-only; no periodic components are present in the time series. The args for the seasonal component (i.e.,sorder.minmax, scp.minmax and sseg.max) will be ignored. 
					
					\item  \code{'harmonic'}: \code{y} has a periodic/seasonal component. The term 'season' is a misnomer, being used here to broadly refer to any periodic variations present in \code{y}. The periodicity is NOT a model parameter estimated by beast but a known constant given by the user through \code{freq}. By default, the periodic component is modeled as a harmonic curve--a combination of sins and cosines.
					
					\item  If \code{'dummy'}, the same as \code{'harmonic'} except that the periodic/seasonal component is modeled as a non-parametric curve. The arg \code{sorder.minmax} is irrelevant and is ignored.
				}  
  
  }


  
  
  \item{sorder.minmax}{ a vector of 2 integers (>=1); the min and max harmonic orders considered to fit the seasonal component. \code{sorder.minmax} is used only used if the time series has a seasonal component (i.e., \code{season='harmonic'}) and ignored for trend-only data or when \code{season='dummy'}. If the min and max orders are equal (\code{sorder.minmax[1]=sorder.minmax[2]}), BEAST assumes a constant harmonic order used and won't infer the posterior probability of harmonic orders.
  }
  
  \item{scp.minmax}{ a vector of 2 integers (>=0); the min and max number of seasonal changepints (scp) allowed in segmenting the seasonal component. \code{scp.minmax} is used only if \code{y} has a seasonal component (i.e., \code{season='harmonic' or 'dummy' }) and ignored for trend-only data. If the min and max changepoint numbers are equal, BEAST assumes a constant number of scp and won't infer the posterior probability of the number of changepoints, but it still estimates the occurance probability of the changepoints over time (i.e., the most likely times at which these changepoints occur). If both the min and max numbers are set to 0, no changepoints are allowed; then a global harmonic model is used to fit the seasonal component, but still, the most likley harmonic order will be inferred if sorder.minmax[1] is not equal to sorder.minmax[2].}
    

  \item{sseg.min}{ an integer; the min segment length allowed between two neighboring season changepoints. That is, when fitting a piecewise harmonic seasonal model, no two changepoints are allowed to occur within a time window of length \code{sseg.min}. \code{sseg.min} must be an unitless integer--the number of time intervals/data points so that the time window in the original unit is \code{sseg.min*deltat}. \code{sseg.min} defaults to NULL and its value will be given a default value in reference to \code{freq}.  }	
	   
  %%%%%%%%%%%%%%%%%%%%%	
  
   \item{torder.minmax}{ a vector of 2 integers (>=0); the min and max orders of the polynomials considered to fit the trend component. The 0-th order corresponds to a constant term/a flat line and the 1st order is a line. If \code{torder.minmax[1]=torder.minmax[2]}, BEAST assumes a constant polynomial order used and won't infer the posterior probability of polynomial orders.  }
  
  \item{tcp.minmax}{ a vector of 2 integers; the min and max number of trend changepints (tcp) allowed in segmenting the trend component. If the min and max changepoint numbers are equal, BEAST assumes a constant number of changepoints and won't infer the posterior probability of the number of changepoints for the trend, but it still estimates the occurance probability of the changepoints over time (i.e., the most likely times at which these changepoints occur in the trend). If both the min and max numbers are set to 0, no changepoints are allowed; then a global polynomial trend is used to fit the trend component, but still, the most likley polynomial order will be inferred if torder.minmax[1] is not equal to torder.minmax[2].}
    
	
  \item{tseg.min}{ an integer; the min segment length allowed between two neighboring trend changepoints. That is, when fitting a piecewise polynomial trend model, no two changepoints are allowed to occur within a time window of length tseg.min. \code{tseg.min} must be an unitless integer--the number of time intervals/data points so that the time window in the original unit is \code{tseg.min*deltat}. \code{tseg.min} defaults to NULL and its value will be given a default value in reference to \code{freq} if the time series has a cyclic component.}	
	   
  %%%%%%%%%%%%%%%%%%%%%	
   
  \item{detrend}{ logical; If \code{TRUE}, a global trend is first fitted and removed from the time series before running BEAST; after BEAST finishes, the global trend is added back to the BEAST result. }
  
  \item{deseasonalize}{ logical; If \code{TRUE}, a global seasonal model is first fitted and removed from the time series before running BEAST; after BEAST finishes, the global seasonal curve is added back to the BEAST result. \code{deseasonalize} is ignored if \code{season='none'} (i.e., trend-only data). }	
  
  \item{mcmc.seed}{ integer (>=0); the seed for the random number generator used for Monte Carlo Markove Chain (mcmc).  If \code{mcmc.seed=0}, an arbitrary seed is picked and the fitting results vary across runs. If fixed to the same non-zero integer, the result can be re-produced for different runs. But the results from the same seed may still vary if run on different computers because the random generator libray depends on CPU's instruction sets.}	 
 
  \item{mcmc.chains}{ integer (>0); the number of  MCMC chains.}
  
  \item{mcmc.thin}{ integer (>0); a factor to thin chains (e.g., if thinningFactor=5, samples will be taken every 3 iterations)}
  
  \item{mcmc.burnin}{ integer (>0); the number of burn-in samples discarded at the start of each chain}
  
  \item{mcmc.samples}{ integer (>=0); the number of samples collected per MCMC chain. The total number of iterations is \code{(burnin+samples*thin)*chains}.}		  

  \item{print.options}{ boolean. If \code{TRUE},the full list of input parameters to BEAST  will be printed out prior to the MCMC inference; the naming for this list (e.g., metadata, prior, and mcmc) differs slightly from the input to \code{beast}, but there is a one-to-one correspondence (e.g., prior$trendMinSepDist=tseg.min). Internaly, beast convers the input parameters to the forms of metadata, prior,and mcmc. Type 'View(beast)' to see the details or check the \code{beast123} function. }	
  
  \item{progressbar}{ boolean; control whether a progressbar will be displayed or not. }	
  
  
  \item{gui}{ boolean. If \code{TRUE}, BEAST will be run in a GUI demostration mode, with a GUI window to show an animation of the MCMC sampling in the model space step by step. Note that "\code{gui=TRUE}" works only for Windows x64 systems not Windows 32 or Linux/Mac systems.}	
 	
  \item{\dots}{additional parameters. There are many more settings for the implemenation but not made available in the beast() interface; please use the function \code{beast123()} instead }
 
}

\value{
  The output is an object of class "beast". It is a list, consisting of the following variables. In the explantions below, we assume the input \code{y} is a single time series of length \code{N}:

  \item{time}{a vector of size \code{1xN}: the times at the \code{N} sampled locatons. By default, it is simply set to \code{1:N}}
  \item{data}{a vector, matrix, or 3D array; this is a copy of the input \code{data} if extra$dumpInputData = TRUE. If extra$dumpInputData=FALSE, it is set to NULL. If the orignal input \code{data} is irregular, the copy here is the regular version aggragted from the original at the time interval specified by metadata$deltaTime. }
  \item{marg_lik}{numeric; the average of the model marginal likhood; the larger marg_lik, the better the fitting for a given time series.}
  \item{R2}{numeric; the R-square of the model fiting.}
  \item{RMSE}{numeric; the RMSE of the model fiting.}
  \item{sig2}{numeric; the estimated variance of the model error.}
  \item{trend}{a list object numeric consisting of various outputs related to the estimated trend component:
  	\itemize{
		 \item \code{ncp}: [Number of ChangePoints]. a numeric scalar; the mean number of trend changepoints.
		 \item \code{ncpPr}: [Probability of the Number of ChangePoints]. A vector of length \code{(tcp.minmax[2]+1)=tcp.max+1}. It gives a probability distribution of having a certain number of trend changepoints over the range of [0,tcp.max]; for example, \code{ncpPr[1]} is the probability of having no trend changepoint; \code{ncpPr[i]} is the probability of having (i-1) changepoints: Note that it is \code{ncpPr[i]} not \code{ncpPr[i-1]} because ncpPr[1] is used for having zero changepoint.
		 \item \code{cpOccPr}: [ChangePoint OCCurence PRobability]. a vector of length N; it gives a probability distribution of having a changepoint in the trend at each point of time. Plotting \code{cpOccPr} will depict a continous curve of probability-of-being-changepoint. Of particular note, in the curve, a higher peak indicates a higher chance of being a changepoint only at that particular SINGLE point in time and does not neccessarily mean a higher chance of observing a changepoint AROUND that time. For example, a window of cpOccPr values \code{c(0,0,0.5,0,0)} (i.e., the peak prob is 0.5 and the summed prob is 0.5) is less likely to be a changepoint compared to another window \code{c(0.1,0.2,0.21,0.2,0.1)} (i.e., the peak prob is 0.21 but the summed prob is 0.71).
		 \item \code{order}: a vector of length N; the average polynomial order needed to approximate the fitted trend. As an average over many sampled individual piece-wise polynomial trends, \code{order} is not neccessarly an integer.
		 \item \code{cp}: [Changepoints] a vector of length \code{tcp.max=tcp.minmax[2]}; the most possible changepoint locations in the trend component. The locations are obtained by first applying a sum-filtering to the \code{cpOccPr} curve with a filter window size of {tseg.min} and then picking up to a total \code{ncp} of the highest peaks in the filterd curve. If \code{ncp<tcp.max}, the remaining of the vector is filled with NaNs.
		 \item \code{cpPr}: [Changepoints PRobability] a vector of length \code{tcp.max=tcp.minmax[2]}; the probabilities associated with the changepoints \code{cp}. Filled with NaNs for the remaning elements if \code{ncp<tcp.max}.
		 \item \code{cpCI}: [Changepoints Credible Interval] a matrix of dimension \code{tcp.max x 2}; the credibable intervals for the detected changepoints \code{cp}.
		 \item \code{cpAbruptChange}: [Abrupt change at Changepoints]  a vector of length \code{tcp.max}; the jumps in the fitted trend curves at the detected changepoints \code{cp}.
		 \item \code{Y}: a vector of length N; the estimated trend component. It is the Bayesian model averaging of all the individual sampled trend.
		 \item \code{SD}: [Standard Deviation] a vector of length N; the estimated standard deviation of the estimated trend component.
		 \item \code{CI}: [Standard Deviation] a matrix of dimension \code{N x 2}; the estimated credible interval of the estimated trend. One vector of the matrix is for the upper envelope and another for the lower envelope.
		 \item \code{slp}: [Slope] a vector of length N;  the time-varying slope of the fitted trend component .
		 \item \code{slpSD}: [Standar Deviation of Slope] a vector of length N;  the SD of the slope for the trend component.
		 \item \code{slpSignPr}: [PRobability of slope having a postive sign] a vector of length N;  the probabity of the slope being positive (i.e., increasing trend) for the trend component. For example, if {slpSignPr=0.80} at a given point in time, it means that 80% of the individual trend models sampled in the MCMC chain has a positive slope at that point.

		 \item \code{pos_ncp}: 
		 \item \code{neg_ncp}:  
		 \item \code{pos_ncpPr}:
		 \item \code{neg_ncpPr}:
		 \item \code{pos_cpOccPr}:
		 \item \code{neg_cpOccPr}:
		 \item \code{pos_cp}:
		 \item \code{neg_cp}:
		 \item \code{pos_cpPr}:
		 \item \code{neg_cpPr}:
		 \item \code{pos_cpAbruptChange}:
		 \item \code{neg_cpAbruptChange}:
		 \item \code{pos_cpCI}:
		 \item \code{neg_cpCI}:  The above variables have the same outputs as those variables without the prefix 'pos' and 'neg', except that we differentiate the changepoints with a POStive jump in the trend from those changepoints with a NEGative jump. For example, \code{pos_ncp} refers to the average number of trend changepoints that jump up (i.e., postively) in the trend.
		 \item \code{inc_ncp}: 
		 \item \code{dec_ncp}:  
		 \item \code{inc_ncpPr}:
		 \item \code{dec_ncpPr}:
		 \item \code{inc_cpOccPr}:
		 \item \code{dec_cpOccPr}:
		 \item \code{inc_cp}:
		 \item \code{dec_cp}:
		 \item \code{inc_cpPr}:
		 \item \code{dec_cpPr}:
		 \item \code{inc_cpAbruptChange}:
		 \item \code{dec_cpAbruptChange}:
		 \item \code{inc_cpCI}:
		 \item \code{dec_cpCI}:  The above variables have the same outputs as those variables without the prefix 'inc' and 'dec', except that we differentiate the changepoints at which the trend slope increases from those changepoints at which the trend slope decreaes. For example, if the trend slopes before and after a chngpt is 0.4 and 2.5, then the changepont is counted toward \code{inc_ncp}.

 
	} %\itemize
		
  }%\item{trend}
 
 

 \item{season}{a list object numeric consisting of various outputs related to the estimated seasonal/periodic component:
  	\itemize{
		 \item \code{ncp}: [Number of ChangePoints]. a numeric scalar; the mean number of seasonal changepoints.
		 \item \code{ncpPr}: [Probability of the Number of ChangePoints]. A vector of length \code{(scp.minmax[2]+1)=scp.max+1}. It gives a probability distribution of having a certain number of seasonal changepoints over the range of [0,scp.max]; for example, \code{ncpPr[1]} is the probability of having no seasonal changepoint; \code{ncpPr[i]} is the probability of having (i-1) changepoints: Note that  the index is i rather than (i-1) because ncpPr[1] is used for having zero changepoint.
		 \item \code{cpOccPr}: [ChangePoint OCCurence PRobability]. a vector of length N; it gives a probability distribution of having a changepoint in the seasonal component at each point of time. Plotting \code{cpOccPr} will depict a continous curve of probability-of-being-changepoint over the time. Of particular note, in the curve, a higher value at a peak indicates a higher chance of being a changepoint only at that particular SINGLE point in time, and does not neccessarily mean a higher chance of observing a changepoint AROUND that time. For example, a window of cpOccPr values \code{c(0,0,0.5,0,0)} (i.e., the peak prob is 0.5 and the summed prob is 0.5) is less likely to be a changepoint compared to another window values \code{c(0.1,0.2,0.3,0.2,0.1)} (i.e., the peak prob is 0.3 but the summed prob is 0.8).
		 \item \code{order}: a vector of length N; the average harmonic order needed to approximate the seasonal component. As an average over many sampled individual piece-wise harmonic curves, \code{order} is not neccessarly an integer.
		 \item \code{cp}: [Changepoints] a vector of length \code{scp.max=scp.minmax[2]}; the most possible  changepoint locations in the seasonal component. The locations are obtained by first applying a sum-filtering to the \code{cpOccPr} curve with a filter window size of {sseg.min} and then picking up to a total \code{ncp} of the highest peaks in the filterd curve. If \code{ncp<scp.max}, the remaining of the vector is filled with NaNs.
		 \item \code{cpPr}: [Changepoints PRobability] a vector of length \code{scp.max}; the probabilities associated with the changepoints \code{cp}. Filled with NaNs for the remaning elements if \code{ncp<scp.max}.
		 \item \code{cpCI}: [Changepoints Credible Interval] a matrix of dimension \code{scp.max x 2}; the credibable intervals for the detected changepoints \code{cp}.
		 \item \code{cpAbruptChange}: [Abrupt change at Changepoints]  a vector of length \code{scp.max}; the jumps in the fitted trend curves at the detected changepoints \code{cp}.
		 \item \code{Y}: a vector of length N; the estimated trend component. It is the Bayesian model averaging of all the individual sampled trend.
		 \item \code{SD}: [Standard Deviation] a vector of length N; the estimated standard deviation of the estimated trend component.
		 \item \code{CI}: [Standard Deviation] a matrix of dimension \code{N x 2}; the estimated credible interval of the estimated trend. One vector of the matrix is for the upper envelope and another for the lower envelope.
		 \item \code{amp}: [AMPlitude] a vector of length N;  the time-varying amplitude of the estimated seasonality.
		 \item \code{ampSD}: [Standar Deviation of AMPlitude] a vector of length N; , the SD of the amplitude of the seasonality.
 

		 \item \code{pos_ncp}: 
		 \item \code{neg_ncp}:  
		 \item \code{pos_ncpPr}:
		 \item \code{neg_ncpPr}:
		 \item \code{pos_cpOccPr}:
		 \item \code{neg_cpOccPr}:
		 \item \code{pos_cp}:
		 \item \code{neg_cp}:
		 \item \code{pos_cpPr}:
		 \item \code{neg_cpPr}:
		 \item \code{pos_cpAbruptChange}:
		 \item \code{neg_cpAbruptChange}:
		 \item \code{pos_cpCI}:
		 \item \code{neg_cpCI}:  The above variables have the same outputs as those variables without the prefix 'pos' and 'neg', except that we differentiate the changepoints with a POStive jump in the trend from those changepoints with a NEGative jump. For example, \code{pos_ncp} refers to the average number of trend changepoints that jump up (i.e., postively) in the trend.
 

 
	} %\itemize
		
  }%\item{trend}  
 

} %value
 
\seealso{
  \code{\link[Rbeast]{beast}},  \code{\link[Rbeast]{beast123}}, \code{\link[Rbeast]{minesweeper}} ,\code{\link[Rbeast]{beast}},  \code{\link[Rbeast]{beast123}}, \code{\link[Rbeast]{geeLandsat}}  
}

 
\examples{

library(Rbeast)

\donttest{


######################################################################################
# 'ohio' is a data.frame on an irregular Landsat time series of reflectances
# ndvi (e.g., surface greenness) at an Ohio site. It has multiple columns
# of alternative time/date formats, such as year, month, day, doy (date of year),
# rdate (R's date class), and time (fractional year)

 data(ohio)
 str(ohio)

######################################################################################
# beast.irreg accepts irregular time series and aggregates them into regular ones prior 
# to model fitting. For the aggregation, both "time" and "deltat" are needed to specify
# indvidial times of data points and the the regular time interval desired. If there is 
# a cyclic componet, freq in the unit of time intervals/datapoints should also be given;
# if not, a possible value is guessed via auto-correlation


######################################################################################
# Below, 'time' is given as numeric values, which can be of any arbitray unit. Although
# here 1/12 can be interepreted as 1/12 year or 1 month, beast itself doesn't care about
# the time unit. So, the unit of 1/12 is irrelevant for BEAST. 'freq' is missing and a
# guess of it is used.

 o=beast.irreg(ohio$ndvi, time=ohio$time,deltat=1/12) 
 plot(o)
 print(o)

######################################################################################
# Explictly provide 'freq'

 o=beast.irreg(ohio$ndvi, time=ohio$time,deltat=1/12, freq=12) 

######################################################################################
# Aggregate the time series at a half-monthly time interval 

 o=beast.irreg(ohio$ndvi, time=ohio$time,deltat=1/24, freq=24) 

######################################################################################
# 'time' is given as R's dates. The unit is YEAR. 1/12 refers to 1/12 year or 1 month

 o=beast.irreg(ohio$ndvi, time=ohio$rdate,deltat=1/12) 

######################################################################################
# If the input time has formats (e.g., date strings) other than the above two, use the 
# beast123() function instead.

}

}



\references{
\enumerate{
\item Zhao, K., Wulder, M.A., Hu, T., Bright, R., Wu, Q., Qin, H., Li, Y., Toman, E., Mallick, B., Zhang, X. and Brown, M., 2019. Detecting change-point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics: A Bayesian ensemble algorithm. Remote Sensing of Environment, 232, p.111181 (the beast algorithm paper).
\item Zhao, K., Valle, D., Popescu, S., Zhang, X. and Mallick, B., 2013. Hyperspectral remote sensing of plant biochemistry using Bayesian model averaging with variable and band selection. Remote Sensing of Environment, 132, pp.102-119 (the Bayesian MCMC scheme used in beast). 
\item Hu, T., Toman, E.M., Chen, G., Shao, G., Zhou, Y., Li, Y., Zhao, K. and Feng, Y., 2021. Mapping fine-scale human disturbances in a working landscape with Landsat time series on Google Earth Engine. ISPRS Journal of Photogrammetry and Remote Sensing, 176, pp.250-261(a beast application paper). 
}

}

\keyword{misc}
