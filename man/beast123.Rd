\name{beast123}
\docType{methods}
\alias{beast123}
\alias{BEAST123}
\alias{beast.123}
 
\title{Bayesian time series decomposition into changepoints, trend, and seasonal/periodic components}

\description{
BEAST is a Bayesian model averaging algorithm for decomposing time series or other 1D sequential data into individual components, including abrupt changes, trends, and periodic/seasonal variations. It is useful for changepoint detection (e.g., breakpoints or structural breaks), nonlinear trend analysis, time series decomposition, and time series segmentation. \code{beast123} is a low-level interface to BEAST.
}

\usage{
beast123(
  Y,
  metadata = list(),
  prior    = list(),
  mcmc     = list(),
  extra    = list(),
  season   = c("harmonic","svd","dummy","none"),
  method   = c("bayes","bic","aic","aicc","hic",
               "bic0.25","bic0.5","bic1.5","bic2"),
  ...
)
}

\arguments{

  \item{Y}{
   \code{Y} is a numeric 1D vector, 2D matrix, or 3D array. Missing values are allowed and can be indicated by \code{NA}, \code{NaN}, or a customized value specified via the 2nd argument \code{metadata} (e.g., \code{metadata$missingValue=-9999}).
    
    \itemize{
      \item If \code{Y} is a vector of size \code{N x 1} or \code{1 x N}, it is treated as a single time series of length \code{N}.
      
      \item If \code{Y} is a 2D matrix or 3D array of dimension \code{N1 x N2} or \code{N1 x N2 x N3} (e.g., stacked images of geospatial data), it contains multiple time series of equal length. The dimension corresponding to time must be specified in \code{metadata$whichDimIsTime}. For example, \code{metadata$whichDimIsTime = 1} for a \code{190 x 35} matrix indicates 35 time series of length 190; \code{metadata$whichDimIsTime = 2} for a \code{100 x 200 x 300} array indicates \code{100 * 300 = 30000} time series of length 200 each.
    }
    
    \code{Y} can be either regular (evenly spaced in time) or irregular/unordered in time.
    \itemize{
      \item \emph{Regular data:} Individual times are determined from the time of the first data point (\code{startTime}) and the time interval between consecutive points (\code{deltaTime}), specified via \code{metadata$startTime} and \code{metadata$deltaTime}. If not provided, both default to \code{1.0}.
      
      \item \emph{Irregular or unordered data:} The times must be explicitly supplied via \code{metadata$time}. The BEAST model is currently formulated for regular data only, so \code{beast123} internally aggregates/re-bins irregular data into a regular series. For this aggregation, \code{metadata$deltaTime} should also be provided to specify the desired bin size (time interval) of the regularized time series.
    }

    
    \code{Y} may contain both a periodic component and a trend, or only a trend. Use the argument \code{season} to specify the case:
    \itemize{
      \item \code{season = "none"}: \code{Y} is treated as trend-only; no periodic components are assumed.
      
      \item \code{season = "harmonic"}: \code{Y} has a periodic/seasonal component. The term "season" is used broadly here to refer to any periodic variation. The period is not a model parameter estimated by BEAST; instead it must be supplied by the user via \code{metadata$period}, the unit of which should be consistent with that of \code{metadata$deltaTime} (see \code{metadata$period} below). If season = "harmonic", The seasonal component is modeled as a harmonic curve (a combination of sines and cosines).
      
      \item \code{season = "dummy"}: As for \code{"harmonic"}, except that the periodic/seasonal component is modeled as a non-parametric curve.
      
      \item \code{season = "svd"} (experimental): As for \code{"harmonic"}, except that the periodic/seasonal component is modeled as a linear combination of basis functions derived from a singular value decomposition (SVD). The SVD-based basis functions can be more parsimonious than harmonic bases in parameterizing seasonal variations and may facilitate the detection of more subtle seasonal changepoints.
    }
  }



  \item{metadata}{
   \code{metadata} is optional. If present, it can be:
    \itemize{
      \item a scalar specifying the period of \code{Y};
      \item a vector of numbers, character strings, or \code{Date}s specifying the times of \code{Y}; or
      \item more commonly, a \code{list} object specifying various parameters describing the first argument \code{Y}.
    }
    If missing, default values are used. However, when \code{Y} is a 2D matrix or 3D array, \code{metadata} should be supplied explicitly  to avoid misinterpreting which dimension corresponds to time. \code{metadata} is not part of the Bayesian BEAST model itself; it provides additional information needed to correctly interpret \code{Y}.
    
    When \code{metadata} is a list, the following fields are supported (not all are needed in every case; the relevant subset depends on the input type and regularity of the series):
    
    \itemize{
      \item \code{metadata$whichDimIsTime}: Integer (\code{<= 3}). Specifies which dimension of \code{Y} corresponds to time for 2D or 3D inputs. Ignored if \code{Y} is a vector.
      
      \item \code{metadata$isRegularOrdered}: Logical. Obsolete and no longer used in this version. Regularity is now inferred from \code{metadata$time}; if \code{metadata$time} is missing, \code{Y} is assumed to be regular.
      
      \item \code{metadata$time}: A vector of the same length as the time dimension of \code{Y}, or a more complex object providing time information. It can be a vector of numbers, Dates, or date strings; it can also be a list of vectors of year, months, and days. Possible formats include:
	  
      \enumerate{
	  
        \item A vector of numeric values (e.g., \code{c(1984.23, 1984.27, 1984.36, ...)}). The time unit is arbitrary but must be consistent with the units used in \code{metadata$startTime}, \code{metadata$deltaTime}, and \code{metadata$period}.

        \item A vector of \code{Date}s (e.g., \code{as.Date(c("1984-03-27","1984-04-10","1984-05-12", ...))}).
		
        \item A vector of character strings. Examples are:
        \itemize{
          \item \code{c("1984-03-27","1984-04-10","1984-05-12")}
          \item \code{c("1984/03/27","1984,04,10","1984 05 12")} (delimiters can differ as long as the Y-M-D order is consistent)
          \item \code{c("LT4-1984-03-27","LT4-1984-04-10","LT4-1984+05,12")}
          \item \code{c("LT4-1984087ndvi","LT4-1984101ndvi","LT4-1984133ndvi")}
          \item \code{c("1984,,abc 3/ 27","1984,,ddxfdd 4/ 10","ggd1984,, 5/ ttt 12")}
        }
        BEAST uses several heuristics to parse date strings without an explicit format specifier, but may fail when strings are ambiguous (e.g., \code{"LC8-2020-09-20-1984"}, where both 2020 and 1984 look like possible years). To ensure correct parsing, use a list with an explicit date format specifier (see next item).
        
        \item A list object \code{time = list(datestr = ..., strfmt = "...")} consisting of:
        \itemize{
          \item \code{time$datestr}: a character vector of date strings, and
          \item \code{time$strfmt}: a format string that specifies how to parse \code{datestr}.
        }
		
        Three types of formats are supported:
        \itemize{
          \item (a) Fixed pattern of year, month, and day positions. For example, to extract 2001/12/02, 2002/11/08, etc. from
          \code{time$datestr = c("P23R34-2001.1202333xd","O93X94-2002.1108133fd","TP3R34-2009.0122333td")},
          use \code{time$strfmt = "P23R34-yyyy.mmdd333xd"}, where \code{yyyy}, \code{mm}, and \code{dd} denote the year, month, and day. All other positions are treated as wildcards and nd can be filled with any other letters different from \code{yyyy, mm and dd}..
          
          \item (b) Fixed pattern of year and day-of-year (DOY) positions. For example, to extract year and DOY from \code{"P23R342001888045"}, use \code{time$strfmt = "123123yyyy888doy"}, where \code{yyyy} and \code{doy} are specifiers and other characters are wildcards. \code{doy} must be three digits.
          
          \item (c) Fixed pattern of separators between year, month, and day. For example, to get 2002/12/02 from \code{"2002,12/02"}, \code{"  2002 , 12/2"}, or \code{"2002,12  /02"}, use \code{time$strfmt = "Y,M/D"}, where whitespace is ignored. To get 2002/12/02 from \code{"2--12, 2012"}, use \code{time$strfmt = "D--M,Y"}.
        }
        
        \item A list object of separate vectors for dates. Use \code{time$year}, \code{time$month}, and \code{time$day} to specify dates, or alternatively \code{time$year} and \code{time$doy} (day of year, 1-365/366). Each vector must have the same length as the time dimension of \code{Y}.
      } % enumerate		
	  
      \item \code{metadata$startTime}: Numeric (defaults to \code{1.0} if missing). The time of the first data point. It can be given as a scalar (e.g., \code{2021.23}) or as a vector of length 3 in the form of \code{c(year, month, day)} (e.g., \code{c(2021, 1, 24)}). \code{metadata$startTime} is needed for regular data; for irregular data it is optional and, if missing, may be derived from \code{metadata$time}.
      
      \item \code{metadata$deltaTime}: Numeric or character string. The time interval between consecutive data points. For regular data it is optional (default \code{1.0}); for irregular data it should be specified, as it determines the bin size when aggregating irregular times to regular intervals. If \code{metadata$time} is numeric, the unit of \code{deltaTime} is arbitrary but must be consistent with the numeric scale of \code{metadata$time}. If \code{metadata$time} is given as \code{Date}s or date strings, \code{deltaTime} is interpreted as a fraction of a year by default. Alternatively, use a string instead of a number to specify the interval with a unit, such as \code{deltaTime="7 days"}, \code{"7d"}, \code{"1/2 months"}, \code{"1mn"}, \code{"1 year"}, or \code{"1y"}.
      
      \item \code{metadata$period}: Numeric or character string. The period of the periodic/seasonal component in \code{Y}. This is needed only when a seasonal component is present (i.e., \code{season = "harmonic"} or \code{season = "dummy"}) and is ignored for trend-only data (\code{season = "none"}). The period must be in the same time unit as \code{deltaTime} and should satisfy \code{period = deltaTime * freq}, where \code{freq} is the number of data points per period. (The older argument \code{freq} is now obsolete and replaced by \code{period}.) The period is not a BEAST model parameter and must be provided by the user. But if \code{period} is missing, BEAST first attempts to infer a plausible period via autocorrelation before fitting. If \code{period <= 0}, no seasonal component is assumed (equivalent to \code{season = "none"}). As with \code{deltaTime}, character strings such as \code{period="1 year"}, \code{"12 months"}, \code{"365d"}, or \code{"366 days"} can be used.
      
      \item \code{metadata$missingValue}: Numeric; a customized value indicating bad or missing values in addition to \code{NA} or \code{NaN}.
      
      \item \code{metadata$maxMissingRateAllowed}: Numeric in \code{[0, 1]}; the maximum proportion of missing values allowed. Time series with a missing rate higher than this threshold are skipped and not fitted by BEAST.
      
      \item \code{metadata$hasOutlier}: Logical; if \code{TRUE}, fit a model with an outlier component representing potential spikes or dips at isolated data points: \code{Y = trend + outlier + error} if \code{season = "none"}, and \code{Y = trend + season + outlier + error} otherwise.
      
      \item \code{metadata$deseasonalize}: Logical; if \code{TRUE}, remove a global seasonal component before fitting BEAST (see examples).
      
      \item \code{metadata$detrend}: Logical; if \code{TRUE}, remove a global trend before fitting BEAST.
    } % itemize
  }

  \item{prior}{
   \code{prior} (optional) is a \code{list} of hyperparameters defining the priors in the Bayesian BEAST model. Because these are part of the model specification, the BEAST results may be sensitive to their values. If \code{prior} is missing, default values are used and printed to the console at the start of the run. The following fields are supported:
    \itemize{
      \item \code{prior$seasonMinOrder}: Integer (\code{>= 1}).
      \item \code{prior$seasonMaxOrder}: Integer (\code{>= 1}). The minimum and maximum harmonic orders considered for the seasonal component. These are used only when the series has a seasonal component (i.e., \code{season = "harmonic" or "svd"}) and ignored for trend-only data or when \code{season}=\code{'none'} or \code{'dummy'}. If \code{seasonMinOrder == seasonMaxOrder}, the harmonic order is fixed and BEAST does not infer the posterior distribution of harmonic orders.
      
      \item \code{prior$seasonMinKnotNum}: Integer (\code{>= 0}).
      \item \code{prior$seasonMaxKnotNum}: Integer (\code{>= 0}). The minimum and maximum numbers of seasonal changepoints allowed in segementing the seasonal component. Used only when the data have a a seasonal component (e.g.,\code{season = "harmonic"},  \code{"svd"}  or \code{"dummy"} and ignored for trend-only data. If \code{seasonMinKnotNum == seasonMaxKnotNum}, the number of seasonal changepoints is fixed but their possible locations and occurrence probabilities over time are still estimated. If \code{seasonMinKnotNum == seasonMaxKnotNum == 0}, no seasonal changepoints are allowed and a single global harmonic model is used with no changepoints.
      
      \item \code{prior$seasonMinSepDist}: Integer (\code{> 0}). The minimum separation, in number of time steps, between neighboring seasonal changepoints. That is, when fitting a piecewise seasonal model, no two changepoints may be closer than \code{seasonMinSepDist} time intervals. The corresponding time window in original time units is \code{seasonMinSepDist * metadata$deltaTime}.
      
      \item \code{prior$seasonLeftMargin}: Integer (\code{>= 0}). The number of leftmost observations excluded from seasonal changepoint detection. No seasonal changepoints are allowed in the initial window of length \code{seasonLeftMargin}. It is specified in units of time steps and thus corresponds to a time window of \code{seasonLeftMargin * metadata$deltaTime}. If missing, it defaults to \code{seasonMinSepDist}.
      
      \item \code{prior$seasonRightMargin}: Integer (\code{>= 0}). The number of rightmost observations excluded from seasonal changepoint detection. Similarly, no seasonal changepoints are allowed in the final window of length \code{seasonRightMargin}. Specified in time steps and corresponds to \code{seasonRightMargin * metadata$deltaTime} in the time unit. If missing, it defaults to \code{seasonMinSepDist}.
      
      \item \code{prior$seasonComplexityFactor}: Numeric (default \code{0.0}). A hyperprior parameter--newly added in Version 1.02-- controlling the complexity of the seasonal curve (i.e., the number of seasonal changepoints). A prior of the form \eqn{ p(k) \propto \exp[\lambda (k+1)]} is placed on the number of seasonal changepoints \eqn{k}, where \eqn{\lambda} is \code{seasonComplexityFactor}. Setting \eqn{\lambda = 0} yields a non-informative prior \eqn{ p(k) \propto 1.0}  where all model dimensions are equally likely \emph{a priori}. Users may tune \code{seasonComplexityFactor} in the range \code{[-20, 20]} or an even wider range: negative values (e.g., \eqn{\lambda = -15.9}) favor fewer changepoints (simpler seasonal curves), whereas positive values (e.g., \eqn{\lambda = 5.76}) favor more changepoints (more complex curves).
      
      \item \code{prior$trendMinOrder}: Integer (\code{>= 0}).
      \item \code{prior$trendMaxOrder}: Integer (\code{>= 0}). The minimum and maximum polynomial orders considered to fit the trend component. Order 0 corresponds to a constant (flat) trend; order 1 to a linear trend. If \code{trendMinOrder == trendMaxOrder}, the polynomial order is fixed and BEAST does not infer the posterior distrubtion of polynomial orders.
      
      \item \code{prior$trendMinKnotNum}: Integer (\code{>= 0}).
      \item \code{prior$trendMaxKnotNum}: Integer (\code{>= 0}). The minimum and maximum numbers of trend changepoints allowed. If \code{trendMinKnotNum == trendMaxKnotNum}, the number of trend changepoints is fixed but ther locations and occurrence probabilities over time are still estimated. If \code{trendMinKnotNum == trendMaxKnotNum == 0}, no trend changepoints are allowed and a single global polynomial trend is fitted.
      
      \item \code{prior$trendMinSepDist}: Integer (\code{> 0}).  The minimum separation, in number of time steps, between neighboring trend changepoints. That is, when fitting a piecewise tremd model, no two changepoints may be closer than \code{trendMinSepDist} time intervals. The corresponding time window in original time units is \code{trendMinSepDist * metadata$deltaTime}.
      
      \item \code{prior$trendLeftMargin}: Integer (\code{>= 0}). The number of leftmost observations excluded from trend changepoint detection. No trend changepoints are allowed in the initial window of length \code{trendLeftMargin}. Specified in time steps; the corresponding time window is \code{trendLeftMargin * metadata$deltaTime}. If missing, defaults to \code{trendMinSepDist}.
      
      \item \code{prior$trendRightMargin}: Integer (\code{>= 0}). The number of rightmost observations excluded from trend changepoint detection. No trend changepoints are allowed in the final window of length \code{trendRightMargin}. Specified in time steps; the corresponding time window is \code{trendRightMargin * metadata$deltaTime}. If missing, defaults to \code{trendMinSepDist}.
      
      \item \code{prior$trendComplexityFactor}: Numeric (default \code{0.0}). Analogous to \code{seasonComplexityFactor}, but for the trend component. A prior of the form \eqn{ p(k) \propto \exp[\lambda (k+1)]} is placed on the number of trend changepoints \eqn{k}, where \eqn{\lambda} is \code{trendComplexityFactor}. Setting \eqn{\lambda = 0} yields a non-informative prior \eqn{ p(k) \propto 1.0}  where all model dimensions are equally likely \emph{a priori}. Users may tune \code{seasonComplexityFactor} in the range \code{[-20, 20]} or an even wider range: negative values (e.g., \eqn{\lambda = -15.9}) favor fewer changepoints (simpler trend curves), whereas positive values (e.g., \eqn{\lambda = 5.76}) favor more changepoints (more complex curves).
	  
      \item \code{prior$precValue}: Numeric (\code{> 0}); default \code{10}. Useful only if \code{prior$precPriorType = "constant"}.
      
      \item \code{prior$precPriorType}: Character string, one of \code{"constant"}, \code{"uniform"} (default), \code{"componentwise"}, or \code{"orderwise"}. These options control how precision parameters (for model coefficients) are treated:
      \enumerate{
        \item \code{"constant"}: A single precision parameter is fixed at \code{prior$precValue}. The results may be sensitive to the choice of \code{prior$precValue}.
        \item \code{"uniform"}: A single precision parameter is treated as a random variable, with initial value \code{prior$precValue}. Its posterior is inferred via MCMC, making the results less sensitive to the initial choice of \code{prior$precValue}.
		
        \item \code{"componentwise"}: Separate precision parameters are used for different components (e.g., season vs. trend), starting from \code{prior$precValue}. Each precision parameter is inferred via MCMC.
		
        \item \code{"orderwise"}: Separate precision parameters are used for different components and different orders within components, starting from \code{prior$precValue}. All the precision parameters are inferred via MCMC.
      } % enumerate
      
      \item \code{prior$outlierMinKnotNum}: Integer (\code{>= 0}).
      \item \code{prior$outlierMaxKnotNum}: Integer (\code{>= 0}). These are used only if \code{metadata$hasOutlier = TRUE} to specify the minimum and maximum numbers of outlier-type changepoints (spikes or dips) allowed in the series.
    } % itemize
  }

  \item{mcmc}{
    \code{mcmc} (optional) is a \code{list} object of parameters configuring the reversible-jump MCMC procedure. These settings are not part of the Bayesian model itself but control the sampling process and chain length. In general, longer chains yield more stable estimates. Supported fields are:
    \itemize{
      \item \code{mcmc$seed}: Integer (\code{>= 0}); the seed for the random number generator. If \code{mcmc$seed = 0}, an arbitrary seed is used and results can vary slightly across runs. Using the same non-zero seed makes results reproducible on the same machine, but differences across platforms/CPUs may still occur from the same seed due to differences in random number libraries or CPU instruction sets.

      \item \code{mcmc$chainNumber}: Integer (\code{> 0}); the number of parallel MCMC chains.
      
      \item \code{mcmc$samples}: Integer (\code{> 0}); the number of samples collected per MCMC chain.
            
      \item \code{mcmc$thinningFactor}: Integer (\code{> 0}); thinning factor. If \code{thinningFactor = k}, every \code{k}-th sample is retained and the others discarded.
      
      \item \code{mcmc$burnin}: Integer (\code{> 0}); the number of initial samples discarded as burn-in per chain.
      
      \item \code{mcmc$maxMoveStepSize}: Integer (\code{> 0}). In the RJMCMC sampler, a "move" proposal shifts changepoint locations. \code{maxMoveStepSize} specifies the maximum step size (in time steps) allowed in such moves.
      
      \item \code{mcmc$seasonResamplingOrderProb}: Numeric in \code{(0, 1)}; probability of proposing a resampling of the seasonal harmonic order.
      
      \item \code{mcmc$trendResamplingOrderProb}: Numeric in \code{(0, 1)}; probability of proposing a resampling of the trend polynomial order.
      
      \item \code{mcmc$credIntervalAlphaLevel}: Numeric in \code{(0, 1)} (default \code{0.95}); the confidence level used to compute credible intervals (e.g., \code{0.95} for 95\% credible intervals).
    }
  }

  \item{extra}{
    \code{extra} (optional) is a \code{list} of flags and options controlling output content, console messages, and parallel computation. Supported fields include:
    \itemize{
      \item \code{extra$dumpInputData}: Logical (default \code{FALSE}). If \code{TRUE}, copies of the input time series are included in the output. If the input \code{Y} is irregular the dumped copy is the aggregated regular series.
      
      \item \code{extra$whichOutputDimIsTime}: Integer (\code{<= 3}). For 2D or 3D inputs (multiple time series, e.g., stacked images), this specifies which output dimension corresponds to time. Defaults to \code{3} for 3D inputs. Ignored if \code{Y} is a single time series.
      
      \item \code{extra$ncpStatMethod}: Character string (deprecated). Formerly used to specify the statistic used to determine the number of changepoints (ncp) when computing the most likely changepoint locations (e.g., \code{out$trend$cp} and \code{out$season$cp}). Possible values were \code{"mode"}, \code{"mean"}, and \code{"median"} (default \code{"mode"}). Individual MCMC-sampled models have a varying number of changepoints. For example, if mcmc$samples=10 and assuming that the numbers of changepoints for the 10 sampled models are [0, 2, 4, 1, 1, 2, 7, 6, 6, 1], the mean ncp is 3.1 (rounded to 3), the median is 2.5 (2), and the mode is 1. \code{ncpStatMethod} is now deprecated: BEAST outputs all possible changepoints together with several summaries of ncp, including \code{ncp}, \code{ncp_median}, \code{ncp_mode}, and \code{ncp_pct90}. The choice of ncp for plotting is now controlled by the \code{ncpStat} argument in \code{\link[Rbeast]{plot.beast}}.
      
      \item \code{extra$computeCredible}: Logical (default \code{TRUE}). If \code{TRUE}, credible intervals are computed and returned.
      
      \item \code{extra$fastCIComputation}: Logical (default \code{TRUE}). If \code{TRUE}, a faster approximate method is used to compute credible intervals. CI computation can be one of the most time-consuming steps, so \code{fastCIComputation = TRUE} is recommended unless more precise CI estimates are desired.
      
      \item \code{extra$computeSeasonOrder}: Logical (default \code{TRUE}). If \code{TRUE}, a posterior estimate of the seasonal harmonic order is returned. Only meaningful when \code{season = "harmonic" or "svd"} and \code{prior$seasonMinOrder != prior$seasonMaxOrder}.
      
      \item \code{extra$computeTrendOrder}: Logical (default \code{TRUE}). If \code{TRUE}, a posterior estimate of the trend polynomial order is returned. Only meaningful when \code{prior$trendMinOrder != prior$trendMaxOrder}.
      
      \item \code{extra$computeSeasonChngpt}: Logical (default \code{TRUE}). If \code{TRUE}, compute the most likely times/positions at which changepoints occur in the seasonal component. This is relevant only when a seasonal component is present (i.e., \code{season = "harmonic"}, \code{"svd"}, or \code{"dummy"}) and \code{prior$seasonMaxKnotNum > 0}.
      
      \item \code{extra$computeTrendChngpt}: Logical (default \code{TRUE}). If \code{TRUE}, compute the most likely changepoint locations in the trend component. This is relevant only when \code{prior$trendMaxKnotNum > 0}.
      
      \item \code{extra$computeSeasonAmp}: Logical (default \code{FALSE}). If \code{TRUE}, output the time-varying amplitude of the seasonal component.
      
      \item \code{extra$computeTrendSlope}: Logical (default \code{FALSE}). If \code{TRUE}, output the time-varying slope of the trend component.
      
      \item \code{extra$tallyPosNegSeasonJump}: Logical (default \code{FALSE}). If \code{TRUE}, classify seasonal changepoints by the sign of the jump (positive vs. negative) and output separate summaries for each group.
      
      \item \code{extra$tallyPosNegTrendJump}: Logical (default \code{FALSE}). If \code{TRUE}, classify trend changepoints by whether the trend level jumps up or down at the changepoint and output separate summaries.
      
      \item \code{extra$tallyIncDecTrendJump}: Logical (default \code{FALSE}). If \code{TRUE}, classify trend changepoints by changes in slope (increasing vs. decreasing) and output separate summaries for those changepoints with increased slopes and those with decreased slopes.
      
      \item \code{extra$printProgress}: Logical (default \code{FALSE}). If \code{TRUE}, a progress bar is displayed showing the progress of the run. For multiple time series (e.g., stacked images), the progress bar also reports an estimate of remaining time.
	  
      \item \code{extra$consoleWidth}: Integer (default \code{0}). The width (in characters) of each status line printed when \code{extra$printProgress = TRUE}. If \code{0}, the width of the current console is used.
      
      \item \code{extra$printParameter}: Logical (default \code{TRUE}). If \code{TRUE}, the values used in \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra} are printed at the start of the run.
   
      
      \item \code{extra$printWarning}: Logical (default \code{TRUE}). If \code{TRUE}, warning messages are printed.
      
      \item \code{extra$quiet}: Logical. If \code{TRUE}, suppress most console output.
      
      \item \code{extra$dumpMCMCSamples}: Logical. If \code{TRUE}, dump individual MCMC samples.
      
      \item \code{extra$numThreadsPerCPU}: Integer (default \code{2}); the number of threads scheduled per CPU core.
      
      \item \code{extra$numParThreads}: Integer (default \code{0}). Total number of parallel threads used when fitting many time series. If \code{0}, the actual number of threads is set to \code{extra$numThreadsPerCPU * (number of CPU cores)}.  That is, each CPU core will generate a number 'numThreadsPerCPU' of thread. On 64-bit Windows, BEAST is group-aware and distributes threads across NUMA nodes. Currently, up to 256 CPU cores are supported.
    }
  }

  \item{season}{
    Character (default \code{"harmonic"}). Specifies whether \code{Y} has a periodic component and how it is modeled:
    \itemize{
      \item \code{"none"}: \code{Y} is trend-only; no periodic components are modeled. Seasonal prior parameters (e.g., seasonMaxKnotNum and seasonMinSepDist) are ignored.
      
      \item \code{"harmonic"}: \code{Y} has a periodic/seasonal component modeled as a harmonic series (e.g., combination of sines and cosines). The term \code{season} is used broadly to refer to any periodic variation. The period is not estimated but must be specified by the user (via \code{metadata$period}); see \code{metadata$period}.
      
      \item \code{"dummy"}: As \code{"harmonic"}, except that the seasonal component is modeled as a non-parametric curve. The harmonic-order prior settings (e.g., \code{prior$seasonMinOrder} and \code{prior$seasonMaxOrder}) are irrelevant and ignored if \code{season="dummy"}.
      
      \item \code{"svd"}: (Experimental) As \code{"harmonic"}, except that the seasonal component is expressed as a linear combination of basis functions derived from a Single-value decomposition. These basis functions can be more parsimonious than pure harmonic bases, and may improve detection of subtle seasonal changepoints.
    }
  }

  \item{method}{
    Character (default \code{"bayes"}). Specifies how the model posterior probability is formulated or approximated:
    \itemize{
      \item \code{"bayes"}: Full Bayesian formulation as described in Zhao et al. (2019).
      
      \item \code{"bic"}: Approximate posterior probabilities using the Bayesian Information Criterion (BIC),
      \eqn{\mathrm{BIC} = n \log(\mathrm{SSE}) + k \log(n)}, where \eqn{k} is the number of parameters and \eqn{n} the number of observations.
      
      \item \code{"aic"}: Approximate posterior probabilities using Akaike's Information Criterion (AIC),
      \eqn{\mathrm{AIC} = n \log(\mathrm{SSE}) + 2k}.
      
      \item \code{"aicc"}: Approximate posterior probabilities using the corrected AIC (AICc),
      \eqn{\mathrm{AICc} = \mathrm{AIC} + \frac{2k^2 + 2k}{n - k - 1}}.
      
      \item \code{"hic"}: Approximate posterior probabilities using the Hannan-Quinn Information Criterion (HIC),
      \eqn{\mathrm{HIC} = n \log(\mathrm{SSE}) + 2k \log(\log(n))}.
      
      \item \code{"bic0.25"}: BIC-type approximation adopted from Kim et al. (2016) \doi{10.1016/j.jspi.2015.09.008} with reduced complexity penalty,
      \eqn{\mathrm{BIC}_{0.25} = n \log(\mathrm{SSE}) + 0.25 k \log(n)}.
      
      \item \code{"bic0.5"}: As above but with penalty factor \code{0.5}.
      
      \item \code{"bic1.5"}: As above but with penalty factor \code{1.5}.
      
      \item \code{"bic2"}: As above but with penalty factor \code{2.0}.
    }
  }

  \item{\dots}{
    Additional parameters reserved for future extensions; currently unused.
  }
}

\value{
  The output is a list object of class \code{"beast"} containing the following fields. Exact dimensions depend on the input \code{Y} and \code{extra$whichOutputDimIsTime}. The descriptions below assume a single time series input of length \code{N}; for 2D/3D inputs, interpret the results according to the 2D/3D dimensions.

  \item{time}{A numeric vector of length \code{N}: the time associated with each sampled observation. By default, this is simply \code{1:N} if \code{metadata$time}, \code{metadata$startTime}, or \code{metadata$deltaTime} are missing.}
  
  \item{data}{A vector, matrix, or 3D array; a copy of the input \code{Y} if \code{extra$dumpInputData = TRUE}. If \code{extra$dumpInputData = FALSE}, this is \code{NULL}. For irregular inputs in \code{Y} , this field stores the aggregated regular series at time intervals spaced by \code{metadata$deltaTime}.}
  
  \item{marg_lik}{Numeric; the average marginal likelihood of the sampled models. Larger values indicate better fit for a given time series.}
  
  \item{R2}{Numeric; coefficient of determination (R-squared) of the fitted model.}
  
  \item{RMSE}{Numeric; root mean squared error of the fitted model.}
  
  \item{sig2}{Numeric; estimated variance of the residual error.}
  
  \item{trend}{
    \code{trend} is a \code{list} of outputs related to the estimated trend component:
    \itemize{
      \item \code{ncp}: Numeric scalar; mean number of trend changepoints. Because individual models sampled by BEAST can have varying numbers of changepoints, several alternative summaries (\code{ncp_mode}, \code{ncp_median}, \code{ncp_pct90}) are also provided. For example, if \code{mcmc$samples = 10} and the numbers of changepoints across models are \code{c(0, 2, 4, 1, 1, 2, 7, 6, 6, 1)}, then the mean number of changepoints (ncp) is \code{3.1}, the median \code{2.5}, the mode \code{1}, and the 90th percentile (\code{ncp_pct90}) \code{6.5}.
      
      \item \code{ncp_mode}: Mode of the number of trend changepoints. See the above for explanations.
      \item \code{ncp_median}: Median of the number of trend changepoints. See the above for explanations.
      \item \code{ncp_pct90}: 90th percentile of the number of trend changepoints. See the above for explanations.
      
      \item \code{ncpPr}: Numeric vector of length \code{prior$trendMaxKnotNum + 1}. Probability distribution over the number of trend changepoints in the range \code{0:prior$trendMaxKnotNum}. For example, \code{ncpPr[1]} is the probability of having no trend changepoint; \code{ncpPr[i]} is the probability of having \code{i - 1} changepoints.
      
      \item \code{cpOccPr}: Numeric vector of length \code{N}. The changepoint occurrence probability at each time index for the trend component. Plotting \code{cpOccPr} yields a continuous probability-of-being-a-changepoint curve. A peak with a high value at a single time step does not necessarily imply a higher \emph{overall} probability of a changepoint in a neighborhood, compared to a broader peak with a lower maximum but higher total probability. Specifically, a higher peak indicates a higher chance of being a changepoint only at that particular SINGLE point in time and but not necessarily mean a higher chance of observing a changepoint AROUND that time. For example, a window of cpOccPr values \code{c(0,0,0.5,0,0)} (i.e., the peak prob is 0.5 and the summed probability over the window is 0.5) is less likely to be a changepoint compared to another window \code{c(0.1,0.2,0.21,0.2,0.1)} where the peak of 0.21 is lower but the summed probablity of 0.71 is larger.
      
      \item \code{order}: Numeric vector of length \code{N}. The average polynomial order of the trend across sampled models at each time step. Because this is a posterior average, it need not be an integer.
      
      \item \code{cp}: Numeric vector (up to length \code{prior$trendMaxKnotNum}). Estimated trend changepoint locations. These are obtained by smoothing \code{cpOccPr} with a window of size \code{prior$trendMinSepDist} and then selecting up to \code{prior$trendMaxKnotNum} peaks in the smoothed curve. Some entries may be \code{NaN} if the number of detected changepoints is fewer than \code{trendMaxKnotNum}. Not all listed changepoints should be treated as "true" changepoints; some may be false positives. Check the associated posterior probabiliites (given in the next field \code{trend$cpPr} for the confidence levels.
      
      \item \code{cpPr}: Numeric vector of length \code{prior$trendMaxKnotNum}; the posterior probabilities associated with each changepoint in \code{cp}. Trailing entries are \code{NaN} if fewer than \code{prior$trendMaxKnotNum} changepoints are identified.
      
      \item \code{cpCI}: Numeric matrix of dimension \code{prior$trendMaxKnotNum x 2}; credible intervals for the changepoint locations in \code{cp}.
      
      \item \code{cpAbruptChange}: Numeric vector of length \code{prior$trendMaxKnotNum}; the estimated magnitude of the jump in the trend at each detected changepoint in \code{cp}.
      
      \item \code{Y}: Numeric vector of length \code{N}; the estimated trend component (Bayesian model average over all sampled trends).
      
      \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the estimated trend.
      
      \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible interval for the trend, with lower and upper envelopes.
      
      \item \code{slp}: Numeric vector of length \code{N}; time-varying slope of the trend component.
      
      \item \code{slpSD}: Numeric vector of length \code{N}; posterior standard deviation of the slope.
      
      \item \code{slpSgnPosPr}: Numeric vector of length \code{N}; posterior probability that the slope of the trend is positive at each time point. For example, \code{slpSgnPosPr = 0.80} at a given time means that 80\% of sampled trend models have a positive slope there.
      
      \item \code{slpSgnZeroPr}: Numeric vector of length \code{N}; posterior probability that the slope of the trend is effectively zero (flat) at each time point. The probability of a negative slope can be derived as \code{1 - slpSgnZeroPr - slpSgnPosPr}.
      
      \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
            \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
            \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
            \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}:
      As above, but restricted to trend changepoints with positive jumps (\code{pos_*}) or negative jumps (\code{neg_*}) in the trend. For example, \code{pos_ncp} is the mean number of changepoints at which the trend level jumps up.
      
      \item \code{inc_ncp}, \code{dec_ncp}, \code{inc_ncpPr}, \code{dec_ncpPr},
            \code{inc_cpOccPr}, \code{dec_cpOccPr}, \code{inc_cp}, \code{dec_cp},
            \code{inc_cpPr}, \code{dec_cpPr}, \code{inc_cpAbruptChange},
            \code{dec_cpAbruptChange}, \code{inc_cpCI}, \code{dec_cpCI}:
      As above, but restricted to changepoints where the \emph{trend slope} increases (\code{inc_*}) or decreases (\code{dec_*}). For example, if the trend slope changes from \code{0.4} to \code{2.5} at a changepoint, that changepoint is counted toward \code{inc_ncp}.
    }
  }

  \item{season}{
    \code{season} is a \code{list} analogous to \code{trend}, but for the seasonal/periodic component (when present):
    \itemize{
      \item \code{ncp}: Mean number of seasonal changepoints. Because individual models sampled by BEAST can have varying numbers of changepoints, several alternative summaries (\code{ncp_mode}, \code{ncp_median}, \code{ncp_pct90}) are also provided. For example, if \code{mcmc$samples = 10} and the numbers of changepoints across models are \code{c(0, 2, 4, 1, 1, 2, 7, 6, 6, 1)}, then the mean number of changepoints (ncp) is \code{3.1}, the median \code{2.5}, the mode \code{1}, and the 90th percentile (\code{ncp_pct90}) \code{6.5}.
	  
      \item \code{ncp_mode}: Mode of the number of seasonal changepoints. See the above for explanations.
      \item \code{ncp_median}: Median of the number of seasonal changepoints. See the above for explanations.
      \item \code{ncp_pct90}: 90th percentile of the number of seasonal changepoints. See the above for explanations.
	  
      \item \code{ncpPr}: Numeric vector of length \code{prior$seasonMaxKnotNum + 1}; probability distribution over the number of seasonal changepoints in \code{0:prior$seasonMaxKnotNum}. For example, \code{ncpPr[1]} is the probability of having no seasonal changepoint; \code{ncpPr[i]} is the probability of having \code{i - 1} changepoints.	  
	  
      \item \code{cpOccPr}: Numeric vector of length \code{N}; seasonal changepoint occurrence probability over time. Plotting \code{cpOccPr} yields a continuous probability-of-being-a-changepoint curve. A peak with a high value at a single time step does not necessarily imply a higher \emph{overall} probability of a changepoint in a neighborhood, compared to a broader peak with a lower maximum but higher total probability. Specifically, a higher peak indicates a higher chance of being a changepoint only at that particular SINGLE point in time and does not necessarily mean a higher chance of observing a changepoint AROUND that time. For example, a window of cpOccPr values \code{c(0,0,0.5,0,0)} (i.e., the peak prob is 0.5 and the summed probability over the window is 0.5) is less likely to be a changepoint compared to another window \code{c(0.1,0.2,0.21,0.2,0.1)} where the peak of 0.21 is lower but the summed probablity of 0.71 is larger.
	  
      \item \code{order}: Numeric vector of length \code{N}; average harmonic order needed to approximate the seasonal component. Because this is a posterior average, it need not be an integer
	  
      \item \code{cp}: Numeric vector (up to length \code{prior$seasonMaxKnotNum}); estimated seasonal changepoint locations.  These are obtained by smoothing \code{cpOccPr} with a window of size \code{prior$seasonMinSepDist} and then selecting up to \code{prior$seasonMaxKnotNum} peaks in the smoothed curve. Some entries may be \code{NaN} if the number of detected changepoints is fewer than \code{seasonMaxKnotNum}. Not all listed changepoints should be treated as "true" changepoints; some may be false positives. Check the associated posterior probabiliites (given in the next field \code{season$cpPr} for the confidence levels.
	  
      \item \code{cpPr}: Numeric vector of length \code{prior$seasonMaxKnotNum}; the posterior probabilities associated with each changepoint in \code{cp}. Trailing entries are \code{NaN} if fewer than \code{prior$seasonMaxKnotNum} changepoints are identified.
	  
      \item \code{cpCI}: Numeric matrix of dimension \code{prior$seasonMaxKnotNum x 2}; credible intervals for seasonal changepoint locations.
	  
      \item \code{cpAbruptChange}: Numeric vector of length \code{prior$seasonMaxKnotNum}; magnitude of jumps in the seasonal curve at each changepoint.
	  
      \item \code{Y}: Numeric vector of length \code{N}; estimated seasonal component (Bayesian model average).
	  
      \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the seasonal component.
      \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible interval for the seasonal component.
      \item \code{amp}: Numeric vector of length \code{N}; time-varying amplitude of the seasonality.
      \item \code{ampSD}: Numeric vector of length \code{N}; posterior standard deviation of the amplitude.
      
      \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
            \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
            \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
            \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}:
      Seasonal analogues of the trend outputs with the same names, but restricted to seasonal changepoints with positive (\code{pos_*}) or negative (\code{neg_*}) jumps in the seasonal component. For example, \code{pos_ncp} refers to the average number of seasonal changepoints that jump up (i.e., positively) in the seasonal curve.
    }
  }
  
 \item{outlier}{
   \code{outlier} is a \code{list} analogous to \code{trend} or \code{season}, but for the outlier component ( present only if setting \code{hasOutlier=TRUE})
  } %\item{outlier}  
}

\seealso{
  \code{\link[Rbeast]{beast}},
  \code{\link[Rbeast]{beast.irreg}},
  \code{\link[Rbeast]{minesweeper}},
  \code{\link[Rbeast]{tetris}},
  \code{\link[Rbeast]{geeLandsat}}
}

\examples{

#-------------------------------- NOTE ----------------------------------------------#
# beast123() is an all-inclusive function that unifies the functionality of beast() 
# and beast.irreg(). It can handle single, multiple, or 3D stacked time series, either
# regular or irregular. It allows customization via four list arguments:
#   metadata -- additional information about the input Y
#   prior    -- prior parameters for the BEAST model
#   mcmc     -- MCMC settings
#   extra    -- miscellaneous options controlling output and parallel computation
#
# Although functionally similar to beast() and beast.irreg(), beast123() is designed
# to efficiently handle multiple time series concurrently (e.g., stacked satellite
# images) via internal multithreading. When processing stacks of raster layers, 
# DO NOT iterate pixel-by-pixel using beast() or beast.irreg() wrapped by external
# parallel tools (e.g., doParallel, foreach). Instead, use beast123(), which
# implements parallelism internally.
 
#------------------------------ Example 1: seasonal time series ----------------------#
# Yellowstone is a half-monthly NDVI time series of length 774 for a site in Yellowstone
# starting from July 1-15, 1981 (start ~ c(1981, 7, 7); 24 observations per year).

library(Rbeast)
data(Yellowstone)
plot(Yellowstone)

# Below, the four optional arguments are omitted, so default values are used.
# By default, Y is assumed to be regular with a seasonal component. The actual
# default values are printed at the start of the run and can be used as templates
# for customization.
\donttest{
  o <- beast123(Yellowstone)
  plot(o)
}

#------------------------------ Example 2: trend-only series -------------------------#
# Nile is an annual river flow time series (no periodic variation). Set season = "none"
# to indicate trend-only analysis. Default values are used for other options.
# Unlike beast(), beast123() does NOT use the time attributes of a 'ts' object.
# For example, Nile is treated as numeric data only with the default times 
# 1:length(Nile); its (start=1871, end=1970, freq=1) attributes are ignored 
# unless specified through 'metadata'.

\donttest{
  o <- beast123(Nile, season = "none")
  o <- beast123(Nile, extra = list(dumpInputData = TRUE, quiet = TRUE),
                season = "none")
  o <- beast123(Nile, metadata = list(startTime = 1871, hasOutlier = TRUE),
                season = "none")
  plot(o)
}

#------------------------------ Example 3: full API usage ---------------------------#
# Explicitly specify metadata, prior, mcmc, and extra. Among these, 'prior' contains
# the true statistical model parameters; the others configure input, output, and
# computation.

\dontrun{

  # metadata: extra information describing the input time series Y.
  metadata <- list()
  # metadata$isRegularOrdered <- TRUE    # no longer used in this version
  metadata$whichDimIsTime      <- 1      # which dimension is time for 2D/3D inputs
  metadata$startTime           <- c(1981, 7, 7)
  # alternatively: metadata$startTime <- 1981.5137
  #                metadata$startTime <- as.Date("1981-07-07")
  metadata$deltaTime           <- 1/24   # half-monthly: 24 obs/year
  metadata$period              <- 1.0    # 1-year period: 24 * (1/24) = 1
  metadata$omissionValue       <- NaN    # NaNs are omitted by default
  metadata$maxMissingRateAllowed <- 0.75 # skip series with > 75% missing
  metadata$deseasonalize       <- FALSE  # do not pre-remove global seasonality
  metadata$detrend             <- FALSE  # do not pre-remove global trend
  
  # prior: only true model parameters specifying the Bayesian formulation
  prior <- list()
  prior$seasonMinOrder    <- 1     # min harmonic order allowed to fit seasonal cmpnt
  prior$seasonMaxOrder    <- 5     # max harmonic order allowed to fit seasonal cmpnt
  prior$seasonMinKnotNum  <- 0     # min number of changepnts in seasonal cmpnt
  prior$seasonMaxKnotNum  <- 3     # max number of changepnts in seasonal cmpnt
  prior$seasonMinSepDist  <- 10    # min inter-chngpts separation for seasonal cmpnt
  prior$trendMinOrder     <- 0     # min polynomial order allowed to fit trend cmpnt 
  prior$trendMaxOrder     <- 1     # max polynomial order allowed to fit trend cmpnt   
  prior$trendMinKnotNum   <- 0     # min number of changepnts in trend cmpnt
  prior$trendMaxKnotNum   <- 15    # max number of changepnts in trend cmpnt 
  prior$trendMinSepDist   <- 5     # min inter-chngpts separation for trend cmpnt 
  prior$precValue         <- 10.0  # Initial value of precision parameter(no need
                                   # to change it unless for precPrioType='const')
  prior$precPriorType     <- "uniform"  # "constant", "uniform", "componentwise", "orderwise"
  
  # mcmc: settings for the MCMC sampler
  mcmc <- list()
  mcmc$seed                      <- 9543434  # arbitray seed for random number generator
  mcmc$samples                   <- 3000     # samples collected per chain
  mcmc$thinningFactor            <- 3        # take every 3rd sample and discard others 
  mcmc$burnin                    <- 150      # discard the initial 150 samples per chain
  mcmc$chainNumber               <- 3        # number of chains 
  mcmc$maxMoveStepSize           <- 4        # max random jump step when proposing new chngpts
  mcmc$trendResamplingOrderProb  <- 0.10     # prob of choosing to resample polynomial order
  mcmc$seasonResamplingOrderProb <- 0.10     # prob of choosing to resample harmonic order
  mcmc$credIntervalAlphaLevel    <- 0.95     # the significance level for credible interval 
  
  # extra: output and computation options
  extra <- list()
  extra$dumpInputData         <- FALSE  # If true, a copy of input time series is outputted 
  extra$whichOutputDimIsTime  <- 1      # For 2D or 3D inputs, which dim of the output refers
                                        # to time? Ignored if the input is a single time series
  extra$computeCredible       <- FALSE  # If true, compute CI: computing CI is time-intensive. 
  extra$fastCIComputation     <- TRUE   # If true, a faster way is used to get CI, but it is 
                                        # still time-intensive. That is why the function beast()
                                        # is slow because it always compute CI.
  extra$computeSeasonOrder    <- FALSE  # If true, dump the estimated harmonic order over time
  extra$computeTrendOrder     <- FALSE  # If true, dump the estimated polynomial order over time
  extra$computeSeasonChngpt   <- TRUE   # If true, get the most likely locations of s chgnpts
  extra$computeTrendChngpt    <- TRUE   # If true, get the most likely locations of t chgnpts
  extra$computeSeasonAmp      <- FALSE  # If true, get time-varying amplitude of seasonality
  extra$computeTrendSlope     <- FALSE  # If true, get time-varying slope of trend
  extra$tallyPosNegSeasonJump <- FALSE  # If true, get those changpts with +/- jumps in season
  extra$tallyPosNegTrendJump  <- FALSE  # If true, get those changpts with +/- jumps in trend 
  extra$tallyIncDecTrendJump  <- FALSE  # If true, get those changpts with increasing/
                                        # decreasing trend slopes 
  extra$printProgress         <- TRUE
  extra$printParameter        <- TRUE
  extra$quiet                 <- FALSE  # if TRUE, print warning messages, if any
  extra$consoleWidth          <- 0      # If 0, the console width is from the current console
  extra$numThreadsPerCPU      <- 2      # 'numThreadsPerCPU' and 'numParThreads' are used to 
  extra$numParThreads         <- 0      #   configure multithreading runs; they're used only 
                                        #   if Y has multiple time series (e.g.,stacked images)
  
  o <- beast123(Yellowstone, metadata, prior, mcmc, extra, season = "harmonic")
  plot(o)
}

#------------------------------ Example 4: irregular time series ---------------------#
# ohio is a data frame of Landsat NDVI observations at unevenly spaced times.

\donttest{
\dontrun{

  data(ohio)
  str(ohio)
  
  ###################################################################################
  # Individual times are provided as fractional years via ohio$time
  
  metadata <- list()
  metadata$time      <- ohio$time  # Must supply individual times for irregular inputs
  metadata$deltaTime <- 1/12       # Must supply the desired time interval for aggregation
  metadata$period    <- 1.0
  o <- beast123(ohio$ndvi, metadata)   # defaults used for missing parameters
  
  ###################################################################################
  # Another accepted time format: Ohio dates as Date objects
  class(ohio$rdate)
  
  metadata <- list()
  metadata$time      <- ohio$rdate # Must supply individual times for irregular inputs
  metadata$deltaTime <- 1/12       # Must supply the desired time interval for aggregation
  o <- beast123(ohio$ndvi, metadata)  # Default values used for those missing parameters
  
  ###################################################################################
  # Another accepted time format: separate Y, M, D columns
  ohio$Y      # Year                       
  ohio$M      # Month
  ohio$D      # Day
  
  metadata <- list()
  metadata$time$year   <- ohio$Y   # Must supply individual times for irregular inputs
  metadata$time$month  <- ohio$M
  metadata$time$day    <- ohio$D
  metadata$deltaTime   <- 1/12     # Must supply the desired time interval for aggregation
  o <- beast123(ohio$ndvi, metadata) # Default values used for those missing parameters
  
  ###################################################################################
  # Another accepted time format: year and DOY
  ohio$Y        # Year
  ohio$doy      # Day of year
  
  metadata <- list()
  metadata$time$year  <- ohio$Y   # Must supply individual times for irregular inputs 
  metadata$time$doy   <- ohio$doy
  metadata$deltaTime  <- 1/12     # Must supply the desired time interval for aggregation
  o <- beast123(ohio$ndvi, metadata) # Default values used for those missing parameters
  
  
  ###################################################################################
  # Another accepted time format: date strings with custom strfmt
  ohio$datestr1
  
  metadata <- list()
  metadata$time$datestr  <- ohio$datestr1
  metadata$time$strfmt   <- "????yyyy?mm?dd"
  metadata$deltaTime     <- 1/12
  o <- beast123(ohio$ndvi, metadata)
  
 ###################################################################################
 # Another accepted time format: date strings with custom strfmt
  ohio$datestr2
  
  metadata <- list()
  metadata$deltaTime     <- 1/12
  metadata$time$datestr  <- ohio$datestr2
  metadata$time$strfmt   <- "????yyyydoy????"
  o <- beast123(ohio$ndvi, metadata)
  
 ###################################################################################
 # Another accepted time format: date strings with custom strfmt
  ohio$datestr3
  
  metadata <- list()
  metadata$deltaTime     <- 1/12
  metadata$time$datestr  <- ohio$datestr3
  metadata$time$strfmt   <- "Y,,M/D"
  o <- beast123(ohio$ndvi, metadata)
}
}

#------------------ Example 5: multiple time series (matrix input) -------------------#
# simdata is a 2D matrix of dimension 300 x 3, representing 3 time series of length 300.

\dontrun{
  data(simdata)  # dim of simdata: 300 x 3 (time x num_of_time_series) 
  dim(simdata)   # the first dimenion refer to time (i.e, 300)
  
  metadata <- list()
  metadata$whichDimIsTime <- 1  # first dimension is time
  metadata$period         <- 24 # assume startTime = 1, deltaTime = 1 by default
  
  extra <- list()
  extra$whichOutputDimIsTime <- 2  # Which dim of the output arrays refers to  time?
  
  o <- beast123(simdata, metadata, extra = extra)
  
  # Lists of inputs args can also be provided inline:
  o <- beast123(simdata,
                metadata = list(whichDimIsTime = 1, period = 24),
                extra    = list(whichOutputDimIsTime = 2))
  
  # Field names can be shortened, provided no ambiguity arises:
  o <- beast123(simdata,
                metadata = list(whichDim = 1, per = 24),
                extra    = list(whichOut = 2))
  
  #------------------ Example 5b: transposed simdata ---------------------------------#
  simdata1 <- t(simdata)   # 3 (series) x 300 (time)
                           # dim of simdata1: 3 x 300 (num of ts  x time )
						   
  metadata <- list()
  metadata$whichDimIsTime <- 2  # time is second dimension
  metadata$period         <- 24 # assume startTime = 1, deltaTime = 1 by default
  o <- beast123(simdata1, metadata)
  
  o <- beast123(simdata1, metadata = list(whichDim = 2, per = 24))
}

#------------------ Example 6: 3D stacked image time series -------------------------#
# imagestack$ndvi is a 3D array of size 12 x 9 x 1066 (row x col x time). Each pixel
# corresponds to a time series of length 1066.

\dontrun{
  data(imagestack)
  dim(imagestack$ndvi)   # 12 x 9 x 1066
  imagestack$datestr     # A character vector of 1066 date strings
  
  metadata <- list()
  metadata$whichDimIsTime <- 3  # Which dim of the input refer to time for 3D inputs?
                                # 1066 is the ts length, so dim is set to '3' here.
                                # In this example, this arg is not needed because 
                                # the time$datestr can also help to match and pick up
                                # the right time dimesion of imagestack$ndvi.
  metadata$time$datestr   <- imagestack$datestr
  metadata$time$strfmt    <- "LT05_018032_20080311.yyyy-mm-dd"
  metadata$deltaTime      <- 1/12   # aggregate to monthly (i.e., 1/12 yr)
  metadata$period         <- 1.0    # 1-year period
  
  extra <- list()
  extra$dumpInputData    <- TRUE   # Dump a copy of aggregated input ts 
  extra$numThreadsPerCPU <- 2      # Each cpu core will be assigned 2 threads
  extra$numParThreads    <- 0      # If 0, total_num_threads=numThreadsPerCPU*num_of_cpu_core
                                   # if >0, used to specify the total number of threads
  
  # Default values for missing parameters 
  o <- beast123(imagestack$ndvi, metadata = metadata, extra = extra)
  
  print(o, c(5, 3))     # result for pixel at row 5, col 3
  plot(o,  c(5, 3))     # plot result for pixel at row 5, col 3
  image(o$trend$ncp)    # map number of trend changepoints over space
}

#------------------ Example 7: GeoTIFF stacks via raster package --------------------#
# Handle 3D stacked images from a series of NDVI GeoTIFF files (e.g., ndvi.zip with
# 437 NDVI TIFF files of dimension 12 x 9). Example code is available at:
#   https://github.com/zhaokg/Rbeast/blob/master/R/beast123_raster_example.txt

}

\references{
	\enumerate{
	\item Zhao, K., Wulder, M.A., Hu, T., Bright, R., Wu, Q., Qin, H., Li, Y., Toman, E., Mallick, B., Zhang, X. and Brown, M., 2019. Detecting change-point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics: A Bayesian ensemble algorithm. Remote Sensing of Environment, 232, p.111181 (the beast algorithm paper).
	\item Zhao, K., Valle, D., Popescu, S., Zhang, X. and Mallick, B., 2013. Hyperspectral remote sensing of plant biochemistry using Bayesian model averaging with variable and band selection. Remote Sensing of Environment, 132, pp.102-119 (the Bayesian MCMC scheme used in beast). 
	\item Hu, T., Toman, E.M., Chen, G., Shao, G., Zhou, Y., Li, Y., Zhao, K. and Feng, Y., 2021. Mapping fine-scale human disturbances in a working landscape with Landsat time series on Google Earth Engine. ISPRS Journal of Photogrammetry and Remote Sensing, 176, pp.250-261(a beast application paper). 
	}
}
\keyword{misc}
