\name{beast.irreg}
\docType{methods}
\alias{beast.irreg}
\alias{BEAST.irreg}

\title{Bayesian decomposition of irregular time series for changepoints, trend, and seasonality}

\description{
   \code{beast.irreg} applies the BEAST (Bayesian Estimator of Abrupt change, Seasonal change, and Trend) model to \emph{irregular} or unordered time series or 1D sequential data.  BEAST is a Bayesian model averaging algorithm for decomposing time series or other 1D sequential data into individual components, including abrupt changes, trends, and periodic/seasonal variations. It is useful for changepoint detection (e.g., breakpoints or structural breaks), nonlinear trend analysis, time series decomposition, and time series segmentation. \code{beast123} is a low-level interface to BEAST.

   Internally, irregular observations are first aggregated into an evenly spaced (regular) time series at a user-specified time interval, and then decomposed into individual components such as abrupt changes, nonlinear trends, and periodic/seasonal variations. 

}

\usage{

  beast.irreg(
       y, 
       time,        
       deltat         = NULL,
       season         = c("harmonic", "svd", "dummy", "none"),  
       period         = NULL,  		   
       scp.minmax     = c(0, 10),   sorder.minmax   = c(0, 5),  	  
       tcp.minmax     = c(0, 10),   torder.minmax   = c(0, 1), 	   
       sseg.min       = NULL,       sseg.leftmargin = NULL,  sseg.rightmargin = NULL,
       tseg.min       = NULL,       tseg.leftmargin = NULL,  tseg.rightmargin = NULL, 
       s.complexfct   = 0.0,
       t.complexfct   = 0.0,		   
       method         = c("bayes", "bic", "aic", "aicc", "hic",
	                      "bic0.25", "bic0.5", "bic1.5", "bic2"),
       detrend        = FALSE, 
       deseasonalize  = FALSE,
       mcmc.seed      = 0,      
       mcmc.burnin    = 200, 
       mcmc.chains    = 3,
       mcmc.thin      = 5,
       mcmc.samples   = 8000,
       precValue      = 1.5,
       precPriorType  = c("componentwise", "uniform", "constant", "orderwise"),
       hasOutlier     = FALSE,
       ocp.minmax     = c(0, 10),			 	   
       print.param    = TRUE,
       print.progress = TRUE,
       print.warning  = TRUE,
       quiet          = FALSE,
       dump.ci        = FALSE,
       dump.mcmc      = FALSE,
       gui            = FALSE,
       ...
  )
}

\arguments{

  \item{y}{
    \code{y} is a numeric vector for an irregular or unordered time series. Missing values such as \code{NA} and \code{NaN} are allowed.

    \itemize{
      \item If \code{y} is evenly spaced in time (regular), use \code{\link[Rbeast]{beast}} instead.
      \item If \code{y} is a matrix or 3D array (e.g., a stack of images) consisting of multiple regular or irregular time series, use \code{\link[Rbeast]{beast123}} instead.
    }

    If \code{y} is a \emph{list} of multiple time series, the multivariate version of the BEAST algorithm is invoked to jointly decompose the series and detect common changepoints. This feature is currently \code{experimental} and under development. See \code{\link[Rbeast]{ohio}} for a working example.
  }

  \item{time}{
   \code{time} is a vector (or list) specifying the times of each observation in \code{y}. Its length must match the time dimension of \code{y}. It can be numeric, \code{Date}, character, or a list of date components. Supported formats include:

    \enumerate{
      \item \strong{Numeric vector} \cr
      For example \code{c(1984.23, 1984.27, 1984.36, ...)}. The unit of \code{time} is irrelevant to BEAST as long as it is used consistently with \code{deltat} and \code{period}.

      \item \strong{R \code{Date} vector} \cr
      For example \code{as.Date(c("1984-03-27", "1984-04-10", "1984-05-12", ...))}.

      \item \strong{Character vector of date strings} \cr
      For example:
      \itemize{
        \item \code{c("1984-03-27", "1984-04-10", "1984-05-12")}
        \item \code{c("1984/03/27", "1984,04,10", "1984 05 12")} (delimiters may differ as long as the year-month-day order is consistent)
        \item \code{c("LT4-1984-03-27",  "LT4-1984-04-10",  "LT4-1984+05,12")}
        \item \code{c("LT4-1984087ndvi", "LT4-1984101ndvi", "LT4-1984133ndvi")}
        \item \code{c("1984,,abc 3/ 27", "1984,,ddxfdd 4/ 10", "ggd1984,, 5/ ttt 12")}
      }

      BEAST uses several heuristics to parse date strings without an explicit format specifier and may fail for ambiguous patterns (e.g., in \code{"LC8-2020-09-20-1984"} it is unclear whether 2020 or 1984 is the year). For robust parsing, consider using a list with an explicit format (see the next).

      \item \strong{List with date strings and format} \cr
      A list of the form \code{time = list(datestr = ..., strfmt = "...")}, where \code{time$datestr} is a character vector of date strings and \code{time$strfmt} is a format specifier describing how to extract year, month, day, or day-of-year. Three classes of formats are supported:

      \itemize{
        \item (a) \emph{Fixed positions for year/month/day.} \cr
          For example, to extract 2001/12/02 etc. from
          \code{time$datestr = c("P23R34-2001.1202333xd", "O93X94-2002.1108133fd", "TP3R34-2009.0122333td")}
          use \code{time$strfmt = "P23R34-yyyy.mmdd333xd"}, where \code{yyyy}, \code{mm}, and \code{dd} mark the positions of year, month, and day; all other characters are wildcards.

        \item (b) \emph{Fixed positions for year and day-of-year (doy).} \cr
          For example, to extract years and days-of-year from strings like \code{"P23R342001888045"} use \code{time$strfmt = "123123yyyy888doy"}, where \code{yyyy} and \code{doy} mark year and day-of-year; all other positions are wildcards. \code{doy} must be three digits.

        \item (c) \emph{Patterns based on separators between Y/M/D.} \cr
          For example, to extract 2002/12/02 from \code{"2002,12/02"}, \code{"  2002 , 12/2"}, \code{"2002,12  /02 "} use \code{time$strfmt = "Y,M/D"}; whitespace is ignored. To extract 2002/12/02 from \code{"2--12, 2012"}, use \code{time$strfmt = "D--M,Y"}.
      }

      \item \strong{List of date components} \cr
      A list specifying dates component-wise, using either
      \itemize{
        \item \code{time$year}, \code{time$month}, and \code{time$day}, or
        \item \code{time$year} and \code{time$doy}
      }
      where each element is a vector of the same length as the time dimension of \code{y}. For the \code{doy} representation, values must lie between 1 and 365/366.
    }
  }

  \item{deltat}{
    Numeric or character; the time interval used to \emph{aggregate} the irregular time series into a regular one.

    The BEAST model is formulated for regular (evenly spaced) data; therefore \code{beast.irreg} first re-bins the irregular time series into regular intervals of length \code{deltat}. If \code{deltat} is missing, a heuristic guess is used. The unit of \code{deltat} must be consistent with \code{time}:

    \itemize{
      \item If \code{time} is numeric, the unit of \code{deltat} is arbitrary but must be consistent with \code{time}.
      \item If \code{time} is a \code{Date} vector or date strings, the default unit of \code{deltat} is fractional years.
    }

    To specify the unit explicitly, supply a character string, for example \code{"7 days"}, \code{"7d"}, \code{"1/2 months"}, \code{"1 mn"}, \code{"1.0 year"}, or \code{"1y"}.
  }

  \item{season}{
    Character (default \code{"harmonic"}); indicates whether \code{y} has a periodic component, and how it should be modeled. Allowed values are:

    \itemize{
      \item \code{"none"}: \code{y} is trend-only; no periodic component is modeled. Seasonal arguments (e.g., \code{sorder.minmax}, \code{scp.minmax}, \code{sseg.min}) are ignored.

      \item \code{"harmonic"}: \code{y} has a periodic/seasonal component. Here, \sQuote{season} is used broadly to mean any periodic variation. The period is a known constant supplied via \code{period} and is \emph{not} estimated as a model parameter. The seasonal component is modeled as a harmonic curve (sum of sines and cosines).

      \item \code{"dummy"}: As in \code{"harmonic"}, but the periodic component is modeled via non-parametric dummy bases. The harmonic-order argument \code{sorder.minmax} is irrelevant and ignored.

      \item \code{"svd"}: (experimental) As in \code{"harmonic"}, but the seasonal component is modeled as a linear combination of basis functions derived via singular value decomposition (SVD). These SVD-based bases can be more parsimonious than the standard harmonic bases and can improve sensitivity to subtle changepoints.
    }
  }

  \item{period}{
    Numeric or character. The period of the seasonal/periodic component in \code{y}. Required only when a periodic component is present (i.e., \code{season} is \code{"harmonic"}, \code{"svd"}, or \code{"dummy"}), and ignored if \code{season = "none"}.

    The period must have units consistent with \code{deltat}, and satisfies
    \code{period = deltat * freq}, where \code{freq} is the number of samples per period. The historical \code{freq} argument (number of data points per period) is still supported via \code{...}, but is deprecated; if both \code{period} and \code{freq} are supplied, \code{period} takes precedence.

    \code{period} (or equivalently \code{freq}) is not itself a BEAST model parameter and must usually be specified by the user. If \code{period} is missing, BEAST attempts to guess it via autocorrelation before fitting the model.

    If \code{period <= 0}, \code{season = "none"} is assumed and a trend-only model is fitted. To specify units explicitly for date-based time axes, use a string such as \code{"1.0 year"}, \code{"12 months"}, \code{"365d"}, or \code{"366 days"}.
  }

  \item{scp.minmax}{
    Integer vector of length 2 (\code{>= 0}); minimum and maximum numbers of \emph{seasonal} changepoints (SCPs) allowed in the seasonal component. Used only when a seasonal component is present (i.e., \code{season != "none"}) and ignored otherwise.

    If \code{scp.minmax[1] == scp.minmax[2]}, BEAST assumes a fixed number of seasonal changepoints and does not infer the posterior on the number of changepoints, but still estimates when those changepoints occur.

    If both values are 0, no seasonal changepoints are allowed; a global harmonic (or SVD/dummy) model is used, but the most likely harmonic order is still inferred if \code{sorder.minmax[1] != sorder.minmax[2]}.
  }

  \item{sorder.minmax}{
    Integer vector of length 2 (\code{>= 1}); minimum and maximum harmonic orders considered for the seasonal component. Used only if \code{season = "harmonic"} or \code{"svd"} and ignored for trend-only data or when \code{season = "dummy"}.

    If \code{sorder.minmax[1] == sorder.minmax[2]}, BEAST assumes a fixed harmonic order and does not infer the posterior distribution of harmonic orders.
  }

  \item{tcp.minmax}{
    Integer vector of length 2 (\code{>= 0}); minimum and maximum numbers of \emph{trend} changepoints (TCPs) allowed in the trend component.

    If \code{tcp.minmax[1] == tcp.minmax[2]}, BEAST assumes a fixed number of trend changepoints and does not infer the posterior on the number of trend changepoints, but still estimates their occurrence probabilities over time.

    If both values are 0, no trend changepoints are allowed; a global polynomial trend is used, but the most likely polynomial order is still inferred if \code{torder.minmax[1] != torder.minmax[2]}.
  }

  \item{torder.minmax}{
    Integer vector of length 2 (\code{>= 0}); minimum and maximum polynomial orders considered for the trend component. Order 0 corresponds to a constant (flat) trend; order 1 is a line.

    If \code{torder.minmax[1] == torder.minmax[2]}, BEAST assumes a fixed polynomial order and does not infer the posterior distribution of trend orders.
  }

  \item{sseg.min}{
    Integer (\code{> 0}); minimum length (in data points) of a segment between two neighboring seasonal changepoints. When fitting a piecewise seasonal model, no two seasonal changepoints are allowed to occur within \code{sseg.min} time steps.

    \code{sseg.min} is unitless (number of time intervals); in the original time unit the window length is \code{sseg.min * deltat}. If \code{NULL}, a default based on the implied frequency is used.
  }

  \item{sseg.leftmargin}{
    Integer (\code{>= 0}); number of leftmost data points excluded from seasonal changepoint detection. No seasonal changepoints are allowed in the initial window of length \code{sseg.leftmargin}.

    \code{sseg.leftmargin} is unitless (number of samples). In the original time unit, the excluded window length is \code{sseg.leftmargin * deltat}. If \code{NULL}, defaults to \code{sseg.min}.
  }

  \item{sseg.rightmargin}{
    Integer (\code{>= 0}); number of rightmost data points excluded from seasonal changepoint detection. No seasonal changepoints are allowed in the ending window of length \code{sseg.rightmargin}.

    \code{sseg.rightmargin} is unitless, and the corresponding time window is \code{sseg.rightmargin * deltat}. If \code{NULL}, defaults to \code{sseg.min}.
  }

  \item{tseg.min}{
    Integer (\code{> 0}); minimum length (in data points) of a segment between two neighboring trend changepoints. When fitting a piecewise polynomial trend, no two trend changepoints are allowed within a window of length \code{tseg.min}.

    \code{tseg.min} is unitless; in the original time unit the window is \code{tseg.min * deltat}. If \code{NULL}, a default based on the implied frequency is used when a cyclic component is present.
  }

  \item{tseg.leftmargin}{
    Integer (\code{>= 0}); number of leftmost data points excluded from trend changepoint detection. No trend changepoints are allowed in the starting window of length \code{tseg.leftmargin}.

    \code{tseg.leftmargin} is unitless; the excluded time window is \code{tseg.leftmargin * deltat}. If \code{NULL}, defaults to \code{tseg.min}.
  }

  \item{tseg.rightmargin}{
    Integer (\code{>= 0}); number of rightmost data points excluded from trend changepoint detection. No trend changepoints are allowed in the ending window of length \code{tseg.rightmargin}.

    \code{tseg.rightmargin} is unitless; the excluded time window is \code{tseg.rightmargin * deltat}. If \code{NULL}, defaults to \code{tseg.min}.
  }

  \item{s.complexfct}{
	 Numeric (default \code{0.0}). A hyperprior parameter--newly added in Version 1.02-- controlling the complexity of the seasonal curve (i.e., the number of seasonal changepoints). A prior of the form \eqn{ p(k) \propto \exp[\lambda (k+1)]} is placed on the number of seasonal changepoints \eqn{k}, where \eqn{\lambda} is \code{seasonComplexityFactor}. Setting \eqn{\lambda = 0} yields a non-informative prior \eqn{ p(k) \propto 1.0}  where all model dimensions are equally likely \emph{a priori}. Users may tune \code{seasonComplexityFactor} in the range \code{[-20, 20]} or an even wider range: negative values (e.g., \eqn{\lambda = -15.9}) favor fewer changepoints (simpler seasonal curves), whereas positive values (e.g., \eqn{\lambda = 5.76}) favor more changepoints (more complex curves).
  }

  \item{t.complexfct}{
    Numeric scalar (default \code{0.0}); analogous to \code{s.complexfct} but for the trend component and the number of trend changepoints.
  }

  \item{method}{
    Character (default \code{"bayes"}); specifies how model posterior probabilities are formulated or approximated:

    \itemize{
      \item \code{"bayes"}: full Bayesian formulation as in Zhao et al. (2019).
      \item \code{"bic"}: BIC approximation, \code{bic = n * ln(SSE) + k * ln(n)}, where \code{k} and \code{n} are the numbers of parameters and data points.
      \item \code{"aic"}: AIC approximation, \code{aic = n * ln(SSE) + 2k}.
      \item \code{"aicc"}: corrected AIC, \code{aicc = aic + (2k^2 + 2k) / (n - k - 1)}.
      \item \code{"hic"}: Hannan--Quinn information criterion, \code{hic = n * ln(SSE) + 2k * ln(ln(n))}.
      \item \code{"bic0.25"}: BIC-type approximation adopted from Kim et al. (2016) <doi:10.1016/j.jspi.2015.09.008>, \code{bic0.25 = n * ln(SSE) + 0.25 k * ln(n)}, with a weaker penalty on model complexity.
      \item \code{"bic0.50"}: same as above but with penalty factor 0.50.
      \item \code{"bic1.5"}: same as above but with penalty factor 1.5.
      \item \code{"bic2"}: same as above but with penalty factor 2.0.
    }
  }

  \item{detrend}{
    Logical; if \code{TRUE}, a global trend is first fitted and removed from the series before running BEAST, and then added back to the final result. This can help when the global trend dominates other components.
  }

  \item{deseasonalize}{
    Logical; if \code{TRUE}, a global seasonal model is first fitted and removed from the series before running BEAST, then added back to the final result. Ignored when \code{season = "none"}.
  }

  \item{mcmc.seed}{
    Integer (\code{>= 0}); seed for the random number generator used in the MCMC sampler. If \code{mcmc.seed = 0}, an arbitrary seed is chosen and results vary across runs. Using a fixed non-zero seed improves reproducibility on the same machine; results may still differ across architectures because the underlying random number generator can depend on CPU instruction sets.
  }

  \item{mcmc.chains}{
    Integer (\code{> 0}); number of parallel MCMC chains.
  }

  \item{mcmc.thin}{
    Integer (\code{> 0}); thinning factor for the chains (e.g., if \code{mcmc.thin = 5}, every 5th iteration is retained).
  }

  \item{mcmc.burnin}{
    Integer (\code{> 0}); number of burn-in samples discarded at the beginning of each chain.
  }

  \item{mcmc.samples}{
    Integer (\code{>= 0}); number of post-burn-in samples collected per MCMC chain. The total number of iterations is \code{(mcmc.burnin + mcmc.samples * mcmc.thin) * mcmc.chains}.
  }

  \item{precValue}{
    Numeric (\code{> 0}); hyperparameter for the precision prior. It is directly used only when \code{precPriorType = "constant"}; otherwise it serves as an initial value that is updated within MCMC (see below).
  }

  \item{precPriorType}{
    Character; one of \code{"constant"}, \code{"uniform"}, \code{"componentwise"} (default), or \code{"orderwise"}. These options control how precision parameters for model coefficients are treated:

    \enumerate{
      \item \code{"constant"}: a single precision parameter is fixed to \code{precValue}. The result may be sensitive to \code{precValue}.
      \item \code{"uniform"}: a single precision parameter is treated as a random variable; its initial value is \code{precValue}, but its posterior is inferred via MCMC, making results less sensitive to the initial choice.
      \item \code{"componentwise"}: separate precision parameters are used for different components (e.g., one for season, another for trend), initialized by \code{precValue} and updated via MCMC.
      \item \code{"orderwise"}: separate precision parameters are used not only for components but also for individual orders within each component, again initialized by \code{precValue} and inferred via MCMC.
    }
  }

  \item{hasOutlier}{
    Logical; if \code{TRUE}, fits a model with an additional outlier component capturing spikes or dips at isolated data points:
    \itemize{
      \item \code{Y = trend + outlier + error} when \code{season = "none"},
      \item \code{Y = trend + season + outlier + error} otherwise.
    }
    Outliers are modeled as changepoints that cannot be captured by trend or seasonal terms.
  }

  \item{ocp.minmax}{
    Integer vector of length 2 (\code{>= 0}); minimum and maximum numbers of \emph{outlier-type} changepoints (OCPs) allowed in the series. OCPs correspond to isolated spikes/dips that are not captured by trend or seasonality.
  }

  \item{print.param}{
    Logical; if \code{TRUE}, the full list of internal BEAST parameters (organized as \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra}) is printed before MCMC. The internal naming differs slightly from the \code{beast.irreg} interface, but there is a one-to-one mapping (e.g., \code{prior$trendMinSepDist <- tseg.min}). See \code{beast123} or \code{View(beast)} for details.
  }

  \item{print.progress}{
    Logical; if \code{TRUE}, prints a progress bar during model fitting.
  }

  \item{print.warning}{
    Logical; if \code{TRUE}, prints warning messages.
  }

  \item{quiet}{
    Logical; if \code{TRUE}, suppresses all console output (overrides other printing options).
  }

  \item{dump.ci}{
    Logical; if \code{TRUE}, computes explicit credible intervals (i.e., \code{out$season$CI}, \code{out$trend$CI}) for the estimated seasonal and trend components. This requires sorting and can be time-consuming. If only symmetric intervals are needed, use the standard deviations (\code{out$trend$SD}, \code{out$season$SD}) and set \code{dump.ci = FALSE}.
  }

  \item{dump.mcmc}{
    Logical; if \code{TRUE}, dumps individual MCMC samples for further custom analysis.
  }

  \item{gui}{
    Logical; if \code{TRUE}, launches a GUI window that animates MCMC sampling in the model space (numbers and timings of seasonal and trend changepoints). This experimental GUI is currently available only on 64-bit Windows, not on 32-bit Windows, macOS, or Linux.
  }

  \item{\dots}{
    Additional implementation-level arguments passed to the underlying \code{beast123()} engine. For full control over advanced settings, use \code{\link[Rbeast]{beast123}} directly.
  }

}

\value{
  An object of class \code{"beast"}, i.e., a list with components similar to those returned by \code{\link[Rbeast]{beast}} and \code{\link[Rbeast]{beast123}}. Below we assume the input \code{y} is a single time series of length \code{N}:

  \item{time}{
    Numeric vector of length \code{N}; the regularized time axis after aggregating the original irregular series. If no explicit time is available, it defaults to \code{1:N}.
  }

  \item{data}{
    A vector, matrix, or 3D array containing a copy of the regularized input data if \code{extra$dumpInputData = TRUE}. Otherwise \code{NULL}. If the original input was irregular, this field stores the aggregated regular series at the time interval specified by \code{metadata$deltaTime}.
  }

  \item{marg_lik}{
    Numeric; the (average) model marginal likelihood. Larger values correspond to better fits for a given time series (e.g., \code{-1} is better than \code{-10}; \code{10} is better than \code{-1}).
  }

  \item{R2}{
    Numeric; coefficient of determination (\eqn{R^2}) for the fitted model.
  }

  \item{RMSE}{
    Numeric; root mean squared error of the fitted model.
  }

  \item{sig2}{
    Numeric; estimated variance of the model residuals.
  }

  \item{trend}{
    List containing summaries for the estimated trend component:

    \itemize{
      \item \code{ncp}: Mean number of trend changepoints across sampled models. Because individual models have varying numbers of changepoints, additional summaries are provided: \code{ncp_mode}, \code{ncp_median}, \code{ncp_pct90}. For example, if \code{mcmc$samples = 10} yields changepoint counts \code{c(0, 2, 4, 1, 1, 2, 7, 6, 6, 1)}, then the mean is 3.1, median 2.5, mode 1, and 90th percentile 6.5.

      \item \code{ncp_mode}: Mode of the posterior distribution of the number of trend changepoints.
      \item \code{ncp_median}: Median of the posterior distribution of the number of trend changepoints.
      \item \code{ncp_pct90}: 90th percentile of the posterior distribution of the number of trend changepoints.

      \item \code{ncpPr}: Vector of length \code{tcp.max + 1} where \code{tcp.max = tcp.minmax[2]}; gives the probability of having 0, 1, \dots, \code{tcp.max} trend changepoints. For example, \code{ncpPr[1]} is the probability of having no trend changepoint; \code{ncpPr[i]} is the probability of having \code{i - 1} changepoints.

      \item \code{cpOccPr}: Numeric vector of length \code{N}; the marginal probability at each time index of being a trend changepoint. Plotting \code{cpOccPr} yields a continuous curve of "probability of being a changepoint". \emph{Note}: a higher peak in this curve reflects a higher probability at that specific time index, but not necessarily a higher probability of a changepoint in a neighborhood around that time. For instance, a window \code{c(0, 0, 0.5, 0, 0)} (peak 0.5, sum 0.5) can be less likely to contain a changepoint than \code{c(0.1, 0.2, 0.21, 0.2, 0.1)} (peak 0.21, sum 0.71).

      \item \code{order}: Numeric vector of length \code{N}; average polynomial order of the trend at each time index, averaged over sampled piecewise polynomial trends. It is not necessarily an integer.

      \item \code{cp}: Numeric vector of length \code{tcp.max}; the most probable locations of trend changepoints. These are derived by smoothing \code{cpOccPr} with a window of length \code{tseg.min} and picking up to \code{prior$MaxKnotNum} (subject to \code{tcp.max}) of the highest peaks in the smoothed curve. Entries may be \code{NaN} if insufficient changepoints are identified. Many entries may be false positives; they should not be treated as confirmed changepoints without further scrutiny.

      \item \code{cpPr}: Numeric vector of length \code{tcp.max}; the probabilities associated with the detected changepoints in \code{cp}. Remaining entries are \code{NaN} if \code{ncp < tcp.max}.

      \item \code{cpCI}: Numeric matrix of dimension \code{tcp.max x 2}; credible intervals for the detected changepoints in \code{cp}.

      \item \code{cpAbruptChange}: Numeric vector of length \code{tcp.max}; the jumps in the fitted trend at the detected changepoints.

      \item \code{Y}: Numeric vector of length \code{N}; posterior mean estimate of the trend component (Bayesian model average over sampled models).

      \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the trend estimate.

      \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible intervals (lower and upper envelopes) for the trend component.

      \item \code{slp}: Numeric vector of length \code{N}; time-varying slope of the fitted trend.

      \item \code{slpSD}: Numeric vector of length \code{N}; posterior standard deviation of the trend slope.

      \item \code{slpSgnPosPr}: Numeric vector of length \code{N}; probability that the trend slope is positive (i.e., increasing). For example, \code{slpSgnPosPr[t] = 0.8} means that at time index \code{t}, 80\% of sampled trend curves have positive slope.

      \item \code{slpSgnZeroPr}: Numeric vector of length \code{N}; probability that the trend slope is approximately zero (flat). The probability of a negative slope is given by \code{1 - slpSgnZeroPr - slpSgnPosPr}.

      \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
            \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
            \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
            \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}: \cr
            As above, but distinguishing changepoints where the trend \emph{jumps up} (positive jump) versus \emph{jumps down} (negative jump). For example, \code{pos_ncp} is the average number of changepoints with positive jumps.

      \item \code{inc_ncp}, \code{dec_ncp}, \code{inc_ncpPr}, \code{dec_ncpPr},
            \code{inc_cpOccPr}, \code{dec_cpOccPr}, \code{inc_cp}, \code{dec_cp},
            \code{inc_cpPr}, \code{dec_cpPr}, \code{inc_cpAbruptChange},
            \code{dec_cpAbruptChange}, \code{inc_cpCI}, \code{dec_cpCI}: \cr
            Analogous to the above, but distinguishing changepoints where the trend \emph{slope} increases vs. decreases. For instance, if the slope changes from 0.4 to 2.5 at a changepoint, that changepoint contributes to \code{inc_ncp}.
    }
  }

  \item{season}{
    List containing summaries for the estimated seasonal/periodic component (if present):

    \itemize{
      \item \code{ncp}: Mean number of seasonal changepoints.
      \item \code{ncpPr}: Vector of length \code{scp.max + 1}, where \code{scp.max = scp.minmax[2]}; the probability of having 0, 1, \dots, \code{scp.max} seasonal changepoints.

      \item \code{cpOccPr}: Numeric vector of length \code{N}; marginal probability that each time index is a seasonal changepoint. The same interpretation caveats as for the trend component apply.

      \item \code{order}: Numeric vector of length \code{N}; average harmonic order required to approximate the seasonal component. This is an average over sampled piecewise harmonic curves and is not necessarily an integer.

      \item \code{cp}: Numeric vector of length \code{scp.max}; the most probable seasonal changepoint locations, derived from the smoothed \code{cpOccPr} curve using a window size of \code{sseg.min}. If \code{ncp < scp.max}, the remaining entries are filled with \code{NaN}.

      \item \code{cpPr}: Numeric vector of length \code{scp.max}; probabilities associated with \code{cp}. Remaining entries are \code{NaN} if \code{ncp < scp.max}.

      \item \code{cpCI}: Numeric matrix of dimension \code{scp.max x 2}; credible intervals for the detected seasonal changepoints.

      \item \code{cpAbruptChange}: Numeric vector of length \code{scp.max}; jumps in the fitted seasonal component at the detected changepoints.

      \item \code{Y}: Numeric vector of length \code{N}; posterior mean estimate of the seasonal component.

      \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the seasonal estimate.

      \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible intervals (lower and upper envelopes) for the seasonal component.

      \item \code{amp}: Numeric vector of length \code{N}; time-varying amplitude of the seasonal component.

      \item \code{ampSD}: Numeric vector of length \code{N}; posterior standard deviation of the amplitude.

      \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
            \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
            \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
            \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}: \cr
            Analogous to the trend component, but for seasonal changepoints that correspond to positive vs. negative jumps in the seasonal signal.
    }
  }
  
 \item{outlier}{
   \code{outlier} is a \code{list} analogous to \code{trend} or \code{season}, but for the outlier component ( present only if setting \code{hasOutlier=TRUE})
  } %\item{outlier}  

}

\seealso{
  \code{\link[Rbeast]{beast}},
  \code{\link[Rbeast]{beast123}},
  \code{\link[Rbeast]{minesweeper}},
  \code{\link[Rbeast]{tetris}},
  \code{\link[Rbeast]{geeLandsat}}
}

\note{
  The three functions \code{beast()}, \code{beast.irreg()}, and \code{beast123()} implement the same BEAST algorithm but expose different APIs:

  \itemize{
    \item \code{beast()} operates on regular (evenly spaced) time series.
    \item \code{beast.irreg()} accepts irregular/unordered data and internally aggregates it to regular intervals.
    \item \code{beast123()} exposes a low-level interface via four lists: \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra}.
  }

  There is a one-to-one correspondence between arguments of \code{beast()} / \code{beast.irreg()} and fields in \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra} used by \code{beast123()}, Examples include:
  \cr
  %\format{
	  \tabular{lcl}{
		\code{start}             \tab <-> \tab \code{metadata$startTime} \cr
		\code{deltat}            \tab <-> \tab \code{metadata$deltaTime} \cr
		\code{deseasonalize}     \tab <-> \tab \code{metadata$deseasonalize} \cr
		\code{hasOutlier}        \tab <-> \tab \code{metadata$hasOutlier}\cr
		\code{scp.minmax[1:2]}   \tab <-> \tab \code{prior$seasonMinKnotNum, prior$seasonMaxKnotNum}\cr
		\code{sorder.minmax[1:2]} \tab <-> \tab  \code{prior$seasonMinOrder, prior$seasonMaxOrder}\cr
		\code{sseg.min}           \tab <-> \tab \code{prior$seasonMinSepDist}\cr
		\code{tcp.minmax[1:2]}    \tab <-> \tab \code{prior$trendMinKnotNum, prior$trendMaxKnotNum}\cr
		\code{torder.minmax[1:2]} \tab <-> \tab \code{prior$trendMinOrder, prior$trendMaxOrder}\cr
		\code{tseg.leftmargin}    \tab <-> \tab \code{prior$trendLeftMargin}\cr
		\code{tseg.rightmargin}  \tab <-> \tab \code{prior$trendRightMargin}\cr
		\code{s.complexfct}      \tab <-> \tab \code{prior$seasonComplexityFactor}\cr
		\code{t.complexfct}      \tab <-> \tab \code{prior$trendComplexityFactor}\cr
		\code{mcmc.seed}         \tab <-> \tab \code{mcmc$seed}\cr
		\code{dump.ci}           \tab <-> \tab \code{extra$computeCredible}.\cr
	  }
  %} %\format
 
  Advanced users who need full control over all internal options are encouraged to use \code{beast123()} directly.
}

\examples{

library(Rbeast)

\donttest{

################################################################################
# Note: The BEAST algorithm is implemented for regular time series. 
# \code{beast.irreg} accepts irregular data but internally aggregates it to a 
# regular series before applying BEAST. For aggregation, both "time" and 
# "deltat" are needed to specify the original timestamps via "time" and the desired 
# regular interval via "deltat". If a cyclic component exists, "period" should also
# be provided; otherwise BEAST attempts to guess it via autocorrelation.
################################################################################

# 'ohio' is a data.frame containing an irregular Landsat time series of 
# reflectances and NDVI at an Ohio site. It includes multiple alternative 
# date formats: year (Y), month (M), day (D), day-of-year (doy), R "Date" 
# (rdate), and fractional year (time).

data(ohio)
str(ohio)
plot(ohio$rdate, ohio$ndvi, type = "o")  # NDVI is irregular and unordered in time

################################################################################
# Example 1: 'time' as numeric values
# Below, 'time' is given as numeric values, which can be of any arbitray unit. Although
# here 1/12 can be interepreted as 1/12 year, BEAST itself doesn't care about the unit. 
# So, the unit of 1/12 is irrelevant for BEAST. 'period' is missing
# and a guess of it is used.
################################################################################

o <- beast.irreg(ohio$ndvi, time = ohio$time, deltat = 1/12)
plot(o)
print(o)

################################################################################
# Example 2: Aggregate to a monthly interval (deltat = 1/12) and provide 'period'
################################################################################

o <- beast.irreg(ohio$ndvi, time = ohio$time, deltat = 1/12, period = 1.0)
# Alternative (deprecated argument 'freq'):
# o <- beast.irreg(ohio$ndvi, time = ohio$time, deltat = 1/12, freq = 12)

}

\dontrun{

################################################################################
# Example 3: Aggregate at a half-monthly interval.
# Here period = 1: freq = period/deltat = 1/(1/24)=24 data points per period
################################################################################

o <- beast.irreg(ohio$ndvi, time = ohio$time, deltat = 1/24, period = 1)

################################################################################
# Example 4: 'time' as  R Dates (i.e, ohio$rdate). Unit is YEAR; 1/12 is ~1 month.
################################################################################

o <- beast.irreg(ohio$ndvi, time = ohio$rdate, deltat = 1/12)

################################################################################
# Example 5: 'time' as date strings. The unit is YEAR such that 1/12 is a month
################################################################################

o=beast.irreg(ohio$ndvi, time=ohio$datestr1,deltat=1/12) #"LT4-1984-03-27"  (YYYY-MM-DD)
o=beast.irreg(ohio$ndvi, time=ohio$datestr2,deltat=1/12) #"LT4-1984087ndvi" (YYYYDOY)
o=beast.irreg(ohio$ndvi, time=ohio$datestr3,deltat=1/12) #"1984,, 3/ 27"    (YYYY M D)

################################################################################
# Example 6: 'time' as date strings with explicit format specifiers
################################################################################

TIME <- list()
TIME$datestr <- ohio$datestr1
TIME$strfmt  <- "LT4-YYYY-MM-DD"    # e.g., "LT4-1984-03-27"
o <- beast.irreg(ohio$ndvi, time = TIME, deltat = 1/12)

TIME <- list()
TIME$datestr <- ohio$datestr2
TIME$strfmt  <- "LT4-YYYYDOYndvi"   # e.g., "LT4-1984087ndvi"
o <- beast.irreg(ohio$ndvi, time = TIME, deltat = 1/12)

################################################################################
# Example 7: 'time' as a list of date components
################################################################################

TIME <- list()
TIME$year  <- ohio$Y
TIME$month <- ohio$M
TIME$day   <- ohio$D
o <- beast.irreg(ohio$ndvi, time = TIME, deltat = 1/12)

TIME <- list()
TIME$year <- ohio$Y
TIME$doy  <- ohio$doy
o <- beast.irreg(ohio$ndvi, time = TIME, deltat = 1/12)

}

}

\references{
  \enumerate{
    \item Zhao, K., Wulder, M. A., Hu, T., Bright, R., Wu, Q., Qin, H., Li, Y., Toman, E., Mallick, B., Zhang, X. and Brown, M. (2019).
    Detecting change-point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics:
    A Bayesian ensemble algorithm. \emph{Remote Sensing of Environment}, 232, 111181. (The main BEAST algorithm paper.)

    \item Zhao, K., Valle, D., Popescu, S., Zhang, X. and Mallick, B. (2013).
    Hyperspectral remote sensing of plant biochemistry using Bayesian model averaging with variable and band selection.
    \emph{Remote Sensing of Environment}, 132, 102--119. (The Bayesian MCMC scheme used in BEAST.)

    \item Hu, T., Toman, E. M., Chen, G., Shao, G., Zhou, Y., Li, Y., Zhao, K. and Feng, Y. (2021).
    Mapping fine-scale human disturbances in a working landscape with Landsat time series on Google Earth Engine.
    \emph{ISPRS Journal of Photogrammetry and Remote Sensing}, 176, 250--261. (Example application of BEAST.)
  }
}

\keyword{misc}
