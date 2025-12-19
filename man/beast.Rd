\name{beast}
\docType{methods}
\alias{beast}
\alias{Beast}
\alias{BEAST}
\alias{rbeast}
\alias{Rbeast}
\alias{RBEAST}

\title{Bayesian changepoint detection and time series decomposition into trend, seasonality, and abrupt changes}

\description{
\code{beast} is a high-level interface to the BEAST algorithm, a Bayesian model averaging approach for decomposing regular time series or other 1D sequential data into individual components, including abrupt changes, trends, and periodic/seasonal variations. BEAST is useful for changepoint detection (e.g., breakpoints, joinpoints, or structural breaks), nonlinear trend analysis, time series decomposition, and time series segmentation.
\cr\cr

   % checkRd reports the error for the following: "height/width attributes should be in pixels"
   %  \if{html}{\figure{Nile.jpg}{options: width="95\%" alt="Figure: Nile.jpg"}}
   
   % package biplotEZ:  the folllowing passes the check.
   % \if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}
   
   % package inca::inca.Rd:  the folllowing passes the check.
   % \if{html}{\figure{logo.png}{options: style='float:right;width:156px;'}}
   
   % package frailtyMMpen::frailtyMM.Rd:  the folllowing passes the check.
   %\if{html}{\figure{fig1.png}{options: style="width:750px;max-width:75\%;"}}
   
   
\if{html}{\figure{Nile.jpg}{options: style="width:80\%;max-width:95\%" alt="Figure: Nile.jpg"}}
\if{latex}{\figure{Nile.pdf}{options: width=10cm}}
}

\usage{
beast(
  y,
  start          = 1,
  deltat         = 1,
  season         = c("harmonic", "svd", "dummy", "none"),
  period         = NULL,
  scp.minmax     = c(0, 10), sorder.minmax   = c(0, 5),
  tcp.minmax     = c(0, 10), torder.minmax   = c(0, 1),
  sseg.min       = NULL,     sseg.leftmargin = NULL, sseg.rightmargin = NULL,
  tseg.min       = NULL,     tseg.leftmargin = NULL, tseg.rightmargin = NULL,
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
  \code{y} is a numeric vector representing an evenly spaced regular time series. Missing values such as \code{NA} and \code{NaN} are allowed.
  \itemize{
    \item If \code{y} is irregular or unordered in time (e.g., daily data spanning multiple years with leap years: 365 points in some years, 366 in others), use \code{\link[Rbeast]{beast.irreg}} instead.
    \item If \code{y} is a matrix or 3D array consisting of multiple regular or irregular time series (e.g., stacked images), use \code{\link[Rbeast]{beast123}} instead.
    \item If \code{y} is an object of class \code{"ts"}, \code{"xts"}, or \code{"zoo"}, its time attributes (i.e., \code{start}, \code{end}, \code{frequency}) are used to infer \code{start}, \code{deltat}, \code{period}, and \code{season}. User-specified values for these arguments are then ignored to honor the time attributes of \code{y}. For example, if \code{y} is a ts object with \code{frequency = 1}, \code{season = "none"} is always assumed; if \code{frequency > 1} (i.e., some periodic component is present in \code{y}) but \code{season = "none"} is supplied, it is silently replaced by \code{season="harmonic"}.
  }
  If \code{y} is provided as a \emph{list} of multiple time series, the multivariate version of the BEAST algorithm is invoked to decompose multiple series and detect common changepoints jointly. This feature is \emph{experimental} and under active development. See \code{\link[Rbeast]{ohio}} for a working example.
  }

  \item{start}{
  \code{start} is numeric (default \code{1.0}) or \code{Date}; the time of the first data point of \code{y}. It can be specified as:
  \itemize{
    \item a scalar (e.g., \code{2021.0644});
    \item a numeric vector \code{c(year, month, day)} (e.g., \code{c(2021, 1, 24)});
    \item an \code{R} \code{Date} object (e.g., \code{as.Date("2021-01-24")}).
  }
  }

  \item{deltat}{
  \code{deltat} is numeric (default \code{1.0}) or character string; the time interval between consecutive data points. Its unit must be consistent with \code{start}.
  \itemize{
    \item If \code{start} is numeric, the unit is arbitrary and is not interpreted by BEAST (e.g., \code{2021.3} can be be of any unit, a time, a distance, etc.).
    \item If \code{start} is a \code{c(year, month, day)} vector or a \code{Date}, \code{deltat} is interpreted in units of years. For example, for a monthly series with \code{start = c(2021, 1, 24)}, \code{start} is internally converted to a fractional year \code{2021 + (24 - 0.5)/365 = 2021.0644} and \code{deltat = 1/12} can be used to specify a 1-month interval (1/12 yr).
    \item Alternatively, \code{deltat} can be a character string encoding both the value and the unit, such as \code{"7 days"}, \code{"7d"}, \code{"1/2 months"}, \code{"1 mn"}, \code{"1 year"}, or \code{"1y"}.
  }
  }

  \item{season}{
  Character (default \code{"harmonic"}). Specifies whether \code{y} has a periodic component and how it is modeled:
  \itemize{
    \item \code{"none"}: \code{y} is treated as trend-only; no periodic components are modeled. Seasonal parameters (e.g., \code{sorder.minmax}, \code{scp.minmax}, \code{sseg.min}) are ignored.
    \item \code{"harmonic"}: \code{y} has a periodic/seasonal component. The term \code{season} is used broadly for any periodic variation in \code{y}. The period of the seasonal component is not a BEAST model parameter; it is treated as known and must be supplied via \code{period}. For \code{"harmonic"}, the seasonal component is modeled as a harmonic curve (a combination of sines and cosines).
    \item \code{"dummy"}: As for \code{"harmonic"}, except that the periodic/seasonal component is modeled as a non-parametric curve. The harmonic order argument \code{sorder.minmax} is then irrelevant and ignored.
    \item \code{"svd"} (experimental): As for \code{"harmonic"}, but with the seasonal component expressed as a linear combination of basis functions derived from a singular value decomposition (SVD). These basis functions can be more parsimonious than pure harmonic bases and may improve detection of subtle seasonal changepoints.
  }
  }

  \item{period}{
  Numeric or character string. The period of the seasonal/periodic component in \code{y}. It is needed only when a periodic component is present (e.g., \code{season = "harmonic"}, \code{"svd"}, or \code{"dummy"}) and is ignored for trend-only data (\code{season = "none"}). The period must be in the same time unit as \code{deltat}, and it should satisfy \code{period = deltat * freq}, where \code{freq} is the number of data points per period.
  \itemize{
    \item In earlier versions, a separate \code{freq} argument was used; it is now obsolete and replaced by \code{period}. For backward compatibility, \code{freq} is still accepted via \code{...}, but \code{period} takes precedence if both are provided.
    \item If \code{period} is missing, BEAST attempts to guess a plausible period via autocorrelation. This guess can be unreliable, so users are strongly encouraged to specify \code{period} explicitly whenever a periodic component is present.
    \item If \code{period <= 0}, \code{season = "none"} is assumed and a trend-only model is fitted.
    \item When \code{start} or \code{deltat} is specified using dates, \code{period} may also be provided as a string, such as \code{"1 year"}, \code{"12 months"}, \code{"365d"}, or \code{"366 days"}.
  }
  }

  \item{scp.minmax}{
  A numeric vector of length 2 (integers \code{>= 0}); the minimum and maximum numbers of seasonal changepoints (scp) allowed in segmenting the seasonal component. Used only when \code{y} has a seasonal component (i.e., \code{season = "harmonic"}, \code{"svd"}, or \code{"dummy"}) and ignored for trend-only data. \code{scp.minmax[2]} couldn't be smaller than \code{scp.minmax[1]}.
  \itemize{
    \item If \code{scp.minmax[1] == scp.minmax[2]}, BEAST assumes a fixed number of seasonal changepoints and does not infer the posterior distribution over the number of changepoints, though it still estimates the probabilities and locations of most likely changepoints over time.
    \item If \code{scp.minmax[1] == scp.minmax[2] == 0}, both the min and max numbers of scp are set to 0. That is, no seasonal changepoints are allowed; a single global seasonal model is used, but the harmonic order may still be inferred if \code{sorder.minmax[1] != sorder.minmax[2]}.
  }
  }

  \item{sorder.minmax}{
  A numeric vector of length 2 (integers \code{>= 1}); the minimum and maximum harmonic orders considered for the seasonal component. Used only when \code{season = "harmonic"} or \code{"svd"} and ignored for trend-only data or when \code{season = "dummy"}.
  \itemize{
    \item If \code{sorder.minmax[1] == sorder.minmax[2]}, BEAST assumes a fixed harmonic order and does not infer the posterior distribution over harmonic orders.
  }
  }

  \item{tcp.minmax}{
  A numeric vector of length 2 (integers \code{>= 0}); the minimum and maximum numbers of trend changepoints (tcp) allowed in segmenting the trend component. 
  \itemize{
    \item If \code{tcp.minmax[1] == tcp.minmax[2]}, BEAST assumes a fixed number of trend changepoints and does not infer the posterior distribution over the number of changepoints, though it still estimates changepoint probabilities and locations.
    \item If \code{tcp.minmax[1] == tcp.minmax[2] == 0}, both the min and max numbers are set to 0. That is, no trend changepoints are allowed, and a single global polynomial trend is fitted. The polynomial order may still be inferred if \code{torder.minmax[1] != torder.minmax[2]}.
  }
  }

  \item{torder.minmax}{
  A numeric vector of length 2 (integers \code{>= 0}); the minimum and maximum polynomial orders considered for the trend component. If missing, \code{tcp.minmax} defaults to \code{c(0,1)}. Order 0 corresponds to a constant (flat) trend; order 1 corresponds to a linear trend.
  \itemize{
    \item If \code{torder.minmax[1] == torder.minmax[2]}, BEAST assumes a fixed polynomial order and does not infer the posterior distribution over polynomial orders.
  }
  }

  \item{sseg.min}{
  An integer (\code{> 0}); the minimum segment length, in number of time steps, allowed between neighboring seasonal changepoints. When fitting a piecewise seasonal model, two seasonal changepoints cannot be closer than \code{sseg.min} time intervals.
  \code{sseg.min} is unitless (counts of time steps); the corresponding time window in the underlying units is \code{sseg.min * deltat}. If \code{NULL}, a default is chosen internally (typically based on the the period).
  }

  \item{sseg.leftmargin}{
  Integer (\code{>= 0}); the number of leftmost observations excluded from seasonal changepoint detection. No seasonal changepoints are allowed in the initial window of length \code{sseg.leftmargin}. This is specified in time steps; the corresponding time window is \code{sseg.leftmargin * deltat}. If \code{NULL}, it defaults to \code{sseg.min}.
  }

  \item{sseg.rightmargin}{
  Integer (\code{>= 0}); the number of rightmost observations excluded from seasonal changepoint detection. No seasonal changepoints are allowed in the final window of length \code{sseg.rightmargin}. This is specified in time steps; the corresponding time window is \code{sseg.rightmargin * deltat}. If \code{NULL}, it defaults to \code{sseg.min}.
  }

  \item{tseg.min}{
  Integer (\code{> 0}); the minimum segment length, in number of time steps, allowed between neighboring trend changepoints. When fitting a piecewise polynomial trend, two trend changepoints cannot be closer than \code{tseg.min} time intervals.
  \code{tseg.min} is unitless (counts of time steps); the corresponding time window is \code{tseg.min * deltat}. If \code{NULL}, a default is chosen internally (often in reference to the presence or absence of a cyclic component).
  }

  \item{tseg.leftmargin}{
  Integer (\code{>= 0}); the number of leftmost observations excluded from trend changepoint detection. No trend changepoints are allowed in the initial window of length \code{tseg.leftmargin}. This is specified in time steps; the corresponding time window is \code{tseg.leftmargin * deltat}. If \code{NULL}, it defaults to \code{tseg.min}.
  }

  \item{tseg.rightmargin}{
  Integer (\code{>= 0}); the number of rightmost observations excluded from trend changepoint detection. No trend changepoints are allowed in the final window of length \code{tseg.rightmargin}. This is specified in time steps; the corresponding time window is \code{tseg.rightmargin * deltat}. If \code{NULL}, it defaults to \code{tseg.min}.
  }

  \item{s.complexfct}{
  Numeric (default \code{0.0}); a hyperprior parameter--newly added in Version 1.02--controlling the complexity of the seasonal curve (i.e., the number of seasonal changepoints). A prior of the form \eqn{ p(k) \propto \exp[\lambda (k+1)]} is placed on the number of seasonal changepoints \eqn{k}, where \eqn{\lambda} is \code{seasonComplexityFactor}. Setting \eqn{\lambda = 0} yields a non-informative prior \eqn{ p(k) \propto 1.0}  where all model dimensions are equally likely \emph{a priori}. Users may tune \code{seasonComplexityFactor} in the range \code{[-20, 20]} or an even wider range: negative values (e.g., \eqn{\lambda = -15.9}) favor fewer changepoints (simpler seasonal curves), whereas positive values (e.g., \eqn{\lambda = 5.76}) favor more changepoints (more complex curves).
  }

  \item{t.complexfct}{
  Numeric (default \code{0.0}); analogous to \code{s.complexfct}, but for the trend component and the number of trend changepoints.
  }

  \item{method}{
  Character (default \code{"bayes"}). Specifies how model posterior probabilities are formulated or approximated:
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

  \item{detrend}{
  Logical; if \code{TRUE}, a global trend is first fitted and removed from the series before running BEAST, and then added back to the BEAST result at the end.
  }

  \item{deseasonalize}{
  Logical; if \code{TRUE}, a global seasonal model is first fitted and removed before running BEAST, and then added back to the BEAST result. Ignored if \code{season = "none"} (trend-only data).
  }

  \item{mcmc.seed}{
  Integer (\code{>= 0}); seed for the random number generator used in the MCMC. If \code{mcmc.seed = 0}, an arbitrary seed is used and results may vary across runs. Using the same non-zero seed makes results reproducible on the same machine, though differences across platforms/CPUs may still arise due to differences in random number libraries or CPU instruction sets.
  }

  \item{mcmc.chains}{
  Integer (\code{> 0}); the number of parallel MCMC chains.
  }

  \item{mcmc.thin}{
  Integer (\code{> 0}); thinning factor. If \code{mcmc.thin = k}, every \code{k}-th iteration is retained and the others are discarded.
  }

  \item{mcmc.burnin}{
  Integer (\code{> 0}); number of initial iterations discarded as burn-in for each chain.
  }

  \item{mcmc.samples}{
  Integer (\code{>= 0}); number of samples collected per MCMC chain. The total number of iterations is \code{(mcmc.burnin + mcmc.samples * mcmc.thin) * mcmc.chains}.
  }

  \item{precValue}{
  Numeric (\code{> 0}); hyperparameter for the precision priors on model coefficients. The default is \code{1.5}. It is used directly only when \code{precPriorType = "constant"} (see below); in other cases it serves as an initial value.
  }

  \item{precPriorType}{
  Character; one of \code{"constant"}, \code{"uniform"}, \code{"componentwise"} (default), or \code{"orderwise"}. These control how precision parameters for the model coefficients are treated:
  \enumerate{
    \item \code{"constant"}: A single precision parameter is fixed at \code{precValue}. The fit may be sensitive to the chosen value.
    \item \code{"uniform"}: A single precision parameter is treated as a random variable with initial value \code{precValue}; its posterior is inferred via MCMC. The results are less sensitive to the initial choice of \code{precValue}.
    \item \code{"componentwise"}: Separate precision parameters are used for different components (e.g., season vs. trend), each initialized by \code{precValue} and inferred via MCMC.
    \item \code{"orderwise"}: Separate precision parameters are used for different components and for different orders within each component, all initialized by \code{precValue} and inferred via MCMC.
  }
  }

  \item{hasOutlier}{
  Logical; if \code{TRUE}, fit a model with an outlier component representing potential spikes or dips at isolated data points:
  \itemize{
    \item \code{Y = trend + outlier + error} if \code{season = "none"},
    \item \code{Y = trend + season + outlier + error} otherwise.
  }
  }

  \item{ocp.minmax}{
  Numeric vector of length 2 (integers \code{>= 0}); the minimum and maximum numbers of outlier-type changepoints (ocp) allowed in the series. Outliers refer to spikes or dips at isolated times that cannot be captured by the trend or seasonal components.
  }

  \item{print.param}{
  Logical; if \code{TRUE}, the full list of internal BEAST parameters (\code{metadata}, \code{prior}, \code{mcmc}, \code{extra}) is printed before MCMC begins. These internal objects correspond one-to-one with the arguments of \code{beast}. For example, \code{prior$trendMinSepDist} corresponds to \code{tseg.min}. See \code{\link[Rbeast]{beast123}} or inspect the source of \code{beast} for details.
  }

  \item{print.progress}{
  Logical; if \code{TRUE}, a progress bar is printed.
  }

  \item{print.warning}{
  Logical; if \code{TRUE}, warning messages are printed.
  }

  \item{quiet}{
  Logical; if \code{TRUE}, suppress most console output (overrides \code{print.param} and \code{print.progress}).
  }

  \item{dump.ci}{
  Logical; if \code{TRUE}, credible intervals for the trend and seasonal components (e.g., \code{out$trend$CI}, \code{out$season$CI}) are computed and returned. Computing credible intervals is relatively time-consuming (due to sorting and summarizing many MCMC samples). If only symmetric uncertainty summaries are needed, the posterior standard deviations \code{out$trend$SD} and \code{out$season$SD} may suffice and \code{dump.ci} can be set to \code{FALSE}.
  }

  \item{dump.mcmc}{
  Logical; if \code{TRUE}, individual MCMC samples are dumped to the output.
  }

  \item{gui}{
  Logical; if \code{TRUE}, BEAST runs with a GUI window showing an animation of the MCMC sampling in model space (numbers and locations of trend/seasonal changepoints). This feature is \emph{experimental} and currently available only on 64-bit Windows systems (not 32-bit Windows, macOS, or Linux).
  }

  \item{\dots}{
  Additional arguments reserved for future extensions and backward compatibility. Certain low-level settings are only available via \code{beast123()} (through \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra}), not via \code{beast()}.
  }
}

\value{
The result is an object of class \code{"beast"}, i.e., a list with structure identical to the outputs of \code{\link[Rbeast]{beast.irreg}} and \code{\link[Rbeast]{beast123}}. The exact array dimensions depend on the input \code{y}. Below we assume a single time series input of length \code{N}.

  \item{time}{
  Numeric vector of length \code{N}; the time associated with each observation. By default, this is simply \code{1:N} if \code{start}, \code{deltat}, and any time attributes are missing.
  }

  \item{data}{
  A vector, matrix, or 3D array; a copy of the input \code{y} if the underlying \code{extra$dumpInputData = TRUE}. If \code{extra$dumpInputData = FALSE}, this is \code{NULL}. For irregular inputs (as in \code{\link[Rbeast]{beast.irreg}}), this field stores the aggregated regular series at time intervals spaced by \code{deltat} (or \code{metadata$deltaTime} in the \code{beast123} interface).
  }

  \item{marg_lik}{
  Numeric; the average marginal likelihood of the sampled models. Larger values indicate better fit for a given time series (e.g., \code{-1} is better than \code{-10}; \code{10} is better than \code{-1} or \code{-10}).
  }

  \item{R2}{
  Numeric; coefficient of determination (R-squared) of the fitted model.
  }

  \item{RMSE}{
  Numeric; root mean squared error of the fitted model.
  }

  \item{sig2}{
  Numeric; estimated variance of the residual error.
  }

  \item{trend}{
  \code{trend} is a \code{list} of outputs related to the estimated trend component:
  \itemize{
    \item \code{ncp}: Mean number of trend changepoints. Since individual sampled models can have different numbers of changepoints, several alternative summaries (\code{ncp_mode}, \code{ncp_median}, \code{ncp_pct90}) are also provided. For example, if \code{mcmc$samples = 10} and the numbers of changepoints across models are \code{c(0, 2, 4, 1, 1, 2, 7, 6, 6, 1)}, then the mean \code{ncp} is \code{3.1}, the median \code{2.5}, the mode \code{1}, and the 90th percentile (\code{ncp_pct90}) \code{6.5}.
    \item \code{ncp_mode}: Mode of the number of trend changepoints.
    \item \code{ncp_median}: Median of the number of trend changepoints.
    \item \code{ncp_pct90}: 90th percentile of the number of trend changepoints.
    \item \code{ncpPr}: Numeric vector of length \code{tcp.minmax[2] + 1}; probability distribution over the number of trend changepoints in \code{0:tcp.minmax[2]}. For example, \code{ncpPr[1]} is the probability of having no trend changepoint; \code{ncpPr[i]} is the probability of having \code{i - 1} changepoints.
    \item \code{cpOccPr}: Numeric vector of length \code{N}; changepoint occurrence probability at each time index for the trend component. Plotting \code{cpOccPr} yields a continuous "probability-of-being-a-changepoint" curve. A higher peak at a single time step does \emph{not} necessarily imply a higher \emph{overall} probability of a changepoint in a neighborhood compared to a broader peak with lower maximum but higher summed probability. For example, a window of \code{cpOccPr} values \code{c(0, 0, 0.5, 0, 0)} (peak 0.5, sum 0.5) likely indicates a smaller chance of a changepoint than another window \code{c(0.1, 0.2, 0.21, 0.2, 0.1)} (peak 0.21, sum 0.71).
    \item \code{order}: Numeric vector of length \code{N}; average polynomial order of the trend over sampled models at each time step. As a posterior average, it need not be an integer.
    \item \code{cp}: Numeric vector of length up to \code{tcp.minmax[2]}; estimated trend changepoint locations. These are obtained by smoothing \code{cpOccPr} with a window of size \code{tseg.min} and selecting up to \code{tcp.minmax[2]} peaks from the smoothed curve. Some entries may be \code{NaN} if fewer changepoints are identified. Not all reported changepoints should be treated as "true"; some may be false positives. Check the associated posterior probabilities (\code{cpPr}) for confidence.
    \item \code{cpPr}: Numeric vector of length \code{tcp.minmax[2]}; posterior probability associated with each changepoint in \code{cp}. Trailing entries are \code{NaN} if fewer changepoints are identified.
    \item \code{cpCI}: Numeric matrix of dimension \code{tcp.minmax[2] x 2}; credible intervals for changepoint locations in \code{cp}.
    \item \code{cpAbruptChange}: Numeric vector of length \code{tcp.minmax[2]}; magnitude of jumps in the trend at the detected changepoints.
    \item \code{Y}: Numeric vector of length \code{N}; estimated trend component (Bayesian model average over sampled trends).
    \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the trend.
    \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible interval for the trend, with lower and upper envelopes.
    \item \code{slp}: Numeric vector of length \code{N}; time-varying slope of the trend component.
    \item \code{slpSD}: Numeric vector of length \code{N}; posterior standard deviation of the slope.
    \item \code{slpSgnPosPr}: Numeric vector of length \code{N}; posterior probability that the slope is positive at each time point (i.e., increasing trend). For example, \code{slpSgnPosPr = 0.80} at a given time indicates that 80\% of sampled trends have a positive slope there.
    \item \code{slpSgnZeroPr}: Numeric vector of length \code{N}; posterior probability that the slope is effectively zero at each time point. The probability of negative slope can be obtained as \code{1 - slpSgnZeroPr - slpSgnPosPr}.
    \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
          \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
          \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
          \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}:
      As above, but restricted to trend changepoints with positive jumps (\code{pos_*}) or negative jumps (\code{neg_*}) in the trend. For example, \code{pos_ncp} is the mean number of trend changepoints where the trend level jumps up.
    \item \code{inc_ncp}, \code{dec_ncp}, \code{inc_ncpPr}, \code{dec_ncpPr},
          \code{inc_cpOccPr}, \code{dec_cpOccPr}, \code{inc_cp}, \code{dec_cp},
          \code{inc_cpPr}, \code{dec_cpPr}, \code{inc_cpAbruptChange},
          \code{dec_cpAbruptChange}, \code{inc_cpCI}, \code{dec_cpCI}:
      As above, but restricted to changepoints where the trend slope increases (\code{inc_*}) or decreases (\code{dec_*}). For example, if the trend slope changes from \code{0.4} to \code{2.5}, that changepoint contributes to \code{inc_ncp}.
  }
  }

  \item{season}{
   \code{season} is a \code{list} analogous to \code{trend}, but for the seasonal/periodic component (when present):
  \itemize{
    \item \code{ncp}: Mean number of seasonal changepoints.
    \item \code{ncp_mode}, \code{ncp_median}, \code{ncp_pct90}: As for \code{trend}, but for seasonal changepoints.
    \item \code{ncpPr}: Numeric vector of length \code{scp.minmax[2] + 1}; probability distribution over the number of seasonal changepoints in \code{0:scp.minmax[2]}. For example, \code{ncpPr[1]} is the probability of having no seasonal changepoint.
    \item \code{cpOccPr}: Numeric vector of length \code{N}; seasonal changepoint occurrence probability over time. The same caveat applies as for \code{trend$cpOccPr}: the height of a single peak does not fully determine the overall probability of a changepoint in its neighborhood.
    \item \code{order}: Numeric vector of length \code{N}; average harmonic order needed to approximate the seasonal component. As a posterior average over piecewise harmonic/SVD-based curves, this need not be an integer.
    \item \code{cp}, \code{cpPr}, \code{cpCI}, \code{cpAbruptChange}:
      Analogous to the trend fields, but for the seasonal component, with length determined by \code{scp.minmax[2]}.
    \item \code{Y}: Numeric vector of length \code{N}; estimated seasonal component (Bayesian model average).
    \item \code{SD}: Numeric vector of length \code{N}; posterior standard deviation of the seasonal component.
    \item \code{CI}: Numeric matrix of dimension \code{N x 2}; credible interval for the seasonal component.
    \item \code{amp}: Numeric vector of length \code{N}; time-varying amplitude of the seasonal component.
    \item \code{ampSD}: Numeric vector of length \code{N}; posterior standard deviation of the amplitude.
    \item \code{pos_ncp}, \code{neg_ncp}, \code{pos_ncpPr}, \code{neg_ncpPr},
          \code{pos_cpOccPr}, \code{neg_cpOccPr}, \code{pos_cp}, \code{neg_cp},
          \code{pos_cpPr}, \code{neg_cpPr}, \code{pos_cpAbruptChange},
          \code{neg_cpAbruptChange}, \code{pos_cpCI}, \code{neg_cpCI}:
      Seasonal analogues of the trend outputs with the same names, restricted to seasonal changepoints with positive (\code{pos_*}) or negative (\code{neg_*}) jumps in the seasonal curve. For example, \code{pos_ncp} refers to the average number of seasonal changepoints at which the seasonal component jumps up.
  }
  }
  
 \item{outlier}{
   \code{outlier} is a \code{list} analogous to \code{trend} or \code{season}, but for the outlier component ( present only if setting \code{hasOutlier=TRUE})
  } %\item{outlier}
  
}

\seealso{
  \code{\link[Rbeast]{beast.irreg}},
  \code{\link[Rbeast]{beast123}},
  \code{\link[Rbeast]{minesweeper}},
  \code{\link[Rbeast]{tetris}},
  \code{\link[Rbeast]{geeLandsat}}
}

\note{
The three functions \code{beast()}, \code{beast.irreg()}, and \code{beast123()} implement the same BEAST algorithm but with different APIs. There is a one-to-one correspondence between (1) the arguments of \code{beast()} / \code{beast.irreg()} and (2) the \code{metadata}, \code{prior}, \code{mcmc}, and \code{extra} lists used by \code{beast123()}. Examples include:
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
 

For large data sets, irregular time series, or stacked images, \code{beast123()} is generally the recommended interface.
}

\examples{
library(Rbeast)

\donttest{

#------------------------------------Example 1----------------------------------------#
# 'googletrend_beach' is the monthly Google Trends popularity of searching for 'beach'
# in the US from 2004 to 2022. Sudden changes coincide with known extreme weather
# events (e.g., 2006 North American Blizzard, 2011 record US summer heat, record warm
# January in 2016) or the COVID-19 outbreak.

out <- beast(googletrend_beach)

plot(out)
plot(out, vars = c("t", "slpsgn"))  # plot trend and slope-sign probabilities only.
                                    # In the slpsgn panel, the upper red band is the
                                    # probability that the trend slope is positive,
                                    # the middle green band the probability that the
                                    # slope is effectively zero, and the lower blue
                                    # band the probability that it is negative.
                                    # See ?plot.beast for details.

#------------------------------------Example 2----------------------------------------#
# Yellowstone is a half-monthly satellite NDVI time series of length 774 starting
# from July 1-15, 1981 (start ~ c(1981, 7, 7)) at a forest site in Yellowstone.
# There are 24 data points per year. The 1988 Yellowstone Fire caused a sudden drop
# in greenness. Note that the beast function  hanldes only evenly-spaced regular time
# series. Irregular data need to be first  aggegrated at a regular time interval of 
# your choice--the aggregation functionality is implemented in beast.irreg() and beast123().

data(Yellowstone)
plot(1981.5 + (0:773) / 24, Yellowstone, type = "l") # A sudden drop in greenness in 1988 
                                                     # due to the 1988 Yellowstone Fire

# Yellowstone is a plain numeric vector (no time attributes). Below, no extra
# arguments are supplied, so default values (start = 1, deltat = 1) are used and
# time is 1:774. The period is missing and is guessed via autocorrelation.
# Autocorrelation-based period estimates can be unreliable; examples below show
# how to specify time/period explicitly.

o <- beast(Yellowstone)   # By default, time = 1:length(Yellowstone), season = "harmonic"
plot(o)

# o <- beast(Yellowstone, quiet = TRUE)                        # suppress warnings
# o <- beast(Yellowstone, quiet = TRUE, print.progress = FALSE) # suppress all output

#------------------------------------Example 3----------------------------------------#
# Time information (start, deltat, period) specified explicitly of Yellowstone.

# (1) Arbitrary unit: 1981.5137 can be interpreted flexibly in any units not neccessarily
# referting to times
o <- beast(Yellowstone, start = 1981.5137, deltat = 1/24, period = 1.0)

# Strings can be used to explicitly specify time unitsas dates or years:
# o <- beast(Yellowstone, start = 1981.5137, deltat = "1/24 year", period = 1.0)
# o <- beast(Yellowstone, start = 1981.5137, deltat = "0.5 mon",   period = 1.0)
# o <- beast(Yellowstone, start = 1981.5137, deltat = 1/24, period = "1 year")
# o <- beast(Yellowstone, start = 1981.5137, deltat = 1/24, period = "365 days")

# (2) start as year-month-day (unit is year: deltat = 1/24 year = 0.5 month)
# o <- beast(Yellowstone, start = c(1981, 7, 7), deltat = 1/24, period = 1.0)

# (3) start as Date (unit is year: deltat = 1/24 year = 0.5 month)
# o <- beast(Yellowstone, start = as.Date("1981-07-07"), deltat = 1/24, period = 1.0)

print(o)     # 'o' is a list with many fields
str(o)       # See a list of fields in the BEAST output o

plot(o)                            # plot many default variables
plot(o, vars = c("y","s","t"))     # plot the data, seasonal, and trend components only
plot(o, vars = c("s","scp","samp","t","tcp","tslp")) # selected variables (see ?plot.beast)
plot(o, vars = c("s","t"), col = c("red","blue"))    # Specify colors of selected subplots

plot(o$time, o$season$Y, type = "l")    # fitted seasonal component
plot(o$time, o$season$cpOccPr)          # seasonal changepoint probabilities
plot(o$time, o$trend$Y, type = "l")     # fitted trend component
plot(o$time, o$trend$cpOccPr)           # trend changepoint probabilities
plot(o$time, o$season$order)            # average harmonic order over time

plot(o, interactive = TRUE)            # interactively choose variables to plot

}
\donttest{

#------------------------------------Example 4----------------------------------------#
# Specify other arguments explicitly.  Default values are used for missing parameters.
# Note that beast(), beast.irreg(), and beast123() call the same internal C/C++ library,
# so in beast(), the input parameters will be converted to metadata, prior, mcmc, and 
# extra parameters as explained for the beast123() function. Or type 'View(beast)' to 
# check the parameter assignment in the code.
 
 
 # In R's terminology, the  number of datapoints per period is also called 'freq'. In this
 # version, the 'freq' argument is obsolete and replaced by 'period'.
 
 # period=deltat*number_of_datapoints_per_period = 1.0*24=24 because deltat is set to 1.0 by
 # default and this signal has 24 samples per period. 
 o = beast(Yellowstone, period=24.0, mcmc.samples=5000, tseg.min=20)
 
 # period=deltat*number_of_datapoints_per_period = 1/24*24=1.0.
 # o = beast(Yellowstone, deltat=1/24 period=1.0, mcmc.samples=5000, tseg.min=20)
  
 o = beast( 
     Yellowstone,            # Yellowstone: a pure numeric vector wo time info
     start   = 1981.51, 
     deltat  = 1/24,         
     period  = 1.0,           # Period=delta*number_of_datapoints_per_period
     season  = 'harmonic',    # Periodic compnt exisits,fitted as a harmonic curve 
     scp.minmax     = c(0,3), # Min and max numbers of seasonal changpts allowed
     sorder.minmax  = c(1,5), # Min and max harmonic orders allowed
     sseg.min       = 24,     # The min length of segments btw neighboring chnpts
	                          # '24' means 24 datapoints; the unit is datapoint.
     sseg.leftmargin= 40,     # No seasonal chgpts allowed in the starting 40 datapoints
     tcp.minmax     = c(0,10),# Min and max numbers of changpts allowed in the trend
     torder.minmax  = c(0,1), # Min and maxx polynomial orders to fit trend
     tseg.min       = 24,     # The min length of segments btw neighboring trend chnpts
     tseg.leftmargin= 10,     # No trend chgpts allowed in the starting 10 datapoints
     s.complexfct   = 0.26,   # Manually tune it to fit a more complex seasonal curve
	                          #  than the non-informative prior on number of changepts
     t.complexfct   = -5.2,   # Manually tune it to fit a more complex trend curve
	                          #  than the non-informative prior on number of changepts 
     deseasonalize  = TRUE,   # Remove the global seasonality before fitting the beast model
     detrend        = TRUE,   # Remove the global trend before fitting the beast model
     mcmc.seed      = 0,      # A seed for mcmc's random nummber generator; use a
                              # non-zero integer to reproduce results across runs
     mcmc.burnin    = 500,    # Number of initial iterations discarded
     mcmc.chains    = 2,      # Number of chains
     mcmc.thin      = 3,      # Include samples every 3 iterations
     mcmc.samples   = 6000,   # Number of samples taken per chain
                              # total iteration: (500+3*6000)*2	
     print.param     = FALSE  # Do not print the parameters							  
     )
 plot(o)
 plot(o,vars=c('t','slpsgn') )         # plot only trend and slope sign 
 plot(o,vars=c('t','slpsgn'), relative.heights =c(.8,.2) ) # run "?plot.beast" for more info
 plot(o, interactive=TRUE)
 
}
 
#------------------------------------Example 5----------------------------------------#
# Run an interactive GUI to visualize how BEAST is samplinig from the possible model 
# spaces in terms of the numbers and timings of seasonal and trend changepoints.
# The GUI inferface allows changing the option parameters interactively. This GUI is 
# only available on Win x64 machines, not Mac or Linux.

\dontrun{
 beast(Yellowstone, period=24, gui=TRUE) 
}

#------------------------------------Example 6----------------------------------------#
# Apply beast to trend-only data. 'Nile' is the ANNUAL river flow of the river
# Nile at Aswan since 1871. It is a 'ts' object; its time attributes (start=1871, 
# end=1970,frequency=1) are used to replace the user-supplied start,deltat, and freq, 
# if any. 

\donttest{
 data(Nile)  
 plot(Nile)     
 attributes(Nile) # a ts object with time attributes (i.e., tsp=(start,end,freq)
 
 o = beast(Nile)  # start=1871, delta=1, and freq=1 taken from Nile itself
 plot(o)
 
 o = beast(Nile,             # the same as above. The user-supplied values (i.e., 2023,
           start=2023,       # 9999) are ignored bcz Nile carries its own time attributes.
           period=9999,      # Its frequency tag is 1 (i.e., trend-only), so season='none'
           season='harmonic' # is used instead of the supplied 'harmonic'
		   )
} 
 
#------------------------------------Example 7----------------------------------------#
# NileVec is  a pure data vector. The first run below is WRONG bcz NileVec was assumed
# to have a perodic component by default and beast gets a best estimate of freq=6 while 
# the true value is freq=1. To fit a trend-only model, season='none' has to be explicitly
# specified, as in the 2nd & 3rd funs.
\donttest{
 NileVec = as.vector(Nile) # NileVec is not a ts obj but a pure numeric data vector
 o       = beast(NileVec)  # WRONG WAY to call: No time attributes available to interpret 
                           # NileVec. By default, beast assumes season='harmonic', start=1,
                           # & deltat=1. 'freq' is missing and guessed to be 6 (WRONG).    
						   
 plot(o)                   # WRONG Results: The result has a suprious seasonal component 
							  
 o=beast(NileVec,season='none') # The correct way to call: Use season='none' for trend-only 
                                # analysis; the default time is the integer indices
                                # "1:length(NileVec)'. 
 print(o$time)							 
								
 o=beast(NileVec,               # Recommended way to call: The true time attributes are 
         start  = 1871,         # given explicitly through start and deltat (or freq if 
         deltat = 1,            # there is a  cyclic/seasonal cmponent). 
         season = 'none')  
 print(o$time)			 
 plot(o)
}


#------------------------------------Example 8----------------------------------------#
# beast can handle missing data. co2 is a monthly time series (i.e.,freq=12) starting
# from Jan 1959. We generate some missing values at random indices
 
\dontrun{

 data(co2)  
 attributes(co2)                          # A ts object with time attributes (i.e., tsp)
 badIdx      = sample( 1:length(co2), 50) # Get a set of random indices
 co2[badIdx] = NA                         # Insert some data gaps   

 out=beast(co2) # co2 is a ts object and its 'tsp' time attributes are used to get the
                # true time info. No need to specify 'start','deltat', & freq explicity.
				
 out=beast(co2,                  # The supplied time/period values will be ignored bcz
           start  = c(1959,1,15),# co2 is a ts object; the correct period = 1 will be 
           deltat = 1/12,        # used.
           period = 365)  
 print(out)
 plot(out)
} 



#------------------------------------Example 9----------------------------------------#
# Apply beast to time seris-like sequence data: the unit of sequences is not 
# necessarily time.
 
\donttest{
  data(CNAchrom11) # DNA copy number alterations in Chromesome 11 for cell line GM05296
                   # The data is orderd by genomic position (not time), and the values
                   # are the log2-based intensity ratio of copy numbers between the sample
                   # the reference. A value of zero means no gain or loss in copy number.
  o = beast(CNAchrom11,season='none') # season is a misnomer here bcz the data has nothing
                                      # to do with time. Regardless, we fit only a trend.
  plot(o)									  
 
} 


#------------------------------------Example 10---------------------------------------#
# Apply beast to time seris-like data: the unit of sequences is not necessarily time.
 
\donttest{
  # Age of Death of Successive Kings of England
  # If the data link is deprecated, install the time series data library instead,
  # which is available at https://pkg.yangzhuoranyang.com/tsdl/
  # install.packages("devtools")
  # devtools::install_github("FinYang/tsdl")
  # kings = tsdl::tsdl[[293]]
  
  kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
  out   = beast(kings,season='none')
  plot(out) 
  
} 

 
#------------------------------------Example 11---------------------------------------#
# Another example from the tsdl data library
 

\donttest{
  # Number of monthly births in New York from Jan 1946 to Dec 1959
  # If the data link becomes invalid, install the time series data package instead
  # install.packages("devtools")
  # devtools::install_github("FinYang/tsdl")
  # kings = tsdl::tsdl[[534]]
  
  births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") 
  out    = beast(births,start=c(1946,1,15), deltat=1/12 )  
  plot(out) # the result is wrong bcz the guessed freq via auto-correlation by beast 
            # is 2 rather than 12, so we recommend always specifying 'freq' explicitly
            # for those time series with a periodic component, as shown below.
  out    = beast(births,start=c(1946,1,15), deltat=1/12, freq  =12 )  
  out    = beast(births,start=c(1946,1,15), deltat=1/12, period=1.0 )  
  plot(out)  

  # Fit the seasonal component using a singular-value-decompostion-based basis functions
  # rathat than the default harmonic sin/cos basis functions.
  out    = beast(births,start=c(1946,1,15), deltat=1/12, period=1.0, season="svd" )  
  plot(out)    
  
}

#------------------------------------Example 12---------------------------------------#
#    Daily confirmed COVID-19 new cases and deaths across the globe
 
 \dontrun{
 data(covid19)
 plot(covid19$date, covid19$newcases, type='l')
 
 newcases = sqrt( covid19$newcases )  # Apply a square root-transformation
 
 # This ts varies periodically every 7 days. 7 days can't be precisely represented 
 # in  the unit of year bcz some years has 365 days and others has 366. BEAST can hanlde 
 # this in two ways.


 #(1) Use the date number as the time unit--the num of days lapsed since 1970-01-01. 
  
  datenum  = as.numeric(covid19$date) 
  o        = beast(newcases, start=min(datenum), deltat=1, period=7) 
  o$time   = as.Date(o$time, origin='1970-01-01') # Convert from integers to Date.
  plot(o)
 
 #(2) Use strings to explicitly specify deltat and period with a unit. 
 
  startdate = covid19$date[1]
  o         = beast(newcases, start=startdate, deltat='1day', period='7days') 
  plot(o)
 
 }
 
#------------------------------------Example 13---------------------------------------#
# The old API interface of beast is still made available but NOT recommended. It is 
# kept mainly to ensure the working of the sample code on Page 475 in the text
# Ecological Metods by Drs. Southwood and Henderson.

\dontrun{

  # The interface as shown here will be deprecated and NOT recommended.
  beast(Yellowstone, 24)  #24 is the freq: number of datapoints per period
  
   
  # Specify the model or MCMC parameters through opt as in Rbeast v0.2
  opt=list()             #Create an empty list to append individual model parameters
  opt$period=24          #Period of the cyclic component (i.e.,freq in the new version)
  opt$minSeasonOrder=2   #Min harmonic order allowed in fitting season component
  opt$maxSeasonOrder=8   #Max harmonic order allowed in fititing season component
  opt$minTrendOrder=0    #Min polynomial order allowed to fit trend (0 for constant)
  opt$maxTrendOrder=1    #Max polynomial order allowed to fit trend (1 for linear term)
  opt$minSepDist_Season=20#Min separation time btw neighboring season changepoints 
  opt$minSepDist_Trend=20 #Min separation time btw neighboring trend  changepoints
  opt$maxKnotNum_Season=4 #Max number of season changepoints allowed 
  opt$maxKnotNum_Trend=10 #Max number of trend changepoints allowed  
  opt$omittedValue=NA    #A customized value to indicate bad/missing values in the time 
                          #series, in additon to those NA or NaN values.
  					
  # The following parameters used to configure the reverisible-jump MCMC (RJMCC) sampler
  opt$chainNumber=2       #Number of parallel MCMC chains 
  opt$sample=1000         #Number of samples to be collected per chain
  opt$thinningFactor=3    #A factor to thin chains  
  opt$burnin=500          #Number of burn-in samples discarded at the start 
  opt$maxMoveStepSize=30  #For the move proposal, the max window allowed in jumping from 
                           #the current changepoint
  opt$resamplingSeasonOrderProb=0.2 #The probability of selecting a re-sampling proposal 
                                    #(e.g., resample seasonal harmonic order)
  opt$resamplingTrendOrderProb=0.2  #The probability of selecting a re-sampling proposal 
                                    #(e.g., resample trend polynomial order)
								   
  opt$seed=65654    #A seed for the random generator: If seed=0,random numbers differ
                    #for different BEAST runs. Setting seed to a chosen non-zero integer 
                    #will allow reproducing the same result for different BEAST runs.
 
  beast(Yellowstone, opt)  
 }
 
#------------------------------------Example 14---------------------------------------#
# Fit a model with an outlier component: Y = trend + outlier + error. 
# Outliers here refer to spikes or dips at isolated points that can't be capatured by the 
# trend
\dontrun{ 
 NileVec        = as.vector(Nile)
 NileVec[50]    = NileVec[50] + 1500                 # Add an large artificial spike at t=50
 o = beast(NileVec, season='none', hasOutlier=TRUE)
 plot(o)
 
 o = beast(Nile, season='none', hasOutlier=TRUE)     # Fit a model with outliers
 plot(o) 
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
