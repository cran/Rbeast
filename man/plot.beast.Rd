\name{plot.beast}
\alias{plot.beast}
\title{Bayesian changepoint detection and time series decomposition}
\description{
		Plot the result obtained from the beast function.
}
\usage{

\method{plot}{beast}(
     x, 
     index = 1,
     vars  = c('st','s','scp','sorder','t','tcp','torder','o','ocp','error'),  
     col         = NULL, 
     main        = "BEAST decomposition and changepoint detection",
     xlab        = 'Time',
     ylab        = NULL,
     cex.main    = 1,
     cex.lab     = 1,  
     relative.heights = NULL,           
     interactive =FALSE,
     ...	 
   ) 
}
\arguments{
  \item{x}{a "beast" object returned  by \code{\link[Rbeast]{beast}},\code{\link[Rbeast]{beast.irreg}}, or \code{\link[Rbeast]{beast123}}. It may contain one or many time series.}
  
  \item{index}{an integer (default to 1 ) or a vector of two integers to specify the index of the time series to plot if \code{x} contains results for multiple time series. \code{index} is always 1 if \code{x} has 1 time series. If \code{x} is returned by \code{\link[Rbeast]{beast123}} with a 2D input,\code{index} should be a single integer. If \code{x} is from \code{\link[Rbeast]{beast123}} appied to 3D arrays of time series (e.g., stacked satellite images), \code{index} can be a linear index or two subscripts to specify the row and column of the pixel/grid.}
  
  \item{vars}{a vector of strings indicating the elements or variables of \code{x} to plot. Possible vars strings include 'st' (season plus trend), 's' (season component), 't' (trend component), 'o' (outliers), 'scp', 'tcp', 'ocp'  (ocurrence probability of seasonal/trend/outlier changepoint), 'sorder' (seasoanl harmonic order), 'torder' (trend polynomial order), 'samp' (amplitude of seasonality), 'tslp' (slope of trend), and 'error' (remainder).}
  
  \item{relative.heights}{a numeric vector of the same length as that of \code{vars} to specify the relative heights of subplots of individual variables in \code{vars}. }
  
  \item{col}{a string vector of the same length as that of \code{vars} to specify the colors of individual subplots assocaited with \code{vars}. }
  
  \item{main}{a string; the main title. }
  
  \item{xlab}{a string: the x axis title.}

  \item{ylab}{a string vector of the same length as that of \code{vars} to specify the y axis names of individual 
  subplots assocaited with \code{vars} }
   

  \item{cex.main}{cex for the main title}
  
  \item{cex.lab}{cex for the axis title}
  
  \item{interactive}{a bool scaler. If TRUE, an interactive GUI is used for examining individual elements of \code{x}.}
  \item{\dots}{additional parameters to be implemented. }
 
}
  
\value{
   This function creates various plots to demonstrate the results of a beast decomposition. . 
   }
 
\seealso{
  \code{\link[Rbeast]{beast}}, \code{\link[Rbeast]{beast.irreg}},  \code{\link[Rbeast]{beast123}}, \code{\link[Rbeast]{minesweeper}},  \code{\link[Rbeast]{tetris}}, \code{\link[Rbeast]{geeLandsat}} 
}
\examples{

 library(Rbeast)
 data(simdata)
\dontrun{ 
 result=beast123(simdata, metadata=list(whichDimIsTime=1))
 plot(result,1)
 plot(result,2)
}
}
\keyword{misc}
\references{
\enumerate{
\item Zhao, K., Wulder, M.A., Hu, T., Bright, R., Wu, Q., Qin, H., Li, Y., Toman, E., Mallick, B., Zhang, X. and Brown, M., 2019. Detecting change-point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics: A Bayesian ensemble algorithm. Remote Sensing of Environment, 232, p.111181 (the beast algorithm paper).
\item Zhao, K., Valle, D., Popescu, S., Zhang, X. and Mallick, B., 2013. Hyperspectral remote sensing of plant biochemistry using Bayesian model averaging with variable and band selection. Remote Sensing of Environment, 132, pp.102-119 (the Bayesian MCMC scheme used in beast). 
\item Hu, T., Toman, E.M., Chen, G., Shao, G., Zhou, Y., Li, Y., Zhao, K. and Feng, Y., 2021. Mapping fine-scale human disturbances in a working landscape with Landsat time series on Google Earth Engine. ISPRS Journal of Photogrammetry and Remote Sensing, 176, pp.250-261(a beast application paper). 
}

}
