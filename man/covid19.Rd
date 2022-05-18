\name{covid19}
\alias{Covid19}
\alias{covid19}
\docType{data}
\title{Daily confirmed COVID19 cases and deaths in the world}
\description{
  \code{covid19} is a data frame consisting of daily confirmed COVID19 cases and deaths in the world from Jan 22, 2020 to Dec 16, 2021.
}
\usage{
    data(covid19)
}
\source{
   https://ourworldindata.org/grapher/daily-covid-cases-deaths?country=~OWID_WRL (last accessed on Dec 16, 2021)
}
 
\examples{

 library(Rbeast)
 data(covid19)  
 
 \dontrun{
 newcases = covid19$newcases
 
 # This time series has a periodical variation of 7 days. 7 days can't be precisely 
 # represented in  the unit of year bcz some years has 365 days and others has 366. 
 # So, here we use  the date number as the time unit (i.e., the numeber of days lapsed 
 #  since 1970-01-01). 
 
 datenum = as.numeric(covid19$date) 
 o       = beast(newcases, start=min(datenum), deltat=1, freq=7) 
 o$time  = as.Date(o$time, origin='1970-01-01') # Convert from integers to Date.
 plot(o)
 
 # Apply BEAST to the square root-transformed time series
 o        = beast(sqrt(newcases), start=min(datenum), deltat=1, freq=7) 
 o$time   = as.Date(o$time, origin='1970-01-01')  # Convert from integers to Date.
 plot(o)
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
