#' Number of phone calls dataset
#'
#' Number of call arrivals per 5-minute interval handled on weekdays between 7:00 am and 9:05 pm
#' from March 3, 2003 in a large North American commercial bank.
#'
#' @docType data
#'
#' @usage calls
#'
#' @format A numerical time series of class \code{msts} and \code{ts}.
#'
#' @keywords datasets
#'
#' @references Forecasting Time Series With Complex Seasonal Patterns Using Exponential Smoothing
#' Alysha M. De Livera, Rob J. Hyndman & Ralph D. Snyder
#' Journal of the American Statistical Association Volume 106, 2011 - Issue 496
#'
#' @source \href{https://robjhyndman.com/data/callcenter.txt}{Data file}
#'
#' @examples
#' plot(calls, ylab = "Calls handled")
"calls"
