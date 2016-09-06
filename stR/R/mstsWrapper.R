#' @import forecast
#' @importFrom stats optim
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats time

#' @rdname AutoSTR
#' @title Automatic STR decomposition for time series data
#' @description Automatically selects parameters for an STR decomposition of time series data.
#' The time series should be of class \code{ts} or \code{msts}.
#' @param data A time series of class \code{ts} or \code{msts}.
#' @inheritParams robust
#' @inheritParams gapCV
#' @inheritParams lambdas
#' @inheritParams reltol
#' @inheritParams confidence
#' @param nsKnots An optional vector parameter, defining the number of seasonal knots (per period) for each sesonal component.
#' @inheritParams trace
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \item \strong{optim.CV.MSE} -- best cross validated Mean Squared Error (n-fold) achieved during minimisation procedure.
#' @templateVar topLevel3 \item \strong{nFold} -- the input \code{nFold} parameter.
#' @templateVar topLevel4 \item \strong{gapCV} -- the input \code{gapCV} parameter.
#' @templateVar topLevel5 \item \strong{method} -- always contains string \code{"AutoSTR"} for this function.
#' @template returnValue
#' @author Alexander Dokumentov
#' @references Dokumentov, A., and Hyndman, R.J. (2016)
#' STR: A Seasonal-Trend Decomposition Procedure Based on Regression
#' \href{http://robjhyndman.com/working-papers/str/}{robjhyndman.com/working-papers/str/}
#' @seealso \code{\link{STR}}
#' @examples
#' \dontrun{
#'
#' # Decomposition of a multiple seasonal time series
#' decomp <- AutoSTR(calls)
#' plot(decomp)
#'
#' # Decomposition of a monthly time series
#' decomp <- AutoSTR(log(grocery))
#' plot(decomp)
#' }
#' @export

AutoSTR = function(data, robust = FALSE, gapCV = NULL, lambdas = NULL, reltol = 0.001,
  confidence = NULL, nsKnots = NULL, trace = FALSE)
{
  nFold = 5 # Not configurable parameter
  if("msts" %in% class(data)) {
    periods = attr(data, "msts")
  } else if ("ts" %in% class(data)) {
    periods = attr(data, "tsp")[3] # For class ts
  } else {
    stop('Parameter "data" must be of class "msts".')
  }
  if(min(periods) >= length(data)/2)
    stop("Series too short")
  periods <- periods[periods < length(data)/2] # Removing periods which are too long
  if(identical(periods, 1))
    stop("Non-seasonal time series")
  if(any(confidence <= 0 | confidence >= 1))
    stop("confidence must be between 0 and 1")

  if(is.null(gapCV)) gapCV = max(min(max(periods), floor(length(data)/nFold)-1), 1)

  times = as.vector(time(data))
  vData = as.vector(data)
  trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
  trendSeasons = rep(1, length(vData))
  trendTimeKnots = seq(from = first(times), to = last(times), length.out = max(16, length(vData)/max(periods)*2+1))
  trendData = rep(1, length(vData))
  trend = list(name = "Trend", data = trendData, times = times, seasons = trendSeasons,
               timeKnots = trendTimeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))

  predictors = list(trend)
  for(i in seq_along(periods)) {
    p = periods[i]
    if(is.null(nsKnots[i])) {
      mp = max(1, periods[periods<p])
      length.out = floor(p/sqrt(mp)) + 1
    } else {
      length.out = nsKnots[i]
    }
    seasonalStructure = list(segments = list(c(0,p)), sKnots = c(as.list(tail(head(seq(from=0, to=p, length.out=length.out), -1), -1)), list(c(p,0))))
    seasons = seq_along(vData) %% p
    seasonTimeKnots = seq(from = first(times), to = last(times), length.out = length(vData)/max(periods)+1)
    seasonData = rep(1, length(vData))
    season = list(name = paste("Seasonality", p), data = seasonData, times = times, seasons = seasons,
                   timeKnots = seasonTimeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,1,1))
    predictors[[length(predictors)+1]] = season
  }

  str = STR(data, predictors, gapCV = gapCV, nFold = nFold, reltol = reltol, confidence = confidence, lambdas = lambdas, trace = trace, robust = robust)

  return(str)
}
