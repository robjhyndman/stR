#' @import forecast

#' @title Estimates model parameters and decomposes data.
#' @description Estimates model parameters and decomposes input (time series of class \code{msts}) using the estimated model.
#'
#' @seealso \code{\link{AutoSTR}}, \code{\link{AutoSTR.ts}}
#' @param data A time series of class \code{msts}.
#' @inheritParams gapCV
#' @inheritParams lambdas
#' @inheritParams reltol
#' @inheritParams confidence
#' @param nsKnots An optional vector parameter. It defines number of seasonal knots (per period) for each sesonal component.
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \item \strong{optim.CV.MSE} -- best cross validated Mean Squared Error (n-fold) achieved during minimisation procedure.
#' @templateVar topLevel3 \item \strong{nFold} -- the input \code{nFold} parameter.
#' @templateVar topLevel4 \item \strong{gapCV} -- the input \code{gapCV} parameter.
#' @templateVar topLevel5 \item \strong{method} -- always contains string \code{"AutoSTR"} for this function.
#' @template returnValue
#' @author Alexander Dokumentov
#' @export

AutoSTR.msts = function(data, gapCV = NULL, lambdas = NULL, reltol = 0.001, confidence = NULL, nsKnots = NULL)
{
  if("msts" %in% class(data)) {
    periods = attr(data, "msts")
  } else if ("ts" %in% class(data)) {
    periods = attr(data, "tsp")[3] # The method also "secretly" works with class ts
  } else {
    stop('Parameter "data" must be of class "msts".')
  }
  periods = periods[periods < length(data)/2] # Removing periods which are too long
  if(is.null(gapCV)) gapCV = max(periods)

  times = as.vector(time(data))
  data = as.vector(data)
  trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
  trendSeasons = rep(1, length(data))
  trendTimeKnots = seq(from = first(times), to = last(times), length.out = max(16, length(data)/max(periods)*2+1))
  trendData = rep(1, length(data))
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
    seasons = seq_along(data) %% p
    seasonTimeKnots = seq(from = first(times), to = last(times), length.out = length(data)/max(periods)+1)
    seasonData = rep(1, length(data))
    season = list(name = paste("Seasonality", p), data = seasonData, times = times, seasons = seasons,
                   timeKnots = seasonTimeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,1,1))
    predictors[[length(predictors)+1]] = season
  }

  str = AutoSTR(data, predictors, gapCV = gapCV, reltol = reltol, confidence = confidence, lambdas = lambdas)

  return(str)
}
