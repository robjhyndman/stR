#' @title Estimates model parameters and decomposes data.
#' @description Estimates model parameters and decomposes input (time series of class \code{msts}) using the estimated model.
#'
#' @seealso \code{\link{AutoSTR}}
#' @param data A time series or a vector.
#' @param gapCV Same meaning as in \code{\link{AutoSTR}}.
#' @param lambdas A structure which defines initial values of lambda parameters for optimisation.
#' @param reltol Same meaning as in \code{\link{AutoSTR}}.
#' @param confidence Same meaning as in \code{\link{STR}}.
#' @param nsKnots An optional vector parameter. It defines number of seasonal knots (per period) for each sesonal component.
#' @return A structure containing input and output data.
#' @author Alex Dokumentov
#' @export

AutoSTR.msts = function(data, gapCV = NULL, lambdas = NULL, reltol = 0.001, confidence = NULL, nsKnots = NULL)
{
  if(!("msts" %in% class(data))) stop('Parameter "data" must be of class "msts".')
  periods = attr(data, "msts")
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
