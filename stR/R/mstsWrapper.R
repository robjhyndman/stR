#' Estimates model parameters and decomposes data using the estimated model.
#'
#' @seealso \code{\link{AutoSTR.default}} \code{\link{AutoSTR}}
#' @param data a time series or a vector.
#' @param ... other parameters.
#' @return A structure containing input and output data.
#' @export

AutoSTR.msts = function(x, gapCV = NULL, lambdas = NULL, reltol = 0.001, confidence = NULL, nsKnots = NULL)
{
   if(!("msts" %in% class(x))) stop('Parameter "x" must be of class "msts".')
  # attributes(x)
  periods = attr(x, "msts")
  if(is.null(gapCV)) gapCV = max(periods)

  data = as.vector(x) # Do we need it?

  trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
  trendSeasons = rep(1, length(x))
  times = as.vector(time(x))
  trendTimeKnots = seq(from = first(times), to = last(times), length.out = max(16, length(x)/max(periods)*2+1))
  trendData = rep(1, length(x))
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
    seasons = seq_along(x) %% p
    seasonTimeKnots = seq(from = first(times), to = last(times), length.out = length(x)/max(periods)+1)
    seasonData = rep(1, length(x))
    season = list(name = paste("Seasonality", p), data = seasonData, times = times, seasons = seasons,
                   timeKnots = seasonTimeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,1,1))
    predictors[[length(predictors)+1]] = season
  }

  str = AutoSTR(data, predictors, gapCV = gapCV, reltol = reltol, confidence = confidence, lambdas = lambdas)

  return(str)
}
