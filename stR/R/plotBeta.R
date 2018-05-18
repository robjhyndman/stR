#' @importFrom rgl open3d
#' @importFrom rgl lines3d
#' @importFrom rgl rgl.bringtotop
#' @importFrom rgl persp3d
#' @importFrom rgl title3d
#' @importFrom rgl axis3d
#' @importFrom graphics axis
#' @importFrom graphics filled.contour
#' @importFrom graphics title
#' @importFrom grDevices rainbow

#' @name plotBeta
#' @rdname plotBeta
#'
#' @title Plots the varying beta coefficients of decomposition
#' @description \code{plotBeta} plots the varying beta coefficients of STR decomposition.
#' It plots coefficients only only for independent seasons (one less season than defined).
#' @seealso \code{\link{plot.STR}}
#' @param x Result of STR decomposition.
#' @param xTime Times for data to plot.
#' @param predictorN Predictor number in the decomposition to plot the corresponding beta coefficiets.
#' @param dim Dimensions to use to plot the beta coefficients.
#' When \code{1}, the standard charts are used.
#' When \code{2}, \code{graphics:::filled.contour} function is used.
#' When \code{3}, \code{rgl:::persp3d} is used. The default value is \code{1}.
#' @param type Type of the graph for one dimensional plots.
#' @param pch Symbol code to plot points in 1-dimensional charts. Default value is \code{20}.
#' @param palette Color palette for 2 - and 3 - dimentional plots.
#' @author Alexander Dokumentov
#' @examples
#' \donttest{
#'
#' fit <- AutoSTR(log(grocery))
#' for(i in 1:2) plotBeta(fit, predictorN = i, dim = 2)
#'
#' ########################################
#'
#' TrendSeasonalStructure <- list(segments = list(c(0,1)),
#' sKnots = list(c(1,0)))
#' DailySeasonalStructure <- list(segments = list(c(0,48)),
#'                                sKnots = c(as.list(1:47), list(c(48,0))))
#' WeeklySeasonalStructure <- list(segments = list(c(0,336)),
#'                                 sKnots = c(as.list(seq(4,332,4)), list(c(336,0))))
#' WDSeasonalStructure <- list(segments = list(c(0,48), c(100,148)),
#'                             sKnots = c(as.list(c(1:47,101:147)), list(c(0,48,100,148))))
#'
#' TrendSeasons <- rep(1, nrow(electricity))
#' DailySeasons <- as.vector(electricity[,"DailySeasonality"])
#' WeeklySeasons <- as.vector(electricity[,"WeeklySeasonality"])
#' WDSeasons <- as.vector(electricity[,"WorkingDaySeasonality"])
#'
#' Data <- as.vector(electricity[,"Consumption"])
#' Times <- as.vector(electricity[,"Time"])
#' TempM <- as.vector(electricity[,"Temperature"])
#' TempM2 <- TempM^2
#'
#' TrendTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 116)
#' SeasonTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 24)
#' SeasonTimeKnots2 <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 12)
#'
#' TrendData <- rep(1, length(Times))
#' SeasonData <- rep(1, length(Times))
#'
#' Trend <- list(name = "Trend",
#'               data = TrendData,
#'               times = Times,
#'               seasons = TrendSeasons,
#'               timeKnots = TrendTimeKnots,
#'               seasonalStructure = TrendSeasonalStructure,
#'               lambdas = c(1500,0,0))
#' WSeason <- list(name = "Weekly seas",
#'                 data = SeasonData,
#'                 times = Times,
#'                 seasons = WeeklySeasons,
#'                 timeKnots = SeasonTimeKnots2,
#'                 seasonalStructure = WeeklySeasonalStructure,
#'                 lambdas = c(0.8,0.6,100))
#' WDSeason <- list(name = "Dayly seas",
#'                  data = SeasonData,
#'                  times = Times,
#'                  seasons = WDSeasons,
#'                  timeKnots = SeasonTimeKnots,
#'                  seasonalStructure = WDSeasonalStructure,
#'                  lambdas = c(0.003,0,240))
#' TrendTempM <- list(name = "Trend temp Mel",
#'                    data = TempM,
#'                    times = Times,
#'                    seasons = TrendSeasons,
#'                    timeKnots = TrendTimeKnots,
#'                    seasonalStructure = TrendSeasonalStructure,
#'                    lambdas = c(1e7,0,0))
#' TrendTempM2 <- list(name = "Trend temp Mel^2",
#'                     data = TempM2,
#'                     times = Times,
#'                     seasons = TrendSeasons,
#'                     timeKnots = TrendTimeKnots,
#'                     seasonalStructure = TrendSeasonalStructure,
#'                     lambdas = c(0.01,0,0)) # Starting parameter is too far from the optimal value
#' Predictors <- list(Trend, WSeason, WDSeason, TrendTempM, TrendTempM2)
#'
#' elec.fit <- STR(data = Data,
#'                 predictors = Predictors,
#'                 gapCV = 48*7)
#'
#' plot(elec.fit,
#'      xTime = as.Date("2000-01-11")+((Times-1)/48-10),
#'      forecastPanels = NULL)
#'
#' plotBeta(elec.fit, predictorN = 4)
#' plotBeta(elec.fit, predictorN = 5) # Beta coefficients are too "wiggly"
#'
#' }
#' @export

plotBeta = function(x, xTime = NULL, predictorN = 1, dim = c(1, 2, 3), type = "o", pch = 20,
                    palette = function(n) rainbow(n, start=0.0, end=0.7))
{
  beta = x$output$predictors[[predictorN]]$beta
  timeKnots = x$input$predictors[[predictorN]]$timeKnots
  if(is.null(xTime)) {
    translatedTimes = timeKnots
  } else {
    times = x$input$predictors[[predictorN]]$times
    translatedTimes = xTime[1]
    for(i in seq_along(timeKnots)) {
      leftInd = max(which(times <= timeKnots[i]))
      rightInd = min(which(times >= timeKnots[i]))
      ratio = ifelse(leftInd == rightInd,
                     0,
                     (timeKnots[i] - times[leftInd]) / (times[rightInd] - times[leftInd]))
      translatedTimes[i] = xTime[leftInd] + ratio * (xTime[rightInd] - xTime[leftInd])
    }
  }
  if(length(beta) == length(translatedTimes)) {
    plot(translatedTimes, beta, type = type, pch = pch,
         xlab = "Time", ylab = "Beta",
         main = x$input$predictors[[predictorN]]$name)
  } else {
    m = matrix(beta, ncol = length(translatedTimes))
    if(dim[1] == 1) {
      plot(translatedTimes, m[1,], ylim = range(m), type = type, pch = pch,
           xlab = "Time", ylab = "Beta",
           main = x$input$predictors[[predictorN]]$name)
      for(i in tail(seq_len(nrow(m)), -1)) {
        lines(translatedTimes, m[i,], type = type, pch = pch, col = i)
      }
    } else if(dim[1] == 2) {
      ylabs = vapply(x$input$predictors[[predictorN]]$seasonalStructure$sKnots,
             FUN = function(x) do.call("paste", as.list(format(x))),
             FUN.VALUE = "")
      filled.contour(translatedTimes, seq_len(nrow(m)), t(m),
                     zlim = range(m), color.palette = palette,
                     plot.title = title(main = x$input$predictors[[predictorN]]$name, xlab = "Time", ylab = "Seasons"),
                     plot.axes = {
                       axis(side = 1, at = translatedTimes, labels = format(translatedTimes));
                       axis(side = 2, at = seq_len(nrow(m)), labels = head(ylabs, nrow(m)))
                     }
      )
    } else if(dim[1] == 3) {
      rng = range(m)
      if(diff(rng) == 0) {
        col = "green"
      }
      else {
        col = (t(m) - rng[1])/diff(rng)
        r = palette(65536)
        col = r[round(col * 65535) + 1]
      }
      ylabs = vapply(x$input$predictors[[predictorN]]$seasonalStructure$sKnots,
                     FUN = function(x) do.call("paste", as.list(format(x))),
                     FUN.VALUE = "")
      open3d(windowRect=c(10, 35, 810, 835))
      persp3d(y = seq_len(nrow(m)), x = translatedTimes, z = t(m),
              aspect = c(1, 1, 1), ylab = "Seasons", xlab = "Time", zlab = "Beta", col = col, axes = FALSE)
      axis3d(edge = 'y', at = seq_len(nrow(m)), labels = head(ylabs, nrow(m)))
      axis3d(edge = 'x', at = translatedTimes, labels = format(translatedTimes))
      axis3d(edge = 'z')
      title3d(main = x$input$predictors[[predictorN]]$name)
      rgl.bringtotop()
    } else {
      stop("dim paramemetr is incorrect. Must be 1, 2, or 3.")
    }
  }
}

# for(i in 1:5)
#   plotBeta(elec.fit, xTime = as.Date("2000-01-11") + ((Times-1)/48-10), predictorN = i, dim = 2)
