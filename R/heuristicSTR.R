#' @rdname heuristicSTR
#' @name heuristicSTR
#' @title Automatic STR decomposition with heuristic search of the parameters
#' @description Automatically selects parameters (lambda coefficients) for an STR decomposition of time series data.
#' Heuristic approach can give a better estimate compare to a standard optmisaton methods used in \code{\link{STR}}.
#'
#' If a parallel backend is registered for use before \code{STR} call,
#' \code{heuristicSTR} will use it for n-fold cross validation computations.
#'
#' @seealso \code{\link{STR}} \code{\link{STRmodel}} \code{\link{AutoSTR}}
#' @inheritParams data
#' @inheritParams predictors
#' @inheritParams confidence
#' @inheritParams lambdas
#' @inheritParams pattern
#' @inheritParams nFold
#' @inheritParams reltol
#' @inheritParams gapCV
#' @inheritParams solver
#' @inheritParams trace
#' @param ratioGap Ratio to define hyperparameter bounds for one-dimensional search.
#' @param relCV Minimum improvement required after all predictors tried. It is used to exit heuristic serach of lambda parameters.
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \item \strong{optim.CV.MSE} or \strong{optim.CV.MAE} -- best cross validated Mean Squared Error or Mean Absolute Error (n-fold) achieved during minimisation procedure.
#' @templateVar topLevel3 \item \strong{nFold} -- the input \code{nFold} parameter.
#' @templateVar topLevel4 \item \strong{gapCV} -- the input \code{gapCV} parameter.
#' @templateVar topLevel5 \item \strong{method} -- contains strings \code{"STR"} or \code{"RSTR"} depending on used method.
#' @template returnValue
#' @examples
#' \donttest{
#'
#' TrendSeasonalStructure <- list(segments = list(c(0,1)),
#' sKnots = list(c(1,0)))
#' WDSeasonalStructure <- list(segments = list(c(0,48), c(100,148)),
#'                             sKnots = c(as.list(c(1:47,101:147)), list(c(0,48,100,148))))
#'
#' TrendSeasons <- rep(1, nrow(electricity))
#' WDSeasons <- as.vector(electricity[,"WorkingDaySeasonality"])
#'
#' Data <- as.vector(electricity[,"Consumption"])
#' Times <- as.vector(electricity[,"Time"])
#' TempM <- as.vector(electricity[,"Temperature"])
#' TempM2 <- TempM^2
#'
#' TrendTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 116)
#' SeasonTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 24)
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
#' WDSeason <- list(name = "Dayly seas",
#'                  data = SeasonData,
#'                  times = Times,
#'                  seasons = WDSeasons,
#'                  timeKnots = SeasonTimeKnots,
#'                  seasonalStructure = WDSeasonalStructure,
#'                  lambdas = c(0.003,0,240))
#' StaticTempM <- list(name = "Temp Mel",
#'                     data = TempM,
#'                     times = Times,
#'                     seasons = NULL,
#'                     timeKnots = NULL,
#'                     seasonalStructure = NULL,
#'                     lambdas = c(0,0,0))
#' StaticTempM2 <- list(name = "Temp Mel^2",
#'                      data = TempM2,
#'                      times = Times,
#'                      seasons = NULL,
#'                      timeKnots = NULL,
#'                      seasonalStructure = NULL,
#'                      lambdas = c(0,0,0))
#' Predictors <- list(Trend, WDSeason, StaticTempM, StaticTempM2)
#'
#' elec.fit <- heuristicSTR(data = Data,
#'                 predictors = Predictors,
#'                 gapCV = 48*7)
#'
#' plot(elec.fit,
#'      xTime = as.Date("2000-01-11")+((Times-1)/48-10),
#'      forecastPanels = NULL)
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
#' elec.fit <- heuristicSTR(data = Data,
#'                          predictors = Predictors,
#'                          gapCV = 48*7)
#'
#' plot(elec.fit,
#'      xTime = as.Date("2000-01-11")+((Times-1)/48-10),
#'      forecastPanels = NULL)
#'
#' plotBeta(elec.fit, predictorN = 4)
#' plotBeta(elec.fit, predictorN = 5)
#'
#' }
#' @author Alexander Dokumentov
#' @references Dokumentov, A., and Hyndman, R.J. (2016)
#' STR: A Seasonal-Trend Decomposition Procedure Based on Regression
#' \href{https://www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}{www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}
#' @export

heuristicSTR = function(data, predictors,
                        confidence = NULL, lambdas = NULL,
                        pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                        solver = c("Matrix", "cholesky"),
                        trace = FALSE,
                        ratioGap = 1e12, # Ratio to define bounds for one-dimensional search.
                        relCV = 0.01 # Minimum improvement required after all predictors tried. It is used to exit.
)
{
  lambdas = upLambdas(data = data, predictors = predictors,
                      confidence = confidence, lambdas = lambdas,
                      pattern = pattern, nFold = nFold,
                      reltol = reltol, gapCV = gapCV,
                      solver = solver, trace = trace, ratioGap = ratioGap)
  result = STR(data = data, predictors = predictors,
               confidence = confidence, lambdas = lambdas,
               pattern = pattern, nFold = nFold,
               reltol = reltol, gapCV = gapCV,
               solver = solver, trace = trace)
  return(result)
}

# Tries to find optimal lambda parameters optimising them in groups (one group per predictor), over
# the residuals and allowing only to increase L1 norm of the group of the parameters
upLambdas = function(data, predictors,
                     confidence = NULL, lambdas = NULL,
                     pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                     solver = c("Matrix", "cholesky"),
                     trace = FALSE,
                     ratioGap = 1e6, # Ratio to define bounds for one-dimensional search
                     nPasses = 2,
                     maxLambda = 1e6
)
{
  if(is.null(lambdas)) {
    lambdas = lapply(predictors, FUN = function(p) list(lambdas = p$lambdas))
  } else {
    lambdas = lapply(lambdas, FUN = function(p) list(lambdas = p$lambdas))
  }

  lData = length(data)
  if(nFold*gapCV > lData) {
    stop(paste0("nFold*gapCV should be less or equal to the data length.\nnFold = ",
                nFold, "\ngapCV = ", gapCV, "\nlength of the data = ", lData))
  }
  subInds = lapply(1:nFold, FUN = function(i) sort(unlist(lapply(1:gapCV, FUN = function(j) seq(from = (i-1)*gapCV+j, to = lData, by = nFold*gapCV)))))
  complInds = lapply(subInds, FUN = function(s) setdiff(1:lData, s))

  strDesign = STRDesign(predictors)
  C = strDesign$cm$matrix
  fcastC = lapply(subInds, FUN = function(si) C[si,])
  trainC = lapply(complInds, FUN = function(ci) C[ci,])
  fcastData = lapply(subInds, FUN = function(si) data[si])
  trainData = lapply(complInds, FUN = function(ci) data[ci])
  rm = strDesign$rm
  regMatrix = rm$matrix
  regSeats = rm$seats

  newLambdas = lambdas
  if(trace) {
    dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
  }
  oldLambdas = oldFit = fit = NULL
  for(j in seq_len(nPasses)) {
    for(i in seq_along(predictors)) {
      if(sum(abs(newLambdas[[i]]$lambdas)) == 0) next
      if(trace) {
        cat("\nPredictor: "); cat(i); cat("\n")
      }
      if(is.null(fit)) {
        fit = try(STRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = NULL, trace = F), silent = !trace)
        if("try-error" %in% class(fit)) {
          if(trace) cat("\nError in STRmodel. Reverting lambda coefficients.")
          fit = oldFit
          newLambdas = oldLambdas
        }
      }
      newData = fit$output$predictors[[i]]$data + fit$output$random$data
      newPredictors = fit$input$predictors[i]
      startLambdas = newLambdas[i]
      fit_ = STR_(data = newData,
                        predictors = newPredictors,
                        confidence = confidence,
                        lambdas = startLambdas,
                        pattern = extractPattern(newPredictors),
                        nFold = nFold,
                        reltol = reltol,
                        gapCV = gapCV,
                        solver = solver,
                        trace = F,
                        ratioGap = ratioGap)
      oldNorm = sum(abs(newLambdas[[i]]$lambdas))
      newNorm = sum(abs(fit_$input$lambdas[[1]]$lambdas))
      if(newNorm < oldNorm) {
        next
      }
      else {
        oldLambdas = newLambdas
        oldFit = fit
        newLambdas[[i]]$lambdas = pmin(fit_$input$lambdas[[1]]$lambdas, maxLambda)
        fit = NULL
        if(trace) {
          dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
        }
      }
    }
  }
  return(newLambdas)
}
