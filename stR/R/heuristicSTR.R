#' @rdname heuristicSTR
#' @name heuristicSTR
#' @title Automatic STR decomposition with heuristic hyperparameters search
#' @description Automatically selects parameters for an STR decomposition of time series data.
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
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \item \strong{optim.CV.MSE} or \strong{optim.CV.MAE} -- best cross validated Mean Squared Error or Mean Absolute Error (n-fold) achieved during minimisation procedure.
#' @templateVar topLevel3 \item \strong{nFold} -- the input \code{nFold} parameter.
#' @templateVar topLevel4 \item \strong{gapCV} -- the input \code{gapCV} parameter.
#' @templateVar topLevel5 \item \strong{method} -- contains strings \code{"STR"} or \code{"RSTR"} depending on used method.
#' @template returnValue
#' @examples
#' \dontrun{
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
#' }
#' @author Alexander Dokumentov
#' @references Dokumentov, A., and Hyndman, R.J. (2016)
#' STR: A Seasonal-Trend Decomposition Procedure Based on Regression
#' \href{http://robjhyndman.com/working-papers/str/}{robjhyndman.com/working-papers/str/}
#' @export

heuristicSTR = function(data, predictors,
                        confidence = NULL, lambdas = NULL,
                        pattern = stR:::extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                        solver = c("Matrix", "cholesky"),
                        trace = FALSE,
                        ratioGap = 1e6, # Ratio to define bounds for one-dimensional search
                        relCV = 0.01 # Minimum improvement required after all predictors tried. It is used to exit.
)
{
    lambdas = upLambdas(data = data, predictors = predictors,
                            confidence = confidence, lambdas = lambdas,
                            pattern = pattern, nFold = nFold,
                            reltol = reltol*10, gapCV = gapCV,
                            solver = solver, trace = trace)
    result1 = STR(data = data, predictors = predictors,
                  confidence = confidence, lambdas = lambdas,
                  pattern = pattern, nFold = nFold,
                  reltol = reltol*10, gapCV = gapCV,
                  solver = solver, trace = trace)
    lambdas = upDownLambdas(data = data, predictors = predictors,
                            confidence = confidence, lambdas = result1$input$lambdas,
                            pattern = pattern, nFold = nFold,
                            reltol = reltol*10, gapCV = gapCV,
                            solver = solver, trace = trace)
    result2 = STR(data = data, predictors = predictors,
                  confidence = confidence, lambdas = lambdas,
                  pattern = pattern, nFold = nFold,
                  reltol = reltol, gapCV = gapCV,
                  solver = solver, trace = trace)
    return(result2)
}

# Tries to find optimal lambda parameters optimising them in groups (one group per predictor) and over
# the residuals
upDownLambdas = function(data, predictors,
                confidence = NULL, lambdas = NULL,
                pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                solver = c("Matrix", "cholesky"),
                trace = FALSE,
                ratioGap = 1e6, # Ratio to define bounds for one-dimensional search
                relCV = 0.01 # Minimum improvement required after all predictors tried. It is used to exit.
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

  strDesign = stR:::STRDesign(predictors)
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
    cat("\n")
    dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
  }
  newCV = stR:::nFoldSTRCV(n = nFold,
                        trainData = trainData, fcastData = fcastData,
                        trainC = trainC, fcastC = fcastC,
                        regMatrix = regMatrix, regSeats = regSeats,
                        lambdas = newLambdas,
                        solver = solver,
                        trace = trace)
  if(trace) { cat("\nCV: "); cat(newCV) }

  loopCV = newCV
  while(TRUE) {
    for(i in seq_along(predictors)) {
      if(trace) { cat("\n\nPredictor: "); cat(i) }
      if(trace) {
        cat("\n")
        dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
      }
      fit = STRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = NULL, trace = F)
      newData = fit$output$predictors[[i]]$data + fit$output$random$data
      newPredictors = fit$input$predictors[i]
      startLambdas = newLambdas[i]
      fit_ = stR:::STR_(data = newData,
                        predictors = newPredictors,
                        confidence = confidence,
                        lambdas = startLambdas,
                        pattern = stR:::extractPattern(newPredictors),
                        nFold = nFold,
                        reltol = reltol,
                        gapCV = gapCV,
                        solver = solver,
                        trace = F,
                        ratioGap = ratioGap)
      candidateLambdas = newLambdas
      candidateLambdas[[i]]$lambdas = fit_$input$lambdas[[1]]$lambdas # It can be a formula like: l = alpha*estimatedL + (1-alpha)*l
      candidateCV = stR:::nFoldSTRCV(n = nFold,
                            trainData = trainData, fcastData = fcastData,
                            trainC = trainC, fcastC = fcastC,
                            regMatrix = regMatrix, regSeats = regSeats,
                            lambdas = candidateLambdas,
                            solver = solver,
                            trace = trace)
      if(trace) { cat("\nCandidate CV: "); cat(candidateCV) }
      if(candidateCV < newCV) {
        if(trace) { cat("\nCandidate is better") }
        newLambdas = candidateLambdas
        newCV = candidateCV
        if(trace) {
          cat("\n")
          dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
        }
      }
    }
    if(abs(loopCV - newCV)/newCV < relCV) break
    loopCV = newCV
  }
  return(newLambdas)
}

# Tries to find optimal lambda parameters optimising them in groups (one group per predictor), over
# the residuals and allowing only to increase L1 norm of the group of the parameters
upLambdas = function(data, predictors,
                        confidence = NULL, lambdas = NULL,
                        pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                        solver = c("Matrix", "cholesky"),
                        trace = FALSE,
                        ratioGap = 1e6, # Ratio to define bounds for one-dimensional search
                        nPasses = 2
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

  strDesign = stR:::STRDesign(predictors)
  C = strDesign$cm$matrix
  fcastC = lapply(subInds, FUN = function(si) C[si,])
  trainC = lapply(complInds, FUN = function(ci) C[ci,])
  fcastData = lapply(subInds, FUN = function(si) data[si])
  trainData = lapply(complInds, FUN = function(ci) data[ci])
  rm = strDesign$rm
  regMatrix = rm$matrix
  regSeats = rm$seats

  newLambdas = lambdas
  if(trace) dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
  for(j in seq_len(nPasses)) {
    for(i in seq_along(predictors)) {
      if(trace) cat("\nPredictor: "); cat(i); cat("\n")
      # dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")}); cat("\n")
      fit = STRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = NULL, trace = F)
      newData = fit$output$predictors[[i]]$data + fit$output$random$data
      newPredictors = fit$input$predictors[i]
      startLambdas = newLambdas[i]
      fit_ = stR:::STR_(data = newData,
                        predictors = newPredictors,
                        confidence = confidence,
                        lambdas = startLambdas,
                        pattern = stR:::extractPattern(newPredictors),
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
        newLambdas[[i]]$lambdas = fit_$input$lambdas[[1]]$lambdas # It can be a formula like: l = alpha*estimatedL + (1-alpha)*l
        if(trace) dummy = lapply(newLambdas, FUN = function(p) {cat(format(p$lambdas, digits = 4, scientific = T, width = 9)); cat("  ")})
      }
    }
  }
  return(newLambdas)
}


# tm = system.time({
#   elec.fit.h <- heuristicSTR(data = Data,
#                              predictors = Predictors,
#                              # confidence = 0.95,
#                              gapCV = 48*7,
#                              trace = TRUE)
# }); print(tm)
#
# plot(elec.fit.h,
#      xTime = as.Date("2000-01-11") + ((Times-1)/48-10))
#
# plotBeta(elec.fit.h, predictorN = 1, dim = 3)

# tm = system.time({
# lambdas <- upLambdas(data = Data,
#                            predictors = Predictors,
#                            # confidence = 0.95,
#                            gapCV = 48*7,
#                            trace = TRUE)
# }); print(tm)
#
# tm = system.time({
#   elec.fit <- STR(data = Data,
#                   predictors = Predictors,
#                   lambdas = lambdas,
#                   # confidence = 0.95,
#                   gapCV = 48*7,
#                   # gapCV = 1,
#                   # solver = c("iterative", "cg-chol"),
#                   # solver = c("iterative", "cg"),
#                   # solver = c("iterative", "lsmr-chol"),
#                   # solver = c("iterative", "lsmr"),
#                   iterControl = list(maxiter = 400, tol = 1e-5),
#                   trace = TRUE)
# }); print(tm)
#
# plot(elec.fit,
#      xTime = as.Date("2000-01-11") + ((Times-1)/48-10))
