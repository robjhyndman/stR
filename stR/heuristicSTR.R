heuristicSTR = function(data, predictors,
                confidence = NULL, lambdas = NULL,
                pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                solver = c("MatrixModels", "cholesky"),
                trace = FALSE,
                ratioGap = 1e6 # Ratio to define bounds for one-dimensional search
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
  cv = stR:::nFoldSTRCV(n = nFold,
                        trainData = trainData, fcastData = fcastData,
                        trainC = trainC, fcastC = fcastC,
                        regMatrix = regMatrix, regSeats = regSeats,
                        lambdas = newLambdas,
                        solver = solver,
                        trace = trace)
  cat("\nCV: "); cat(cv); cat("\n")

  oldCV = Inf
  while(TRUE) {
    for(i in seq_along(predictors)) {
      cat("\nPredictor: "); cat(i); cat("\n")
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
      newLambdas[[i]]$lambdas = fit_$input$lambdas[[1]]$lambdas # It can be a formula like: l = alpha*estimatedL + (1-alpha)*l
      cv = stR:::nFoldSTRCV(n = nFold,
                            trainData = trainData, fcastData = fcastData,
                            trainC = trainC, fcastC = fcastC,
                            regMatrix = regMatrix, regSeats = regSeats,
                            lambdas = newLambdas,
                            solver = solver,
                            trace = trace)
      cat("\nCV: "); cat(cv); cat("\n")
    }
    if(abs(oldCV - cv)/cv < 0.001) break
    oldCV = cv
  }

  result = STRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = confidence, trace = trace)
  # result$optim.CV.MSE = optP$value
  result$nFold = nFold
  result$gapCV = gapCV
  result$method = "STR"
  return(result)
}

elec.fit.h <- heuristicSTR(data = Data,
                predictors = Predictors,
                # confidence = 0.95,
                gapCV = 48*7,
                trace = TRUE)


# fit = elec.fit
# i1 = 5
# i2 = 1
# data = fit$input$data
# pr1 = fit$output$predictors[[i1]]$data
# pr2 = fit$output$predictors[[i2]]$data
# r = fit$output$random$data
# ri = pr1 + pr2 + r
# ri = pr1 + r + rnorm(length(r),0,10)
# ri = pr1 + r
# ri = pr2 + r
#
#
# plot(ri, type = "l")
#
#
# Predictors2 <- list(Trend, TrendTempM2)
# Predictors2 <- list(TrendTempM2)
# Predictors2 <- list(Trend)
#
# elec.fit.i <- STR(data = ri,
#                 predictors = Predictors2,
#                 # confidence = 0.95,
#                 gapCV = 7*48,
#                 trace = TRUE,
#                 reltol = 0.0005)
#
# plot(elec.fit.i$output$predictors[[1]]$beta, type = "b")
#
# plot(elec.fit$output$predictors[[5]]$beta, type = "b")
#
#
# plot(TrendTempM2$data, type = "b")
#
# elec.fit.i2 <- STRmodel(data = ri,
#                        lambdas = list(list(lambdas = c(1000000000,0,0))),
#                        predictors = Predictors2)
#
# plot(elec.fit.i2$output$predictors[[1]]$beta, type = "b")
#
# b = elec.fit.i2$output$predictors[[1]]$beta
# x = b
# # x = rnorm(length(b))
#
# length(which(diff(sign(diff(x))) < 0) + 2)
#
# length(b) /28
#
#
# library(quantmod)
