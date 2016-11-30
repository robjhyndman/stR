plotBeta = function(x, xTime = NULL, predictorN = 1, type = "o", pch = 20)
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
  plot(translatedTimes, beta, type = type, pch = pch, xlab = "Time", ylab = paste0(x$input$predictors[[predictorN]]$name, ": beta"))
}

plotBeta(elec.fit, xTime = xTime, predictorN = 1)
