library(graphics)
library(rgl)

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
    plot(translatedTimes, beta, type = type, pch = pch, xlab = "Time", ylab = paste0(x$input$predictors[[predictorN]]$name, ": beta"))
  } else {
    m = matrix(beta, ncol = length(translatedTimes))
    if(dim[1] == 1) {
      plot(translatedTimes, m[1,], ylim = range(m), type = type, pch = pch, xlab = "Time", ylab = paste0(x$input$predictors[[predictorN]]$name, ": beta"))
      for(i in tail(seq_len(nrow(m)), -1)) {
        lines(translatedTimes, m[i,], type = type, pch = pch, col = i)
      }
    } else if(dim[1] == 2) {
      ylabs = vapply(x$input$predictors[[predictorN]]$seasonalStructure$sKnots,
             FUN = function(x) do.call("paste", as.list(format(x))),
             FUN.VALUE = "")
      filled.contour(translatedTimes, seq_len(nrow(m)), t(m),
                     zlim = range(m), color.palette = palette,
                     plot.axes = {
                       axis(side = 1, at = translatedTimes, labels = format(translatedTimes));
                       axis(side = 2, at = seq_len(nrow(m)), labels = head(ylabs, nrow(m)))
                     }
      )
    } else if(dim[1] == 3) {
      color.palette = function(n) rainbow(n, start=0.0, end=0.7)
      rng = range(m)
      if(diff(rng) == 0) {
        col = "green"
      }
      else {
        col = (t(m) - rng[1])/diff(rng)
        r = color.palette(65536)
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
      rgl.bringtotop()
    } else {
      stop("dim paramemetr is incorrect. Must be 1, 2, or 3.")
    }
  }
}

# for(i in 1:5)
#   plotBeta(elec.fit, xTime = as.Date("2000-01-11") + ((Times-1)/48-10), predictorN = i, dim = 2)
