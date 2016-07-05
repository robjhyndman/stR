getLimits = function(l)
{
  d = lapply(l, FUN = function(x) c(x$data, x$upper, x$lower))
  return(c(min(unlist(d), na.rm=T), max(unlist(d), na.rm=T)))
}

getYlab = function(l)
{
  names = unlist(lapply(l, FUN = function(x) x$name))
  return(do.call("paste", c(as.list(names), sep = ", ")))
}

getLegend = function(z)
{
  x = z$input
  l0 = NULL
  if(!is.null(z$cvMSE)) {
    l0 = paste0("LOO MSE = ", signif(z$cvMSE, 4), ", ")
  }
  if(!is.null(z$optim.CV.MSE)) {
    l0 = paste0(l0, z$nFold, " fold ", z$gapCV, " gap MSE = ", signif(z$optim.CV.MSE, 4), ", ")
  }
  if(!is.null(z$optim.CV.MAE)) {
    l0 = paste0(l0, z$nFold, " fold ", z$gapCV, " gap MAE = ", signif(z$optim.CV.MAE, 4), ", ")
  }

  if(length(x$lambdas) > 0) {
    l = "Lambdas ="
    for(p in x$lambdas) {
      l = paste(l, " (", paste(signif(p$lambdas, 3), collapse=","), ")", sep = "")
    }
  }
  else {
    l = NULL
  }

  return(paste0(l0 ,l))
}

createLayoutMatrix = function(dataScreens, predictorScreens, randomScreens, forecastScreens, rHeader = 1, r = 3)
{
  nScreens = max(unlist(predictorScreens), dataScreens, randomScreens, forecastScreens)
  i = 1
  v = rep(i, rHeader); i = i + 1
  for(k in seq_len(nScreens)) {
    v = c(v, rep(i, r)); i = i + 1
  }
  return(as.matrix(v))
}

getDataToPlot = function(scr, x, dataScreens, predictorScreens, randomScreens, forecastScreens, dataColor, predictorColors, randomColor, forecastColor)
{
  toPlot = list()
  j = 1
  for(k in seq_along(predictorScreens)) {
    if(scr %in% predictorScreens[[k]] && !is.null(x$output$predictors[[k]]$upper)) {
      for(l in rev(seq_len(ncol(x$output$predictors[[k]]$upper)))) {
        toPlot[[j]] = list(upper = x$output$predictors[[k]]$upper[,l],
                           lower = x$output$predictors[[k]]$lower[,l],
                           type = "polygon", col = "grey", border = "darkgrey", name = NULL)
        j = j + 1
      }
    }
  }
  if(scr %in% forecastScreens && !is.null(x$output$forecast$upper)) {
    for(l in rev(seq_len(ncol(x$output$forecast$upper)))) {
      toPlot[[j]] = list(upper = x$output$forecast$upper[,l],
                         lower = x$output$forecast$lower[,l],
                         type = "polygon", col = "grey", border = "darkgrey", name = NULL)
      j = j + 1
    }
  }
  if(scr %in% dataScreens) {
    toPlot[[j]] = list(data = x$input$data, type = "l", col = dataColor, name = "Observed")
    j = j + 1
  }
  if(scr %in% randomScreens) {
    toPlot[[j]] = list(data = x$output$random$data, type = "h", col = randomColor, name = "Random")
    j = j + 1
  }
  for(k in seq_along(predictorScreens)) {
    if(scr %in% predictorScreens[[k]]) {
      toPlot[[j]] = list(data = x$output$predictors[[k]]$data, type = "l", col = predictorColors[k], name = x$input$predictors[[k]]$name)
      j = j + 1
    }
  }
  if(scr %in% forecastScreens) {
    toPlot[[j]] = list(data = x$output$forecast$data, type = "l", col = forecastColor, name = "Fit/Forecast")
    j = j + 1
  }
  return(toPlot)
}

#' @name plot
#' @rdname plot
#'
#' @title Plots results of STR and RSTR decompositions.
#' @description {\link{plot.STR}} and \code{\link{plot.RSTR}} plot results of STR and RSTR decompositions.
#' @seealso \code{\link{plot.STR}} \code{\link{plot.RSTR}} \code{\link{STR}} \code{\link{AutoSTR}}
#' @param x Result of STR (or RSTR) function.
#' @param xTime Times for data to plot.
#' @param dataScreens Vector of screen numbers to plot the original data.
#' @param predictorScreens A list of vectors of numbers where every such vector describes which screens should be used for plotting the corresponding predictor.
#' @param randomScreens Vector of screen numbers to plot the residuals.
#' @param forecastScreens Vector of screen numbers to plot the fit/forecast.
#' @param dataColor Color to plot data.
#' @param predictorColors Vector of colors to plot components corresponding to the predictors.
#' @param randomColor Color to plot the residuals.
#' @param forecastColor Color to plot the fit/forecast.
#' @param lwd Width of lines at the plots
#' @param vLines Vector of times where vertical lines will be plotted.
#' @author Alex Dokumentov
NULL

#' @rdname plot
#' @export

plot.STR = function(x, xTime = seq_along(x$input$data), dataScreens = 1,
                    predictorScreens = as.list(seq_along(x$output$predictors)), randomScreens = length(x$output$predictors)+1, forecastScreens = length(x$output$predictors)+2,
                    dataColor = "black", predictorColors = rep("red", length(x$output$predictors)), randomColor = "red", forecastColor = "blue",
                    lwd = 1, vLines = NULL)
{
  if(length(x$input$data) != length(xTime)) stop("Lengths of x and xTime should be same...")
  op = par(no.readonly = T)

  lm = createLayoutMatrix(dataScreens, predictorScreens, randomScreens, forecastScreens)
  layout(lm)
  par(mar = c(0.01, 4, 0, 0.05))
  plot.new()
  legend = getLegend(x)
  if(!is.null(legend)) {
    legend("topleft", horiz = F, bty = "n", legend = legend, ncol = 2)
  }

  nScreens = max(unlist(predictorScreens), dataScreens, randomScreens, forecastScreens)
  for(scr in 1:nScreens) {
    toPlot = getDataToPlot(scr, x, dataScreens, predictorScreens, randomScreens, forecastScreens, dataColor, predictorColors, randomColor, forecastColor)
    ylim = getLimits(toPlot)
    ylab = getYlab(toPlot)
    plot(xTime, x$input$data, ylab = ylab, type="n", ylim = ylim, lwd = lwd)
    abline(h=0, col = "grey")
    if(!is.null(vLines)) abline(v=vLines, col = "grey", lwd = lwd)
    for(p in toPlot) {
      if(p$type == "polygon") {
        #         polygon(c(seq_along(p$upper), rev(seq_along(p$lower))), c(p$upper, rev(p$lower)), col = p$col, border = p$border)
        polygon(c(xTime, rev(xTime)), c(p$upper, rev(p$lower)), col = p$col, border = p$border)
      }
      else {
        lines(xTime, p$data, col = p$col, type = p$type, lwd = lwd)
      }
    }
  }

  par(op)
}

#' @rdname plot
#' @details \code{\link{plot.RSTR}} redirects call to \code{\link{plot.STR}}.
#' @export

plot.RSTR = function(...) plot.STR(...)
