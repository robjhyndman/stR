#' @importFrom graphics Axis
#' @importFrom graphics abline
#' @importFrom graphics layout
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics plot.new
#' @importFrom graphics polygon

getLimits <- function(l) {
  d <- lapply(l, FUN = function(x) c(x$data, x$upper, x$lower))
  return(c(min(unlist(d), na.rm = TRUE), max(unlist(d), na.rm = TRUE)))
}

getYlab <- function(l) {
  names <- unlist(lapply(l, FUN = function(x) x$name))
  return(do.call("paste", c(as.list(names), sep = ", ")))
}

getLegend <- function(z) {
  x <- z$input
  l0 <- NULL
  if (!is.null(z$cvMSE)) {
    l0 <- paste0("LOO MSE = ", signif(z$cvMSE, 4), ", ")
  }
  if (!is.null(z$optim.CV.MSE)) {
    l0 <- paste0(l0, z$nFold, " fold ", z$gapCV, " gap MSE = ", signif(z$optim.CV.MSE, 4), ", ")
  }
  if (!is.null(z$optim.CV.MAE)) {
    l0 <- paste0(l0, z$nFold, " fold ", z$gapCV, " gap MAE = ", signif(z$optim.CV.MAE, 4), ", ")
  }

  if (length(x$lambdas) > 0) {
    l <- "Lambdas ="
    for (p in x$lambdas) {
      l <- paste(l, " (", paste(signif(p$lambdas, 3), collapse = ","), ")", sep = "")
    }
  } else {
    l <- NULL
  }

  return(paste0(l0, l))
}

createLayoutMatrix <- function(dataPanels, predictorPanels, randomPanels, forecastPanels, rHeader = 1, r = 3) {
  nPanels <- max(unlist(predictorPanels), dataPanels, randomPanels, forecastPanels)
  i <- 1
  v <- rep(i, rHeader)
  i <- i + 1
  for (k in seq_len(nPanels)) {
    v <- c(v, rep(i, r))
    i <- i + 1
  }
  return(as.matrix(v))
}

getDataToPlot <- function(scr, x, dataPanels, predictorPanels, randomPanels, forecastPanels, dataColor, predictorColors, randomColor, forecastColor) {
  toPlot <- list()
  j <- 1
  for (k in seq_along(predictorPanels)) {
    if (scr %in% predictorPanels[[k]] && !is.null(x$output$predictors[[k]]$upper)) {
      for (l in rev(seq_len(ncol(x$output$predictors[[k]]$upper)))) {
        toPlot[[j]] <- list(
          upper = x$output$predictors[[k]]$upper[, l],
          lower = x$output$predictors[[k]]$lower[, l],
          type = "polygon", col = "grey", border = "darkgrey", name = NULL
        )
        j <- j + 1
      }
    }
  }
  if (scr %in% forecastPanels && !is.null(x$output$forecast$upper)) {
    for (l in rev(seq_len(ncol(x$output$forecast$upper)))) {
      toPlot[[j]] <- list(
        upper = x$output$forecast$upper[, l],
        lower = x$output$forecast$lower[, l],
        type = "polygon", col = "grey", border = "darkgrey", name = NULL
      )
      j <- j + 1
    }
  }
  if (scr %in% dataPanels) {
    toPlot[[j]] <- list(data = x$input$data, type = "l", col = dataColor, name = "Observed")
    j <- j + 1
  }
  if (scr %in% randomPanels) {
    toPlot[[j]] <- list(data = x$output$random$data, type = "h", col = randomColor, name = "Random")
    j <- j + 1
  }
  for (k in seq_along(predictorPanels)) {
    if (scr %in% predictorPanels[[k]]) {
      toPlot[[j]] <- list(data = x$output$predictors[[k]]$data, type = "l", col = predictorColors[k], name = x$input$predictors[[k]]$name)
      j <- j + 1
    }
  }
  if (scr %in% forecastPanels) {
    toPlot[[j]] <- list(data = x$output$forecast$data, type = "l", col = forecastColor, name = "Fit/Forecast")
    j <- j + 1
  }
  return(toPlot)
}

#' @name plot.STR
#' @rdname plot.STR
#'
#' @title Plots the results of decomposition
#' @description \code{plot.STR} plots results of STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param x Result of STR decomposition.
#' @param xTime Times for data to plot.
#' @param dataPanels Vector of panel numbers in which to plot the original data. Set to \code{NULL} to not show data.
#' @param predictorPanels A list of vectors of numbers where every such vector describes which panels should be used for plotting the corresponding predictor.
#' @param randomPanels Vector of panel numbers in which to plot the residuals.  Set to \code{NULL} to not show residuals.
#' @param forecastPanels Vector of panel numbers in which to plot the fit/forecast.  Set to \code{NULL} to not show forecasts.
#' @param dataColor Color to plot data.
#' @param predictorColors Vector of colors to plot components corresponding to the predictors.
#' @param randomColor Color to plot the residuals.
#' @param forecastColor Color to plot the fit/forecast.
#' @param vLines Vector of times where vertical lines will be plotted.
#' @param xlab Label for horizontal axis.
#' @param main Main heading for plot.
#' @param showLegend When \code{TRUE} (default) legend is shown at top of plot.
#' @param ... Other parameters to be passed directly to plot and lines functions in the implementation.
#' @author Alexander Dokumentov
#' @examples
#' \donttest{
#' fit <- AutoSTR(log(grocery))
#' plot(fit, forecastPanels = 0, randomColor = "DarkGreen", vLines = 2000:2010, lwd = 2)
#' }
#' @method plot STR
#' @export

plot.STR <- function(x, xTime = NULL, dataPanels = 1,
                     predictorPanels = as.list(seq_along(x$output$predictors)),
                     randomPanels = length(x$output$predictors) + 1,
                     forecastPanels = length(x$output$predictors) + 2,
                     dataColor = "black",
                     predictorColors = rep("red", length(x$output$predictors)),
                     randomColor = "red",
                     forecastColor = "blue",
                     vLines = NULL,
                     xlab = "Time",
                     main = ifelse(x$method %in% c("STR", "STRmodel"), "STR decomposition", "Robust STR decomposition"),
                     showLegend = TRUE, ...) {
  if (is.null(xTime)) {
    xTime <- as.vector(time(x$input$data))
  }
  if (length(x$input$data) != length(xTime)) {
    stop("Lengths of x and xTime should be same.")
  }
  op <- par(no.readonly = TRUE) # Resets parameters to the default state
  on.exit(par(op))

  lm <- createLayoutMatrix(dataPanels, predictorPanels, randomPanels, forecastPanels)
  layout(lm)
  par(mar = c(0, 4, 0, 0.5), oma = c(4.5, 0, 2, 0))
  plot.new()
  if (showLegend) {
    legendtext <- getLegend(x)
    if (!is.null(legendtext)) {
      legend("topleft", horiz = FALSE, bty = "n", legend = legendtext)
    }
  }

  nPanels <- max(unlist(predictorPanels), dataPanels, randomPanels, forecastPanels)
  for (scr in 1:nPanels) {
    toPlot <- getDataToPlot(scr, x, dataPanels, predictorPanels, randomPanels, forecastPanels, dataColor, predictorColors, randomColor, forecastColor)
    ylim <- getLimits(toPlot)
    ylab <- getYlab(toPlot)
    plot(xTime, x$input$data, ylab = ylab, type = "n", ylim = ylim, xaxt = "n", ...)
    Axis(x = xTime, side = 1, labels = scr == nPanels)
    abline(h = 0, col = "grey")
    if (!is.null(vLines)) {
      abline(v = vLines, col = "grey", ...)
    }
    for (p in toPlot) {
      if (p$type == "polygon") {
        polygon(c(xTime, rev(xTime)), c(p$upper, rev(p$lower)), col = p$col, border = p$border)
      } else {
        lines(xTime, p$data, col = p$col, type = p$type, ...)
      }
    }
  }
  mtext(xlab, side = 1, outer = TRUE, line = 2.5, cex = 0.9)
  mtext(main, side = 3, outer = TRUE)
}
