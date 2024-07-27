#' @import quantreg
#' @import SparseM
#' @importFrom stats optim
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats time
#' @importFrom methods as
#' @importFrom methods new

getLowerUpperRSTR <- function(m, confidence) {
  confidence <- sort(union(confidence, 1 - confidence))
  lu <- matrix(0, ncol(m), length(confidence))
  for (j in 1:ncol(m)) {
    lu[j, ] <- quantile(m[, j], confidence, names = FALSE)
  }
  return(list(lower = lu[, 1:(ncol(lu) / 2), drop = FALSE], upper = lu[, (ncol(lu) / 2 + 1):ncol(lu), drop = FALSE]))
}

#' @title Robust STR decomposition
#' @description Robust Seasonal-Trend decomposition of time series data using Regression (robust version of \code{\link{STRmodel}}).
#' @seealso \code{\link{STRmodel}} \code{\link{STR}}
#' @inheritParams data
#' @inheritParams predictors
#' @inheritParams strDesign
#' @inheritParams lambdas
#' @inheritParams confidence
#' @inheritParams nMCIter
#' @inheritParams control
#' @inheritParams reportDimensionsOnly
#' @inheritParams trace
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{method} -- always contains string \code{"RSTRmodel"} for this function.
#' @templateVar topLevel2 \strong{}
#' @templateVar topLevel3 \strong{}
#' @templateVar topLevel4 \strong{}
#' @templateVar topLevel5 \strong{}
#' @template returnValue
#' @references Dokumentov, A., and Hyndman, R.J. (2022)
#' STR: Seasonal-Trend decomposition using Regression,
#' \emph{INFORMS Journal on Data Science}, 1(1), 50-62.
#' \url{https://robjhyndman.com/publications/str/}
#' @examples
#' \donttest{
#' n <- 70
#' trendSeasonalStructure <- list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
#' ns <- 5
#' seasonalStructure <- list(
#'   segments = list(c(0, ns)),
#'   sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
#' )
#' seasons <- (0:(n - 1)) %% ns + 1
#' trendSeasons <- rep(1, length(seasons))
#' times <- seq_along(seasons)
#' data <- seasons + times / 4
#' set.seed(1234567890)
#' data <- data + rnorm(length(data), 0, 0.2)
#' data[20] <- data[20] + 3
#' data[50] <- data[50] - 5
#' plot(times, data, type = "l")
#' timeKnots <- times
#' trendData <- rep(1, n)
#' seasonData <- rep(1, n)
#' trend <- list(
#'   data = trendData, times = times, seasons = trendSeasons,
#'   timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1, 0, 0)
#' )
#' season <- list(
#'   data = seasonData, times = times, seasons = seasons,
#'   timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(1, 0, 1)
#' )
#' predictors <- list(trend, season)
#' rstr <- RSTRmodel(data, predictors, confidence = 0.8)
#' plot(rstr)
#' }
#' @author Alexander Dokumentov
#' @export

RSTRmodel <- function(data, predictors = NULL, strDesign = NULL, lambdas = NULL,
                      confidence = NULL, # confidence = c(0.8, 0.95)
                      nMCIter = 100,
                      control = list(nnzlmax = 1000000, nsubmax = 300000, tmpmax = 50000),
                      reportDimensionsOnly = FALSE,
                      trace = FALSE) {
  if (is.null(strDesign) && !is.null(predictors)) {
    strDesign <- STRDesign(predictors, norm = 1)
    lambdas <- predictors
  }
  if (is.null(strDesign)) stop("(strDesign and lambdas) or predictors should be provided...")
  cm <- strDesign$cm
  rm <- strDesign$rm
  lm <- lambdaMatrix(lambdas, rm$seats)
  design <- rbind(cm$matrix, lm %*% rm$matrix)
  if (trace) {
    cat("\nDesign matrix dimensions: ")
    cat(dim(design))
    cat("\n")
  }
  if (reportDimensionsOnly) {
    return(NULL)
  }

  noNA <- !is.na(data)
  y <- as.vector(data)[noNA]
  X <- design[c(noNA, rep(TRUE, nrow(design) - length(noNA))), ] # noNA should be extended with TRUE values to keep rows resposible for regularisation
  C <- cm$matrix[noNA, ]
  CC <- cm$matrix

  X2 <- as(X, "dgTMatrix")
  X.csr <- as.matrix.csr(new("matrix.coo", ra = X2@x, ia = X2@i + 1L, ja = X2@j + 1L, dimension = X2@Dim))

  suppressWarnings({
    fit <- rq.fit.sfn(X.csr, y = c(y, rep(0, nrow(X) - length(y))), control = control)
  })
  coef <- fit$coef
  dataHat <- CC %*% coef

  if (is.null(predictors)) predictors <- strDesign$predictors
  components <- extract(as.vector(coef), as.vector(data) - as.vector(dataHat), NULL, cm$matrix, cm$seats, predictors, NULL)

  if (!is.null(confidence)) {
    yHat <- (X.csr %*% coef)[seq_along(y)]
    res <- y - yHat

    if (getDoParWorkers() <= 1) registerDoSEQ() # A way to avoid warning from %dopar% when no parallel backend is registered
    # compList = list()
    # for(i in 1:nMCIter) {
    compList <- foreach(i = 1:nMCIter) %dopar% {
      if (trace) {
        cat("\nIteration ")
        cat(i)
      }

      rand <- sample(res) # TODO: Autocorrelation is lost here
      dy <- rand - res
      suppressWarnings({
        dFit <- rq.fit.sfn(X.csr, y = c(dy, rep(0, nrow(X) - length(dy))), control = control)
      })
      dCoef <- dFit$coef
      coefR <- coef + dCoef
      dataHatR <- CC %*% coefR
      componentsR <- extract(as.vector(coefR), as.vector(data) - as.vector(dataHatR), NULL, cm$matrix, cm$seats, predictors, NULL)
      # compList[[length(compList)+1]] = componentsR
      componentsR
    }

    m <- matrix(0, length(compList), length(components$forecast$data))
    for (i in seq_along(compList)) {
      m[i, ] <- compList[[i]]$forecast$data
    }

    lu <- getLowerUpperRSTR(m, confidence)
    components$forecast$upper <- lu$upper
    components$forecast$lower <- lu$lower

    for (p in seq_along(components$predictors)) {
      m <- matrix(0, length(compList), length(components$predictors[[p]]$data))
      for (i in seq_along(compList)) {
        m[i, ] <- compList[[i]]$predictors[[p]]$data
      }

      lu <- getLowerUpperRSTR(m, confidence)
      components$predictors[[p]]$upper <- lu$upper
      components$predictors[[p]]$lower <- lu$lower
    }
  }

  result <- list(output = components, input = list(data = data, predictors = predictors, lambdas = lambdas), method = "RSTRmodel")
  class(result) <- "STR"
  return(result)
}

nFoldRSTRCV <- function(n, trainData, fcastData, trainC, fcastC, regMatrix, regSeats, lambdas, control) {
  SAE <- 0
  l <- 0
  lm <- lambdaMatrix(lambdas, regSeats)
  R <- lm %*% regMatrix
  # resultList = list()
  # for(i in 1:n) {
  resultList <- foreach(i = 1:n) %dopar% {
    noNA <- !is.na(trainData[[i]])
    y <- (trainData[[i]])[noNA]
    C <- (trainC[[i]])[noNA, ]
    X <- rbind(C, R)

    X2 <- as(X, "dgTMatrix")
    X.csr <- as.matrix.csr(new("matrix.coo", ra = X2@x, ia = X2@i + 1L, ja = X2@j + 1L, dimension = X2@Dim))

    suppressWarnings({
      fit <- rq.fit.sfn(X.csr, y = c(y, rep(0, nrow(X) - length(y))), control = control)
    })
    coef <- fit$coef

    fcast <- fcastC[[i]] %*% coef
    resid <- fcastData[[i]] - as.vector(fcast)
    # resultList[[length(resultList) + 1]] = c(SAE = sum(abs(resid), na.rm = TRUE), l = sum(!is.na(resid)))
    c(SAE = sum(abs(resid), na.rm = TRUE), l = sum(!is.na(resid)))
  }
  for (i in seq_along(resultList)) {
    SAE <- SAE + resultList[[i]][1]
    l <- l + resultList[[i]][2]
  }
  if (l == 0) {
    return(Inf)
  }
  return(SAE / l)
}

RSTR_ <- function(data, predictors,
                  confidence = NULL, # confidence = c(0.8, 0.95),
                  nMCIter = 100,
                  lambdas = NULL,
                  pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
                  control = list(nnzlmax = 1000000, nsubmax = 300000, tmpmax = 50000),
                  trace = FALSE) {
  if (getDoParWorkers() <= 1) registerDoSEQ() # A way to avoid warning from %dopar% when no parallel backend is registered
  f <- function(p) {
    p <- exp(p) # Optimisation is on log scale
    if (trace) {
      cat("\nParameters = [")
      cat(p)
      cat("]\n")
    }
    newLambdas <- createLambdas(p, pattern = pattern, original = origP)
    cv <- nFoldRSTRCV(
      n = nFold,
      trainData = trainData, fcastData = fcastData,
      trainC = trainC, fcastC = fcastC,
      regMatrix = regMatrix, regSeats = regSeats,
      lambdas = newLambdas,
      control = control
    )
    if (trace) {
      cat("CV = ")
      cat(cv)
      cat("\n")
    }
    return(cv)
  }

  lData <- length(data)
  subInds <- lapply(1:nFold, FUN = function(i) sort(unlist(lapply(1:gapCV, FUN = function(j) seq(from = (i - 1) * gapCV + j, to = lData, by = nFold * gapCV)))))
  complInds <- lapply(subInds, FUN = function(s) setdiff(1:lData, s))

  strDesign <- STRDesign(predictors)
  C <- strDesign$cm$matrix
  fcastC <- lapply(subInds, FUN = function(si) C[si, ])
  trainC <- lapply(complInds, FUN = function(ci) C[ci, ])
  fcastData <- lapply(subInds, FUN = function(si) data[si])
  trainData <- lapply(complInds, FUN = function(ci) data[ci])
  rm <- strDesign$rm
  regMatrix <- rm$matrix
  regSeats <- rm$seats

  if (!is.null(lambdas)) {
    initP <- extractP(lambdas, pattern)
    origP <- abs(extractP(lambdas, rep(TRUE, length(pattern))))
  } else {
    initP <- extractP(predictors, pattern)
    origP <- abs(extractP(predictors, rep(TRUE, length(pattern))))
  }
  # Optimisation is performed on log scale
  optP <- optim(par = log(initP), fn = f, method = "Nelder-Mead", control = list(reltol = reltol))
  newLambdas <- createLambdas(exp(optP$par), pattern, original = origP)

  result <- RSTRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = confidence, nMCIter = nMCIter, control = control, trace = trace)
  result$optim.CV.MAE <- optP$value
  result$nFold <- nFold
  result$gapCV <- gapCV
  result$method <- "RSTR"
  return(result)
}
