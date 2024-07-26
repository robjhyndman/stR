#' @import compiler
#' @import quantreg
#' @import Matrix
#' @import foreach
#' @importFrom stats optim
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats time
# library(doMC)
# registerDoMC(8) #Number of CPU cores

ensure = function(...) stopifnot(...)

initialize = cmpfun(function(env, nRows, nCols, len)
{
  env$d <- c(nRows, nCols)
  env$ra <- numeric(len)
  env$ia <- integer(len)
  env$ja <- integer(len)
  env$l <- 0L
})

append = cmpfun(function(env, val, i, j)
{
  b <- env$l + 1L
  env$l <- env$l + length(val)
  env$ra[b:env$l] <- val
  env$ia[b:env$l] <- i
  env$ja[b:env$l] <- j
})

dims = cmpfun(function(env, nRows, nCols)
{
  env$d <- c(nRows, nCols)
})

last <- cmpfun(function(x)
{
  tail(x, n = 1)
})

first <- cmpfun(function(x)
{
  head(x, n = 1)
})

# One dimensional territory belonging to a seasonal knot (for all seasonal knots).
# Territory is the half sum of all lengths of all segments attached to the knot
# (there can be more or less than two attached to a knot if the seasonal
# structure is not a circle or a segment).
sTerritory = cmpfun(function(seasonalStructure)
{
  sKnots = seasonalStructure$sKnots
  sKnotsV = sort(unlist(sKnots))
  segments = seasonalStructure$segments

  lefts = sapply(segments, FUN = min)
  rights = sapply(segments, FUN = max)

  sKnotsNoLefts = lapply(sKnots, FUN = function(k) { k[!(k %in% lefts)] })
  sKnotsNoRights = lapply(sKnots, FUN = function(k) { k[!(k %in% rights)] })

  lDistsSum = sapply(sKnotsNoLefts, FUN = function(k) { ifelse(length(k) == 0, 0, sum(sapply(k, FUN = function(kk) { kk - max(sKnotsV[sKnotsV < kk]) }))) })
  rDistsSum = sapply(sKnotsNoRights, FUN = function(k) { ifelse(length(k) == 0, 0, sum(sapply(k, FUN = function(kk) { min(sKnotsV[sKnotsV > kk]) - kk }))) })

  return((lDistsSum + rDistsSum)/2)
})

# Seasonal weights taking into account the territory around a seasonal knot and the norm
sWeights = cmpfun(function(seasonalStructure, norm = 2)
{
  return(sTerritory(seasonalStructure)^(1/norm))
})

tTerritory = cmpfun(function(timeKnots)
{
  distances = diff(timeKnots)
  return((c(0, distances) + c(distances, 0))/2)
})

tWeights = cmpfun(function(timeKnots, norm = 2)
{
  return(tTerritory(timeKnots)^(1/norm))
})

# Transforms reduced season matrix into full season matrix.
# The source and the result are in vectorised forms (seasons are changing quicker).
# Reduced seasonal matrix is a seasonal matrix where one season is omitted as input value
# (since value of that season can be calculated as negative of weighted sum of other values)
seasonalTransformer = cmpfun(function(nKnots, seasonalStructure)
{
  sKnots = seasonalStructure$sKnots
  nSKnots = length(sKnots)
  weights = sTerritory(seasonalStructure)
  lw = length(weights)
  m = rbind(Diagonal(n = nSKnots-1), Matrix(data = -weights[-lw] / weights[lw], nrow = 1, sparse = TRUE))

  l = list(m)
  for(i in 1:nKnots) {
    l[[i]] = m
  }

  return(bdiag(l))
})

#Transposes vectorised full seasonal matrix (to make time changing quicker in the representation)
seasonalTransposer = cmpfun(function(nKnots, nSKnots)
{
  m = new.env(parent = .GlobalEnv)
  initialize(env = m, nRows = nKnots*nSKnots, nCols = nKnots*nSKnots, len = nKnots*nSKnots)

  for(t in 1:nKnots) {
    for(s in 1:nSKnots) {
      append(m, 1, (s-1)*nKnots + t, (t-1)*nSKnots + s)
    }
  }

  return(sparseMatrix(x = m$ra[1L:m$l], i = as.integer(m$ia[1L:m$l]), j = as.integer(m$ja[1L:m$l]), dims = as.integer(m$d)))
})

# Example data:
# All connection points must be knots as well.
# Distances from any connection point to the nearest knots must be same for every connection point.
# data = c(1,3,4,1,2,3,4,1,2,3,5,1,2,3,4,5)
# times = c(1,3,4,5.5,12,16,20,21,22,22.2,23,24,27,28,30,32)
# seasons = c(0,3,5,8,11,16,20,0,3,5,8,12,213,222,100,110)
# timeKnots = c(0,3,5.5,17,33)
# ttLambda = 0.7
# ssLambda = 0.02
# stLambda = 0.9
# lambdas = c(ttLambda, ssLambda, stLambda)
# seasonalStructure = list(segments = list(c(0,24), c(100,124), c(212,224), c(312,324)),
#                          sKnots = list(c(0,24,324),4,8,c(12, 212),16,20,c(100,124,224),104,108,c(112,312),116,120,216,220,316,320)
#                          )
# predictor = list(data = data, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = lambdas)

checkPredictor = cmpfun(function(predictor)
{
  tkn = predictor$timeKnots
  tm = predictor$times
  ss = predictor$seasonalStructure
  seg = ss$segments
  skn = ss$sKnots
  if(length(predictor$data) != length(predictor$times)) {cat("\nVector lengths of data and times should be same..."); return(FALSE)}
  if(is.null(tkn) && is.null(predictor$seasons) && is.null(ss)) return(TRUE)
  if(length(unlist(skn)) != length(unique(unlist(skn)))) {cat("\nPoints in sKnots should be mentioned once..."); return(FALSE)}
  if(!all(unlist(seg) %in% unlist(skn))) {cat("\nPoints in segments should be mentioned in sKnots..."); return(FALSE)}
  if(length(predictor$seasons) != length(predictor$times)) {cat("\nVector lengths of seasons and times should be same..."); return(FALSE)}
  if(min(tm) < min(tkn)) {cat("\nThe first time knot should be before the first observation time..."); return(FALSE)}
  if(max(tm) > max(tkn)) {cat("\nThe last time knot should be beyond the last observation time..."); return(FALSE)}
  return(TRUE)
})

# ensure(checkPredictor(predictor))

# Too slow
# knotToIndex = cmpfun(function(knot, sKnots) { which(sapply(sKnots, FUN = function(k) {knot %in% k})) })

# Twice faster than above
knotToIndex = cmpfun(function(knot, sKnots) {
  for(k in seq_along(sKnots)) if(knot %in% sKnots[[k]]) return(k)
  return(0)
})

# The fastest
knotToIndex2 = cmpfun(function(knot, flatKnots, indexes) {
  indexes[flatKnots == knot]
})

# Creates a matrix which distributes any value between knots
# to the corresponding values at knots and any value at a knot
# is applied to this knot directly.
getInfluenceMatrix = cmpfun(function(seasons, sKnots)
{
  m = new.env(parent = .GlobalEnv)
  initialize(m, nRows = length(seasons), nCols = length(sKnots), len = 2*length(seasons))

  flatKnots = unlist(sKnots)
  sIndexes = unlist(lapply(seq_along(sKnots), FUN = function(i) rep(i, length(sKnots[[i]]))))
  for(i in seq_along(seasons)) {
    s = seasons[i]
    if(s %in% flatKnots) {
      append(m, 1, i, knotToIndex2(s, flatKnots, sIndexes))
    }
    else {
      sLeft = max(flatKnots[flatKnots <= s])
      sRight = min(flatKnots[flatKnots > s])
      d = sRight - sLeft
      append(m, (sRight - s)/d, i, knotToIndex2(sLeft, flatKnots, sIndexes))
      append(m, (s - sLeft)/d, i, knotToIndex2(sRight, flatKnots, sIndexes))
    }
  }
  return(sparseMatrix(x = m$ra[1L:m$l], i = as.integer(m$ia[1L:m$l]), j = as.integer(m$ja[1L:m$l]), dims = as.integer(m$d)))
})

# Creates a matrix which takes values at seasonal and time knots and interpolates
# these values at coordinates of the observed points.
seasonalPredictorConstructor = cmpfun(function(predictor)
{
  if(!checkPredictor(predictor)) stop(paste0('Predictor "', predictor$name, '" has wrong structure...'))
  data = predictor$data
  times = predictor$times
  seasons = predictor$seasons
  segments = predictor$seasonalStructure$segments
  nRows = length(data)
  tKnots = predictor$timeKnots
  nKnots = length(tKnots)
  seasonalStructure = predictor$seasonalStructure
  sKnots = seasonalStructure$sKnots
  nSKnots = length(sKnots)
  nCols = length(tKnots) * length(sKnots)

  # Static predictor: single coefficient to estimate without any regularization
  if(is.null(tKnots) && is.null(seasons) && is.null(seasonalStructure)) {
    return(Matrix(data = data, nrow = length(data), ncol = 1, sparse = TRUE))
  }

  msw = getInfluenceMatrix(seasons, sKnots)
  mtw = getInfluenceMatrix(times, tKnots)

  m = Matrix(data = 0, nrow = nRows, ncol = nCols, sparse = TRUE)

  iCol = 1
  nSCol = dim(msw)[2]
  for(t in seq_along(tKnots)) {
    m[,iCol:(iCol+nSCol-1)] = Diagonal(x = mtw[,t]) %*% msw
    iCol = iCol + nSCol
  }

  mData = Diagonal(x = data)
  if(nSKnots == 1) {
    result = mData %*% m
  }
  else {
    mTr = seasonalTransformer(nKnots, seasonalStructure)
    result = (mData %*% m) %*% mTr
  }
  return(result)
})

# Creates a sparse matrix which takes first discrete differences of a vector
diff1 = cmpfun(function(nCols)
{
  return(diff(Diagonal(nCols)))
})

# Creates a sparse matrix which takes second discrete differences of a vector
diff2 = cmpfun(function(nCols)
{
  return(diff1(nCols-1) %*% diff1(nCols))
})

vector2Derivatives = cmpfun(function(knots, weights)
{
  ensure(length(knots)-2 == length(weights))

  m = new.env(parent = .GlobalEnv)
  nCols = length(knots)
  nRows = nCols - 2
  initialize(env = m, nRows = nRows, nCols = nCols, len = nRows*3)

  for(i in 1:nRows) {
    delta1 = knots[i+1] - knots[i]
    delta2 = knots[i+2] - knots[i+1]
    delta = knots[i+2] - knots[i]
    append(m, 2 * weights[i] * c(1/(delta1*delta), -1/(delta1*delta2), 1/(delta2*delta)), c(i, i, i), c(i, i+1, i+2))
  }

  return(sparseMatrix(x = m$ra[1L:m$l], i = as.integer(m$ia[1L:m$l]), j = as.integer(m$ja[1L:m$l]), dims = as.integer(m$d)))
})

lrKnots = cmpfun(function(seasonalStructure)
{
  segments = seasonalStructure$segments
  lefts = sapply(segments, FUN = min)
  rights = sapply(segments, FUN = max)

  sKnots = seasonalStructure$sKnots
  sKnotsV = sort(unlist(sKnots))

  lKnots = lapply(sKnots, FUN = function(k) { sapply(k, FUN = function(kk) { ifelse(kk %in% lefts, NA, max(sKnotsV[sKnotsV < kk])) }) })
  rKnots = lapply(sKnots, FUN = function(k) { sapply(k, FUN = function(kk) { ifelse(kk %in% rights, NA, min(sKnotsV[sKnotsV > kk])) }) })
  return(list(lKnots = lKnots, rKnots = rKnots))
})

# Creates a matrix which translates a seasonal column to the second derivatives
# applied to that column. Since the seasonal structure/topology can be different
# from cycle, the procedure is rather complicated and takes into account cases
# when a node in the seasonal graph can have none/multiple incoming vertexes and/or
# none/multiple outcoming vertexes.
cycle2Derivatives = cmpfun(function(seasonalStructure, norm = 2)
{
  sKnots = seasonalStructure$sKnots
  m = new.env(parent = .GlobalEnv)
  nCols = nSKnots = length(sKnots)
  initialize(env = m, nRows = -1, nCols = nCols, len = nSKnots*3)

  lr = lrKnots(seasonalStructure)
  lKnots = lr$lKnots
  rKnots = lr$rKnots
  nlKnots = sapply(lKnots, FUN = function(k) sum(!is.na(k)))
  nrKnots = sapply(rKnots, FUN = function(k) sum(!is.na(k)))

  nRow = 0
  for(i in 1:nSKnots) {
    for(jPrev in seq_along(lKnots[[i]])) {
      lk = lKnots[[i]][jPrev]
      if(is.na(lk)) next
      for(jNext in seq_along(rKnots[[i]])) {
        rk = rKnots[[i]][jNext]
        if(is.na(rk)) next

        k1 = sKnots[[i]][jPrev]
        d1 = abs(k1 - lk)

        k2 = sKnots[[i]][jNext]
        d2 = abs(rk - k2)

        d = d1 + d2
        w = ((d1/nrKnots[i] + d2/nlKnots[i])/2)^(1/norm)
        nRow = nRow + 1
        append(m, 2*w*c(1/(d1*d), -1/(d1*d2), 1/(d2*d)), c(nRow, nRow, nRow), c(knotToIndex(lk, sKnots), i, knotToIndex(rk, sKnots)))
      }
    }
  }

  dims(env = m, nRows = nRow, nCols = nCols)
  return(sparseMatrix(x = m$ra[1L:m$l], i = as.integer(m$ia[1L:m$l]), j = as.integer(m$ja[1L:m$l]), dims = as.integer(m$d)))
})

# Creates a matrix to calculate regularization values
# when second derivatives are taken along seasonal dimension.
ssRegulariser = cmpfun(function(predictor, norm = 2)
{
  seasonalStructure = predictor$seasonalStructure
  dataLength = length(predictor$data)

  knots = predictor$timeKnots
  sKnots = seasonalStructure$sKnots
  nKnots = length(knots)
  nSKnots = length(sKnots)

  m = cycle2Derivatives(seasonalStructure, norm)

  weights = tWeights(knots, norm) # tWeights (not sWeights) because width along t dimension should be taken into account

  ensure(length(weights) == nKnots)

  listM = list()
  for(i in 1:nKnots) {
    listM[[i]] = weights[i] * m
  }

  m1 = bdiag(listM)
  m2 = seasonalTransformer(nKnots, seasonalStructure)
  return(m1 %*% m2)
})

# Creates a matrix to calculate regularization values
# when second derivatives are taken along time dimension.
ttRegulariser = cmpfun(function(predictor, norm = 2)
{
  seasonalStructure = predictor$seasonalStructure
  timeKnots = predictor$timeKnots
  sKnots = seasonalStructure$sKnots
  nKnots = length(timeKnots)
  nSKnots = length(sKnots)

  m = vector2Derivatives(timeKnots, tWeights(timeKnots, norm)[c(-1,-nKnots)])

  weights = sWeights(seasonalStructure, norm) # sWeights (not tWeights) because width along s dimension should be taken into account

  listM = list()
  for(i in 1:nSKnots) {
    listM[[i]] = weights[i]*m
  }
  m1 = bdiag(listM)

  if(nSKnots >= 2) {
    m2 = seasonalTransposer(nKnots, nSKnots)
    m3 = seasonalTransformer(nKnots, seasonalStructure)
    return(m1 %*% (m2 %*% m3))
  }
  else {
    return(m1)
  }
})

# Translates full seasonal matrix coordinates to vector positions
translST = cmpfun(function(s, t, nSKnots)
{
  return((t-1)*nSKnots + (s-1)%%nSKnots + 1)
})

# Creates a matrix to calculate regularization values
# when second derivatives are taken along s and t dimensions.
stRegulariser = cmpfun(function(predictor, norm = 2)
{
  timeKnots = predictor$timeKnots
  seasonalStructure = predictor$seasonalStructure
  sKnots = seasonalStructure$sKnots
  nKnots = length(timeKnots)
  nSKnots = length(sKnots)

  m = new.env(parent = .GlobalEnv)
  nCols = nKnots*nSKnots
  initialize(m, nRows = -1, nCols = nCols, len = 4*(nKnots-1)*nSKnots)

  lrk = lrKnots(seasonalStructure)
  rKnots = lrk$rKnots

  nRow = 0
  for(t in 1:(nKnots-1)) {
    for(s in 1:nSKnots) {
      for(ss in seq_along(rKnots[[s]])) {
        rKnot = rKnots[[s]][ss]
        if(is.na(rKnot)) next
        knot = sKnots[[s]][ss]
        tDistance = abs(timeKnots[t+1] - timeKnots[t])
        sDistance = abs(rKnot - knot)
        area = tDistance * sDistance
        rKnotInd = knotToIndex(rKnot, sKnots)
        nRow = nRow + 1
        append(m,
          (area ^ (1/norm - 1)) * c(1, -1, -1, 1),
          c(nRow, nRow, nRow, nRow),
          c(translST(s, t, nSKnots), translST(rKnotInd, t, nSKnots), translST(s, t+1, nSKnots), translST(rKnotInd, t+1, nSKnots))
        )
      }
    }
  }
  dims(env = m, nRows = nRow, nCols = nCols)

  m1 = sparseMatrix(x = m$ra[1L:m$l], i = as.integer(m$ia[1L:m$l]), j = as.integer(m$ja[1L:m$l]), dims = as.integer(m$d))
  m2 = seasonalTransformer(nKnots, seasonalStructure)
  return(m1 %*% m2)
})

# Creates regularization matrix for one predictor.
# norm parameter specifies which norm is used L2 or L1. It affects how distance between knots should affect knot weigts.
# lambdas0or1 is a "technical" parameter and when it is TRUE then lambdas are not taken into account. It is used to create
# a "template" matrix to be later adjusted by multiplying by appropriate lambdas. It is done for performance.
predictorRegulariser = cmpfun(function(predictor, norm = 2, lambdas0or1 = FALSE)
{
  nKnots = length(predictor$timeKnots)
  nSKnots = length(predictor$seasonalStructure$sKnots)
  l1 = l2 = l3 = 0
  result = Matrix(data = 0, nrow = 0, ncol = max(nKnots, 1) * max(nSKnots-1, 1)) # Empty matrix,
  # it has 0 rows when l1 = l2 = l3 = 0. The only information it passes in this case is the number of columns.
  if(predictor$lambdas[1] != 0) {
    reg = ifelse(lambdas0or1, 1, predictor$lambdas[1]) * ttRegulariser(predictor, norm)
    l1 = dim(reg)[1]
    result = rbind(result, reg)
  }
  if(predictor$lambdas[2] != 0) {
    reg = ifelse(lambdas0or1, 1, predictor$lambdas[2]) * ssRegulariser(predictor, norm)
    l2 = dim(reg)[1]
    result = rbind(result, reg)
  }
  if(predictor$lambdas[3] != 0) {
    reg = ifelse(lambdas0or1, 1, predictor$lambdas[3]) * stRegulariser(predictor, norm)
    l3 = dim(reg)[1]
    result = rbind(result, reg)
  }
  return(list(matrix = result, lengths = c(l1, l2, l3)))
})

# The "constructor" matrix is responsible for reconstruction the data from the parameters.
constructorMatrix = function(predictors)
{
  l = list()
  i = 1
  predictorSeats = list()
  j = 1
  startCol = 1
  for(predictor in predictors) {
    m = seasonalPredictorConstructor(predictor)
    nCol = ncol(m)
    predictorSeats[[j]] = c(start = startCol, length = nCol)
    startCol = startCol + nCol
    j = j + 1
    l[[i]] = m
    i = i + 1
  }
  return(list(matrix = do.call(cbind, l), seats = predictorSeats))
}

# The "regulariser" matrix is responsible for penalty calculations.
regulariserMatrix = cmpfun(function(predictors, norm = 2, lambdas0or1 = FALSE)
{
  l = list()
  i = 1
  seats = list()
  startRow = 1
  for(predictor in predictors) {
    pr = predictorRegulariser(predictor, norm, lambdas0or1)
    l[[i]] = pr$matrix
    pl = pr$lengths
    seats[[i]] = list(start = startRow, lengths = pl)
    startRow = startRow + sum(pl)
    i = i + 1
  }
  return(list(matrix = bdiag(l), seats = seats))
})

# Calculates a design matrix. The design matrix consists of two matrices one on top of the other:
# "constructor" matrix and "regulariser".
# The first one is responsible for reconstruction the data from the parameters.
# The second is resposible for penalty calculating the penalty corresponding to the parameters.
designMatrix = cmpfun(function(predictors, norm = 2)
{
  cm = constructorMatrix(predictors)$matrix
  rm = regulariserMatrix(predictors, norm)$matrix
  return(rbind(cm, rm))
})

targetVector = cmpfun(function(data, designMatrix)
{
  return(c(data, rep(0, nrow(designMatrix) - length(data))))
})

# Calculates lower and upper confidence/forecasting intervals.
getLowerUpper = cmpfun(function(data, covMatrix, constr, range, confidence)
{
  if(is.null(covMatrix) || is.null(confidence)) return(list())
  sigmas = sqrt(diag(constr %*% covMatrix[range, range] %*% t(constr)))
  lower = upper = NULL
  for(conf in confidence) {
    q1 = qnorm(p = conf, mean = data, sd = sigmas)
    q2 = qnorm(p = 1-conf, mean = data, sd = sigmas)
    lower = cbind(lower, pmin(q1, q2))
    upper = cbind(upper, pmax(q1, q2))
  }
  return(list(lower = lower, upper = upper))
})

extract = cmpfun(function(coef, resid, covMatrix, constructorMatrix, seats, predictors, confidence = NULL)
{
  predictorsData = list()
  for(i in seq_along(predictors)) {
    range = seats[[i]]["start"]:(seats[[i]]["start"] + seats[[i]]["length"] - 1)
    beta = coef[range]
    constr = constructorMatrix[, range, drop = FALSE]
    data = as.vector(constr %*% beta)
    predictorsData[[i]] = c(list(data = data, beta = beta), getLowerUpper(data, covMatrix, constr, range, confidence))
  }
  data = as.vector(constructorMatrix %*% coef)
  forecastData = c(list(data = data, beta = coef), getLowerUpper(data, covMatrix, constructorMatrix, seq_along(coef), confidence))
  return(list(predictors = predictorsData, random = list(data = resid), forecast = forecastData))
})

STRDesign = function(predictors, norm = 2)
{
  cm = constructorMatrix(predictors)
  rm = regulariserMatrix(predictors, norm, lambdas0or1 = TRUE)
  return(list(cm = cm, rm = rm, predictors = predictors, norm = norm))
}

lambdaMatrix = function(lambdas, seats)
{
  ensure(length(lambdas) == length(seats))
  v = NULL
  for(i in seq_along(lambdas)) {
    v = c(v, rep(lambdas[[i]]$lambdas[1], seats[[i]]$lengths[1]), rep(lambdas[[i]]$lambdas[2], seats[[i]]$lengths[2]), rep(lambdas[[i]]$lambdas[3], seats[[i]]$lengths[3]))
  }
  return(Diagonal(x = v))
}

# Returns covariance (diagonal) matrix of errors/residuals.
getISigma = function(resid, firstLength, seats)
{
  d = rep(mean(head(resid, firstLength)^2), firstLength)
  resid = tail(resid, -firstLength)
  for(i in seq_along(seats)) {
    for(j in seq_along(seats[[i]]$lengths)) {
      if(seats[[i]]$lengths[j] > 0) {
        start = seats[[i]]$start
        lBefore = sum(seats[[i]]$lengths[seq_len(j-1)])
        range = (start + lBefore):(start + lBefore + seats[[i]]$lengths[j] - 1)
        d = c(d, rep(mean(resid[range]^2), length(range)))
      }
    }
  }
  return(Diagonal(length(d), d))
}

#' @title STR decomposition
#' @description Seasonal-Trend decomposition of time series data using Regression.
#' @seealso \code{\link{AutoSTR}}
#' @inheritParams data
#' @inheritParams predictors
#' @inheritParams strDesign
#' @inheritParams lambdas
#' @inheritParams confidence
#' @inheritParams solver
#' @inheritParams reportDimensionsOnly
#' @inheritParams trace
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \strong{}
#' @templateVar topLevel3 \strong{}
#' @templateVar topLevel4 \strong{}
#' @templateVar topLevel5 \item \strong{method} -- always contains string \code{"STRmodel"} for this function.
#' @template returnValue
#' @references Dokumentov, A., and Hyndman, R.J. (2016)
#' STR: A Seasonal-Trend Decomposition Procedure Based on Regression
#' \href{https://www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}{www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}
#' @examples
#' n <- 50
#' trendSeasonalStructure <- list(segments = list(c(0,1)), sKnots = list(c(1,0)))
#' ns <- 5
#' seasonalStructure <- list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
#' seasons <- (0:(n-1))%%ns + 1
#' trendSeasons <- rep(1, length(seasons))
#' times <- seq_along(seasons)
#' data <- seasons + times/4
#' plot(times, data, type = "l")
#' timeKnots <- times
#' trendData <- rep(1, n)
#' seasonData <- rep(1, n)
#' trend <- list(data = trendData, times = times, seasons = trendSeasons,
#'   timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
#' season <- list(data = seasonData, times = times, seasons = seasons,
#'   timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(10,0,0))
#' predictors <- list(trend, season)
#'
#' str1 <- STRmodel(data, predictors)
#' plot(str1)
#'
#' data[c(3,4,7,20,24,29,35,37,45)] <- NA
#' plot(times, data, type = "l")
#' str2 <- STRmodel(data, predictors)
#' plot(str2)
#'
#' @author Alexander Dokumentov
#' @export

STRmodel = function(data, predictors = NULL, strDesign = NULL, lambdas = NULL,
               confidence = NULL, # confidence = c(0.8, 0.95)
               solver = c("Matrix", "cholesky"),
               reportDimensionsOnly = FALSE,
               trace = FALSE)
{
  if(is.null(strDesign) && !is.null(predictors)) {
    strDesign = STRDesign(predictors, norm = 2)
  }
  if(is.null(lambdas)) {
    lambdas = predictors
  }
  if(is.null(strDesign)) stop("(strDesign and lambdas) or predictors should be provided...")
  if(any(confidence <= 0 | confidence >= 1))
    stop("confidence must be between 0 and 1")
  cm = strDesign$cm
  rm = strDesign$rm
  lm = lambdaMatrix(lambdas, rm$seats)
  design = rbind(cm$matrix, lm %*% rm$matrix)
  if(trace) {cat("\nDesign matrix dimensions: "); cat(dim(design)); cat("\n")}
  if(reportDimensionsOnly) return(NULL)

  noNA = !is.na(as.vector(data))
  y = as.vector(data)[noNA]
  X = design[c(noNA, rep(TRUE, nrow(design) - length(noNA))),,drop=FALSE] # noNA should be extended with TRUE values to keep rows resposible for regularisation
  if(trace) {cat("X matrix (NA removed) dimensions: "); cat(dim(X)); cat("\n")}
  C = cm$matrix[noNA,,drop=FALSE]
  CC = cm$matrix

  if(is.null(confidence)) {
    coef = lmSolver(X, y, type = solver[1], method = solver[2], trace = trace)
    dataHat = CC %*% coef

    if(is.null(predictors)) predictors = strDesign$predictors
    components = extract(coef = as.vector(coef), resid = as.vector(data) - as.vector(dataHat),
                         covMatrix = NULL, constructorMatrix = cm$matrix, seats = cm$seats,
                         predictors = predictors, confidence = NULL)
    result = list(output = components, input = list(data = data, predictors = predictors, lambdas = lambdas), method = "STRmodel")
    class(result) = "STR"
    return(result)
  } else {
    XtXinv = solve(crossprod(X))
    partialB = XtXinv %*% t(C)
    partialH = C %*% partialB
    partialDiagH = diag(partialH)

    coef = as.vector(partialB %*% y)
    yHat = as.vector(C %*% coef)
    yHatPlus = as.vector(X %*% coef)
    dataHat = as.vector(CC %*% coef)
    resid = y - yHat
    yPlus = c(y, rep(0, length(yHatPlus) - length(y)))
    residPlus = yPlus - yHatPlus
    ISigma = getISigma(residPlus, length(y), rm$seats)
    cvResid = resid/(1-partialDiagH)
    cvMSE = sum((cvResid)^2)/length(resid) # Estimated covarience of the errors
    # Sigma = cvMSE * (XtXinv %*% crossprod(C) %*% XtXinv) # Covariance matrix of the parameters
    Sigma = crossprod((sqrt(ISigma) %*% X) %*% XtXinv) # Covariance matrix of the parameters

    if(is.null(predictors)) predictors = strDesign$predictors
    components = extract(as.vector(coef), as.vector(data) - as.vector(dataHat), Sigma, cm$matrix, cm$seats, predictors, confidence)
    result = list(output = components, input = list(data = data, predictors = predictors, lambdas = lambdas), cvMSE = cvMSE, method = "STRmodel")
    class(result) = "STR"
    return(result)
  }
}

nFoldSTRCV = function(n,
                      trainData, fcastData, completeData = NULL, # parameter completeData is required only for iterative methods
                      trainC, fcastC, completeC,
                      regMatrix, regSeats, lambdas,
                      solver = c("Matrix", "cholesky"), trace = FALSE, iterControl = list(maxiter = 20, tol = 1E-6))
{
  SSE = 0
  l = 0
  lm = lambdaMatrix(lambdas, regSeats)
  R = lm %*% regMatrix

  e = NULL
  if(solver[1] == "iterative") {
    if(solver[2] %in% c("cg-chol", "lsmr-chol", "lsmr")) {
      e = new.env(parent = .GlobalEnv)
    }
    if(solver[2] %in% c("cg-chol", "lsmr-chol")) {
      noNA = !is.na(completeData)
      y = completeData[noNA]
      C = completeC[noNA,]
      X = rbind(C, R)
      coef0 = try(lmSolver(X, y, type = solver[1], method = solver[2], env = e, iterControl = iterControl, trace = trace), silent = !trace)
      if("try-error" %in% class(coef0)) {
        if(trace) cat("\nError in lmSolver... iterative solvers without preconditioners will be used...\n")
      }
    }
  }

  resultList = list()
  # for(i in rev(1:n)) {
  resultList = foreach(i = 1:n) %dopar% {
    noNA = !is.na(trainData[[i]])
    y = (trainData[[i]])[noNA]
    C = (trainC[[i]])[noNA,]
    X = rbind(C, R)
    coef = try(lmSolver(X, y, type = solver[1], method = solver[2], env = e, iterControl = iterControl, trace = trace), silent = !trace)
    if("try-error" %in% class(coef)) {
      if(trace) cat("\nError in lmSolver...\n")
      # return(Inf)
      # next
      c(SSE = Inf, l = 1)
      # c(SSE = 0, l = 0)
    } else {
      fcast = fcastC[[i]] %*% coef
      resid = fcastData[[i]] - as.vector(fcast)
      # resultList[[length(resultList) + 1]] = c(SSE = sum(resid^2, na.rm = TRUE), l = sum(!is.na(resid)))
      c(SSE = sum(resid^2, na.rm = TRUE), l = sum(!is.na(resid)))
    }
  }
  for(i in seq_along(resultList)) {
    SSE = SSE + resultList[[i]][1]
    l = l + resultList[[i]][2]
  }
  if(l == 0) return(Inf)
  return(SSE/l)
}

extractPattern = function(predictors)
{
  pattern = NULL
  for(i in seq_along(predictors)) {
    pattern = c(pattern, predictors[[i]]$lambdas > 0)
  }
  return(pattern)
}

extractP = function(predictors, pattern)
{
  lambdas = NULL
  for(i in seq_along(predictors)) {
    lambdas = c(lambdas, predictors[[i]]$lambdas)
  }
  return(lambdas[pattern])
}

createLambdas = function(p, pattern, original)
{
  ensure(length(pattern) %% 3 == 0)
  ensure(length(pattern) == length(original))

  # pLong = rep(0, length(pattern))
  pLong = original
  pLong[pattern] = p
  i = 1
  l = list()
  for(i in seq_len(length(pattern)%/%3)) {
    pInd = (i-1)*3+1
    l[[i]] = list(lambdas = pLong[pInd:(pInd+2)])
  }
  return(l)
}

#' @rdname STR
#' @name STR
#' @title Automatic STR decomposition
#' @description Automatically selects parameters for an STR decomposition of time series data.
#'
#' If a parallel backend is registered for use before \code{STR} call,
#' \code{STR} will use it for n-fold cross validation computations.
#'
#' @seealso \code{\link{STRmodel}} \code{\link{RSTRmodel}} \code{\link{AutoSTR}}
#' @inheritParams data
#' @inheritParams predictors
#' @inheritParams confidence
#' @inheritParams robust
#' @inheritParams lambdas
#' @inheritParams pattern
#' @inheritParams nFold
#' @inheritParams reltol
#' @inheritParams gapCV
#' @inheritParams solver
#' @inheritParams nMCIter
#' @inheritParams control
#' @inheritParams trace
#' @param iterControl Control parameters for some experimental features.
#' This should not be used by an ordinary user.
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
#' elec.fit <- STR(data = Data,
#'                 predictors = Predictors,
#'                 gapCV = 48*7)
#'
#' plot(elec.fit,
#'      xTime = as.Date("2000-01-11")+((Times-1)/48-10),
#'      forecastPanels = NULL)
#'
#' #########################################################
#'
#' n <- 70
#' trendSeasonalStructure <- list(segments = list(c(0,1)), sKnots = list(c(1,0)))
#' ns <- 5
#' seasonalStructure <- list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
#' seasons <- (0:(n-1))%%ns + 1
#' trendSeasons <- rep(1, length(seasons))
#' times <- seq_along(seasons)
#' data <- seasons + times/4
#' set.seed(1234567890)
#' data <- data + rnorm(length(data), 0, 0.2)
#' data[20] <- data[20]+3
#' data[50] <- data[50]-5
#' plot(times, data, type = "l")
#' timeKnots <- times
#' trendData <- rep(1, n)
#' seasonData <- rep(1, n)
#' trend <- list(data = trendData, times = times, seasons = trendSeasons,
#'   timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
#' season <- list(data = seasonData, times = times, seasons = seasons,
#'   timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,0,1))
#' predictors <- list(trend, season)
#' rstr <- STR(data, predictors, reltol = 0.0000001, gapCV = 10,
#'                 confidence = 0.95, nMCIter = 400, robust = TRUE)
#' plot(rstr)
#' }
#' @author Alexander Dokumentov
#' @references Dokumentov, A., and Hyndman, R.J. (2016)
#' STR: A Seasonal-Trend Decomposition Procedure Based on Regression
#' \href{https://www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}{www.monash.edu/business/econometrics-and-business-statistics/research/publications/ebs/wp13-15.pdf}
#' @export

STR = function(data, predictors,
               confidence = NULL,
               robust = FALSE,
               lambdas = NULL,
               pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
               solver = c("Matrix", "cholesky"),
               nMCIter = 100,
               control = list(nnzlmax = 1000000, nsubmax = 300000, tmpmax = 50000),
               trace = FALSE,
               iterControl = list(maxiter = 20, tol = 1E-6)
)
{
  if(robust) {
    return(
      RSTR_(data = data, predictors = predictors,
           confidence = confidence,
           nMCIter = nMCIter,
           lambdas = lambdas,
           pattern = pattern, nFold = nFold,
           reltol = reltol, gapCV = gapCV,
           control = control,
           trace = FALSE)
    )
  } else {
    return(
      STR_(data = data, predictors = predictors,
           confidence = confidence, lambdas = lambdas,
           pattern = pattern, nFold = nFold,
           reltol = reltol, gapCV = gapCV,
           solver = solver, trace = trace, iterControl = iterControl)
    )
  }
}

STR_ = function(data, predictors,
  confidence = NULL, lambdas = NULL,
  pattern = extractPattern(predictors), nFold = 5, reltol = 0.005, gapCV = 1,
  solver = c("Matrix", "cholesky"),
  trace = FALSE,
  ratioGap = 1e6, # Ratio to define bounds for one-dimensional search
  iterControl = list(maxiter = 20, tol = 1E-6)
)
{
  if(any(confidence <= 0 | confidence >= 1)) stop("confidence must be between 0 and 1")
  if(gapCV < 1) stop("gapCV must be greater or equal to 1")

  if(getDoParWorkers() <= 1) registerDoSEQ() # A way to avoid warning from %dopar% when no parallel backend is registered
  f = function(p)
  {
    p = exp(p) # Optimisation is on log scale
    if(trace) {cat("\nParameters = ["); cat(p); cat("]\n")}
    newLambdas = createLambdas(p, pattern = pattern, original = origP)
    cv = nFoldSTRCV(n = nFold,
                    trainData = trainData, fcastData = fcastData, completeData = data,
                    trainC = trainC, fcastC = fcastC, completeC = C,
                    regMatrix = regMatrix, regSeats = regSeats,
                    lambdas = newLambdas,
                    solver = solver,
                    trace = trace,
                    iterControl = iterControl)
    if(trace) {cat("CV = "); cat(format(cv, digits = 16)); cat("\n")}
    return(cv)
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

  if(!is.null(lambdas)) {
    initP = extractP(lambdas, pattern)
    origP = abs(extractP(lambdas, rep(TRUE, length(pattern))))
  } else {
    initP = extractP(predictors, pattern)
    origP = abs(extractP(predictors, rep(TRUE, length(pattern))))
  }

#  cat("\ninitP: "); cat(initP)
#   if(length(initP) == 1) {
#     lower = initP/ratioGap
#     upper = initP*ratioGap
#     cat("\nlower: "); cat(lower)
#     cat("\nupper: "); cat(upper)
#  }
#  cat("\n")

  # Optimisation is performed on log scale
  optP = optim(par = log(initP),
               fn = f,
               method = ifelse(length(initP) > 1, "Nelder-Mead", "Brent"),
               lower = ifelse(length(initP) > 1, -Inf, log(initP/ratioGap)),
               upper = ifelse(length(initP) > 1, Inf, log(initP*ratioGap)),
               control = list(reltol = reltol))
  newLambdas = createLambdas(exp(optP$par), pattern = pattern, original = origP)

  result = STRmodel(data, strDesign = strDesign, lambdas = newLambdas, confidence = confidence, trace = trace)
  result$optim.CV.MSE = optP$value
  result$nFold = nFold
  result$gapCV = gapCV
  result$method = "STR"
  return(result)
}
