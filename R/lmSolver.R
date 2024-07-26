# Solves system || y - Xb || -> min against b.
# Environment env is used to pass some useful data
# (such as last solution and Cholesky decomposition)
# between different calls of lmSolver.
lmSolver = function(X, y, type = "Matrix", method = "cholesky", env = NULL, iterControl = list(maxiter = 20, tol = 1E-6), trace = TRUE)
{
  if(length(y) > nrow(X)) stop("y is too long in lmSolver...")
  y = c(y, rep(0, nrow(X) - length(y)))
  if(type == "Matrix") {
    if(method == "cholesky") {
      result = .solve.dgC.chol(t(X), y)
      return(result$coef)
    }
    if(method == "qr") {
      b = solve(qr(X), y) # Not optimal
      return(b)
    }
    stop("Unknown method in lmSolver...")
  }
  if(type == "iterative"){
    if(method == "cg") {
      f = function(z) crossprod(X, X %*% z)
      if(is.null(env) || is.null(env$L) || is.null(env$Lt) || is.null(env$P) || is.null(env$Pt)) {
        invm = 1/colSums(X^2)
        invf = function(z) invm*z
      } else {
        invf = function(z) solve(env$P, solve(env$Lt, solve(env$L, solve(env$Pt, z))))
      }
      Xty = crossprod(X, y)
      if(is.null(env) || is.null(env$b0)) b0 = rep(0, length(Xty))
      else b0 = env$b0
      result = olscg(FUN = f, y = Xty, b = b0, invFUN = invf, iterControl = iterControl, trace = trace)
      return(result$b)
    }
    if(method == "cg-chol") {
      if(is.null(env)) {
        stop("env variable is NULL.")
      }
      if(is.null(env$L) ||
         is.null(env$Lt) ||
         is.null(env$P) ||
         is.null(env$Pt))
      {
        result = .solve.dgC.chol(t(X), y)
        eL = expand(result$L)
        env$L = eL$L
        env$Lt = t(eL$L)
        env$P = eL$P
        env$Pt = t(eL$P)
        env$b0 = result$coef
        return(result$coef)
      } else {
        f = function(z) crossprod(X, X %*% z)
        invf = function(z) solve(env$P, solve(env$Lt, solve(env$L, solve(env$Pt, z))))
        Xty = crossprod(X, y)
        if(is.null(env$b0)) b0 = rep(0, length(Xty))
        else b0 = env$b0
        result = olscg(FUN = f, y = Xty, b = b0, invFUN = invf, iterControl = iterControl, trace = trace)
        return(result$b)
      }
    }
    if(method == "lsmr-chol") {
      if(is.null(env)) {
        stop("env variable is NULL.")
      }
      if(is.null(env$L) ||
         is.null(env$Lt) ||
         is.null(env$P) ||
         is.null(env$Pt))
      {
        result = .solve.dgC.chol(t(X), y)
        eL = expand(result$L)
        env$L = eL$L
        env$Lt = t(eL$L)
        env$P = eL$P
        env$Pt = t(eL$P)
        env$b0 = result$coef
        return(result$coef)
      } else {
        A = function(x, k) {
          if(k == 1) {
            return(X %*% solve(env$P, solve(env$Lt, x)))
          } else {
            return(solve(env$L, solve(env$Pt, crossprod(X, x))))
          }
        }
        if(!is.null(env$b0)) {
          x0 = env$b0
        } else {
          x0 = 0
        }
        result = lsmr(A = A, b = y - X %*% x0, atol = iterControl$tol, btol = iterControl$tol, itnlim = iterControl$maxiter)
        if(trace) {
          cat("\nIterations: "); cat(result$itn); cat("   ")
        }
        return(solve(env$P, solve(env$Lt, result$x)) + x0)
      }
    }
    if(method == "lsmr") {
      D = sqrt(colSums(X^2))
      invD = 1/D
      A = function(x, k) {
        if(k == 1) {
          return(X %*% (invD * x))
        } else {
          return(invD * crossprod(X, x))
        }
      }
      if(!is.null(env$b0)) {
        x0 = env$b0
      } else {
        x0 = rep(0, ncol(X))
      }
      result = lsmr(A = A, b = y - X %*% x0, atol = iterControl$tol, btol = iterControl$tol, itnlim = iterControl$maxiter)
      if(trace) {
        cat("\nIterations: "); cat(result$itn); cat("   ")
      }
      output = invD * result$x + x0
      if(!is.null(env)) {
        env$b0 = output
      }
      return(output)
    }
    stop("Unknown method in lmSolver...")
  }
  stop("Unknown type in lmSolver...")
}
