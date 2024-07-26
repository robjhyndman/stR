# Conjugate gradient method.
# The code copied from the R package "pcg" and slightly refactored.
olscg <- cmpfun(function(FUN, y, b, invFUN, iterControl = list(maxiter = 1e+03, tol = 1e-06), trace = TRUE) {
  r <- y - FUN(b)
  z <- invFUN(r)
  p <- z
  iter <- 0
  sumr2 <- sum(r^2)

  while (sumr2 > iterControl$tol & iter < iterControl$maxiter) {
    iter <- iter + 1
    Ap <- FUN(p)
    a <- as.numeric((t(r) %*% z) / (t(p) %*% Ap))
    b <- b + a * p
    r1 <- r - a * Ap
    z1 <- invFUN(r1)
    bet <- as.numeric((t(z1) %*% r1) / (t(z) %*% r))
    p <- z1 + bet * p
    z <- z1
    r <- r1
    sumr2 <- sum(r^2)
    # cat("\n"); cat(sumr2)
  }

  # if (iter >= maxiter) {
  #   warning("olscg did not converge. You may increase maxiter number.")
  # }
  if (trace) {
    cat("\nIter: ")
    cat(iter)
    cat(" Error: ")
    cat(sumr2)
    cat("   ")
  }

  return(list(b = b, iter = iter, success = iter < iterControl$maxiter))
})
