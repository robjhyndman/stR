olscg = function (FUN, b, x, invFUN, maxiter = 1e+05, tol = 1e-06)
{
  r = b - FUN(x)
  z = invFUN(r)
  p = z
  iter = 0
  sumr2 = sum(r^2)

  while (sumr2 > tol & iter < maxiter) {
    iter = iter + 1
    Ap = FUN(p)
    a = as.numeric((t(r) %*% z)/(t(p) %*% Ap))
    x = x + a * p
    r1 = r - a * Ap
    z1 = invFUN(r1)
    bet = as.numeric((t(z1) %*% r1)/(t(z) %*% r))
    p = z1 + bet * p
    z = z1
    r = r1
    sumr2 = sum(r^2)
    # cat("\n"); cat(sumr2)
  }

  if (iter >= maxiter) {
    warning("olscg did not converge. You may increase maxiter number.")
  }
  cat("\nIter: "); cat(iter); cat(" Error: "); cat(sumr2)

  return(list(x = x, iter = iter, success = iter < maxiter))
}


# if(method == "test2") {
#   f = function(z) crossprod(X, X %*% z)
#   invm = 1/colSums(X^2)
#   invf = function(z) invm*z
#   Cty = crossprod(C, b)
#   if(is.null(b0)) {
#     cat("\nb0 is null...")
#     b0 = rep(0, length(Cty))
#   } else {
#     cat("\nb0 is not null...")
#   }
#   result = olscg(FUN = f, b = Cty, x = b0, invFUN = invf)
#   return(result$x)
# }
