set.seed(123456789)
m = matrix(rnorm(80000000), 10000, 8000)
v = rnorm(8000)

ref = solve(qr(m), v)
# as.vector(solve(t(m) %*% m) %*% t(m) %*% v) # Takes ages

tmm = Matrix::crossprod(m)
ctmm = Matrix::chol(tmm)
ref - as.vector(Matrix::solve(Matrix::crossprod(m), Matrix::crossprod(m, v)))
ref - as.vector(Matrix::solve(ctmm, Matrix::solve(t(ctmm), Matrix::crossprod(m, v))))


el = expand(lu(tmm))
ref - as.vector(Matrix::solve(el$U, (Matrix::solve(el$L, Matrix::solve(el$P, Matrix::crossprod(m, v))))))

lhs.l <- as(Matrix(t(m) %*% m, sparse = TRUE), "dgCMatrix")
rhs.l <- as.matrix(t(m) %*% v)
ref - cgm_c(lhs.l, rhs.l)

f = function(x)
{
  tmm %*% x
}

invm = diag(1/diag(t(m) %*% m))

invf = function(x)
{
  invm %*% x
}

b = as.vector(t(m) %*% v)

result = olscg(FUN = f, b = b, x = rep(0, length(b)), invFUN = invf)

ref - result$result
