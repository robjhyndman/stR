library(Matrix)
set.seed(123456789)
m = matrix(rnorm(8000), 100, 80)
v = rnorm(100)

ref = solve(qr(m), v)
# as.vector(solve(t(m) %*% m) %*% t(m) %*% v) # Takes ages

tmm = Matrix::crossprod(m)
ctmm = Matrix::chol(tmm)
sum(abs(ref - as.vector(Matrix::solve(Matrix::crossprod(m), Matrix::crossprod(m, v)))))
sum(abs(ref - as.vector(Matrix::solve(ctmm, Matrix::solve(t(ctmm), Matrix::crossprod(m, v))))))


el = expand(lu(tmm))
sum(abs(ref - as.vector(Matrix::solve(el$U, (Matrix::solve(el$L, Matrix::solve(el$P, Matrix::crossprod(m, v))))))))

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

max(abs(ref - result$x))

dim(m)
b = t(t(v))

chol = .solve.dgC.chol(t(m), b)
eL = expand(chol$L)
L = eL$L
P = eL$P
Lt = t(L)
Pt = t(P)

# dim(m)
# dim(b)

A = function(x, k) {
  if(k == 1) {
    return(m %*% solve(P, solve(Lt, x)))
  } else {
    return(solve(L, solve(Pt, crossprod(m, x))))
  }
}
# result = lsmr(A = m, b = b)

# A = m
# invm = diag(1/sqrt(colSums(m^2)))

result = lsmr(A = A, b = b, atol = 1e-6, btol = 1e-6)
result$itn

max(abs(ref - solve(P, solve(Lt, result$x))))
# max(abs(ref - result$x))

X = Matrix(0, 4, 3, sparse = TRUE)
X[1,1] = 3
X[1,2] = 1
X[2,3] = 4
X[3,1] = -1
X[3,3] = -2
X[4,3] = 3

y = rep(1,nrow(X))

result = .solve.dgC.chol(t(X), y)

max(abs(X %*% result$coef - y))


# chol(t(X) %*% X)

eL = expand(result$L)
L = eL$L
P = eL$P

# perm = result$L@perm + 1
sum(abs(t(P) %*% (L %*% t(L)) %*% P - crossprod(X)))


