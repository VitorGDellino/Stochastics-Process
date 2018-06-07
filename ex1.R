# Primeiro exercício

#-------------- Primeiro Método --------------
potM <- function(M, n){
  if (n == 1) return (M)
  if (n == 2) return (M %*% M)
  if (n == 3) return (M %*% potM(M, n-1))
}

M <- matrix(c(0.3,0.2,0,0.3,0.6,0,0.4,0.2,1), nrow = 3, ncol = 3)
Mres <- potM(M, 3)[1, 3]
Mres

#-------------- Segundo Método --------------
potEigen <- function(M, n){
  eigen <- eigen(t(M))
  eigenValue <- eigen$values
  eigenVector <- eigen$vectors
  Q <- as.matrix(eigenVector)
  Dm <- diag(eigenValue^n)
  Mres <- Q %*% Dm %*% solve(Q)
  return (t(Mres))
}

M <- matrix(c(0.3,0.2,0,0.3,0.6,0,0.4,0.2,1), nrow = 3, ncol = 3)
Mres <- potEigen(M, 3)[1,3]
Mres