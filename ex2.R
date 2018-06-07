# Segundo Exercicio

# ------------------- A ---------------
potEigen <- function(M, n){
  eigen <- eigen(t(M))
  eigenValue <- eigen$values
  eigenVector <- eigen$vectors
  Q <- as.matrix(eigenVector)
  Dm <- diag(eigenValue^n)
  Mres <- Q %*% Dm %*% solve(Q)
  return (t(Mres))
}

findEigen <- function(M){
  eigen <- eigen(t(M))
  eigen$values
}

# Potencia 37 
Mres <- potEigen(M, 37)
Mres
findEigen(Mres)

# Potencia 38
Mres <- potEigen(M, 38)
Mres
findEigen(Mres)

# Potencia 39
Mres <- potEigen(M, 39)
Mres
findEigen(Mres)

# Potencia 40
Mres <- potEigen(M, 40)
Mres
findEigen(Mres)

# ------------------ B -----------------