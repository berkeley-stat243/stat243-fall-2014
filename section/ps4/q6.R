load("../../ps/ps4prob6.Rda")
set.seed(2)
temp <- matrix(runif(n*K), n, K)
theta.old <- temp/rowSums(temp)
#Theta.old <- theta.old %*% t(theta.old)
Theta.old <- tcrossproduct(theta.old)

init_q <- function() {
  q <- 0
  length(q) <- n*n*K
  dim(q) <- c(n, n, K)
  return(q)
}

# original code (w/out unneccessary if)

  for (i in 1:n) {
    for (j in 1:n) {
      for (z in 1:K) {
        q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /
          Theta.old[i, j]
      }
    }
  }

# Chris' solution

  for (z in 1:K) {
    q[ , , z] <- outer(theta.old[, z],theta.old[, z]) /
      Theta.old
  }

# Jarrod's alternate

  for (z in 1:K) {
    q[ , , z] <- tcrossprod(theta.old[, z]) /
      Theta.old
  }

# alternate 1

  for (z in 1:K) {
    q[ , , z] <- theta.old[, z] %o% theta.old[, z] /
      Theta.old
  }

# alternate 1.a

  for (z in 1:K) {
    q[ , , z] <- matrix(theta.old[, z] %o% theta.old[, z], n, n) /
      Theta.old
  }

# alternate 2

  for (z in 1:K) {
    q[ , , z] <- theta.old[, z] %*% t(theta.old[, z]) /
      Theta.old
  }

# alternate 3

  for (i in 1:n) {
    q[i, , ] <- t(theta.old[i, ] * t(theta.old[i, ])) /
      Theta.old[i, ]
  }

# alternate 3.a

  for (i in 1:n) {
    q[i, , ] <- array(1, dim=c(n,1)) %*% theta.old[i, ] * theta.old[i, ]) /
      Theta.old[i, ]
  }


# alternate 4

  for (i in 1:n) {
    q[i, , ] <- array(1, dim=c(n,1)) %*% theta.old[i, ] * theta.old[i, ]) /
      Theta.old[i, ]
  }

# alternate 5

  q1 <- array(0, dim=c(n, K, n)
  for (i in 1:n) {
    q1[,,i]<-sweep(theta.old,MARGIN=2,theta.old[i,],'*')
  }
  for (j in 1:K) {
    q[,,j]<-q1[,j,]
  }
  for (z in 1:K) {
    q[,,z]<-q[,,z]/Theta.old
  }  


