
## @knitr q4.sol
updown <- sample(c(0, 1), n, replace=TRUE)
steps <-  sample(c(-1, 1), n, replace=TRUE)
xsteps <- (updown == 0) * steps
ysteps <- (updown == 1) * steps
x <- c(0, cumsum(xsteps))
y <- c(0, cumsum(ysteps))


## @knitr q5.orig
nalineX <- is.na(xvar)
nalineY <- is.na(yvar)
xvar[nalineX | nalineY] <- 0
yvar[nalineX | nalineY] <- 0
useline <- !(nalineX | nalineY)


## @knitr q5.sol1
naline <- is.na(xvar)
naline[is.na(yvar)] <- TRUE
xvar[naline] <- 0
yvar[naline] <- 0
useline <- 1L - naline
#naline <- which(naline)

## @knitr q5.sol2
xvar[is.na(xvar) | is.na(yvar)] <- 0
yvar[is.na(xvar) | is.na(yvar)] <- 0
useline <- !(is.na(xvar) | is.na(yvar))

## @knitr q5.sol3
naline <- is.na(xvar + yvar)
useline <- as.integer(!(naline))

## @knitr q5.sol4
naline <- unique(c(which(xvar %in% NA), which(yvar %in% NA)))
useline <- as.integer(rep(1, length(xvar)))
useline[naline] <- as.integer(0)

## @knitr q6.p1
load("ps4prob6.Rda")
set.seed(2)
temp <- matrix(runif(n*K), n, K)
theta.old <- temp/rowSums(temp)
Theta.old <- theta.old %*% t(theta.old)

## @knitr q6.p2
Theta.old <- tcrossproduct(theta.old)

## @knitr p2
init_q <- function() {
  q <- 0
  length(q) <- n*n*K
  dim(q) <- c(n, n, K)
  return(q)
}

## @knitr q6.orig
for (i in 1:n) {
  for (j in 1:n) {
    for (z in 1:K) {
      q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /
                        Theta.old[i, j]
    }
  }
}

## @knitr p4
  for (z in 1:K) {
    q[ , , z] <- outer(theta.old[, z],theta.old[, z]) /
                     Theta.old
  }

## @knitr q6.sol
for (z in 1:K) {
  q[ , , z] <- tcrossprod(theta.old[, z]) / Theta.old
}

## @knitr q6.sol1
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

## @knitr q6.alt1
for (i in 1:n) {
  q[i, , ] <- theta.old[i, ] * t(theta.old[i, ]) /
                 Theta.old[i, ]
}

## @knitr q6.alt2
# alternate 3
# alternate 3.a

for (i in 1:n) {
  q[i, , ] <- array(1, dim=c(n,1)) %*% theta.old[i, ] * theta.old[i, ] /
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

