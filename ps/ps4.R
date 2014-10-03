## @knitr prob5

library(inline)
# this code is simply a placeholder to demonstrate that I can
# modify the input arguments as desired in C;
# in reality 'src' would contain substantive computations
src <- '
        tablex[0] = 7;
'

dummyFun <- cfunction(signature(tablex = "integer", tabley = "integer",
                xvar = "integer", yvar = "integer", useline = "integer",
                n = "integer"), src, convention = ".C")


fastcount <- function(xvar,yvar) {
	nalineX <- is.na(xvar)
	nalineY <- is.na(yvar)
	xvar[nalineX | nalineY] <- 0
	yvar[nalineX | nalineY] <- 0
	useline <- !(nalineX | nalineY);
	tablex <- numeric(max(xvar)+1)
	tabley <- numeric(max(yvar)+1)
	stopifnot(length(xvar) == length(yvar))
	res <- dummyFun(
		tablex = as.integer(tablex), tabley = as.integer(tabley),
		as.integer(xvar), as.integer(yvar), as.integer(useline),
		as.integer(length(xvar)))
	xuse <- which(res$tablex > 0)
	xnames <- xuse - 1
	resb <- rbind(res$tablex[xuse], res$tabley[xuse]) 
	colnames(resb) <- xnames
	return(resb)
}

## @knitr prob6

load('ps4prob6.Rda') # should have A, n, K

ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}

oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) { 
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (z in 1:K) {
        if (theta.old[i, z]*theta.old[j, z] == 0){
          q[i, j, z] <- 0
        } else {
          q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /
            Theta.old[i, j]
        }
      }
    }
  }
  theta.new <- theta.old
  for (z in 1:K) {
    theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z]))
  }
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
      converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check)) 
}

# initialize the parameters at random starting values
temp <- matrix(runif(n*K), n, K)
theta.init <- temp/rowSums(temp)

# do single update
out <- oneUpdate(A, n, K, theta.init)


# in the real code, oneUpdate was called repeatedly in a while loop
# as part of an iterative optimization to find a maximum likelihood estimator

## @knitr done
