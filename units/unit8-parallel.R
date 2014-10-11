############################################################
### Demo code for Unit 8 of Stat243, "Parallel processing"
### Chris Paciorek, October 2014
############################################################

#################################
# 2: Parallelization
#################################

### 2.2.1 The BLAS

require(RhpcBLASctl)
Z <- matrix(rnorm(5000^2), 5000)

blas_set_num_threads(4)
system.time({
X <- crossprod(Z) # Z^t Z produces pos.def. matrix
U <- chol(X)      # U^t U = X
})

blas_set_num_threads(1)
system.time({
X <- crossprod(Z)
U <- chol(X)
})



#################################
# 3: Explicit parallel code in R
#################################

### 3.1 foreach

require(parallel) # one of the core R packages
require(doParallel)
## require(multicore); require(doMC) # alternative to parallel/doParallel
## require(Rmpi); require(doMPI) # when Rmpi is available as the back-end
library(foreach)
library(iterators)

taskFun <- function(){
	mn <- mean(rnorm(10000000))
	return(mn)
}
nCores <- 4  
registerDoParallel(nCores) 
## registerDoMC(nCores) # alternative to registerDoParallel
#
## cl <- startMPIcluster(nCores); registerDoMPI(cl) # when using Rmpi as the back-end
out <- foreach(i = 1:100, .combine = c) %dopar% {
	cat('Starting ', i, 'th job.\n', sep = '')
	outSub <- taskFun()
	cat('Finishing ', i, 'th job.\n', sep = '')
	outSub # this will become part of the out object
}

### 3.2 Parallel apply and vectorization

require(parallel)
nCores <- 4  
### using sockets
#
## ?clusterApply
cl <- makeCluster(nCores) # by default this uses sockets
nSims <- 60
testFun <- function(i){
	mn <- mean(rnorm(1000000))
	return(mn)
}
## if the processes need objects (x and y, here) from the master's workspace:
## clusterExport(cl, c('x', 'y')) 
system.time(
	res <- parSapply(cl, 1:nSims, testFun)
)
system.time(
	res2 <- sapply(1:nSims, testFun)
)
myList <- as.list(1:nSims)
res <- parLapply(cl, myList, testFun)

### using forking
system.time(
	res <- mclapply(seq_len(nSims), testFun, mc.cores = nCores) 
)



require(parallel)
nCores <- 4
library(fields)
ds <- runif(6000000, .1, 10)
system.time(
	corVals <- pvec(ds, Matern, .1, 2, mc.cores = nCores)
)
system.time(
	corVals <- Matern(ds, .1, 2)
)

### 3.3 Explicit parallel programming in R: mcparallel and forking

## mcparallel()

library(parallel)
n <- 10000000
system.time({
	p <- mcparallel(mean(rnorm(n)))
	q <- mcparallel(mean(rgamma(n, shape = 1)))
	res <- mccollect(list(p,q))
})
system.time({
	p <- mean(rnorm(n))
	q <- mean(rgamma(n, shape = 1))
})


## forking

library(fork)
## mode 1
pid <- fork(slave = myfun)
## mode 2
{ # this set of braces is REQUIRED when you don't pass a function 
  # to the slave argument of fork()
	pid <- fork(slave = NULL) 
	if(pid==0) {
		cat("Starting child process execution.\n") 
		tmpChild <- mean(rnorm(10000000))
		cat("Result is ", tmpChild, "\n", sep = "")
		save(tmpChild, file = 'child.RData') # clunky
		cat("Finishing child process execution.\n")
		exit() 
	} else {
		cat("Starting parent process execution.\n")
		tmpParent <- mean(rnorm(10000000))
		cat("Finishing parent process execution.\n")
		wait(pid)  # wait til child is finished so can read
                   # in updated child.RData below
	} 
} 
load('child.RData') # clunky
print(c(tmpParent, tmpChild))


