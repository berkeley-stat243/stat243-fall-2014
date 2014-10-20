library(microbenchmark)

# unlist(lapply(x)) vs sapply(x) ------------------------------------
# are they the same?
# which is faster?
microbenchmark(unlist(as.list(1:1000)),
               simplify2array(as.list(1:1000)),
               times=1000)

# which is easier to read and reason about??

# x[which(logical)] vs x[logical] -----------------------------------
# which is faster?
x = sin(1:1000)
n = 0.5
microbenchmark(x[which(x>n)],
               x[x>n],
               times=1000)

microbenchmark(x[x>n],
               x[which(x>n)],
               times=1000)

# which is easier to read and reason about??

# are they the same?
x <- c(1:10,NA,NaN,Inf)
x[which(x > 5)]
x[x > 5]
