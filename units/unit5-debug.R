#################################################################
### Demo code for Unit 5 of Stat243, "Debugging and Good Practices"
### Chris Paciorek, September 2014
#################################################################

##################################
# 1: Comon syntax errors and bugs
################################## 

mat <- matrix(1:4, 2, 2)[1, ]
dim(mat); print(mat)

colSums(mat)

mat <- matrix(1:4, 2, 2)[1, , drop = FALSE]

colSums(mat)


##################################
# 2: Tips for avoiding bugs
################################## 

## 
## speedOfLight <- 3e8
## nIts <- 1000
## 

################################
# 3: Debugging strategies
################################

### 3.1 Basic strategies

options(width = 55)
library(codetools)
findGlobals(lm)[1:25]
f <- function() {y <- 3; print(x + y)}
findGlobals(f)

options()$warn

x = 1:3
for(i in 1:100){
  if(x > 2) print("hi")
}
warnings()

options(warn = 1)
for(i in 1:100){
  if(x > 2) print("hi")
}

options(warn = 2)
for(i in 1:100){
  if(x > 2) print("hi")
}
i


### 3.3 Using debug()

## simple use of browser via debug()

debug(lm)

n <- nrow(mtcars)
lm(mpg ~ disp, data = mtcars)

ls()
## use browser commands: c, n, Q
## let's step through this with 'n'
print(n)  # typing 'n' won't work!

undebug(lm)


### 3.4 Tracing errors in the call stack


library(MASS)

gamma.est <- function(data) {
  # this fits a gamma distribution to a collection of numbers
  m <- mean(data)
  v <- var(data)
  s <- v/m
  a <- m/s
  return(list(a=a,s=s))
}

calcVar = function(estimates){
  var.of.ests <- apply(estimates, 2, var)
  return(((n-1)^2/n)*var.of.ests)
}

gamma.jackknife.2 <- function(data) {
  ## jackknife the estimation
  
  n <- length(data)
  jack.estimates = gamma.est(data[-1])
  for (omitted.point in 2:n) {
    jack.estimates = rbind(jack.estimates, gamma.est(data[-omitted.point]))
  }
  jack.var = calcVar(jack.estimates)
  return(sqrt(jack.var))
}

## using traceback()
gamma.jackknife.2(cats$Hwt) # jackknife gamma dist. estimates of cat heart weights
traceback()
## something is going on when we try to calculate the variance


## using recover() upon an error
options(error = recover)
gamma.jackknife.2(cats$Hwt)
## choose '2' to go into calcVar
estimates
apply(estimates, 2, var) # ok, so the error does occur here
var(estimates[ ,1])  # can we use var() a single time?
estimates[ ,1]
class(estimates)

options(error = NULL) # reset things

### 3.5 Using trace()

## three ways to insert debugging code in a function
## 1:
## insert "browser()" manually at some point in calcVar()
## 2: 
trace(calcVar, edit = 'emacs')
## 3:
## fix(calcVar, editor = 'emacs')

calcVar
gamma.jackknife.2(cats$Hwt)
# at this point you can use 'n', 'c', 'Q' as with debug()

##########################################################
### Putting it all together in more realistic example
##########################################################

## Goal: write a function that prints out the sizes of local
## variables in the global workspace or in the frame of a function

## First try
sizes <- function(){
  # fails because obj has character strings of object names not objects itself
  pfr = parent.frame()
  objSizes=sapply(ls(pfr), function(obj) {
    object.size(obj) # original code
  })
  print(objSizes)
}

testFun <- function() {
  x <- rnorm(1e6)
  y <- rnorm(1e3)
  z <- 5
  sizes()
}

object.size(rnorm(1e6))
object.size(rnorm(1e3))
object.size(5)

testFun()

## strategy:
## 1) use debug() on testFun()
## 2) use trace or fix to insert browser() into getSize()

## Second try
sizes <- function(){
  pfr = parent.frame()

  getSize <- function(obj) {
    object.size(eval(as.name(obj)))
  }
  
  objSizes=sapply(ls(pfr), getSize)
  print(objSizes)
}

testFun()
## use trace or fix to insert browser() into getSize()

## think about environments...

## Third try
sizes <- function(){
  ## should work though haven't test extensively
  pfr = parent.frame()

  getSize <- function(obj, fr) {
    object.size(eval(as.name(obj), envir = fr))
    ## alternatively:
    ## object.size(get(obj, fr))
  }
  
  objSizes=sapply(ls(pfr), getSize, pfr)
  print(objSizes)
}


################################
# 5: Good coding practices
################################

### 5.5 Dealing with run-time errors

x <- 3
stopifnot(is(x, "matrix"))


mysqrt <- function(x) {
	if (is.list(x)) {
		warning("x is a list; converting to a vector")
		x <- unlist(x)
	}
	if (!is.numeric(x)) {
		stop("What is the square root of 'bob'?")
	} else {
		if (any(x < 0)) {
			warning("mysqrt: found negative values; proceeding anyway")
			x[x >= 0] <- (x[x >= 0])^(1/2)
			x[x < 0] <- NaN
			return(x)
		} else return(x^(1/2))
	}
}
mysqrt(c(1, 2, 3))
mysqrt(c(5, -7))
mysqrt(c("asdf", "sdf"))
mysqrt(list(5, 3, "ab"))

## try()

library(ismev)
library(methods)
n <- 100; nDays <- 365
x <- matrix(rnorm(nDays * n), nr = nDays)
x <- apply(x, 2, max)
x <- cbind(rep(0, 100), x)
params <- matrix(NA, nr = ncol(x), nc = 3)
for(i in 1:ncol(x)){
	fit <- try(gev.fit(x[ ,i], show = FALSE))
	if(!is(fit, "try-error")) 
		params[i, ] = fit$mle
}
params


