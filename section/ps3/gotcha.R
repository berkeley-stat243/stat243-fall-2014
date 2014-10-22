library(microbenchmark)

# unlist(lapply(x)) vs sapply(x) ------------------------------------
# eg.
# data$avgword <- unlist(lapply(lapply(data$wordvec,nchar),mean))
# vs.
# data$avgword <- colMeans(sapply(data$wordvec, nchar))

# are they the same?
#> unlist
#function (x, recursive = TRUE, use.names = TRUE) 
#{
#    if (.Internal(islistfactor(x, recursive))) {
#        lv <- unique(.Internal(unlist(lapply(x, levels), recursive, 
#            FALSE)))
#        nm <- if (use.names) 
#            names(.Internal(unlist(x, recursive, use.names)))
#        res <- .Internal(unlist(lapply(x, as.character), recursive, 
#            FALSE))
#        res <- match(res, lv)
#        structure(res, levels = lv, names = nm, class = "factor")
#    }
#    else .Internal(unlist(x, recursive, use.names))
#}
#> sapply
#function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
#{
#    FUN <- match.fun(FUN)
#    answer <- lapply(X = X, FUN = FUN, ...)
#    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
#        names(answer) <- X
#    if (!identical(simplify, FALSE) && length(answer)) 
#        simplify2array(answer, higher = (simplify == "array"))
#    else answer
#}


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
x <- c(1:10, NA, NaN, Inf)
x[which(x > 5)]
x[x > 5]
