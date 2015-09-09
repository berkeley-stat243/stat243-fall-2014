## @knitr p1

n = 100
x = 1:n
y = x + runif(n)

## @knitr p2

plot(x, y)
plot(x, log(y))

## @knitr p3

too_many_na = function(d, threshold, axis) {
    # check arguments and issue warnings
    if (!all(sapply(d, is.numeric))) warning("Non-numerical elements")
    if (!is.numeric(threshold)) warning("Threshold should be a number")
    if ((threshold < 0) & (threshold > 0)) warning("Threshold should be in (0,1)")
    ## if axis isn't 1 or 2, we will throw an error anyway
    #if (!(axis %in% c(1,2))) warning("Axis must be in 1 or 2")

    ## if you wanted to throw an error instead of giving a warning
    # stopifnot(all(sapply(d, is.numeric)))
    ## or if you have loaded the testthat package
    # expect_that(all(sapply(d, is.numeric)), is_true())

    # do your thing
    na_prop  = apply(is.na(d), axis, mean)
    indices = unname(which(na_prop > threshold))
    return(indices)
}


## @knitr session
print(sessionInfo(), locale=FALSE)
