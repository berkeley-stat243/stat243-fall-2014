options(device=pdf)
source("example.r")

test_that("x and y are set correctly", {
    expect_that(length(x), equals(100))
    expect_that(length(y), equals(100))
})

test_that("too_many_na works correctly", {
   # test argument checking
   expect_that(too_many_na(), throws_error())
   testdf0 = data.frame(as.character(1:4))
   expect_that(too_many_na(testdf0, 0, 1),
               gives_warning("Non-numerical elements"))

   testdf1 = data.frame(x=1:4, y=c(NA,1,2,3))
   expect_that(too_many_na(testdf1, "1", 1),
               gives_warning("Threshold should be a number"))
   expect_that(too_many_na(testdf1, 0, 5),
               throws_error())
   # test functionality
   expect_that(too_many_na(testdf1, 0, 1), is_a("integer"))
   expect_that(too_many_na(testdf1, 0, 1), equals(1))
   row1 = 1
   names(row1) = "x"
   expect_that(too_many_na(testdf1, 0, 1),
               is_equivalent_to(row1))
   expect_that(too_many_na(testdf1, 0, 2), equals(2))
   expect_that(length(too_many_na(testdf1, 0.6, 1)), equals(0))
})


