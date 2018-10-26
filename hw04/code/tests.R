library(testthat)
context("Remove missing values remove_missing()")

test_that("remove_missing removes the right NA values", {
  expect_that(remove_missing(c(NA, 5, 6)) , equals(c(5 , 6)))
  expect_that(remove_missing(c(NA, NA, 8)), equals(c(8)))
  expect_that(remove_missing(c(NA, NA, NA)), equals(logical(0)))
  expect_that(remove_missing(c(NA, NULL, 5)), equals(5))
})

context("Return the minimum value correctly get_minimum()")

test_that("Returns the minimum value", {
  expect_that(get_minimum(c(1,2,3,4)), equals(1))
  expect_that(get_minimum(c(0, 3, 5, -1)), equals(-1))
  expect_that(get_minimum(c("4", 5, 1)), equals("1"))
  expect_that(get_minimum(c(-Inf, Inf, 0)), equals(-Inf))
})

context("Return the maximum value correctly get_maximum()")

test_that("returns the max value", {
  expect_that(get_maximum(c(1,2,3,4)), equals(4))
  expect_that(get_maximum(5), equals(5))
  expect_that(get_maximum(c(Inf, Inf, 1000)), equals(Inf))
  expect_that(get_maximum(c(5 - 5, -4, NA)), equals(0))
})

context("Return the correct range get_range()")

test_that("returns the correct range", {
  expect_that(get_range(c(1,2,4,1)), equals(3))
  expect_that(get_range(c(Inf, 0, 4)), equals(Inf))
  expect_that(get_range(c(-5, NA, 10, 4)), equals(15))
  expect_that(get_range(c("4", 4, "1")), throws_error())
})

context("Returns the correct median get_median()")

test_that("returns the correct median", {
  expect_that(get_median(c(1, 2, 3, 4)), equals(2.5))
  expect_that(get_median(c(1, 2, 3, 4, 5)), equals(3))
  expect_that(get_median(c("4", "hi", 5, 6)), throws_error())
  expect_that(get_median(c(1, 2, Inf, 6)), equals(Inf))
})

context("Return the correct average get_average()")

test_that("return the correct average", {
  expect_that(get_average(c(61, 4, 1, 5)), equals(mean(c(61,4,1,5))))
  expect_that(get_average(c(NULL, 4, 5)), equals(4.5))
  expect_that(get_average(c(0, 0, 0, 4)), equals(1))
  expect_that(get_average(c("4", "nope", 4, 3)), throws_error())
})

context("Return the correct stdev get_stdev()")

test_that("returns the correct stdev", {
  expect_that(get_stdev(c(1, 4, 1)), equals(sd(c(1, 4, 1))))
  expect_that(get_stdev(c("error", "incoming", 4)), throws_error())
  expect_that(get_stdev(c(NA, 4, 1, 5)), equals(sd(c(NA, 4, 1, 5), na.rm = TRUE)))
  expect_that(get_stdev(c(-50, 50, 5, 10)), equals(sd(c(-50, 50, 5, 10))))
})

context("Get back the proper 1st quartile get_quartile1()")

test_that("returns proper quartile 1", {
  expect_that(get_quartile1(c(1, 2, 3, 4)), equals(unname(quantile(c(1,2,3,4),0.25))))
  expect_that(get_quartile1(c(NA, "4", 5)), throws_error())
  expect_that(get_quartile1(c("4", 5, 6)), throws_error())
  expect_that(get_quartile1(c(NA, 5, 6, 10)), equals(unname(
                                                      quantile(c(NA, 5, 6, 10)
                                                              ,0.25,
                                                              na.rm = TRUE ))))
})

context("Get back the proper 3rd quartile get_quartile3()")

test_that("returns proper quartile 3", {
  expect_that(get_quartile1(c(1, 2, 3, 4)), equals(unname(quantile(c(1,2,3,4),0.25))))
  expect_that(get_quartile1(c(NA, "4", 5)), throws_error())
  expect_that(get_quartile1(c("4", 5, 6)), throws_error())
  expect_that(get_quartile1(c(NA, 5, 6, 10)), equals(unname(
    quantile(c(NA, 5, 6, 10)
             ,0.25,
             na.rm = TRUE ))))
})

context("Get back count of missing values count_missing()")

test_that("return number of NA values", {
  expect_that(count_missing(c(NA, NA, 4, NA)), equals(3))
  expect_that(count_missing(c(1, 3, 4, 5)), equals(0))
  expect_that(count_missing(c("shouln't", "raise", "error")), equals(0))
  expect_that(count_missing(c(NULL, NA, NA, 4)), equals(2))
})


context("Rescale vector properly rescale_100()")

test_that("return properly rescaled vector", {
  expect_that(rescale100(c(4, "A", 5),xmin = 0, xmax = 20), throws_error())
  expect_that(rescale100(c(18, 15, 16, 4, 17, 9), xmin = 0, xmax = 20), equals(c(90, 75, 80, 20,85,45)))
  expect_that(rescale100(c(1, 3, 4), xmin = NA, xmax = 10), throws_error())
  expect_that(rescale100(c(1, 2, 3, 4), xmin = 0, xmax = 5), equals(c(20, 40, 60, 80)))
})

context("Drop the lowest score drop_lowest()")

test_that("drop the lowest score", {
  expect_that(drop_lowest(c(12, 45, 32, 1)), equals(c(12, 32, 45)))
  expect_that(drop_lowest(c(1, 1, 3, 4, 5)), equals(c(1, 3, 4, 5)))
  expect_that(drop_lowest(c(3, 1, 4, 5)), equals(c(3, 4, 5)))
  expect_that(drop_lowest(c(2, 2, 2, 2)), equals(c(2, 2, 2)))
})


context("Return the average of the homework scores score_homework()")

test_that("calculate the average score", {
  expect_that(score_homework(c(10, 20, 5, 50), drop = TRUE), equals(80/3))
  expect_that(score_homework(c(10, 20, 5, 50), drop = FALSE), equals(85/4))
  expect_that(score_homework(c(0, 0, 10, 20), drop = TRUE), equals(10))
  expect_that(score_homework(c(0, 0, 10, 20), drop = FALSE), equals(30/4))
})

context("Return the average of quiz scores score_quiz()")

test_that("calculate the average score", {
  expect_that(score_quiz(c(10, 20, 5, 50), drop = TRUE), equals(80/3))
  expect_that(score_quiz(c(10, 20, 5, 50), drop = FALSE), equals(85/4))
  expect_that(score_quiz(c(0, 0, 10, 20), drop = TRUE), equals(10))
  expect_that(score_quiz(c(0, 0, 10, 20), drop = FALSE), equals(30/4))
})

context("Return the correct lab score score_lab()")

test_that("calculate the lab score", {
  expect_that(score_lab(11), equals(100))
  expect_that(score_lab(0), equals(0))
  expect_that(score_lab("8"), equals(40))
  expect_that(score_lab(NA), throws_error())
})