# Test suite for SKthink package

test_that("sk_mean returns correct class", {
  data <- c(1, 2, 3, 4, 5)
  result <- sk_mean(data)
  expect_s3_class(result, "sk_mean")
  expect_equal(as.numeric(result), 3)
})

test_that("sk_stat returns a data frame", {
  result <- sk_stat(mtcars)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("sk_missing returns correct structure", {
  data(airquality)
  result <- sk_missing(airquality)
  expect_true(is.data.frame(result) || is.list(result))
})

test_that("sk_impute handles missing values", {
  data <- data.frame(x = c(1, 2, NA, 4, 5))
  result <- sk_impute(data, method = "mean")
  expect_false(any(is.na(result$x)))
})

test_that("sk_duplicate_remove works correctly", {
  data <- data.frame(x = c(1, 1, 2, 3, 3))
  result <- sk_duplicate_remove(data)
  expect_equal(nrow(result), 3)
})

test_that("sk_shapiro_test returns test results", {
  set.seed(123)
  normal_data <- rnorm(100)
  result <- sk_shapiro_test(normal_data)
  expect_true(is.list(result) || is.data.frame(result))
})

test_that("sk_theme returns a theme object when ggplot2 is available", {
  skip_if_not_installed("ggplot2")
  result <- sk_theme("minimal")
  expect_true(inherits(result, "theme"))
})

test_that("sknote and sknote_stop functions exist", {
  expect_true(is.function(sknote))
  expect_true(is.function(sknote_stop))
})

test_that("System info functions work", {
  info <- get_system_info()
  expect_true(is.list(info))
  expect_true(length(info) > 0)
})
