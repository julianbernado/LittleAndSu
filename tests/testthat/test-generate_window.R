testthat::test_that("basic numeric vector with reasonable window is generated correctly", {
  testthat::expect_equal(c(4,5,6),generate_window(1:10, center = 5, length = 3))
})

testthat::test_that("basic vector with window that touches left edge is generated correctly", {
  testthat::expect_equal(c(1,2,3),generate_window(1:10, center = 1, length = 3))
})

testthat::test_that("basic vector with window that touches right edge is generated correctly", {
  testthat::expect_equal(c(8,9,10),generate_window(1:10, center = 10, length = 3))
})

testthat::test_that("character vector is generated correctly", {
  testthat::expect_equal(c("c", "d", "e"),generate_window(c("a", "b", "c", "d", "e", "f", "g"), center = 4, length = 3))
})

testthat::test_that("error is thrown when window size is too large or not an odd integer", {
  testthat::expect_error(generate_window(1:3, center = 2, length = 5))
  testthat::expect_error(generate_window(1:3, center = 2, length = 2))
  testthat::expect_error(generate_window(1:3, center = 2, length = 1.4))
})

testthat::test_that("error is thrown when user supplies empty vector", {
  testthat::expect_error(generate_window(c(), center = 1, length = 1))
})

testthat::test_that("if length argument is length of supplied vector, the whole vector is returned", {
  testthat::expect_equal(1:10, generate_window(1:10, center = 3, length = 10))
})
