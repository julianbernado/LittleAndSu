test1_df = rbind(
  c(1, 2, 3),
  c(4, NA, 6),
  c(7, 8, 9)
)

testthat::test_that("basic case of lvcf works", {
  imputed_test1_df = carryover(test1_df, method = c("lvcf"))
  testthat::expect_equal(4, as.numeric(unlist(imputed_test1_df[2,2])))
})

testthat::test_that("basic case of random carryover works", {
  imputed_values = numeric(length = 1000)
  for (i in 1:1000) {
    imputed_values[i] = carryover(test1_df, method = "random")[2,2]
  }
  testthat::expect_equal(TRUE, abs((length(which(imputed_values == 4))/1000) - 0.5) < 0.1)
})

testthat::test_that("basic case of population carryover works", {
  test3_df = test1_df
  test3_df[1,3] = 2
  test3_df[3,3] = 8
  imputed_values = numeric(length = 1000)
  for (i in 1:1000) {
    imputed_values[i] = carryover(test3_df, method = "population")[2,2]
  }
  testthat::expect_equal(TRUE, all(imputed_values == 6))
})


test4_df = cbind(test1_df, c(3, 6, 9))
testthat::test_that("we do not impute unbounded cases when bounded_only = TRUE", {
  test4_df[2,3] = NA
  testthat::expect_equal(TRUE, is.na(as.numeric(unlist(carryover(test4_df, method = c("random"), bounded_only = TRUE)[2,2]))))
  testthat::expect_equal(TRUE, is.na(as.numeric(unlist(carryover(test4_df, method = c("random"), bounded_only = TRUE)[2,3]))))
})

testthat::test_that("in the unbounded case population carryover imputes with the correct probability", {
  imputed_values1 = numeric(length = 1000)
  imputed_values2 = numeric(length = 1000)
  for (i in 1:1000) {
    imputed_values1[i] = carryover(test4_df, method = "population", bounded_only = FALSE)[2,2]
    imputed_values2[i] = carryover(test4_df, method = "population", bounded_only = FALSE)[2,3]
  }
  testthat::expect_equal(TRUE, (abs(length(which(imputed_values1 == 4))/1000 - 0.5) < 0.1))
  testthat::expect_equal(TRUE, all(imputed_values2 == 6))
})

testthat::test_that("the population probability works as desired when the differences between subsequent cells is constant", {
  imputed_values = numeric(length = 1000)
  for (i in 1:1000) {
    imputed_values[i] = carryover(test1_df, method = "population")[2,2]
  }
  testthat::expect_equal(TRUE, (abs(length(which(imputed_values == 4))/1000 - 0.5) < 0.1))
})
