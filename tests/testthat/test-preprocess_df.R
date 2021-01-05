testthat::test_that("if a vector is supplied, an error is thrown", {
  testthat::expect_error(preprocess_df(c(1, 2, 3, NA, 4)))
})

pp_test2_df = rbind(
  c(1, 1, 2),
  c(9, 11, 12),
  c(1, NA, 3),
  c(12, 11, NA)
)
testthat::test_that("if an out of bounds or misnamed column ID is supplied, an error is thrown", {
  testthat::expect_error(preprocess_df(pp_test2_df, id_col = 10))
  testthat::expect_error(preprocess_df(pp_test2_df, id_col = "identity"))
})

testthat::test_that("if an out of bounds or misnamed imputation classes column is supplied, an error is thrown", {
  testthat::expect_error(preprocess_df(pp_test2_df, imputation_classes_col = 10))
  testthat::expect_error(preprocess_df(pp_test2_df, imputation_classes_col = "imp_classes"))
})

testthat::test_that("if a non-integer column is specified, an error is thrown", {
  testthat::expect_error(preprocess_df(pp_test2_df, id_col = 2.3))
  testthat::expect_error(preprocess_df(pp_test2_df, imputation_classes_col = 2.3))
})

testthat::test_that("if any of the columns are non-numeric, an error is thrown", {
  pp_test5_df = cbind(c("a", "a", "b", "b"), pp_test2_df)
  testthat::expect_error(preprocess_df(pp_test5_df))
})

testthat::test_that("if the df is empty, an error is thrown", {
  pp_test6_df = data.frame(c1 = as.numeric(), c2 = as.numeric())
  testthat::expect_error(preprocess_df(pp_test6_df))
})
