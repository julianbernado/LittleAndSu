# Test Case 1: Basic usage. This matrix is small enough that we can walk through the method by hand and see if our function lines up. The numbers have also been selected to be easy to work with
ls_test1_df = rbind(
  c(1, 1, 2),
  c(9, 11, 12),
  c(1, NA, 3),
  c(12, 11, NA)
)

# First, we calculate the column averages for the complete cases. This comes out to 5, 6, and 7
# Next, we get the average of these averages: 18/3 = 6
# Then, the wave effects are the column averages divided by the overall average. So our wave effects are 5/6, 1, and 7/6
# Now, we take the weighted (by wave effects) average of each row to find the individual effects.
# Row 1: (1/3)*((6/5) + 1 + 2*(6/7)) = 137/105 = ~1.305
# Row 2: (1/3)*(9*(6/5) + 11 + 12*(6/7)) = 1123/105 = ~10.70
# Row 3: (1/2)*((6/5) + 3*(6/7)) = 66/35 = ~1.89
# Row 4: (1/2)*(12*(6/5) + 11) = 127/10 = 12.7
# With our row effects, we can now go to each missing value and find their donor.
# Row 3, wave 2: the closest row effect is row 1, so we get that our donation will be the value 1
# -We scale this by Row 3's row effect divided by Row 1's to get an imputed value of (66/35)/(137/105) = 198/137 = ~1.45
# Row 4, wave 3: the closest row effect is row 2, so we get that our donation will be the value 12
# -We scale this by Row 4's row effect divided by Row 2's to get an imputed value of 12*(12.7/(1123/105)) = ~14.25
# With our imputed values calculated, we run the Little and Su function see if the values match.

imputed_ls_test1_df = little_and_su(ls_test1_df)

testthat::test_that("imputation coincides with algorithm done by hand", {
  testthat::expect_equal(198/137, unlist(imputed_ls_test1_df[3,2]))
  testthat::expect_equal(12*(12.7/(1123/105)), unlist(imputed_ls_test1_df[4,3]))
})

testthat::test_that("IDs columns are properly taken care of", {
  ls_test2_df = cbind(c(1, 2, 3, 4), ls_test1_df)
  imputed_ls_test2_df = little_and_su(ls_test2_df, id_col = 1)
  testthat::expect_identical(c(1, 2, 3, 4), unlist(imputed_ls_test2_df[,1]))
  testthat::expect_identical(imputed_ls_test1_df, imputed_ls_test2_df[,-1])
})

testthat::test_that("imputation via imputation classes coincides with algorithm done by hand", {
  ls_test3_df = cbind(c(1, 2, 1, 1), ls_test1_df)
  imputed_ls_test3_df = little_and_su(ls_test3_df, imputation_classes_col = 1)
  testthat::expect_equal(198/137, unlist(imputed_ls_test3_df[3,3]))
  testthat::expect_equal(2667/137, unlist(imputed_ls_test3_df[4,4]))
})

testthat::test_that("nothing is imputed for an entirely NA row", {
  ls_test4_df = rbind(ls_test1_df, c(NA, NA, NA))
  imputed_ls_test4_df = little_and_su(ls_test4_df)
  testthat::expect_equal(TRUE, all(is.na(unlist(imputed_ls_test4_df[5,]))))
})

testthat::test_that("nothing is imputed when there are no complete cases", {
  ls_test5_df = ls_test1_df
  ls_test5_df[1, 3] = NA
  ls_test5_df[2, 3] = NA
  imputed_ls_test5_df = little_and_su(ls_test5_df)
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test5_df[1,3])))
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test5_df[2,3])))
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test5_df[3,2])))
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test5_df[4,3])))
})

testthat::test_that("nothing is imputed when there are no complete cases in the same imputation column", {
  ls_test6_df = cbind(c(1, 1, 2, 2), ls_test1_df)
  imputed_ls_test6_df = little_and_su(ls_test6_df, imputation_classes_col = 1, verbose = 1)
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test6_df[3,3])))
  testthat::expect_equal(TRUE, is.na(unlist(imputed_ls_test6_df[4,4])))
})

testthat::test_that("nothing is imputed when there are no complete cases in the same imputation column", {
  ls_test7_df = ls_test1_df
  ls_test7_df[1,] = c(0,0,0)
  imputed_ls_test7_df = little_and_su(ls_test7_df)
  testthat::expect_equal(0, unlist(imputed_ls_test7_df[3,2]))
})

testthat::test_that("dataframes, matrices, and tibbles all impute the same values", {
  ls_test8_matrix = as.matrix(ls_test1_df)
  colnames(ls_test8_matrix) = colnames(imputed_ls_test1_df)
  ls_test8_tibble = tibble::as_tibble(ls_test1_df, .name_repair = "unique")
  colnames(ls_test8_tibble) = colnames(imputed_ls_test1_df)
  testthat::expect_identical(imputed_ls_test1_df, little_and_su(ls_test8_matrix))
  testthat::expect_identical(imputed_ls_test1_df, little_and_su(ls_test8_tibble))
})

# Test Case 9: Basic imputation with windows. We'll run this with window length 3.
ls_test9_df = rbind(
  c(1, 2, 3, 4, 5),
  c(11, 12, 13, 14, 15),
  c(21, NA, 23, 24, NA)
)
# Now, just as we did for TC1, let's calculate the column averages amongst the complete cases.
# They are 6, 7, 8, 9, and 10
# So, our overall average is 8.
# Then, the wave effects are the column averages divided by the overall average, so they are 3/4, 7/8, 1, 9/8, 5/4
# Now, individual effects are calculated differently. Here an individual effect is dependent on the values within a window. So, we'll have to calculate all individual effects for both missing values.
# Let's start with the missing value at [3,2]. The window here is just the first three values. So, let's calculate our effects.
# The individual effect is calculated the same as before, just summing over less. So,
# Row 1 Individual Effect (window 1): (1/3)*((4/3)*1 + (8/7)*2 + 3) = 139/63 = ~2.206
# Row 2 Individual Effect (window 1): (1/3)*((4/3)*11 + (8/7)*12 + 13) =  869/63 = ~13.793
# Row 3 Individual Effect (window 1): (1/2)*((4/3)*21 + 23) = 51/2 = 25.5
# So, for this imputation, clearly the row 2 individual effect is closer, so our donation is 12 and the imputed value should be:
# ((51/2)/(869/63))*12 = 19278/869 = ~22.184
# Now let's move to the missing value at [3,5]. The window here is the last three values. So,
# Row 1 Individual Effect (window 2): (1/3)*(3 + (8/9)*4 + (4/5)*5) = 95/27 = ~3.519
# Row 2 Individual Effect (window 2): (1/3)*(13 + (8/9)*14 + (4/5)*15) = 337/27 = ~12.481
# Row 3 Individual Effect (window 2): (1/2)*(23 + (8/9)*24) = 133/6 = ~22.167
# So, for this imputation, again the row 2 individual effect is closer, so our donation is 15 and the imputed value should be:
# ((133/6)/(337/27))*15 = 17955/674 = ~26.639
# Now let's see if it worked out that way
imputed_ls_test9_df = little_and_su(ls_test9_df, window = 3)

testthat::test_that("handworked algorithm with windows coincides with function", {
  testthat::expect_equal(19278/869, unlist(imputed_ls_test9_df[3,2]))
  testthat::expect_equal(17955/674, unlist(imputed_ls_test9_df[3,5]))
})

# Test Case 10: Donor that is not a complete case. We modify ls_test9_df to make row 2 incomplete, and make sure that we get a donated value from the second row.
ls_test10_df = ls_test9_df
ls_test10_df[2,2] = NA
# Let's run this one more time. Column averages for complete cases are clearly just 1, 2, 3, 4, and 5
# The overall average is thus 3
# So our wave effects are 1/3, 2/3, 1, 4/3, and 5/3
# Now let's get the individual effects (we're only going to look at the NA values at [3,2] and [3,5]:
# Row 1 Individual Effect (window 1): (1/3)*(3*1 + (3/2)*2 + 1*3) = 3
# Row 2 Individual Effect (window 1): (1/2)*(3*11 + 13) = 49/2 = 24.5
# Row 3 Individual Effect (window 1): (1/2)*(3*21 + 23) = 43
# So, Row 3 is closest to Row 2, but it's not complete so our donor will come from Row 1, thus it is 2. The imputed value is then:
# (43/3)*2 = 86/3 = ~28.667
# Now let's move to the [3,5] missing value. The window is the last three values. So,
# Row 1 Individual Effect (window 2): (1/3)*(3 + (3/4)*4 + (3/5)*5) = 3
# Row 2 Individual Effect (window 2): (1/3)*(13 + (3/4)*14 + (3/5)*15) = 65/6 = ~10.833
# Row 3 Individual Effect (window 2): (1/2)*(23 + (3/4)*24) = 41/2 = 20.5
# So, for the imputation, Row 2 Individual Effect is closer, and complete in this window. So, our donation is 15 and our impued value is:
# ((41/2)/(65/6))*15 = 369/13 = ~28.385
# Let's see if these are correct

testthat::test_that("when using windows, a donor can be an incomplete case", {
  imputed_ls_test10_df = little_and_su(ls_test10_df, window = 3)
  testthat::expect_equal(86/3, unlist(imputed_ls_test10_df[3,2]))
  testthat::expect_equal(369/13, unlist(imputed_ls_test10_df[3,5]))
})

testthat::test_that("windows and imputation classes play together fine", {
  ls_test11_df = cbind(c(1, 2, 1), ls_test9_df)
  imputed_ls_test11_df = little_and_su(ls_test11_df, imputation_classes_col = 1, window = 3)
  ls_test11_imp1 = unlist(imputed_ls_test11_df[3,3]) # ~17
  ls_test11_imp2 = unlist(imputed_ls_test11_df[3,6]) # ~43
  # Now, if we divide by the donor value and multiply by the donor individual effects we should get the individual effects from TC15
  ls_test11_ie1 = (ls_test11_imp1/2)*(139/63)
  ls_test11_ie2 = (ls_test11_imp2/5)*(95/27)
  testthat::expect_equal(51/2, ls_test11_ie1)
  testthat::expect_equal(133/6, ls_test11_ie2)
})
