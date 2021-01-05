generate_window = function(x, center, length){
  # This function should return a sequential subset of the supplied vector whose center is at the supplied place and whose length is of the supplied length
  # If the length would stretch past the beginning of the vector, we should loop back around
  # For example, generate_window(1:10, center = 5, length = 3) should return c(4, 5, 6)
  # x can be any vector
  # center should be an integer greater than or equal to 1 and less than or equal to length(x)
  # length should be an odd integer less than or equal to length(x)
  # length(generate_window(x = vec, center = i, length = j)) should always equal j
  if(!is.vector(x)){
    stop("x should be some vector.")
  }
  if(center > length(x) || center < 1){
    stop("center should be a valid index of x.")
  }
  if(length > length(x) || (length %% 2 == 0 && length != length(x)) || length %% 1 != 0){
    stop("window length should be an odd integer less than or equal to length(x).")
  }
  if(length == length(x)){
    window = 1:length(x)
  } else if(center - floor(length/2) < 1){ # In the case that the window would stretch beyond the beginning of the vector
    extra = 1 - (center - floor(length/2)) # We keep track of how much further it would stretch
    window = 1:(center + floor(length/2) + extra) # And make the window's "right arm" take on that extra length to ensure window is correct length
  } else if(center + floor(length/2) > length(x)){ # We act similarly if the window would stretch beyond the end of the vector
    extra = center + floor(length/2) - length(x)
    window = (center-floor(length/2) - extra):length(x)
  } else{ # If neither, we just set the window as what we would expect
    window = (center-floor(length/2)):(center+floor(length/2))
  }
  return(x[window]) # Then we just return the subset of the vector that our window covers
}

#' Little and Su Imputation
#'
#' This function imputes missing values in a logitudinal dataset using the Little and Su algorithm (Little and Su 1989).
#' Each imputed missing value is based on some effect calculated by values from the same unit observed at a different time and
#' some effect calculated by other units' values in the missing value's time period.
#' @param df data.frame, matrix, or tibble. Wide dataset with missing values. Each row should represent some unit and each column should represent a time period.
#' Besides this data an id column and an imputation classes column can be included. See id_col and imputation_classes_col for more.
#' @param window Odd integer. The window within which row effects are calculated. For panel data without many complete observations, consider
#' setting window to some odd integer. Defaults to NULL (which uses all time periods to calculate row effects).
#' @param id_col Integer or character. Identifies the column in the dataset with ID values for each observation. User may supply
#' either the number of the column or its name. Defaults to NULL.
#' @param imputation_classes_col Integer or character. Identifies the column in the dataset with imputation class values for each observation.
#' User may supply either the number of the column or its name. Defaults to NULL.
#' @param verbose Logical. If verbose is true, information about the imputation will be printed when little_and_su() is run.
#' @export
little_and_su = function(df, window = NULL, id_col = NULL, imputation_classes_col = NULL, verbose = FALSE){
  # Before processing, we check that all the information supplied makes sense. First, we check to see if the dataframe is something we can work with.
  imputed_df = preprocess_df(df, id_col = id_col, imputation_classes_col = imputation_classes_col)
  # We save the name of the imputation classes column for later
  if(!is.null(imputation_classes_col)){
    imputation_classes = df[,imputation_classes_col]
  } else{ # If no imputation classes column was specified we act as if one was in which all belong to the same class
    imputation_classes = numeric(length = nrow(imputed_df))
    imputation_classes[] = 1
  }
  # We then save the number of rows and colums in our dataframe
  num_col = ncol(imputed_df)
  num_obs = nrow(imputed_df)
  # And we initialize the window variable if it was not set already
  if(is.null(window)){
    window = num_col
  }
  # We also create a matrix version along with an NA matrix that has 1 when a value is present, otherwise 0
  df_matrix = as.matrix(imputed_df)
  na_matrix = df_matrix
  na_matrix[is.na(df_matrix)] = 0
  na_matrix[!is.na(df_matrix)] = 1
  updating_na_matrix = na_matrix # We create this one that we will update as we go
  df_matrix[is.na(df_matrix)] = 0 # Also, we make NA values 0 for later calculations
  num_present = rowSums(na_matrix) # This will be the number of present waves for each observation
  # We first differentiate between the complete and incomplete cases in order to calculate wave effects
  complete_case_indices = 1:num_obs # Start with all indices
  for (col in colnames(imputed_df)) { # For each wave, remove observations with a missing value for that wave
    complete_case_indices = dplyr::intersect(complete_case_indices, which(!is.na(dplyr::pull(imputed_df, col))))
  }
  incomplete_case_indices = dplyr::setdiff(1:num_obs, complete_case_indices) # the incomplete cases is simply all cases without the complete ones
  totally_missing_indices = which(rowSums(na_matrix) == 0)
  # Furthermore, we create a complete_case_matrix of dimension num_obs by num_col which says whether each case is complete if we center our window around that time period
  complete_case_matrix = matrix(nrow = num_obs, ncol = num_col)
  for (j in 1:num_col) { # For each column in the dataframe
    # The floor function will map anything less than 1 to 0 and keep anything equal to 1
    # The generate window function here selects the window of length window centered around the jth column
    # So when taking the rowsum of the na_matrix in this window, if it is equal to window, then all values are present, so floor(window/window) == 1
    # Thus, this matrix is 1 when a given row is entirely present in the window of length window centered around j
    complete_case_matrix[,j] = floor(rowSums(na_matrix[,generate_window(x = 1:num_col, center = j, length = window)])/window)
  }
  # Now the wave effects are calculated
  wave_effects = numeric(length = num_col)
  wave_averages = numeric(length = num_col)
  wave_averages = colMeans(imputed_df[complete_case_indices,])
  total_average = mean(wave_averages) # To get the overall average we take the average of the wave averages
  wave_effects = (1/total_average)*wave_averages # Then, wave_effects is simply the ratio of each of the wave_averages to the total_average
  # Then the individual effects are calculated in an individual_effects_matrix
  individual_effects_matrix = matrix(nrow = num_obs, ncol = num_col)
  for (j in 1:num_col) {
    individual_effects_matrix[,j] = df_matrix[,generate_window(1:num_col, center = j, length = window)] %*% (1/(generate_window(wave_effects, center = j, length = window))) # We use matrix multiplication to get the summed value for each observation (weighted by the inverse of the wave_effect for that wave)
    window_present = rowSums(na_matrix[,generate_window(1:num_col, center = j, length = window)]) # We then get how many observations are present in each row
    individual_effects_matrix[,j] = (1/window_present)*individual_effects_matrix[,j] # And we divide by that number of observations
  }
  # Now we impute the values for each missing case (except those for which there are NO present values)
  for (i in dplyr::setdiff(incomplete_case_indices, totally_missing_indices)) { # We only need to loop over the incomplete case indices, but can avoid those that are totally incomplete since we don't plan to impute them
    same_class = which(imputation_classes == imputation_classes[i])
    for (j in which(na_matrix[i,] == 0)) { # For a given row, we can only loop over those in the row that are missing
      possible_donors = dplyr::intersect(which(complete_case_matrix[,j]==1), same_class) # To be a possible donor, for that specific window, an observation must be complete, and it must belong to the same class
      if(length(possible_donors) == 0){ # If there are no possible donors
        donor_individual_effect = NA # There's no donor effect
        donor = 1 # And we just use a dummy index for the donor
      } else{
        possible_effects = individual_effects_matrix[possible_donors,j] # For the given row, we look at the individual effects for the particular wave from possible donors
        donor = possible_donors[which.min(abs(possible_effects - individual_effects_matrix[i,j]))] # The donor is the one whose effect is closest
        donor_individual_effect = individual_effects_matrix[donor,j]
      }
      donation = imputed_df[donor,j]
      if(is.na(donor_individual_effect)){ # If there are no values we just impute NA
        imputed_value = NA
      } else if(donor_individual_effect == 0){ # We handle this case separately so we don't divide by zero
        imputed_value = 0
      } else{
        imputed_value = donation*(individual_effects_matrix[i,j]/donor_individual_effect) # Our imputed value is the donation times the ratio of our ith individual effect to our donor's individual effect
      }
      imputed_df[i,j] = imputed_value # Now we impute whatever value we decided
      if(!is.na(imputed_value)){
        updating_na_matrix[i,j] = 1 # We update this matrix
      }
    }
  }
  if(!is.null(imputation_classes_col)){ # If imputation classes were supplied
    imputed_df = cbind(imputation_classes, imputed_df) # We add it back in
  }
  if(!is.null(id_col)){ # If an id column was supplied
    imputed_df = cbind(df[,id_col], imputed_df) # We add it back in
  }
  colnames(imputed_df) = colnames(df)
  if(verbose){
    new_incomplete_case_indices = which(rowSums(updating_na_matrix) < num_col)
    total_imputed = length(incomplete_case_indices) - length(new_incomplete_case_indices)
    class_empty = logical(length = length(unique(imputation_classes)))
    for (i in 1:length(class_empty)) {
      for (j in 1:window) {
        class_window_empty = all(rowSums(na_matrix[which(imputation_classes == unique(imputation_classes)[i]),generate_window(1:num_col, center = j, length = window)]) < window)
        if(class_window_empty){
          class_empty[i] = TRUE
         }
      }
    }
    if(length(complete_case_indices) == 0){
      print("Couldn't impute anything because there were no complete cases. Consider setting the windows argument.")
    } else{
      msg1 = ifelse(total_imputed == 1, " value. ", " values. ")
      msg2 = ifelse(length(new_incomplete_case_indices) == 1, " remains missing.", " remain missing.")
      print(paste0("Imputed ", total_imputed, msg1, length(new_incomplete_case_indices), msg2))
      if(length(new_incomplete_case_indices) > 0){
        if(length(totally_missing_indices) > 0){
          msg3 = ifelse(length(totally_missing_indices) == 1, " observation was", " observations were")
          print(paste0(length(totally_missing_indices), msg3, " entirely missing, so we did not impute."))
        }
        if(length(which(class_empty)) > 0 && !is.null(imputation_classes_col)){
          print("The following class(es) had certain windows where there were no complete cases:")
          print(unique(imputation_classes)[class_empty])
        } else if(length(which(class_empty)) > 0){
          print("Certain windows were incomplete so we could not impute.")
        }
      } else{
        print("All rows imputed!")
      }
    }
  }
  return(as.data.frame(imputed_df))
}
