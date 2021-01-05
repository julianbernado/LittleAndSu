carryover = function(df, method = c("lvcf", "random", "population"), fallback = FALSE, bounded_only = TRUE, id_col = NULL){
  # Before processing, we check that all the information supplied makes sense.
  imputed_df = preprocess_df(df, id_col = id_col, method = method)
  # We then save the number of rows and colums in our dataframe
  num_col = ncol(imputed_df)
  num_obs = nrow(imputed_df)
  # We also create a matrix version along with an NA matrix that has 1 when a value is present, otherwise 0
  df_matrix = as.matrix(imputed_df)
  na_matrix = df_matrix
  na_matrix[is.na(df_matrix)] = 0
  na_matrix[!is.na(df_matrix)] = 1
  present_by_row = rowSums(na_matrix)
  present_by_col = colSums(na_matrix)
  rows_with_na = which(present_by_row != num_col) # We mark which rows have at least one missing value
  cols_with_na = which(present_by_col != num_col) # And mark which columns have at least one missing value
  for (i in rows_with_na) { # For each row with at least one missing element
    for (j in which(na_matrix[i,] == 0)) { # We iterate over all missing values
      last_val = NA # Assuming we don't have a valid last value, we set it to NA
      next_val = NA # Same for next value
      present_value_indices = which(na_matrix[i,] == 1) # And we get which columns in the given row are present
      if(j > 1 && !is.na(df[i,j-1])){ # If we are not the first column and the previous column is present
        last_index = j-1 # We set our last index to the previous column
        last_val = df[i,last_index]
        if(method == "lvcf"){
          imputed_df[i,j] = last_val
          na_matrix[i,j] = 1
        }
      } else if(!bounded_only && length(which(present_value_indices < j)) > 0){ # If the previous value isn't present and we are allowed to look further, we check the next closest value
        last_index = present_value_indices[which(present_value_indices < j)[length(which(present_value_indices < j))]]
        last_val = as.numeric(df[i,last_index])
        if(method == "lvcf"){
          imputed_df[i,j] = last_val
          na_matrix[i,j] = 1
        }
      }
      if(j < num_col && !is.na(df[i,j+1])){ # If we are not the last column and the next column is present
        next_index = j+1 # We set the next index to the next column
        next_val = as.numeric(df[i,next_index])
      } else if(!bounded_only && length(which(present_value_indices > j)) > 0){ # If the next value isn't present and we are allowed to look further, we check the next closest value
        next_index = present_value_indices[which(present_value_indices > j)[1]]
        next_val = as.numeric(df[i,next_index])
      }
      if(!is.na(last_val) && !is.na(next_val)){ # If we have the previous and next value
        if(method == "random"){ # And the method is random
          p = 0.5 # Then we choose one of the two with equal probability
          imputed_df[i,j] = sample(c(last_val, next_val), size = 1, prob = c(1-p, p))
        } else if(method == "population"){ # If the method is population we calculate the proportion whose difference from the current to next period is smaller than their difference from current to last period
          comparison_indices = which(rowSums(na_matrix[,c(last_index, j, next_index)]) == 3) # We limit our search to those rows for which each of these three values is observed
          if(length(comparison_indices) > 0){
            prev_to_now = abs(df_matrix[comparison_indices,last_index] - df_matrix[comparison_indices,j])
            now_to_next = abs(df_matrix[comparison_indices,j] - df_matrix[comparison_indices,next_index])
            diff_abs_diff = prev_to_now - now_to_next
            prop_closer_to_next = length(which(diff_abs_diff > 0))/length(diff_abs_diff)
            prop_closer_to_last = length(which(diff_abs_diff < 0))/length(diff_abs_diff)
            p = prop_closer_to_next + 0.5*(1 - prop_closer_to_last - prop_closer_to_next) # If there are no ties, then prop_closer_to_last + prop_closer_to_next = 1 and we just use the latter, but if there are ties, then essentially they are weighed equally for both. In the extreme example of only ties, we get p=0.5
            imputed_df[i,j] = sample(c(last_val, next_val), size = 1, prob = c(1-p, p)) # And impute with the above decided probabilities
          }
        }
      }
    }
  }
  if(fallback){
    imputed_df = little_and_su(imputed_df)
  }
  if(!is.null(id_col)){ # If an id_col was specified
    imputed_df = cbind(id, imputed_df) # We add it back
    colnames(imputed_df)[1] = id_name
  }
  return(imputed_df)
}
