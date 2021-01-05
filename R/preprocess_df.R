preprocess_df = function(df, id_col = NULL, imputation_classes_col = NULL, method = NULL){
  # Before processing, we check that all the information supplied makes sense. First, we check to see if the dataframe is something we can work with.
  if(!is.matrix(df) && !is.data.frame(df) && !tibble::is_tibble(df)){
    stop("Please supply a matrix, dataframe, or tibble.")
  }
  if(length(unique(colnames(df))) != ncol(df)){
    colnames(df) = paste0("c",as.character(1:ncol(df)))
  }
  imputed_df = tibble::as_tibble(df)
  # First we check if an id_col was supplied
  if(!is.null(id_col)){
    if (is.numeric(id_col) && id_col %% 1 == 0) { # If we were supplied a column index
      if(id_col > ncol(imputed_df)){ # We make sure it's in bounds
        stop("id_col is out of bounds, please supply either a column name or the column index of your id column.")
      }
    } else if(is.character(id_col)){ # If we were supplied with a column name
      if(!(id_col %in% colnames(imputed_df))){ # We make sure it's an actual column in df
        stop("Supplied id_col name is not an actual name in the dataframe. Check for misspelling or that the id column is actually part of the dataframe")
      }
    } else { # If we were given something that's neither numeric nor a character vector
      stop("Please supply either the index of your id column or the name of it in the argument id_col.")
    }
    imputed_df = dplyr::select(imputed_df, -id_col)
  }
  if(dim(as.data.frame(imputed_df))[1] == 0 || dim(as.data.frame(imputed_df))[2] == 0){
    stop("Your data are empty.")
  }
  # Now we check if an imputation_classes_col was supplied
  if(!is.null(imputation_classes_col)){
    if (is.numeric(imputation_classes_col) && imputation_classes_col %% 1 == 0) {
      if(imputation_classes_col > ncol(imputed_df)){ # We make sure it's in bounds
        stop("imputation_classes_col is out of bounds, please supply either a column name or the column index of your id column.")
      }
    } else if(is.character(imputation_classes_col)){
      if(!(imputation_classes_col %in% colnames(imputed_df))){ # We make sure it's an actual column in df
        stop("Supplied imputation_classes_col name is not an actual name in the dataframe. Check for misspelling or that the imputation class column is actually part of the dataframe.")
      }
    } else{ # If we were given something that's neither numeric nor a character vector
      stop("Please supply either the index of your imputation classes column or the name of it in the argument imputation_classes_col.")
    }
    imputed_df = dplyr::select(imputed_df, -imputation_classes_col)
  }
  # Now, in the case of the carryover function, we check to see that a valid method was specified
  if(!is.null(method)){
    if(!(tolower(method) %in% c("lvcf", "random", "population"))){
      stop('Please specify "lvcf", "random", or "population" as a method.')
    }
  }
  # Now, we check if the df is all numeric, as that is all our methods
  for (col in colnames(imputed_df)) {
    if(!is.numeric(dplyr::pull(imputed_df, col))){
      stop("Please supply a df with only numeric columns (aside from imputation classes and id column, which may be factors).")
    }
  }
  return(imputed_df)
}
