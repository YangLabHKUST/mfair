#' Get importance measures of auxiliary covariates.
#'
#' @param object MFAIR object.
#' @param which_factors Which factors, i.e., which fitted functions are evaluated. All K factors are evaluated by default.
#'
#' @return Importance score matrix. Each row is an auxiliary covariate and each column is a factor.
#'
#' @details The `rpart::rpart()` function will automatically change special characters in variable names to a dot which may cause some inconsistency errors. Please ensure that the auxiliary covariates' names do not contain any special characters if you want to use this function.
#'
#' @export
#'
getImportance <- function(object, which_factors = seq_len(object@K)) {
  # Check fitted functions
  if (length(object@tree_lists) == 0) {
    stop("There is no fitted function!")
  } # End
  if (length(object@tree_lists) < max(which_factors)) {
    stop("There are not so many factors in the model!")
  } # End

  # Importance measures of variables in factors interested
  importance <- sapply(object@tree_lists[which_factors],
    FUN = getImportanceSF,
    variables_names = colnames(object@X)
  )
  # importance <- data.frame(Covariate = colnames(object@X), importance)
  colnames(importance) <- paste("Factor", which_factors)

  return(importance)
}

#' Get importance measures of auxiliary covariates in a single factor.
#'
#' @importFrom dplyr left_join
#'
#' @param tree_list A fitted function represented by a list of trees.
#' @param variables_names The names of the auxiliary covariates.
#'
#' @return Importance score vector. Each entry is the importance score of one auxiliary covariate.
#'
getImportanceSF <- function(tree_list, variables_names) {
  importance_list <- lapply(tree_list,
    FUN = function(x) {
      x$variable.importance
    }
  )

  # Data frame to store the importance measures in all trees.
  # Each row is a variable and each column is a tree.
  # The first column stores variable names.
  # We here use gsub() since the rpart() will automatically replace all special characters in strings with dots.
  importance_data_frame <- data.frame(variable = gsub("[[:punct:]]", ".", variables_names))

  # For-loop for each tree
  for (t in seq_len(length(importance_list))) {
    if (!is.null(importance_list[[t]])) {
      # Importance measures of variables contained in the t-th tree
      importance_t <- data.frame(
        variable = names(importance_list[[t]]),
        importance_list[[t]]
      )
      colnames(importance_t)[2] <- paste0("tree_", t)

      # Add the importance measures in the t-th tree to the importance_data_frame
      importance_data_frame <- dplyr::left_join(importance_data_frame, importance_t, by = "variable")
      # importance_data_frame <- merge(importance_data_frame, importance_t,
      #   by = "variable", all = TRUE, sort = FALSE
      # )
    }
  }

  # Add together the importance measures in all trees
  importance <- rowSums(importance_data_frame[, -1], na.rm = TRUE)
  names(importance) <- variables_names

  return(importance)
}
