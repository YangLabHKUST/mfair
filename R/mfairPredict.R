#' Prediction function for MFAIR object.
#'
#' @param object A model object for which prediction is desired.
#' @param which_factors Which factors, i.e., which columns of Z and W, are used to make prediction. All K factors are used by default.
#'
#' @return Predicted matrix with the same dimension as that of Y.
#' @export
#'
setMethod(
  f = "predict",
  signature = "MFAIR",
  definition = function(object, which_factors = c(1:object@K)) {
    # Check Y
    if (object@Y_missing == FALSE) {
      message("The main data matrix Y has no missing entries!")
    }

    # Check inferred loading Z and factor W
    if (length(object@Z) == 0 || length(object@W) == 0) {
      stop("The model has not been fitted!")
    } # End

    Y_hat <- object@Z[, which_factors] %*% t(object@W[, which_factors]) + object@Y_mean

    return(Y_hat)
  }
)

#' Prediction function for MFAIRSingleFactor object.
#'
#' @param object A model object for which prediction is desired.
#'
#' @return Predicted matrix with the same dimension as that of Y.
#' @export
#'
setMethod(
  f = "predict",
  signature = "MFAIRSingleFactor",
  definition = function(object) {
    # Check inferred loading Z and factor W
    if (length(object@mu) == 0 || length(object@nu) == 0) {
      stop("The model has not been fitted!")
    } # End

    return(Y_hat = as.matrix(object@mu) %*% t(object@nu))
  }
)

#' Prediction function for fitted functions.
#'
#' @param object MFAIR object.
#' @param newdata Data frame containing the values at which predictions are required.
#' @param which_factors Which factors, i.e., which fitted functions are used. All K factors are used by default.
#'
#' @return A matrix containing predicted F(X). Each row is a new sample and each column is a factor.
#' @export
#'
predictFX <- function(object, newdata, which_factors = c(1:object@K)) {
  newdata <- as.data.frame(newdata)
  N <- nrow(newdata)

  # Check fitted functions
  if (length(object@tree_lists) == 0) {
    stop("There is no fitted function!")
  } # End
  if (length(object@tree_lists) < max(which_factors)) {
    stop("There are not so many factors in the model!")
  } # End

  # Predicted F(X) in factors interested
  FX <- sapply(object@tree_lists[which_factors],
    FUN = predictFXSF,
    newdata = newdata,
    learning_rate = object@learning_rate
  ) + matrix(rep((object@tree_0)[1, which_factors], each = N),
    nrow = N, ncol = length(which_factors)
  )
  colnames(FX) <- paste("Factor", which_factors)

  return(FX)
}

#' Prediction function for fitted function F() in single factor.
#'
#' @param tree_list A fitted function represented by a list of trees.
#' @param newdata Data frame containing the values at which predictions are required.
#' @param learning_rate Numeric. The learning rate in the gradient boosting part.
#'
#' @return A vector containing predicted F(X). Each entry corresponds to a new sample.
#' @export
#'
predictFXSF <- function(tree_list, newdata, learning_rate) {
  learning_rate * rowSums(sapply(tree_list, predict, newdata = newdata))
}
