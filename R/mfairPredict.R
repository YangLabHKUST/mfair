#' Prediction function for MFAIR object.
#'
#' @param object A model object for which prediction is desired.
#' @param which_factor Which factors, i.e., which columns of Z and W, are used to make prediction. All K factors are used by default.
#'
#' @return Predicted matrix with the same dimension as that of Y.
#' @export
#'
#' @examples
#' mfairObject <- createMFAIR(Y, X)
#' mfairObject <- fitGreedy(mfairObject)
#' Y_hat <- predict(mfairObject)
setMethod(
  f = "predict",
  signature = "MFAIR",
  definition = function(object, which_factor = c(1:object@K)) {
    # Check Y
    if (object@Y_missing == FALSE) {
      message("The main data matrix Y has no missing entries!")
    }

    # Check inferred loading Z and factor W
    if (length(object@Z) == 0 || length(object@W) == 0) {
      stop("The model has not been fitted!")
    } # End

    return(Y_hat = object@Z[, which_factor] %*% t(object@W[, which_factor]))
  }
)

#' Prediction function for MFAIRSingleFactor object.
#'
#' @param MFAIRSingleFactor A model object for which prediction is desired.
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
