#' Predict function for MFAIR object.
#'
#' @param MFAIR MFAIR object.
#'
#' @return Predicted matrix with the same dimension as that of Y.
#' @export
#'
#' @examples
#' MFAIR <- createMFAIR(Y, X)
#' MFAIR <- fitGreedy(MFAIR)
#' Y_hat <- predict(MFAIR)
setMethod(
  f = "predict",
  signature = "MFAIR",
  definition = function(object) {
    # Check Y
    if (object@Y_missing == FALSE) {
      message("The main data matrix Y has no missing entries!")
    } # End

    # Check inferred loading Z and factor W
    if (length(object@Z) == 0 && length(object@W) == 0) {
      stop("The model has not been fitted!")
    } # End

    return(Y_hat = object@Z %*% t(object@W))
  }
)
