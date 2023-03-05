#' Create generic for greedy algorithm.
#'
#' @param object a model object for which greedy algorithm is applied.
#' @param ... Additional arguments.
#'
#' @return MFAIR object containing the information about the fitted MFAI model using greedy algorithm.
#' @export
#'
setGeneric(
  name = "fitGreedy",
  def = function(object, ...) {
    standardGeneric("fitGreedy")
  }
)

#' Greedy algorithm for MFAIR object.
#'
#' @param MFAIR MFAIR object.
#'
#' @return MFAIR object containing the information about the fitted MFAI model using greedy algorithm.
#' @export
#'
setMethod(
  f = "fitGreedy",
  signature = "MFAIR",
  definition = function(object, ...) {
    if (object@Y_missing) {
      object <- fitGreedyMissing(object, ...)
    } else {
      object <- fitGreedyFully(object, ...)
    }
  }
)

#' Fitting the MFAI model with fully observed main data matrix Y using greedy algorithm.
#'
#' @param object MFAIR object.
#' @param K_max Integer. The maximum rank allowed in the MFAI model.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param minsplit Integer Parameter for the gradient boosting part.
#' @param minbucket Integer Parameter for the gradient boosting part.
#' @param maxdepth Integer Parameter for the gradient boosting part.
#' @param tol_snr Numeric. The convergence criterion which determine the inferred rank of data.
#' @param verbose_outer Logical. Whether to display the detailed information when fitting the model.
#' @param save_init Logical. Whether to save the initialization of the model.
#' @param ... See fitGreedyFullySF()
#'
#' @return An MFAIR object containing the information about the fitted MFAI model using greedy algorithm.
#' @export
#'
fitGreedyFully <- function(object, K_max = NULL,
                           learning_rate = 0.1, minsplit = 10, minbucket = round(minsplit / 3), maxdepth = 2,
                           tol_snr = 2e-3, verbose_outer = TRUE,
                           save_init = FALSE, ...) {
  # Set K_max
  if (!is.null(K_max)) {
    object@K_max <- K_max
  }
  # Check K_max
  if (object@K_max > object@N || object@K_max > object@N) {
    warning("The maximum rank allowed can not be larger than the rank of the main data matrix!\n")
    object@K_max <- min(object@N, object@M)
    warning("Reset the K_max = ", object@K_max, "!\n")
  }

  # Whether need initialization
  init_length <- length(object@initialization)
  if(init_length == 0){
    need_init <- rep(TRUE, object@K_max) # Need initialization for each factor
  }else{
    need_init <- rep(FALSE, object@K_max)
    if(init_length < object@K_max){
      warning("Only the first ", init_length, " factors have been initialized, which is less than K_max!\n")
      need_init[-(1:init_length)] <- TRUE
      warning("The remaining factors will be initialized automatically if needed!\n")
    }
  }

  # Residual in the first step is Y itself
  R <- object@Y

  # Begin fitting the MFAI model
  for (k in 1:object@K_max) {
    # Initialize
    if (need_init[k]) {
      init <- initFully(object@Y)
    }else{
      init <- object@initialization[[k]]
    }

    # Set the parameters for the gradient boosting part
    object@boosting_parameters <- rpart.control(minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth)

    # Fitting the single factor MFAI model.
    mfairSF <- fitGreedyFullySF(R, object@X, init, rpart_control = object@boosting_parameters, ...)

    # Predict Y based on one pair of loading/factor
    Y_k <- predict(mfairSF)

    # Whether to stop the greedy algorithm
    if ((var(as.vector(Y_k)) * mfairSF@tau) > tol_snr) {
      if(verbose_outer){
        message("Factor ", k, " retained!")
      }
    } else {
      if(verbose_outer){
        message("Factor ", k, " zeroed out!")
      }
      break
    }

    R <- R - Y_k # Prepare for the fitting for the next factor

    # Save the information about the fitted single factor MFAI model
    object@Z <- cbind(object@Z, mfairSF@mu)
    object@a_sq <- cbind(object@a_sq, mfairSF@a_sq)
    object@W <- cbind(object@W, mfairSF@nu)
    object@b_sq <- cbind(object@b_sq, mfairSF@b_sq)

    object@tau <- c(object@tau, mfairSF@tau)
    object@beta <- c(object@beta, mfairSF@beta)

    object@FX <- cbind(object@FX, mfairSF@FX)
    object@tree_lists <- c(object@tree_lists, list(mfairSF@tree_list))

    object@K <- object@K + 1

    # Save the initialization
    if(save_init){
      object@initialization <- c(object@initialization, list(init))
    }
    # Free the memory
    rm(init)
  }
}

fitGreedyFullySF <- function(Y, X, init, rpart_control,
                             iter_max = 5e+3, tol_stage1 = 0.1, tol_stage2 = 1e-5,
                             verbose_inner = TRUE, save_model = TRUE) {


}
