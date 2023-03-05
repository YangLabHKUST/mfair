#' Each MFAIR object has a number of slots which store information. Key slots to access are listed below.
#'
#' @slot Y numeric. The main data matrix of N samples and M features.
#' @slot X data.frame. The auxiliary information data frame of N samples and C covariates.
#' @slot Y_missing logical. Whether the main data matrix Y is partially observed.
#' @slot N integer. Number of rows (samples) of Y, also the number of rows (samples) of X.
#' @slot M integer. Number of columns (features) of Y.
#' @slot C integer. Number of columns (auxiliary covariates) of X.
#' @slot K_max integer. The maximum rank allowed in the model.
#' @slot K integer. The inferred rank of Y.
#' @slot Z numeric. Estimated loading matrix with dimension N * K, corresponding to the inferred posterior mean of Z in the MFAI model.
#' @slot W numeric. Estimated factor matrix with dimension M * K, corresponding to the inferred posterior mean of W in the MFAI model.
#' @slot tau numeric. A vector of length K, containing the precision parameter for each pair of loading/factor.
#' @slot beta numeric. A vector of length K, containing the precision parameter for each loading Z_k.
#' @slot FX numeric. An N * K matrix representing the prior mean of Z, corresponding to F(X) in the MFAI model.
#' @slot tree_lists list. A list containing K fitted tree lists, each corresponding to function F_k(.) in the MFAI model.
#' @slot initialization list. Initialization of the fitted model.
#' @slot boosting_parameters list. A list of options that control details of the rpart algorithm.
#' @slot project character. Name of the project (for record keeping).
#' @slot  .
#'
#' @return MFAIR class.
#' @export
setClass(
  # Set the name for the class
  "MFAIR",

  # Define the slots
  slots = c(
    Y = "matrix",
    X = "data.frame",
    Y_missing = "logical",
    N = "integer",
    M = "integer",
    C = "integer",
    K_max = "integer",
    K = "integer",
    Z = "matrix",
    W = "matrix",
    tau = "numeric",
    beta = "numeric",
    FX = "matrix",
    tree_lists = "list",
    initialization = "list",
    boosting_parameters = "list",
    project = "character"
  ),

  # Assign the default prototypes
  prototype = list(
    project = "MFAIR",
    K_max = 1L
  )
)

#' Create the MFAIR object with main data matrix and auxiliary information.
#'
#' @param Y numeric. The main data matrix.
#' @param X data.frame. The auxiliary information.
#' @param project Name of the project (for record keeping).
#'
#' @return Returns MFAIR object, with main data matrix and auxiliary information.
#' @export
createMFAIR <- function(Y, X, project = "MFAIR") {
  # Check dimension
  if (nrow(Y) != nrow(X)) {
    stop("The number of samples in Y and X should be consistent!")
  } # End

  # Check Y
  Y_missing <- FALSE
  n_missing <- sum(is.na(Y))
  if(n_missing >= 1){
    Y_missing = TRUE
    if(n_missing == length(Y)){
      stop("The main data matrix Y has no observed values!")
    } # End
  }

  # Inheriting
  object <- new(
    Class = "MFAIR",
    Y = Y,
    X = as.data.frame(X),
    Y_missing = Y_missing,
    N = nrow(Y),
    M = ncol(Y),
    C = ncol(X),
    project = project
  )

  return(object)
}
