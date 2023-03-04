#' Each MFAI object has a number of slots which store information. Key slots to access are listed below.
#'
#' @slot Y numeric. The main data matrix of N samples and M features.
#' @slot X data.frame. The auxiliary information data frame of N samples and C covariates.
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
#' @return MFAI class.
#' @export
setClass(
  # Set the name for the class
  "MFAI",
  
  # Define the slots
  slots = c(
    Y = "matrix",
    X = "data.frame",
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
    project = "MFAI",
    K_max = 1L
  )
)

#' Create the MFAI object with main data matrix and auxiliary information.
#'
#' @param Y numeric. The main data matrix.
#' @param X data.frame. The auxiliary information.
#' @param project Name of the project (for record keeping).
#'
#' @return Returns MFAI object, with main data matrix and auxiliary information.
#' @export
createMFAI <- function(Y, X, project = "MFAI") {
  # Check dimension
  if (nrow(Y) != nrow(X)) {
    stop("The number of samples in Y and X should be consistent!")
  } # End
  
  # Inheriting
  object <- new(
    Class = "MFAI",
    Y = Y,
    X = as.data.frame(X),
    project = project
  )
  
  return(object)
}
