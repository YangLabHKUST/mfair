#' Each MFAIR object has a number of slots which store information. Key slots to access are listed below.
#'
#' @slot Y A matrix. The main data matrix of N samples and M features.
#' @slot X A data.frame. The auxiliary information data frame of N samples and C covariates.
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs integer. Total number of observed entries in Y.
#' @slot N An integer. Number of rows (samples) of Y, also the number of rows (samples) of X.
#' @slot M An integer. Number of columns (features) of Y.
#' @slot C An integer. Number of columns (auxiliary covariates) of X.
#' @slot K_max An integer. The maximum rank allowed in the model.
#' @slot K An integer. The inferred rank of Y.
#' @slot Z An N * K matrix. Estimated loading matrix, corresponding to the inferred posterior mean of Z in the MFAI model.
#' @slot a_sq A matrix containing posterior variance of Z with k-th column corresponding to the k-th loading. For fully observed Y, all N elements of one specific loading share the same posterior variance, then a_sq is a 1 * K matrix. For Y with missing data, elements of one specific loading have different posterior variances, then a_sq is an N * K matrix.
#' @slot W An M * K matrix. Estimated factor matrix, corresponding to the inferred posterior mean of W in the MFAI model.
#' @slot b_sq A matrix containing posterior variance of W with k-th column corresponding to the k-th factor. For fully observed Y, all M elements of one specific factor share the same posterior variance, then b_sq is a 1 * K matrix. For Y with missing data, elements of one specific factor have different posterior variances, then b_sq is an M * K matrix.
#' @slot tau Numeric. A vector of length K, containing the precision parameter for each pair of loading/factor.
#' @slot beta Numeric. A vector of length K, containing the precision parameter for each loading Z_k.
#' @slot FX An N * K matrix representing the prior mean of Z, corresponding to F(X) in the MFAI model.
#' @slot tree_lists A list of length K, containing K fitted tree lists and each list corresponding to function F_k(.) in the MFAI model.
#' @slot initialization A list. Initialization of the fitted model.
#' @slot learning_rate Numeric. The learning rate in the gradient boosting part.
#' @slot tree_parameters A list of options that control details of the rpart algorithm.
#' @slot project Character. Name of the project (for record keeping).
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
    n_obs = "integer",
    N = "integer",
    M = "integer",
    C = "integer",
    K_max = "integer",
    K = "integer",
    Z = "matrix",
    a_sq = "matrix",
    W = "matrix",
    b_sq = "matrix",
    tau = "numeric",
    beta = "numeric",
    FX = "matrix",
    tree_lists = "list",
    initialization = "list",
    learning_rate = "numeric",
    tree_parameters = "list",
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
#' @param Y A matrix. The main data matrix of N samples and M features.
#' @param X A data.frame. The auxiliary information data frame of N samples and C covariates.
#' @param K_max An integer. The maximum rank allowed in the model.
#' @param project Character. Name of the project (for record keeping).
#'
#' @return Returns MFAIR object, with main data matrix and auxiliary information.
#' @export
createMFAIR <- function(Y, X, K_max = 1L, project = "MFAIR") {
  # Data dimension
  N <- nrow(Y)
  M <- ncol(Y)

  # Check dimension
  if (N != nrow(X)) {
    stop("The number of samples in Y and X should be consistent!")
  } # End

  # Check Y
  Y_missing <- FALSE
  n_missing <- sum(is.na(Y))
  if (n_missing >= 1) {
    Y_missing <- TRUE
    if (n_missing == length(Y)) {
      stop("The main data matrix Y has no observed values!")
    } # End
  }

  if (Y_missing) {
    a_sq <- matrix(nrow = N, ncol = 0)
    b_sq <- matrix(nrow = M, ncol = 0)
  } else {
    a_sq <- matrix(nrow = 1, ncol = 0)
    b_sq <- matrix(nrow = 1, ncol = 0)
  }

  # Inheriting
  object <- new(
    Class = "MFAIR",
    Y = Y,
    X = as.data.frame(X),
    Y_missing = Y_missing,
    n_obs = N * M - n_missing,
    N = N,
    M = M,
    C = ncol(X),
    K_max = as.integer(K_max),
    K = 0L,
    Z = matrix(nrow = N, ncol = 0),
    a_sq = a_sq,
    W = matrix(nrow = M, ncol = 0),
    b_sq = b_sq,
    project = project
  )

  return(object)
}

#' MFAIRSingleFactor object contains the key information about the fitted single factor MFAI model.
#'
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs Integer. Total number of observed entries in Y.
#' @slot mu An vector of length N representing the inferred loading, corresponding to the posterior mean of z in the single factor MFAI model.
#' @slot a_sq Numeric. The posterior variance of the loading z. For fully observed Y, all N elements of the loading share the same posterior variance, then a_sq is a single number. For Y with missing data, the elements have different posterior variances, then a_sq is a vector of length N.
#' @slot nu An vector of length M representing the inferred factor, corresponding to the posterior mean of w in the single factor MFAI model.
#' @slot b_sq Numeric. The posterior variance of the factor w. For fully observed Y, all M elements of the factor share the same posterior variance, then b_sq is a single number. For Y with missing data, the elements have different posterior variances, then b_sq is a vector of length M.
#' @slot tau Numeric. Precision parameter this pair of loading/factor.
#' @slot beta Numeric. Precision parameter for this loading z.
#' @slot FX An vector of length N representing the prior mean of z, corresponding to F(X) in the single factor MFAI model.
#' @slot tree_list A list containing multiple decision trees, corresponding to function F(.) in the single factor MFAI model.
#' @slot project Character. Name of the project (for record keeping).
#'
#' @return MFAIRSingleFactor class.
#' @export
setClass(
  # Set the name for the class
  "MFAIRSingleFactor",

  # Define the slots
  slots = c(
    Y_missing = "logical",
    n_obs = "integer",
    mu = "numeric",
    a_sq = "numeric",
    nu = "numeric",
    b_sq = "numeric",
    tau = "numeric",
    beta = "numeric",
    FX = "numeric",
    tree_list = "list",
    project = "character"
  ),

  # Assign the default prototypes
  prototype = list(
    project = "MFAIRSingleFactor",
    tree_list = list()
  )
)
