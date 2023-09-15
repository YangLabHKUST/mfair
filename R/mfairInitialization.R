#' Create the MFAIR object with main data matrix and auxiliary information.
#'
#' @importFrom methods new
#'
#' @param Y A matrix. The main data matrix of N samples and M features.
#' @param X A data.frame. The auxiliary information data frame of N samples and C covariates.
#' @param Y_center Logical. Determines whether centering is performed.
#' @param K_max An integer. The maximum rank allowed in the model.
#' @param project Character. Name of the project (for record keeping).
#'
#' @return Returns MFAIR object, with main data matrix and auxiliary information.
#' @export
createMFAIR <- function(Y, X, Y_center = TRUE, K_max = 1L, project = "MFAIR") {
  # Data dimension
  N <- nrow(Y)
  M <- ncol(Y)

  # Check dimension
  if (N != nrow(X)) {
    stop("The number of samples in Y and X should be consistent!")
  } # End

  if(is.matrix(Y)){
    Y_sparse <- FALSE
  }else{
    Y_sparse <- TRUE
  }

  # Center the matrix Y
  if (Y_center) {
    if(!Y_sparse){
      Y_mean <- mean(Y, na.rm = TRUE)
      Y <- Y - Y_mean
    }
else{
  Y_mean <- mean(summary(Y)$x)
  Y@x <- Y@x - Y_mean
}
  } else {
    Y_mean <- 0
  }

  # Check Y's sparsity
  if(!Y_sparse){
    n_missing <- sum(is.na(Y))
  }
  else{
    n_missing <- length(Y) - length(Y@x)
  }
  if (n_missing >= 1) {
    if (n_missing == length(Y)) {
      stop("The main data matrix Y has no observed values!")
    } # End
    Y_missing <- TRUE
  } else {
    Y_missing <- FALSE
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
    Y_sparse = Y_sparse,
    Y_center = Y_center,
    Y_mean = Y_mean,
    Y_missing = Y_missing,
    n_obs = as.integer(N * M - n_missing),
    N = N,
    M = M,
    C = ncol(X),
    K_max = as.integer(K_max),
    K = 0L,
    Z = matrix(nrow = N, ncol = 0),
    a_sq = a_sq,
    W = matrix(nrow = M, ncol = 0),
    b_sq = b_sq,
    FX = matrix(nrow = N, ncol = 0),
    tree_0 = matrix(nrow = 1, ncol = 0),
    tree_lists = list(),
    project = project
  )

  return(object)
}

#' Initialize the parameters for the single factor MAFI model.
#'
#' @importFrom stats rnorm var
#' @importFrom methods new
#'
#' @param Y_missing Logical. Whether the main data matrix is partially observed. It will be automatically judged if not specified (default value NULL).
#' @param n_obs Integer. Total number of observed entries.
#' @param Y Main data matrix.
#'
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs Integer. Total number of observed entries in Y.
#'
#' @return MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @export
#'
initSF <- function(Y, Y_missing = NULL, n_obs) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- rnorm(N)
  nu <- rep(0.0, M)

  # Whether the main data matrix is partially observed.
  if (is.null(Y_missing)) {
    n_missing <- sum(is.na(Y))
    if (n_missing >= 1) {
      Y_missing <- TRUE
    } else {
      Y_missing <- FALSE
    }
  }

  if (Y_missing) {
    a_sq <- rep(1, N)
    b_sq <- rep(1, M)
  } else {
    a_sq <- 1
    b_sq <- 1
  }

  tau <- 2 / var(as.vector(Y), na.rm = TRUE)
  beta <- 2 / var(mu)
  FX <- rep(0.0, N)

  object <- new(
    Class = "MFAIRSingleFactor",
    Y_missing = Y_missing,
    n_obs = as.integer(n_obs),
    mu = mu,
    a_sq = a_sq,
    nu = nu,
    b_sq = b_sq,
    tau = tau,
    beta = beta,
    FX = FX,
    tree_0 = 0,
    tree_list = list()
  )

  return(object)
}
