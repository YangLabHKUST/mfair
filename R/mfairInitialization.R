#' Create the MFAIR object with main data matrix and auxiliary information.
#'
#' @import Matrix
#' @importFrom methods new
#'
#' @param Y A matrix or Matrix::dgCMatrix. The main data matrix of N samples and M features.
#' @param X A data.frame. The auxiliary information data frame of N samples and C covariates.
#' @param Y_sparse Logical. Determines whether to use spase mode for Y.
#' @param Y_center Logical. Determines whether centering is performed.
#' @param K_max An integer. The maximum rank allowed in the model.
#' @param project Character. Name of the project (for record keeping).
#'
#' @return Returns MFAIR object, with main data matrix and auxiliary information.
#' @export
createMFAIR <- function(Y, X,
                        Y_sparse = FALSE, Y_center = TRUE,
                        K_max = 1L,
                        project = "MFAIR") {
  if (!is.data.frame(X)) {
    stop("X should be a data.frame!")
  } # End

  # Data dimension
  N <- nrow(Y)
  M <- ncol(Y)

  # Check dimension
  if (N != nrow(X)) {
    stop("The number of samples in Y and X should be consistent!")
  } # End

  # Check whether to transfer Y to the sparse matrix mode
  if (class(Y) == "dgCMatrix") { # Y is already in sparse mode
    Y_sparse <- TRUE
    message("The main data matrix Y has been stored in the sparse mode!")
  } else if (Y_sparse == TRUE) { # Y is not in sparse mode, but we want it to be
    obs_tf <- !is.na(Y) # Indicates whether observed or missing
    obs_idx <- which(obs_tf, arr.ind = TRUE) # Indices of observed entries
    Y <- Matrix::sparseMatrix(
      i = obs_idx[, "row"],
      j = obs_idx[, "col"],
      x = Y[obs_tf],
      dims = c(N, M),
      symmetric = FALSE, triangular = FALSE,
      index1 = TRUE,
      repr = "C"
    )
    message("The main data matrix Y has been transferred to the sparse mode!")
  } # Otherwise, Y is not in sparse mode and we don't want it to be

  # Check Y's sparsity
  if (!Y_sparse) {
    n_missing <- sum(is.na(Y))
  } else {
    n_missing <- length(Y) - length(Y@x)
    # Do not store the complete matrix in the sparse mode
    if(n_missing == 0){
      stop("Please do not store the complete matrix in the sparse mode!")
    } # End
  }
  if (n_missing >= 1) {
    if (n_missing == length(Y)) {
      stop("The main data matrix Y has no observed values!")
    } # End
    Y_missing <- TRUE
    message("The main data matrix Y is partially observed!")
  } else {
    Y_missing <- FALSE
    message("The main data matrix Y is completely observed!")
  }

  # Center the matrix Y
  if (Y_center) {
    if (Y_sparse) {
      Y_mean <- mean(Y@x)
      Y@x <- Y@x - Y_mean
    } else {
      Y_mean <- mean(Y, na.rm = TRUE)
      Y <- Y - Y_mean
    }
    message("The main data matrix Y has been centered with mean = ", Y_mean, "!")
  } else {
    Y_mean <- 0
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
    X = X,
    Y_missing = Y_missing,
    Y_sparse = Y_sparse,
    Y_center = Y_center,
    Y_mean = Y_mean,
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
#' @param Y_missing Logical. Whether the main data matrix is partially observed.
#' @param Y_sparse Logical. Whether the main data matrix is in sparse mode.
#' @param n_obs Integer. Total number of observed entries.
#' @param Y Main data matrix.
#'
#'
#' @return MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @export
#'
initSF <- function(Y, Y_missing, Y_sparse, n_obs) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- rnorm(N)
  nu <- rep(0.0, M)

  # # Whether the main data matrix is partially observed.
  # if (is.null(Y_missing)) {
  #   n_missing <- sum(is.na(Y))
  #   if (n_missing >= 1) {
  #     Y_missing <- TRUE
  #   } else {
  #     Y_missing <- FALSE
  #   }
  # }

  if (Y_missing) {
    a_sq <- rep(1, N)
    b_sq <- rep(1, M)
  } else {
    a_sq <- 1
    b_sq <- 1
  }

  if(Y_sparse){
    tau <- 2 / var(as.vector(Y@x), na.rm = TRUE)
  }else{
    tau <- 2 / var(as.vector(Y), na.rm = TRUE)
  }
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
