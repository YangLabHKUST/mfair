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

  # Check missingness
  if (inherits(Y, "sparseMatrix")) {
    n_missing <- length(Y) - length(Y@x)
  } else {
    n_missing <- sum(is.na(Y))
  }
  if (n_missing >= 1) {
    if (n_missing == N * M) {
      stop("The main data matrix Y has no observed values!")
    } # End
    Y_missing <- TRUE
    message("The main data matrix Y has ", n_missing / N / M * 100, "% missing entries!")
  } else {
    Y_missing <- FALSE
    message("The main data matrix Y is completely observed!")
  }

  # Check whether to transfer Y to the sparse matrix mode
  if (Y_sparse) { # We want Y to be in sparse mode
    if (!Y_missing) { # Y is completely observed and sparse mode is unnecessary
      warning("Please do not store the complete matrix in the sparse mode!")
    }
    if (inherits(Y, "sparseMatrix")) { # Y is already in sparse mode
      message("The main data matrix Y has been stored in the sparse mode and no transformation is needed!")
    } else { # Y is not in sparse mode but we want it to be
      obs_indices <- which(!is.na(Y), arr.ind = TRUE) # Indices of observed entries
      Y <- Matrix::sparseMatrix(
        i = obs_indices[, "row"], j = obs_indices[, "col"],
        x = Y[obs_indices],
        dims = c(N, M),
        symmetric = FALSE, triangular = FALSE,
        index1 = TRUE
      )
      message("The main data matrix Y has been transferred to the sparse mode!")
    }
  } else { # We don't want Y to be in sparse mode
    if (Y_missing) { # Y is partially observed
      warning("If there are a large number of missing entries, we recommend setting Y_sparse = TRUE!")
    }
    Y <- as.matrix(Y)
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

  # if (Y_missing) {
    # a_sq <- matrix(nrow = N, ncol = 0)
    # b_sq <- matrix(nrow = M, ncol = 0)
  # } else {
  #   a_sq <- matrix(nrow = 1, ncol = 0)
  #   b_sq <- matrix(nrow = 1, ncol = 0)
  # }

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
    # Z = matrix(nrow = N, ncol = 0),
    # a_sq = a_sq,
    # W = matrix(nrow = M, ncol = 0),
    # b_sq = b_sq,
    Z = list(),
    a_sq = list(),
    W = list(),
    b_sq = list(),
    tau = list(),
    beta = list(),
    # FX = matrix(nrow = N, ncol = 0),
    # tree_0 = matrix(nrow = 1, ncol = 0),
    FX = list(),
    tree_0 = list(),
    tree_lists = list(),
    initialization = list(),
    project = project
  )

  return(object)
}

#' Initialize the parameters for the single factor MAFI model.
#'
#' @import Matrix
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
    a_sq <- rep(1.0, N)
    b_sq <- rep(1.0, M)
  } else {
    a_sq <- 1.0
    b_sq <- 1.0
  }

  nu <- rnorm(M)
  if (Y_sparse) {
    mu <- (Y %*% as.matrix(nu))@x
    tau <- 2 / var(as.vector(Y@x), na.rm = TRUE)
  } else {
    Y_temp <- Y
    Y_temp[is.na(Y_temp)] <- 0
    mu <- as.vector(Y_temp %*% as.matrix(nu))
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
