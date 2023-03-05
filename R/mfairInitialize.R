#' MFAIRSingleFactor object contains the key information about the fitted single factor MFAI model.
#'
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

#' Initialize the parameters for the single factor MAFI model with fully observed main data matrix.
#'
#' @param Y A fully observed main data matrix (no missing entries).
#'
#' @return MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @export
#'
initFully <- function(Y) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- rnorm(N)
  a_sq <- abs(rnorm(1)) + 1
  nu <- rep(0.0, M)
  b_sq <- 1

  tau <- 2 / var(as.vector(Y))
  beta <- 2 / var(mu)

  FX <- rep(0.0, N)

  object <- new(
    mu = mu,
    a_sq = a_sq,
    nu = nu,
    b_sq = b_sq,
    tau = tau,
    beta = beta,
    FX = FX
  )

  return(object)
}

#' Initialize the parameters for the single factor MAFI model with partially observed main data matrix.
#'
#' @param Y A partially observed main data matrix.
#'
#' @return MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @export
#'
initMissing <- function(Y) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- rnorm(N)
  a_sq <- rep(1, N)
  nu <- rep(0.0, M)
  b_sq <- rep(1, M)

  tau <- 2 / var(as.vector(Y), na.rm = TRUE)
  beta <- 2 / var(mu)

  FX <- rep(0.0, N)

  object <- new(
    mu = mu,
    a_sq = a_sq,
    nu = nu,
    b_sq = b_sq,
    tau = tau,
    beta = beta,
    FX = FX
  )

  return(object)
}
