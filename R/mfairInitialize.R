#' Initialize the parameters for the single factor MAFI model.
#'
#' @param Y Main data matrix.
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs Integer. Total number of observed entries in Y.
#'
#' @return MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @export
#'
initSF <- function(Y, Y_missing, n_obs) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- rnorm(N)
  nu <- rep(0.0, M)

  if (Y_missing) {
    a_sq <- rep(1, N)
    b_sq <- rep(1, M)
    tau <- 2 / var(as.vector(Y), na.rm = TRUE)
  } else {
    a_sq <- 1
    b_sq <- 1
    tau <- 2 / var(as.vector(Y))
  }

  beta <- 2 / var(mu)
  FX <- rep(0.0, N)

  object <- new(
    Y_missing = Y_missing,
    n_obs = n_obs,
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
