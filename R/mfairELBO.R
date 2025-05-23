#' Compute the evidence lower bound (ELBO) for fitted single factor MFAI model.
#'
#' @param Y Observed main data matrix.
#' @param object MFAIRSingleFactor object containing the information about the fitted single factor MFAI model.
#' @param obs_indices Indices of the observed entries in the main data matrix Y and needs to be specified only when Y is stored in the sparse mode. The default value is NULL.
#'
#' @return Numeric. The ELBO.
#' @export
#'
getELBO <- function(Y, object, obs_indices = NULL) {
  N <- nrow(Y)
  M <- ncol(Y)

  mu <- object@mu
  mu_sq <- (object@mu)^2
  nu <- object@nu
  nu_sq <- (object@nu)^2

  a_sq <- object@a_sq
  b_sq <- object@b_sq

  tau <- object@tau
  beta <- object@beta

  FX <- object@FX

  if (object@Y_missing) {
    elbo1 <- -object@n_obs * log(2 * pi / tau) / 2

    if (!is.null(obs_indices)) { # Sparse mode
      elbo2 <- -tau *
        sum((Y - projSparse(
          tcrossprod(as.matrix(mu), as.matrix(nu)), obs_indices
        ))^2 +
          projSparse(
            tcrossprod(as.matrix(mu_sq + a_sq), as.matrix(nu_sq + b_sq)), obs_indices
          ) -
          projSparse(
            tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)), obs_indices
          )) / 2
    } else {
      elbo2 <- -tau * sum(
        (Y - tcrossprod(as.matrix(mu), as.matrix(nu)))^2 +
          tcrossprod(as.matrix(mu_sq + a_sq), as.matrix(nu_sq + b_sq)) -
          tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)),
        na.rm = TRUE
      ) / 2
    }
    elbo3 <- -N * log(2 * pi / beta) / 2 -
      beta * (sum(mu_sq) + sum(a_sq) -
        2 * sum(mu * FX) + sum(FX^2)) / 2
    elbo4 <- -M * log(2 * pi) / 2 - (sum(nu_sq) + sum(b_sq)) / 2
    elbo5 <- sum(log(2 * pi * a_sq)) / 2 + N / 2
    elbo6 <- sum(log(2 * pi * b_sq)) / 2 + M / 2
  } else {
    elbo1 <- -N * M * log(2 * pi / tau) / 2
    elbo2 <- -tau * sum((Y - tcrossprod(as.matrix(mu), as.matrix(nu)))^2 +
      tcrossprod(as.matrix(mu_sq + a_sq), as.matrix(nu_sq + b_sq)) -
      tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq))) / 2
    elbo3 <- -N * log(2 * pi / beta) / 2 -
      beta * (sum(mu_sq) + N * a_sq -
        2 * sum(mu * FX) + sum(FX^2)) / 2
    elbo4 <- -M * log(2 * pi) / 2 - (sum(nu_sq) + M * b_sq) / 2
    elbo5 <- N * log(2 * pi * a_sq) / 2 + N / 2
    elbo6 <- M * log(2 * pi * b_sq) / 2 + M / 2
  }

  elbo <- elbo1 + elbo2 + elbo3 + elbo4 + elbo5 + elbo6
  return(elbo)
}
