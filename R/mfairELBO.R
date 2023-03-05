#' Compute the evidence lower bound (ELBO) for fitted single factor MFAI model.
#'
#' @param Y Observed main data matrix.
#' @param object MFAIRSingleFactor object containing the information about the fitted single factor MFAI model.
#'
#' @return Numeric. The ELBO.
#' @export
#'
getELBO <- function(Y, object) {
  N <- nrow(Y)
  M <- ncol(Y)

  # n_missing <- sum(is.na(Y))
  # if (n_missing >= 1) {
  #   Y_missing <- TRUE
  # } else {
  #   Y_missing <- FALSE
  # }

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
    n_obs <- object@n_obs
    elbo1 <- -n_obs * log(2 * pi / tau) / 2
    elbo2 <- -tau * sum((Y - as.matrix(mu) %*% t(nu))^2 + as.matrix(mu_sq + a_sq) %*% t(nu_sq + b_sq) - as.matrix(mu_sq) %*% t(nu_sq), na.rm = TRUE) / 2
    elbo3 <- -N * log(2 * pi / beta) / 2 - beta * (sum(mu_sq) + sum(a_sq) - 2 * sum(mu * FX) + sum(FX^2)) / 2
    elbo4 <- -M * log(2 * pi) / 2 - (sum(nu_sq) + sum(b_sq)) / 2
    elbo5 <- sum(log(2 * pi * a_sq)) / 2 + N / 2
    elbo6 <- sum(log(2 * pi * b_sq)) / 2 + M / 2
  } else {
    elbo1 <- -N * M * log(2 * pi / tau) / 2
    elbo2 <- -tau * sum((Y - as.matrix(mu) %*% t(nu))^2 + as.matrix(mu_sq + a_sq) %*% t(nu_sq + b_sq) - as.matrix(mu_sq) %*% t(nu_sq)) / 2
    elbo3 <- -N * log(2 * pi / beta) / 2 - beta * (sum(mu_sq) + N * a_sq - 2 * sum(mu * FX) + sum(FX^2)) / 2
    elbo4 <- -M * log(2 * pi) / 2 - (sum(nu_sq) + M * b_sq) / 2
    elbo5 <- N * log(2 * pi * a_sq) / 2 + N / 2
    elbo6 <- M * log(2 * pi * b_sq) / 2 + M / 2
  }

  elbo <- elbo1 + elbo2 + elbo3 + elbo4 + elbo5 + elbo6
  return(elbo)
}
