#' Fit the MFAI model using greedy algorithm.
#'
#' @param object MFAIR object.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param minsplit Numeric. Parameter for the gradient boosting part.
#' @param minbucket Numeric. Parameter for the gradient boosting part.
#' @param maxdepth Numeric. Parameter for the gradient boosting part.
#' @param iter_max_bf Integer. Maximum iterations allowed.
#' @param tol_bf Numeric. The convergence criterion.
#' @param verbose_bf_inner Logical. Whether to display the detailed information when fitting the model.
#' @param verbose_bf_outer Logical. Whether to display the detailed information when fitting the model.
#' @param ...
#'
#' @return An MFAIR object containing the information about the fitted MFAI model using backfitting algorithm.
#' @export
#'
fitBack <- function(object,
                    learning_rate = 0.1,
                    minsplit = 10, minbucket = round(minsplit / 3), maxdepth = 2,
                    iter_max_bf = 5000, tol_bf = 0.01,
                    verbose_bf_inner = TRUE, verbose_bf_outer = TRUE, ...) {
  # Check K
  if (object@K == 1) {
    stop("The backfitting algorithm is equivalent to the greedy algorithm when rank K = 1!")
  } # End

  # Check fitted functions F(), i.e., tree_lists
  if (length(object@tree_lists) == 0) {
    warning("The previous tree_lists (i.e., fitted functions) may not be saved!\n")
    warning("The new tree_lists obtained after the backfitting algorithm may not accurately predict the new sample with auxiliary covariates.!\n")
  }

  # Set up parameters for the gradient boosting part
  object@learning_rate <- learning_rate
  object@tree_parameters <- rpart::rpart.control(
    minsplit = minsplit,
    minbucket = minbucket,
    maxdepth = maxdepth
  )

  tau <- object@tau
  beta <- object@beta

  # Begin backfitting algorithm
  if (object@Y_missing) {
    obs_indices <- !is.na(object@Y)

    for (iter in 1:iter_max_bf) {
      for (k in 1:object@K) {
        # The residual (low-rank approximation using all factors but k-th)
        R <- object@Y - predict(object, which_factor = -k)
        mfair_sf <- new(
          Class = "MFAIRSingleFactor",
          Y_missing = object@Y_missing,
          n_obs = object@n_obs,
          mu = object@Z[, k],
          a_sq = object@a_sq[, k],
          nu = object@W[, k],
          b_sq = object@b_sq[, k],
          tau = object@tau[k],
          beta = object@beta[k],
          FX = object@FX[, k],
          tree_list = object@tree_lists[[k]]
        )

        mfair_sf <- fitSFMissing(R, obs_indices, object@X, mfair_sf,
          object@learning_rate,
          tree_parameters = object@tree_parameters,
          ...
        )

        object <- updateMFAIR(object, mfair_sf, k)

        if (verbose_bf_inner) {
          cat(
            "Backfitting for ", k, "-th factor finished!\n",
            sep = ""
          )
        }
      }

      tau_new <- object@tau
      beta_new <- object@beta

      gap <- mean(abs(tau_new - tau) / abs(tau)) + mean(abs(beta_new - beta) / abs(beta))
      if (verbose_bf_outer) {
        cat("Iteration: ", iter, ", relative difference of model parameters: ", gap, ".\n", sep = "")
      }
      if (gap < tol_bf) {
        break
      }

      tau <- tau_new
      beta <- beta_new
    }
    return(object)
  } else {
    for (iter in 1:iter_max_bf) {
      for (k in 1:object@K) {
        # The residual (low-rank approximation using all factors but k-th)
        R <- object@Y - predict(object, which_factor = -k)
        mfair_sf <- new(
          Class = "MFAIRSingleFactor",
          Y_missing = object@Y_missing,
          n_obs = object@n_obs,
          mu = object@Z[, k],
          a_sq = object@a_sq[, k],
          nu = object@W[, k],
          b_sq = object@b_sq[, k],
          tau = object@tau[k],
          beta = object@beta[k],
          FX = object@FX[, k],
          tree_list = object@tree_lists[[k]]
        )

        mfair_sf <- fitSFFully(R, object@X, mfair_sf,
          object@learning_rate,
          tree_parameters = object@tree_parameters,
          ...
        )

        object <- updateMFAIR(object, mfair_sf, k)

        if (verbose_bf_inner) {
          cat(
            "Backfitting for ", k, "-th factor finished!\n",
            sep = ""
          )
        }
      }

      tau_new <- object@tau
      beta_new <- object@beta

      gap <- mean(abs(tau_new - tau) / abs(tau)) + mean(abs(beta_new - beta) / abs(beta))
      if (verbose_bf_outer) {
        cat("Iteration: ", iter, ", relative difference of model parameters: ", gap, ".\n", sep = "")
      }
      if (gap < tol_bf) {
        break
      }

      tau <- tau_new
      beta <- beta_new
    }
    return(object)
  }
}
