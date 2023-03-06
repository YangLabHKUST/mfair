fitBack <- function(object,
                    learning_rate = 0.1,
                    minsplit = 10, minbucket = round(minsplit / 3), maxdepth = 2,
                    iter_max_bf = 5000, tol_bf = 1e-3, verbose_bf = TRUE, ...) {
  # Check K
  if (object@K == 1) {
    stop("The backfitting algorithm is equivalent to the greedy algorithm when rank K = 1!")
  } # End

  # Check fitted functions F(), i.e., tree_lists
  if (length(object@tree_lists) == 0) {
    warning("The tree_lists (i.e., fitted functions) should be saved before backfitting algorithm, otherwise FX (prior mean of the loading Z) should be set as zero!\n")
  }

  # Set up parameters for the gradient boosting part
  object@learning_rate <- learning_rate
  object@tree_parameters <- rpart::rpart.control(minsplit = minsplit,
                                                 minbucket = minbucket,
                                                 maxdepth = maxdepth)

  # Begin backfitting algorithm
  if (object@Y_missing) {
    for (i in 1:iter_max_bf) {
      for (k in 1:object@K) {
        # The residual (low-rank approximation using all factors but k-th)
        R <- object@Y - predict(object, which_factor = -k)
        mfairSF <- new(
          Class = "MFAIRSingleFactor",
          Y_missing = object@Y_missing,
          n_obs = object@n_obs,
          mu = object@Z[, k],
          a_sq = object@a_sq[, k],
          nu = object@nu[, k],
          b_sq = object@b_sq[, k],
          tau = object@tau[k],
          beta = object@beta[k],
          FX = object@FX[, k],
          tree_list = object@tree_lists[[k]]
        )

        mfairSF <- fitSFMissing(R, obs_indices, object@X, init,
                                object@learning_rate, tree_parameters = object@tree_parameters,
                                ...)



        Z[, k] <- init[[k]]$mu
        W[, k] <- init[[k]]$nu

        if (verbose) {
          cat(
            "Backfitting for ", k, "-th factor finished!\n"
          )
        }
      }

      para_new <- sapply(init, FUN = function(x) {
        return(c(x$tau, x$beta))
      })

      gap <- mean(abs(para_new - para) / abs(para))
      cat("diff: ", gap, "\n")
      if (gap < tol) {
        break
      }

      para <- para_new
    }
    return(object)
  } else {
    return(object)
  }
}
