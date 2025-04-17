#' Fit the MFAI model using backfitting algorithm.
#'
#' @import Matrix
#' @importFrom methods new
#' @importFrom rpart rpart.control
#'
#' @param object MFAIR object.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param minsplit Numeric. Parameter for the gradient boosting part.
#' @param minbucket Numeric. Parameter for the gradient boosting part.
#' @param maxdepth Numeric. Parameter for the gradient boosting part.
#' @param other_tree_para A list containing other parameters for the gradient boosting part. See rpart::rpart.control() for details.
#' @param iter_max_bf Integer. Maximum iterations allowed.
#' @param tol_bf Numeric. The convergence criterion.
#' @param verbose_bf_inner Logical. Whether to display the detailed information during the inner loop.
#' @param verbose_bf_outer Logical. Whether to display the detailed information during the outer loop.
#' @param sf_para A list containing parameters for fitting the single factor MFAI model. See fitSFFully(), fitSFMissing(), or fitSFSparse() for details.
#'
#' @return An MFAIR object containing the information about the fitted MFAI model using backfitting algorithm.
#' @export
#'
fitBack <- function(object,
                    learning_rate = 0.1,
                    minsplit = 10, minbucket = round(minsplit / 3),
                    maxdepth = 2, other_tree_para = list(),
                    iter_max_bf = 5000, tol_bf = 0.01,
                    verbose_bf_inner = TRUE, verbose_bf_outer = TRUE,
                    sf_para = list()) {
  # Check K
  if (object@K == 1) {
    warning("The backfitting algorithm is equivalent to the greedy algorithm
         when rank K = 1!\n")
    return(object)
  } # End

  # Check fitted functions F(), i.e., tree_0 and tree_lists
  if (length(object@tree_0) == 0) {
    # object@tree_0 <- matrix(0, nrow = 1, ncol = object@K)
    object@tree_0 <- as.list(rep(0, object@K))
    warning("The previous tree_0 (i.e., fitted functions) may not be saved!\n")
    warning("The new tree_lists obtained after the backfitting algorithm
            may not accurately predict the new sample
            with auxiliary covariates!\n")
  }
  if (length(object@tree_lists) == 0) {
    object@tree_lists <- lapply(
      1:object@K,
      FUN = function(x) {
        list()
      }
    )
    warning("The previous tree_lists (i.e., fitted functions)
            may not be saved!\n")
    warning("The new tree_lists obtained after the backfitting algorithm
            may not accurately predict the new sample
            with auxiliary covariates.!\n")
  }

  # Set up parameters for the gradient boosting part
  object@learning_rate <- learning_rate
  object@tree_parameters <- do.call(
    what = "rpart.control",
    args = append(
      list(
        minsplit = minsplit,
        minbucket = minbucket,
        maxdepth = maxdepth
      ),
      other_tree_para
    )
  )
  # object@tree_parameters <- rpart.control(
  #   minsplit = minsplit,
  #   minbucket = minbucket,
  #   maxdepth = maxdepth
  # )

  # Will be used for the partially observed matrix fitting
  if (object@Y_missing) {
    if (object@Y_sparse) {
      obs_indices <- as.matrix(summary(object@Y)[, c(1, 2)])
    } else {
      obs_indices <- !is.na(object@Y)
    }
  }

  tau <- object@tau
  beta <- object@beta

  # Begin backfitting algorithm
  for (iter in 1:iter_max_bf) {
    for (k in 1:object@K) {
      # Initialize
      mfair_sf <- new(
        Class = "MFAIRSingleFactor",
        Y_missing = object@Y_missing,
        n_obs = object@n_obs,
        mu = object@Z[[k]],
        a_sq = object@a_sq[[k]],
        nu = object@W[[k]],
        b_sq = object@b_sq[[k]],
        tau = object@tau[[k]],
        beta = object@beta[[k]],
        FX = object@FX[[k]],
        tree_0 = object@tree_0[[k]],
        tree_list = object@tree_lists[[k]]
      )

      if (object@Y_sparse) {
        # The residual (low-rank approximation using all factors but k-th)
        R <- object@Y -
          projSparse(
            predict(object, which_factors = -k, add_mean = FALSE),
            obs_indices
          )

        mfair_sf <- do.call(
          what = "fitSFSparse",
          args = append(
            list(
              Y = R, X = object@X, init = mfair_sf,
              obs_indices = obs_indices,
              stage1 = FALSE,
              learning_rate = object@learning_rate,
              tree_parameters = object@tree_parameters
            ),
            sf_para
          )
        )
      } else {
        # The residual (low-rank approximation using all factors but k-th)
        R <- object@Y -
          predict(object, which_factors = -k, add_mean = FALSE)

        if (object@Y_missing) {
          # mfair_sf <- fitSFMissing(R, obs_indices, object@X, mfair_sf,
          #   object@learning_rate,
          #   tree_parameters = object@tree_parameters,
          #   ...
          # )
          mfair_sf <- do.call(
            what = "fitSFMissing",
            args = append(
              list(
                Y = R, X = object@X, init = mfair_sf,
                obs_indices = obs_indices,
                stage1 = FALSE,
                learning_rate = object@learning_rate,
                tree_parameters = object@tree_parameters
              ),
              sf_para
            )
          )
        } else {
          # mfair_sf <- fitSFFully(R, object@X, mfair_sf,
          #   object@learning_rate,
          #   tree_parameters = object@tree_parameters,
          #   ...
          # )
          mfair_sf <- do.call(
            what = "fitSFFully",
            args = append(
              list(
                Y = R, X = object@X,
                init = mfair_sf,
                stage1 = FALSE,
                learning_rate = object@learning_rate,
                tree_parameters = object@tree_parameters
              ),
              sf_para
            )
          )
        }
      }

      object <- updateMFAIR(object, mfair_sf, k)

      if (verbose_bf_inner) {
        cat(
          "Backfitting for Factor ", k, " finished!\n",
          sep = ""
        )
      }
    }

    tau_new <- object@tau
    beta_new <- object@beta

    gap <- mean(abs(tau_new - tau) / abs(tau)) +
      mean(abs(beta_new - beta) / abs(beta))
    if (verbose_bf_outer) {
      cat("Iteration: ", iter,
        ", relative difference of model parameters: ", gap, ".\n",
        sep = ""
      )
    }
    if (gap < tol_bf) {
      break
    }

    tau <- tau_new
    beta <- beta_new
  }
  return(object)
}
