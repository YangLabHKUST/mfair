#' Fit the MFAI model using greedy algorithm.
#'
#' @importFrom rpart rpart.control
#'
#' @param object MFAIR object.
#' @param K_max Integer. The maximum rank allowed in the MFAI model.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param minsplit Integer. Parameter for the gradient boosting part.
#' @param minbucket Integer. Parameter for the gradient boosting part.
#' @param maxdepth Integer. Parameter for the gradient boosting part.
#' @param other_tree_para A list containing other parameters for the gradient boosting part. See rpart::rpart.control() for details.
#' @param tol_snr Numeric. The convergence criterion which determine the inferred rank of data.
#' @param verbose_greedy Logical. Whether to display the detailed information when fitting the model.
#' @param save_init Logical. Whether to save the initialization of the model.
#' @param sf_para A list containing parameters for fitting the single factor MFAI model. See fitSFFully() or fitSFMissing() for details.
#'
#' @return An MFAIR object containing the information about the fitted MFAI model using greedy algorithm.
#' @export
fitGreedy <- function(object, K_max = NULL,
                      learning_rate = 0.1,
                      minsplit = 10, minbucket = round(minsplit / 3),
                      maxdepth = 2, other_tree_para = list(),
                      tol_snr = 2e-3, verbose_greedy = TRUE, save_init = FALSE,
                      sf_para = list()) {
  # Check whether partially observed main data matrix and record the indices
  if (object@Y_missing) {
    obs_indices <- !is.na(object@Y)
  }

  # Set K_max
  if (!is.null(K_max)) {
    object@K_max <- as.integer(K_max)
  }
  message("Set K_max = ", K_max, "!")

  # Check K_max
  if (object@K_max > object@N || object@K_max > object@M) {
    warning("The maximum rank allowed can not be larger than the rank of the main data matrix!\n")
    object@K_max <- min(object@N, object@M)
    warning("Reset K_max = ", object@K_max, "!\n")
  }

  # Check tree_0 and tree_lists
  if (length(object@tree_0) > 0 || length(object@tree_lists) > 0) {
    object@tree_0 <- matrix(nrow = 1, ncol = 0)
    object@tree_lists <- list()
    warning("Fitted function(s) found and has/have been cleared out!")
  }

  # Whether need initialization
  init_length <- length(object@initialization)
  if (init_length == 0) {
    need_init <- rep(TRUE, object@K_max) # Need initialization for each factor
  } else {
    need_init <- rep(FALSE, object@K_max)
    if (init_length < object@K_max) {
      warning("Only the first ", init_length, " factors have been initialized, which is less than K_max!\n")
      need_init[-(1:init_length)] <- TRUE
      warning("The remaining factors will be initialized automatically if needed!\n")
    }
  }

  # Set up parameters for the gradient boosting part
  object@learning_rate <- learning_rate
  # object@tree_parameters <- rpart.control(
  #   minsplit = minsplit,
  #   minbucket = minbucket,
  #   maxdepth = maxdepth
  # )
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

  # Residual in the first step is Y itself
  R <- object@Y

  # Begin fitting the MFAI model
  for (k in 1:object@K_max) {
    # Initialize
    if (need_init[k]) {
      init <- initSF(R, object@Y_missing, object@n_obs)
    } else {
      init <- object@initialization[[k]]
    }

    # Fit the single factor MFAI model
    if (object@Y_missing) {
      mfair_sf <- do.call(
        what = "fitSFMissing",
        args = append(
          list(
            Y = R, obs_indices = obs_indices,
            X = object@X, init = init,
            learning_rate = object@learning_rate,
            tree_parameters = object@tree_parameters
          ),
          sf_para
        )
      )
      # mfair_sf <- fitSFMissing(R, obs_indices, object@X, init,
      #   object@learning_rate,
      #   tree_parameters = object@tree_parameters,
      #   ...
      # )
    } else {
      mfair_sf <- do.call(
        what = "fitSFFully",
        args = append(
          list(
            Y = R, X = object@X, init = init,
            learning_rate = object@learning_rate,
            tree_parameters = object@tree_parameters
          ),
          sf_para
        )
      )
      # mfair_sf <- fitSFFully(R, object@X, init,
      #   object@learning_rate,
      #   tree_parameters = object@tree_parameters,
      #   ...
      # )
    }

    # Predict Y based on one pair of loading/factor
    Y_k <- predict(mfair_sf)

    # Whether to stop the greedy algorithm
    if ((var(as.vector(Y_k)) * mfair_sf@tau) > tol_snr) {
      if (verbose_greedy) {
        message("Factor ", k, " retained!")
      }
    } else {
      if (verbose_greedy) {
        message("Factor ", k, " zeroed out!")
      }
      break
    }

    # Prepare for the fitting for the next factor
    R <- R - Y_k

    # Save the information about the fitted single factor MFAI model
    object <- appendMFAIR(object, mfair_sf)

    # Save the initialization
    if (save_init) {
      object@initialization <- c(object@initialization, list(init))
    }
    # Free the memory
    rm(init)
  }

  return(object)
}
