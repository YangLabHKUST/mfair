#' Fit the single factor MFAI model with partially observed main data matrix stored in the sparse mode.
#'
#' @import Matrix
#' @importFrom rpart rpart
#'
#' @param Y Main data matrix.
#' @param X A data.frame containing the auxiliary information.
#' @param init A MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @param obs_indices Indices of the observed entries in the main data matrix.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param tree_parameters A list containing the parameters for the gradient boosting part.
#' @param stage1 Logical. Whether to perform fitting algorithm stage1. The greedy algorithm needs while the backfitting algorithm does not need.
#' @param iter_max Integer. Maximum iterations allowed.
#' @param tol_stage1 Numeric. Convergence criterion in the first step.
#' @param tol_stage2 Numeric. Convergence criterion in the first step.
#' @param verbose_sf Logical. Whether to display the detailed information.
#' @param verbose_loop Logical. Whether to display the detailed information when looping.
#' @param save_tree_list Logical. Whether to save the tree list.
#'
#' @return A MFAIRSingleFactor object containing the information about the fitted single factor MFAI model.
#'
fitSFSparse <- function(Y, X, init,
                        obs_indices,
                        learning_rate, tree_parameters,
                        stage1 = TRUE,
                        iter_max = 5e+3, tol_stage1 = 0.1, tol_stage2 = 1e-8,
                        verbose_sf = TRUE, verbose_loop = TRUE,
                        save_tree_list = TRUE) {
  N <- nrow(Y)
  M <- ncol(Y)

  # Represent the precision tau as matrix
  tau_mat <- Matrix::sparseMatrix(
    i = obs_indices[, 1], j = obs_indices[, 2],
    x = init@tau,
    dims = c(N, M),
    symmetric = FALSE, triangular = FALSE,
    index1 = TRUE
  )

  ELBO_old <- getELBO(Y, init, obs_indices)

  if (stage1) {
    # Stage 1
    for (iter in 1:iter_max) {
      # E-step
      E_wsq <- (init@nu)^2 + init@b_sq
      init@a_sq <- 1 / (init@beta + as.vector(tau_mat %*% as.matrix(E_wsq)))
      init@mu <- init@a_sq *
        (init@beta * init@FX +
          as.vector(init@tau * (Y %*% as.matrix(init@nu))))

      E_zsq <- (init@mu)^2 + init@a_sq
      init@b_sq <- 1 / (1 + as.vector(t(tau_mat) %*% as.matrix(E_zsq)))
      init@nu <- init@b_sq *
        as.vector(init@tau * crossprod(Y, as.matrix(init@mu)))

      # M-step
      mu_sq <- (init@mu)^2
      nu_sq <- (init@nu)^2

      init@tau <- init@n_obs /
        sum((Y - projSparse(
          tcrossprod(as.matrix(init@mu), as.matrix(init@nu)),
          obs_indices
        ))^2 +
          projSparse(
            tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)),
            obs_indices
          ) -
          projSparse(
            tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)),
            obs_indices
          ))
      tau_mat@x <- rep(init@tau, length(tau_mat@x))

      init@beta <- N / (sum((init@mu - init@FX)^2) + sum(init@a_sq))
      # init@beta <- ifelse(init@beta < 1e-8, 1e-8, init@beta)

      ELBO_current <- getELBO(Y, init, obs_indices)
      if (ELBO_current < ELBO_old) {
        warning("ELBO decreasing!\n")
      }

      gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
      if (verbose_loop) {
        cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
          ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
          sep = ""
        )
      }
      if (gap < tol_stage1) {
        break
      }

      ELBO_old <- ELBO_current
    }

    if (verbose_sf) {
      cat("After ", iter, " iterations Stage 1 ends!\n", sep = "")
    }

    # Tree_0 is the mean of mu vector
    init@tree_0 <- mean(init@mu)
    init@FX <- init@FX + init@tree_0
  } else {
    if (verbose_sf) {
      cat("Stage 1 skipped!\n")
    }
  }

  # Stage 2
  gb_data <- data.frame(r = NA, X)
  for (iter in 1:iter_max) {
    # E-step
    E_wsq <- (init@nu)^2 + init@b_sq
    init@a_sq <- 1 / (init@beta + as.vector(tau_mat %*% as.matrix(E_wsq)))
    init@mu <- init@a_sq *
      (init@beta * init@FX +
        as.vector(init@tau * (Y %*% as.matrix(init@nu))))

    E_zsq <- (init@mu)^2 + init@a_sq
    init@b_sq <- 1 / (1 + as.vector(crossprod(tau_mat, as.matrix(E_zsq))))
    init@nu <- init@b_sq *
      as.vector(init@tau * (crossprod(Y, as.matrix(init@mu))))

    # M-step
    mu_sq <- (init@mu)^2
    nu_sq <- (init@nu)^2

    init@tau <- init@n_obs /
      sum((Y - projSparse(
        tcrossprod(as.matrix(init@mu), as.matrix(init@nu)),
        obs_indices
      ))^2 +
        projSparse(
          tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)),
          obs_indices
        ) -
        projSparse(
          tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)),
          obs_indices
        ))
    tau_mat@x <- rep(init@tau, length(tau_mat@x))

    init@beta <- N / (sum((init@mu - init@FX)^2) + sum(init@a_sq))

    # Gradient boosting
    gb_data$r <- init@mu - init@FX
    fitted_tree <- rpart(r ~ ., data = gb_data, control = tree_parameters)
    init@FX <- init@FX + learning_rate * predict(fitted_tree, gb_data)

    # save tree list
    if (save_tree_list) {
      init@tree_list <- append(init@tree_list, list(fitted_tree))
    }

    ELBO_current <- getELBO(Y, init, obs_indices)
    if (ELBO_current < ELBO_old) {
      warning("ELBO decreasing!\n")
    }

    gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
    if (verbose_loop) {
      cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
        ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
        sep = ""
      )
    }
    if (gap < tol_stage2) {
      break
    }

    ELBO_old <- ELBO_current
  }

  if (verbose_sf) {
    cat("After ", iter, " iterations Stage 2 ends!\n", sep = "")
  }

  return(init)
}

#' Fit the single factor MFAI model with partially observed main data matrix.
#'
#' @import Matrix
#' @importFrom rpart rpart
#'
#' @param Y Main data matrix.
#' @param X A data.frame containing the auxiliary information.
#' @param init A MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @param obs_indices Indices of the observed entries in the main data matrix.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param tree_parameters A list containing the parameters for the gradient boosting part.
#' @param stage1 Logical. Whether to perform fitting algorithm stage1. The greedy algorithm needs while the backfitting algorithm does not need.
#' @param iter_max Integer. Maximum iterations allowed.
#' @param tol_stage1 Numeric. Convergence criterion in the first step.
#' @param tol_stage2 Numeric. Convergence criterion in the first step.
#' @param verbose_sf Logical. Whether to display the detailed information.
#' @param verbose_loop Logical. Whether to display the detailed information when looping.
#' @param save_tree_list Logical. Whether to save the tree list.
#'
#' @return A MFAIRSingleFactor object containing the information about the fitted single factor MFAI model.
#'
fitSFMissing <- function(Y, X, init,
                         obs_indices,
                         learning_rate, tree_parameters,
                         stage1 = TRUE,
                         iter_max = 5e+3, tol_stage1 = 0.1, tol_stage2 = 1e-8,
                         verbose_sf = TRUE, verbose_loop = TRUE,
                         save_tree_list = TRUE) {
  N <- nrow(Y)
  M <- ncol(Y)

  # Observed data in the main data matrix Y
  ProjY <- Y
  ProjY[!obs_indices] <- 0

  # Represent the precision tau as matrix
  tau_mat <- matrix(init@tau, N, M)
  tau_mat[!obs_indices] <- 0

  ELBO_old <- getELBO(Y, init)

  if (stage1) {
    # Stage 1
    for (iter in 1:iter_max) {
      # E-step
      E_wsq <- (init@nu)^2 + init@b_sq
      init@a_sq <- 1 / (init@beta + as.vector(tau_mat %*% as.matrix(E_wsq)))
      init@mu <- init@a_sq *
        (init@beta * init@FX +
          as.vector(init@tau * (ProjY %*% as.matrix(init@nu))))

      E_zsq <- (init@mu)^2 + init@a_sq
      init@b_sq <- 1 / (1 + as.vector(crossprod(tau_mat, as.matrix(E_zsq))))
      init@nu <- init@b_sq *
        as.vector(init@tau * (crossprod(ProjY, as.matrix(init@mu))))

      # M-step
      mu_sq <- (init@mu)^2
      nu_sq <- (init@nu)^2

      init@tau <- init@n_obs /
        sum(
          (Y - tcrossprod(as.matrix(init@mu), as.matrix(init@nu)))^2 +
            tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)) -
            tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)),
          na.rm = TRUE
        )
      tau_mat <- matrix(init@tau, N, M)
      tau_mat[!obs_indices] <- 0

      init@beta <- N / (sum((init@mu - init@FX)^2) + sum(init@a_sq))
      # init@beta <- ifelse(init@beta < 1e-8, 1e-8, init@beta)

      ELBO_current <- getELBO(Y, init)
      if (ELBO_current < ELBO_old) {
        warning("ELBO decreasing!\n")
      }

      gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
      if (verbose_loop) {
        cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
          ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
          sep = ""
        )
      }
      if (gap < tol_stage1) {
        break
      }

      ELBO_old <- ELBO_current
    }

    if (verbose_sf) {
      cat("After ", iter, " iterations Stage 1 ends!\n", sep = "")
    }

    # Tree_0 is the mean of mu vector
    init@tree_0 <- mean(init@mu)
    init@FX <- init@FX + init@tree_0
  } else {
    if (verbose_sf) {
      cat("Stage 1 skipped!\n")
    }
  }

  # Stage 2
  gb_data <- data.frame(r = NA, X)
  for (iter in 1:iter_max) {
    # E-step
    E_wsq <- (init@nu)^2 + init@b_sq
    init@a_sq <- 1 / (init@beta + as.vector(tau_mat %*% as.matrix(E_wsq)))
    init@mu <- init@a_sq *
      (init@beta * init@FX +
        as.vector(init@tau * (ProjY %*% as.matrix(init@nu))))

    E_zsq <- (init@mu)^2 + init@a_sq
    init@b_sq <- 1 / (1 + as.vector(crossprod(tau_mat, as.matrix(E_zsq))))
    init@nu <- init@b_sq *
      as.vector(init@tau * crossprod(ProjY, as.matrix(init@mu)))

    # M-step
    mu_sq <- (init@mu)^2
    nu_sq <- (init@nu)^2

    init@tau <- init@n_obs /
      sum(
        (Y - tcrossprod(as.matrix(init@mu), as.matrix(init@nu)))^2 +
          tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)) -
          tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)),
        na.rm = TRUE
      )
    tau_mat <- matrix(init@tau, N, M)
    tau_mat[!obs_indices] <- 0

    init@beta <- N / (sum((init@mu - init@FX)^2) + sum(init@a_sq))

    # Gradient boosting
    gb_data$r <- init@mu - init@FX
    fitted_tree <- rpart(r ~ ., data = gb_data, control = tree_parameters)
    init@FX <- init@FX + learning_rate * predict(fitted_tree, gb_data)

    # save tree list
    if (save_tree_list) {
      init@tree_list <- append(init@tree_list, list(fitted_tree))
    }

    ELBO_current <- getELBO(Y, init)
    if (ELBO_current < ELBO_old) {
      warning("ELBO decreasing!\n")
    }

    gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
    if (verbose_loop) {
      cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
        ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
        sep = ""
      )
    }
    if (gap < tol_stage2) {
      break
    }

    ELBO_old <- ELBO_current
  }

  if (verbose_sf) {
    cat("After ", iter, " iterations Stage 2 ends!\n", sep = "")
  }

  return(init)
}

#' Fit the single factor MFAI model with fully observed main data matrix.
#'
#' @importFrom rpart rpart
#'
#' @param Y Main data matrix.
#' @param X A data.frame containing the auxiliary information.
#' @param init A MFAIRSingleFactor object containing the initial parameters for the single factor MAFI model.
#' @param learning_rate Numeric. Parameter for the gradient boosting part.
#' @param tree_parameters A list containing the parameters for the gradient boosting part.
#' @param stage1 Logical. Whether to perform fitting algorithm stage1. The greedy algorithm needs while the backfitting algorithm does not need.
#' @param iter_max Integer. Maximum iterations allowed.
#' @param tol_stage1 Numeric. Convergence criterion in the first step.
#' @param tol_stage2 Numeric. Convergence criterion in the first step.
#' @param verbose_sf Logical. Whether to display the detailed information.
#' @param verbose_loop Logical. Whether to display the detailed information when looping.
#' @param save_tree_list Logical. Whether to save the tree list.
#'
#' @return A MFAIRSingleFactor object containing the information about the fitted single factor MFAI model.
#'
fitSFFully <- function(Y, X, init,
                       learning_rate, tree_parameters,
                       stage1 = TRUE,
                       iter_max = 5e+3, tol_stage1 = 0.1, tol_stage2 = 1e-8,
                       verbose_sf = TRUE, verbose_loop = TRUE,
                       save_tree_list = TRUE) {
  N <- nrow(Y)
  M <- ncol(Y)

  ELBO_old <- getELBO(Y, init)

  if (stage1) {
    # Stage 1
    for (iter in 1:iter_max) {
      # E-step
      init@a_sq <- 1 / (init@beta + init@tau * (sum((init@nu)^2) + M * init@b_sq))
      init@mu <- init@a_sq *
        (init@beta * init@FX +
          init@tau * as.vector(Y %*% as.matrix(init@nu)))
      init@b_sq <- 1 / (1 + init@tau * (sum((init@mu)^2) + N * init@a_sq))
      init@nu <- init@b_sq *
        init@tau * as.vector(crossprod(Y, as.matrix(init@mu)))

      # M-step
      mu_sq <- (init@mu)^2
      nu_sq <- (init@nu)^2
      init@tau <- N * M /
        sum((Y - tcrossprod(as.matrix(init@mu), as.matrix(init@nu)))^2 +
          tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)) -
          tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)))
      init@beta <- N / (sum((init@mu - init@FX)^2) + N * init@a_sq)
      # init@beta <- ifelse(init@beta < 1e-8, 1e-8, init@beta)

      ELBO_current <- getELBO(Y, init)
      if (ELBO_current < ELBO_old) {
        warning("ELBO decreasing!\n")
      }

      gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
      if (verbose_loop) {
        cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
          ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
          sep = ""
        )
      }
      if (gap < tol_stage1) {
        break
      }

      ELBO_old <- ELBO_current
    }

    if (verbose_sf) {
      cat("After ", iter, " iterations Stage 1 ends!\n", sep = "")
    }

    # Tree_0 is the mean of mu vector
    init@tree_0 <- mean(init@mu)
    init@FX <- init@FX + init@tree_0
  } else {
    if (verbose_sf) {
      cat("Stage 1 skipped!\n")
    }
  }

  # Stage 2
  gb_data <- data.frame(r = NA, X)
  for (iter in 1:iter_max) {
    # E-step
    init@a_sq <- 1 / (init@beta + init@tau * (sum((init@nu)^2) + M * init@b_sq))
    init@mu <- init@a_sq *
      (init@beta * init@FX +
        init@tau * as.vector(Y %*% as.matrix(init@nu)))
    init@b_sq <- 1 / (1 + init@tau * (sum((init@mu)^2) + N * init@a_sq))
    init@nu <- init@b_sq *
      init@tau * as.vector(crossprod(Y, as.matrix(init@mu)))

    # M-step
    mu_sq <- (init@mu)^2
    nu_sq <- (init@nu)^2
    init@tau <- N * M /
      sum((Y - tcrossprod(as.matrix(init@mu), as.matrix(init@nu)))^2 +
        tcrossprod(as.matrix(mu_sq + init@a_sq), as.matrix(nu_sq + init@b_sq)) -
        tcrossprod(as.matrix(mu_sq), as.matrix(nu_sq)))
    init@beta <- N / (sum((init@mu - init@FX)^2) + N * init@a_sq)
    # init@beta <- ifelse(init@beta < 1e-8, 1e-8, init@beta)

    # Gradient boosting
    gb_data$r <- init@mu - init@FX
    fitted_tree <- rpart(r ~ ., data = gb_data, control = tree_parameters)
    init@FX <- init@FX + learning_rate * predict(fitted_tree, gb_data)

    # save tree list
    if (save_tree_list) {
      init@tree_list <- append(init@tree_list, list(fitted_tree))
    }

    ELBO_current <- getELBO(Y, init)
    if (ELBO_current < ELBO_old) {
      warning("ELBO decreasing!\n")
    }

    gap <- abs((ELBO_current - ELBO_old) / ELBO_old)
    if (verbose_loop) {
      cat("Iteration: ", iter, ", ELBO: ", ELBO_current, ", tau: ", init@tau,
        ", beta: ", init@beta, ", relative difference: ", gap, ".\n",
        sep = ""
      )
    }
    if (gap < tol_stage2) {
      break
    }

    ELBO_old <- ELBO_current
  }

  if (verbose_sf) {
    cat("After ", iter, " iterations Stage 2 ends!\n", sep = "")
  }

  return(init)
}
