#' Cross-validation for softImpute.
#'
#' @import softImpute
#'
#' @param Y A matrix. The main data matrix of N samples and M features.
#' @param rank_max An integer. The maximum rank allowed.
#' @param lambda_range A vector containing the minimal and maximal value of the parameter lambda.
#' @param nfold An integer. The total number of validation sets.
#' @param para_length An integer. The total number of parameter lambda.
#'
#' @return A list containing the cross-validation results.
#' @export
#'
softImputeCrossVal <- function(Y, rank_max = NULL, lambda_range = NULL,
                               nfold = 10, para_length = 100) {
  N <- nrow(Y)
  M <- ncol(Y)
  # Report settings
  message("Info: Matrix dimension: ", N, " * ", M)
  message("Info: Number of cv folds: ", nfold)

  if (is.null(rank_max)) {
    rank_max <- min(N, M) - 1
  }

  if (is.null(lambda_range)) {
    hi_fit <- softImpute(Y, rank.max = rank_max)
    lambda_min <- tail(hi_fit$d, 1) / 100
    lambda_max <- mean((hi_fit$d)[1:2])
  } else {
    lambda_min <- lambda_range[1]
    lambda_max <- lambda_range[2]
  }
  lambda_list <- exp(seq(log(lambda_min),
    log(lambda_max),
    length.out = para_length
  ))

  # Test RMSE
  test_err <- matrix(0, nrow = nfold, ncol = para_length)
  rownames(test_err) <- paste0("fold_", 1:nfold)
  colnames(test_err) <- paste0("lambda_", 1:para_length)

  # Not NA idx
  idx_all <- which(!is.na(Y))
  n <- length(idx_all)
  # nfold
  idx_group <- ceiling(sample(1:n) / n * nfold)

  cat("Start cross validation process...... total", nfold, "validation sets \n")

  for (i in 1:nfold) {
    cat("Validation set", i, "... \n")

    Y_train <- Y_test <- Y

    Y_train[idx_all][idx_group == i] <- NA
    Y_test[idx_all][idx_group != i] <- NA

    for (j in seq(from = para_length, to = 1, by = -1)) {
      if (j == para_length) {
        si_fit <- softImpute(Y_train,
          rank.max = rank_max,
          lambda = lambda_list[j]
        )
      } else {
        # Warm start
        si_fit <- softImpute(Y_train,
          rank.max = rank_max,
          lambda = lambda_list[j], warm.start = si_fit
        )
      }

      test_err[i, j] <- sqrt(mean((Y_test - complete(Y_train, si_fit))^2, na.rm = TRUE))
    }
  }

  cv_sd <- sqrt(apply(test_err, MARGIN = 2, var) / (nfold - 1))
  cv_mean <- colMeans(test_err)
  idx_min <- which.min(cv_mean)
  lambda_best <- lambda_list[idx_min]

  si_cv_results <- list(
    cv_mean = cv_mean, cv_sd = cv_sd,
    lambda_list = lambda_list, lambda_best = lambda_best,
    test_err = test_err
  )

  return(si_cv_results)
}

#' Sparse version of cross-validation for softImpute.
#'
#' @import Matrix
#' @import softImpute
#'
#' @param Y A Matrix::dgCMatrix. The main data matrix of N samples and M features.
#' @param rank_max An integer. The maximum rank allowed.
#' @param lambda_range A vector containing the minimal and maximal value of the parameter lambda.
#' @param nfold An integer. The total number of validation sets.
#' @param para_length An integer. The total number of parameter lambda.
#'
#' @return A list containing the cross-validation results.
#' @export
#'
softImputeCrossValSparse <- function(Y, rank_max = NULL, lambda_range = NULL,
                                     nfold = 10, para_length = 100) {
  N <- nrow(Y)
  M <- ncol(Y)
  # Report settings
  message("Info: Matrix dimension: ", N, " * ", M)
  message("Info: Number of cv folds: ", nfold)

  if (is.null(rank_max)) {
    rank_max <- min(N, M) - 1
  }

  if (is.null(lambda_range)) {
    hi_fit <- softImpute(Y, rank.max = rank_max)
    lambda_min <- tail(hi_fit$d, 1) / 100
    lambda_max <- mean((hi_fit$d)[1:2])
  } else {
    lambda_min <- lambda_range[1]
    lambda_max <- lambda_range[2]
  }
  lambda_list <- exp(seq(log(lambda_min),
    log(lambda_max),
    length.out = para_length
  ))

  # Test RMSE
  test_err <- matrix(0, nrow = nfold, ncol = para_length)
  rownames(test_err) <- paste0("fold_", 1:nfold)
  colnames(test_err) <- paste0("lambda_", 1:para_length)

  # Not NA idx
  idx_all <- as.matrix(summary(Y)[, c(1, 2)])
  n <- nrow(idx_all)
  # nfold
  idx_group <- ceiling(sample(1:n) / n * nfold)

  cat("Start cross validation process...... total", nfold, "validation sets \n")

  for (i in 1:nfold) {
    cat("Validation set", i, "... \n")

    train_idx <- which(idx_group != i)
    train_mean <- mean(Y@x[train_idx]) # Mean value of the training set

    Y_train <- Incomplete(
      i = idx_all[train_idx, 1],
      j = idx_all[train_idx, 2],
      x = Y@x[train_idx] - train_mean
    )

    for (j in seq(from = para_length, to = 1, by = -1)) {
      if (j == para_length) {
        si_fit <- softImpute(Y_train,
          rank.max = rank_max,
          lambda = lambda_list[j]
        )
      } else {
        # Warm start
        si_fit <- softImpute(Y_train,
          rank.max = rank_max,
          lambda = lambda_list[j], warm.start = si_fit
        )
      }

      # The prediction corresponding to the test set
      Y_hat <- impute(
        si_fit,
        i = idx_all[-train_idx, 1], j = idx_all[-train_idx, 2]
      ) + train_mean
      test_err[i, j] <- sqrt(mean((Y@x[-train_idx] - Y_hat)^2))
    }
  }

  cv_sd <- sqrt(apply(test_err, MARGIN = 2, var) / (nfold - 1))
  cv_mean <- colMeans(test_err)
  idx_min <- which.min(cv_mean)
  lambda_best <- lambda_list[idx_min]

  si_cv_results <- list(
    cv_mean = cv_mean, cv_sd = cv_sd,
    lambda_list = lambda_list, lambda_best = lambda_best,
    test_err = test_err
  )

  return(si_cv_results)
}
