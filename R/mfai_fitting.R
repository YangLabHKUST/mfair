pca_boosting_single_factor_twostage <- function(Y, coordinates, init = NULL, learning_rate = 0.1, iter_max = 5000, tol_stage1 = 0.1, tol_stage2 = 1e-6, minsplit = 10, minbucket = round(minsplit / 3), maxdepth = 2, verbose = TRUE, save_models = TRUE) {
  N <- dim(Y)[1]
  M <- dim(Y)[2]
  
  # initialize
  if (is.null(init)) {
    init <- init_generalized(Y, N, M)
  }
  mu <- init$mu
  a2 <- init$a2
  nu <- init$nu
  b2 <- init$b2
  sigma2_new <- sigma2 <- init$sigma2
  beta_new <- beta <- init$beta
  f <- init$f
  tree_list <- init$tree_list
  rm(init)
  
  ELBO_old <- 0
  ELBOs <- NULL
  
  beta_list <- NULL
  sigma2_list <- NULL
  
  # stage 1
  for (iter in 1:iter_max) {
    # E-step
    b2 <- 1 / (1 + (sum(mu^2) + N * a2) / sigma2)
    nu <- b2 * as.vector(t(Y) %*% as.matrix(mu)) / sigma2
    a2 <- 1 / (1 / beta + (sum(nu^2) + M * b2) / sigma2)
    mu <- a2 * (f / beta + as.vector(Y %*% as.matrix(nu)) / sigma2)
    
    # Normalization
    sd_nu <- sd(as.vector(nu))
    if (sd_nu > 1) {
      sd_nu_2 <- sd_nu^2
      mu <- mu * sd_nu
      a2 <- a2 * sd_nu_2
      nu <- nu / sd_nu
      b2 <- b2 / sd_nu_2
    }
    
    # M-step
    mu2 <- mu^2
    nu2 <- nu^2
    sigma2_new <- sum((Y - as.matrix(mu) %*% t(nu))^2 + as.matrix(mu2 + a2) %*% t(nu2 + b2) - as.matrix(mu2) %*% t(nu2)) / (N * M)
    sigma2_new <- ifelse(sigma2_new < 1e-08, 1e-08, sigma2_new)
    sigma2_list <- c(sigma2_list, sigma2_new)
    beta_new <- (sum(mu^2) + N * a2 - 2 * sum(mu * f) + sum(f^2)) / N
    beta_new <- ifelse(beta_new < 1e-8, 1e-8, beta_new)
    beta_list <- c(beta_list, beta_new)
    
    sigma2 <- sigma2_new
    beta <- beta_new
    
    ELBO_current <- elbo_generalized(Y, mu, a2, nu, b2, sigma2, beta, f)
    
    gap <- abs(ELBO_current - ELBO_old) / abs(ELBO_old)
    if (gap < tol_stage1) break
    
    if (iter > 1 & ELBO_current < ELBO_old) message("ELBO decreasing")
    
    ELBO_old <- ELBO_current
    
    ELBOs <- c(ELBOs, ELBO_current)
    
    if (verbose) {
      cat(
        "Iteration: ", iter, " ELBO: ", ELBO_current, " sigma2: ", sigma2,
        " beta: ", beta, "diff", gap, "\n"
      )
    }
  }
  
  cat("After ", iter, " iteration, stage 1 ends!\n")
  
  # stage 2
  
  gbrt_data <- data.frame(r = mu, coordinates)
  for (iter in 1:iter_max) {
    # E-step
    b2 <- 1 / (1 + (sum(mu^2) + N * a2) / sigma2)
    nu <- b2 * as.vector(t(Y) %*% as.matrix(mu)) / sigma2
    a2 <- 1 / (1 / beta + (sum(nu^2) + M * b2) / sigma2)
    mu <- a2 * (f / beta + as.vector(Y %*% as.matrix(nu)) / sigma2)
    
    # Normalization
    sd_nu <- sd(as.vector(nu))
    if (sd_nu > 1) {
      sd_nu_2 <- sd_nu^2
      mu <- mu * sd_nu
      a2 <- a2 * sd_nu_2
      nu <- nu / sd_nu
      b2 <- b2 / sd_nu_2
    }
    
    # M-step
    mu2 <- mu^2
    nu2 <- nu^2
    sigma2_new <- sum((Y - as.matrix(mu) %*% t(nu))^2 + as.matrix(mu2 + a2) %*% t(nu2 + b2) - as.matrix(mu2) %*% t(nu2)) / (N * M)
    sigma2_new <- ifelse(sigma2_new < 1e-08, 1e-08, sigma2_new)
    sigma2_list <- c(sigma2_list, sigma2_new)
    beta_new <- (sum(mu^2) + N * a2 - 2 * sum(mu * f) + sum(f^2)) / N
    beta_new <- ifelse(beta_new < 1e-8, 1e-8, beta_new)
    beta_list <- c(beta_list, beta_new)
    
    # gradient boosting regression tree
    gbrt_data$r <- mu - f
    ct <- rpart.control(minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth)
    rt <- rpart(r ~ ., data = gbrt_data, control = ct)
    f <- f + learning_rate * predict(rt, gbrt_data)
    
    # save tree list
    if (save_models) {
      tree_list <- c(tree_list, list(rt))
    }
    
    # elbo[iter] <- elbo_generalized(Y, mu, a2, nu, b2, sigma2_new, beta_new, f)
    
    # gap <- abs(sigma2_new - sigma2) / abs(sigma2_new) + abs(beta_new - beta) / abs(beta_new)
    # if (gap < tol) {
    #   break
    # }
    
    sigma2 <- sigma2_new
    beta <- beta_new
    
    ELBO_current <- elbo_generalized(Y, mu, a2, nu, b2, sigma2, beta, f)
    
    gap <- abs(ELBO_current - ELBO_old) / abs(ELBO_old)
    if (gap < tol_stage2) break
    
    if (iter > 1 & ELBO_current < ELBO_old) message("ELBO decreasing")
    
    ELBO_old <- ELBO_current
    
    ELBOs <- c(ELBOs, ELBO_current)
    
    if (verbose) {
      cat(
        "Iteration: ", iter, " ELBO: ", ELBO_current, " sigma2: ", sigma2,
        " beta: ", beta, "diff", gap, "\n"
      )
    }
  }
  cat("After ", iter, " iteration, stage 2 ends!\n")
  
  return(list(mu = mu, a2 = a2, nu = nu, b2 = b2, sigma2 = sigma2, beta = beta, f = f, ELBOs = ELBOs, beta_list = beta_list, sigma2_list = sigma2_list, tree_list = tree_list))
}

init_generalized <- function(Y, N, M) {
  mu <- matrix(rnorm(N), N, 1)
  a2 <- abs(rnorm(1)) + 1
  nu <- matrix(0, M, 1)
  b2 <- 1
  
  sigma2 <- var(as.vector(Y)) / 2
  beta <- sigma2
  
  f <- rep(0.0, N)
  
  return(list(mu = mu, a2 = a2, nu = nu, b2 = b2, sigma2 = sigma2, beta = beta, f = f))
}

elbo_generalized <- function(Y, mu, a2, nu, b2, sigma2, beta, f) {
  n <- dim(Y)[1]
  m <- dim(Y)[2]
  
  mu2 <- mu^2
  nu2 <- nu^2
  
  elbo1 <- -m * n * log(2 * pi * sigma2) / 2
  elbo2 <- -sum((Y - as.matrix(mu) %*% t(nu))^2 + as.matrix(mu2 + a2) %*% t(nu2 + b2) - as.matrix(mu2) %*% t(nu2)) / (2 * sigma2)
  elbo3 <- -n * log(2 * pi * beta) / 2 - (sum(mu2) + n * a2 - 2 * sum(mu * f) + sum(f^2)) / 2 / beta
  elbo4 <- -m * log(2 * pi) / 2 - (sum(nu2) + m * b2) / 2
  elbo5 <- n * log(2 * pi * a2) / 2 + n / 2
  elbo6 <- m * log(2 * pi * b2) / 2 + m / 2
  
  elbo <- elbo1 + elbo2 + elbo3 + elbo4 + elbo5 + elbo6
  return(elbo)
}