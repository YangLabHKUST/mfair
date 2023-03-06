
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mfair: Matrix Factorization with Auxiliary Information in R

<!-- badges: start -->
<!-- badges: end -->

Methods for matrix factorization to leverage auxiliary information based
on the paper MFAI. The name of the package `mfair` comes from **Matrix
Factorization with Auxiliary Information in R**.

## Installation

You can install the development version of `mfair` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YangLabHKUST/mfair")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
set.seed(20230306)
library(mfair)

# Simulate data
# Set the data dimension and rank
N <- 100
M <- 100
K_true <- 2L

# Set the proportion of variance explained (PVE)
PVE_Z <- 0.8
PVE_Y <- 0.5

# Generate auxiliary information X
X1 <- runif(N, min = -10, max = 10)
X2 <- runif(N, min = -10, max = 10)
X <- cbind(X1, X2)

# F(X)
FX1 <- X1 / 2 - X2
FX2 <- (X1^2 - X2^2 + 2 * X1 * X2) / 10
FX <- cbind(FX1, FX2)

# Generate loadings
sig1 <- var(FX1) * (1 / PVE_Z - 1)
Z1 <- FX1 + rnorm(n = N, mean = 0, sd = sqrt(sig1))
sig2 <- var(FX2) * (1 / PVE_Z - 1)
Z2 <- FX2 + rnorm(n = N, mean = 0, sd = sqrt(sig2))
Z <- cbind(Z1, Z2)

# Generate factors
W <- matrix(rnorm(M * K_true), nrow = M, ncol = K_true)

# Generate the main data matrix Y
Y <- Z %*% t(W)
Y_var <- var(as.vector(Y))
epsilon <- sqrt(Y_var * (1 / PVE_Y - 1))
Y_obs <- Y + matrix(rnorm(N * M, mean = 0, sd = epsilon),
  nrow = N, ncol = M
)
Y_mean <- mean(Y_obs)

# Create MFAIR object
mfairObject <- createMFAIR(Y_obs - Y_mean, X, K_max = K_true)

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject, verbose_loop = FALSE)
#> After 1 iterations stage 1 ends!
#> After 43 iterations stage 2 ends!
#> Factor 1 retained!
#> After 1 iterations stage 1 ends!
#> After 40 iterations stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject) + Y_mean
#> The main data matrix Y has no missing entries!

# Root-mean-square-error
rmse <- sqrt(mean((Y_obs - Y_hat)^2))
rmse
#> [1] 12.22526
```

`mfair` can also handle the matrix with missing entries:

``` r
# Split the data into the training set and test set
n_all <- N * M
training_ratio <- 0.5
train_set <- sample(1:n_all, n_all * training_ratio, replace = FALSE)
Y_train <- Y_test <- Y_obs
Y_train[-train_set] <- NA
Y_test[train_set] <- NA
train_mean <- mean(Y_train, na.rm = TRUE)

# Create MFAIR object
mfairObject <- createMFAIR(Y_train - train_mean, X, K_max = K_true)

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject, verbose_loop = FALSE)
#> After 1 iterations stage 1 ends!
#> After 57 iterations stage 2 ends!
#> Factor 1 retained!
#> After 1 iterations stage 1 ends!
#> After 59 iterations stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject) + train_mean

# Root-mean-square-error
rmse <- sqrt(mean((Y_test - Y_hat)^2, na.rm = TRUE))
rmse
#> [1] 12.8915
```

## Development

The package is developed by Zhiwei Wang (<zhiwei.wang@connect.ust.hk>).

## Contact Information

Please feel free to contact Zhiwei Wang (<zhiwei.wang@connect.ust.hk>),
Prof. Mingxuan Cai (<mingxcai@cityu.edu.hk>), or Prof. Can Yang
(<macyang@ust.hk>) with any inquiries.
