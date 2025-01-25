
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mfair: Matrix Factorization with Auxiliary Information in R

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/609644044.svg)](https://zenodo.org/badge/latestdoi/609644044)
![GitHub repo
size](https://img.shields.io/github/repo-size/YangLabHKUST/mfair)
![GitHub commit
activity](https://img.shields.io/github/commit-activity/t/YangLabHKUST/mfair)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2FYangLabHKUST%2Fmfair&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=hits&edge_flat=false)](https://hits.seeyoufarm.com)
![GitHub Repo
stars](https://img.shields.io/github/stars/YangLabHKUST/mfair) ![GitHub
forks](https://img.shields.io/github/forks/YangLabHKUST/mfair)
[![R-CMD-check](https://github.com/YangLabHKUST/mfair/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YangLabHKUST/mfair/actions/workflows/R-CMD-check.yaml)
<!-- [![Codecov test coverage](https://codecov.io/gh/YangLabHKUST/mfair/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YangLabHKUST/mfair?branch=main) -->

<!-- badges: end -->

The R package `mfair` implements the methods based on the paper [**MFAI:
A scalable Bayesian matrix factorization approach to leveraging
auxiliary information**](https://doi.org/10.48550/arXiv.2303.02566).
MFAI integrates gradient boosted trees in the probabilistic matrix
factorization framework to leverage auxiliary information effectively
and adaptively.

## Installation

For a quick start, you can install the development version of `mfair`
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YangLabHKUST/mfair")
```

For more illustration and examples, you can alternatively use:

``` r
# install.packages("devtools")
devtools::install_github("YangLabHKUST/mfair", build_vignettes = TRUE)
```

to build vignettes simultaneously. Please note that it can take a few
more minutes.

## Examples

- This is a basic example which shows you how to solve a common problem:

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

# Generate the factor matrix Z (= F(X) + noise)
sig1_sq <- var(FX1) * (1 / PVE_Z - 1)
Z1 <- FX1 + rnorm(n = N, mean = 0, sd = sqrt(sig1_sq))
sig2_sq <- var(FX2) * (1 / PVE_Z - 1)
Z2 <- FX2 + rnorm(n = N, mean = 0, sd = sqrt(sig2_sq))
Z <- cbind(Z1, Z2)

# Generate the loading matrix W
W <- matrix(rnorm(M * K_true), nrow = M, ncol = K_true)

# Generate the main data matrix Y_obs (= Y + noise)
Y <- Z %*% t(W)
Y_var <- var(as.vector(Y))
epsilon_sq <- Y_var * (1 / PVE_Y - 1)
Y_obs <- Y + matrix(
  rnorm(N * M,
    mean = 0,
    sd = sqrt(epsilon_sq)
  ),
  nrow = N, ncol = M
)

# Create MFAIR object
mfairObject <- createMFAIR(Y_obs, as.data.frame(X), K_max = K_true)
#> The main data matrix Y is completely observed!
#> The main data matrix Y has been centered with mean = 0.147726471347656!

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))
#> Set K_max = 2!
#> Initialize the parameters of Factor 1......
#> After 2 iterations Stage 1 ends!
#> After 77 iterations Stage 2 ends!
#> Factor 1 retained!
#> Initialize the parameters of Factor 2......
#> After 2 iterations Stage 1 ends!
#> After 76 iterations Stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject)
#> The main data matrix Y has no missing entries!

# Root-mean-square-error
sqrt(mean((Y_obs - Y_hat)^2))
#> [1] 12.23344

# Predicted/true matrix variance ratio
var(as.vector(Y_hat)) / var(as.vector(Y_obs))
#> [1] 0.4714952

# Prediction/noise variance ratio
var(as.vector(Y_hat)) / var(as.vector(Y_obs - Y_hat))
#> [1] 0.9871629
```

- `mfair` can also handle the matrix with missing entries:

``` r
# Split the data into the training set and test set
n_all <- N * M
training_ratio <- 0.5
train_set <- sample(1:n_all, n_all * training_ratio, replace = FALSE)
Y_train <- Y_test <- Y_obs
Y_train[-train_set] <- NA
Y_test[train_set] <- NA

# Create MFAIR object
mfairObject <- createMFAIR(Y_train, as.data.frame(X), K_max = K_true)
#> The main data matrix Y is partially observed!
#> The main data matrix Y has been centered with mean = 0.187847085351627!

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))
#> Set K_max = 2!
#> Initialize the parameters of Factor 1......
#> After 2 iterations Stage 1 ends!
#> After 96 iterations Stage 2 ends!
#> Factor 1 retained!
#> Initialize the parameters of Factor 2......
#> After 2 iterations Stage 1 ends!
#> After 79 iterations Stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject)

# Root-mean-square-error
sqrt(mean((Y_test - Y_hat)^2, na.rm = TRUE))
#> [1] 13.08594

# Predicted/true matrix variance ratio
var(as.vector(Y_hat), na.rm = TRUE) / var(as.vector(Y_obs), na.rm = TRUE)
#> [1] 0.4080135

# Prediction/noise variance ratio
var(as.vector(Y_hat), na.rm = TRUE) / var(as.vector(Y_obs - Y_hat), na.rm = TRUE)
#> [1] 0.7990528
```

- Empirically, the backfitting algorithm can further improve the
  performance:

``` r
# Refine the MFAI model with the backfitting algorithm
mfairObject <- fitBack(mfairObject,
  verbose_bf_inner = FALSE,
  sf_para = list(verbose_sf = FALSE, verbose_loop = FALSE)
)
#> Iteration: 1, relative difference of model parameters: 0.2677281.
#> Iteration: 2, relative difference of model parameters: 0.06144878.
#> Iteration: 3, relative difference of model parameters: 0.01419201.
#> Iteration: 4, relative difference of model parameters: 0.007719868.

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject)

# Root-mean-square-error
sqrt(mean((Y_test - Y_hat)^2, na.rm = TRUE))
#> [1] 13.03081

# Predicted/true matrix variance ratio
var(as.vector(Y_hat), na.rm = TRUE) / var(as.vector(Y_obs), na.rm = TRUE)
#> [1] 0.4255564

# Prediction/noise variance ratio
var(as.vector(Y_hat), na.rm = TRUE) / var(as.vector(Y_obs - Y_hat), na.rm = TRUE)
#> [1] 0.8398119
```

- Explore the [vignette illustrating the enrichment of the movie genre
  information](https://yanglabhkust.github.io/mfair/articles/ml100k.html):

``` r
vignette("ml100k")
```

- Explore the [vignette illustrating the spatial and temporal dynamics
  of gene regulation among brain
  tissues](https://yanglabhkust.github.io/mfair/articles/neocortex.html):

``` r
vignette("neocortex")
```

- For more documentation and examples, please visit our package
  [website](https://yanglabhkust.github.io/mfair/).

## Citing our work

If you find the `mfair` package or any of the source code in this
repository useful for your work, please cite:

> Wang, Z., Zhang, F., Zheng, C., Hu, X., Cai, M., & Yang, C. (2024).
> MFAI: A Scalable Bayesian Matrix Factorization Approach to Leveraging
> Auxiliary Information. *Journal of Computational and Graphical
> Statistics*, 33(4), 1339–1349.
> <https://doi.org/10.1080/10618600.2024.2319160>

## Development

The R package `mfair` is developed by [Zhiwei
Wang](https://sites.google.com/view/statwangz).

## Contact

Please feel free to contact [Zhiwei
Wang](mailto:zhiwei.wang@connect.ust.hk), [Prof. Mingxuan
Cai](mailto:mingxcai@cityu.edu.hk), or [Prof. Can
Yang](mailto:macyang@ust.hk) if any inquiries.
