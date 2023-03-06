
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
library(MASS)

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
beta1 <- var(FX1) * (1 / PVE_Z - 1)
Z1 <- mvrnorm(n = 1, mu = FX1, Sigma = beta1 * diag(N))
beta2 <- var(FX2) * (1 / PVE_Z - 1)
Z2 <- mvrnorm(n = 1, mu = FX2, Sigma = beta2 * diag(N))
Z <- cbind(Z1, Z2)

# Generate factors
W <- matrix(rnorm(M * K_true), nrow = M, ncol = K_true)

# Generate the main data matrix Y
Y <- Z %*% t(W)
Y_var <- var(as.vector(Y))
epsilon <- sqrt(Y_var * (1 / PVE_Y - 1))
Y_obs <- Y + matrix(rnorm(N * M, sd = epsilon), nrow = N, ncol = M)
Y_mean <- mean(Y_obs)

# Create MFAIR object
mfairObject <- createMFAIR(Y_obs - Y_mean, X, K_max = K_true)

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject)
#> Iteration: 1, ELBO: -43170.38, tau: 0.00313005, beta: 0.2874784, relative difference: 0.0423111
#> After 1 iterations stage 1 ends!
#> Iteration: 1, ELBO: -41826.83, tau: 0.004514965, beta: 0.08873725, relative difference: 0.07211624
#> Iteration: 2, ELBO: -41692.51, tau: 0.004642941, beta: 0.06979789, relative difference: 0.003211431
#> Iteration: 3, ELBO: -41621.75, tau: 0.004650445, beta: 0.06025599, relative difference: 0.001697063
#> Iteration: 4, ELBO: -41576.37, tau: 0.004652064, beta: 0.0533675, relative difference: 0.001090411
#> Iteration: 5, ELBO: -41545.74, tau: 0.004652957, beta: 0.04924563, relative difference: 0.00073673
#> Iteration: 6, ELBO: -41522.64, tau: 0.004653533, beta: 0.04552413, relative difference: 0.0005560182
#> Iteration: 7, ELBO: -41505.79, tau: 0.004653987, beta: 0.04328231, relative difference: 0.0004057907
#> Iteration: 8, ELBO: -41491.73, tau: 0.004654303, beta: 0.04090716, relative difference: 0.0003385632
#> Iteration: 9, ELBO: -41480.55, tau: 0.004654588, beta: 0.03962692, relative difference: 0.0002696195
#> Iteration: 10, ELBO: -41471.88, tau: 0.0046548, beta: 0.03849449, relative difference: 0.000208998
#> Iteration: 11, ELBO: -41463.94, tau: 0.004654961, beta: 0.0371369, relative difference: 0.0001913086
#> Iteration: 12, ELBO: -41457.38, tau: 0.004655129, beta: 0.03652199, relative difference: 0.000158377
#> Iteration: 13, ELBO: -41452.16, tau: 0.004655251, beta: 0.03589739, relative difference: 0.0001259126
#> Iteration: 14, ELBO: -41447.05, tau: 0.004655346, beta: 0.03507123, relative difference: 0.0001231061
#> Iteration: 15, ELBO: -41442.77, tau: 0.004655457, beta: 0.03478304, relative difference: 0.0001033695
#> Iteration: 16, ELBO: -41439.4, tau: 0.004655533, beta: 0.03440735, relative difference: 8.127395e-05
#> Iteration: 17, ELBO: -41436.07, tau: 0.004655591, beta: 0.03380002, relative difference: 8.048962e-05
#> Iteration: 18, ELBO: -41432.4, tau: 0.004655665, beta: 0.03350743, relative difference: 8.850561e-05
#> Iteration: 19, ELBO: -41429.53, tau: 0.004655744, beta: 0.03370584, relative difference: 6.913295e-05
#> Iteration: 20, ELBO: -41427.49, tau: 0.004655779, beta: 0.03360688, relative difference: 4.928165e-05
#> Iteration: 21, ELBO: -41425.18, tau: 0.004655803, beta: 0.03316877, relative difference: 5.571946e-05
#> Iteration: 22, ELBO: -41422.62, tau: 0.004655858, beta: 0.03307876, relative difference: 6.19407e-05
#> Iteration: 23, ELBO: -41420.71, tau: 0.004655911, beta: 0.03330846, relative difference: 4.598959e-05
#> Iteration: 24, ELBO: -41418.58, tau: 0.004655928, beta: 0.03323611, relative difference: 5.146727e-05
#> Iteration: 25, ELBO: -41417.36, tau: 0.00465597, beta: 0.03342904, relative difference: 2.954727e-05
#> Iteration: 26, ELBO: -41415.86, tau: 0.004655969, beta: 0.03314601, relative difference: 3.608314e-05
#> Iteration: 27, ELBO: -41414.2, tau: 0.004656005, beta: 0.03311532, relative difference: 4.021069e-05
#> Iteration: 28, ELBO: -41412.92, tau: 0.004656038, beta: 0.03328036, relative difference: 3.07583e-05
#> Iteration: 29, ELBO: -41411.43, tau: 0.004656048, beta: 0.03326005, relative difference: 3.609213e-05
#> Iteration: 30, ELBO: -41408.24, tau: 0.004656077, beta: 0.03343867, relative difference: 7.703718e-05
#> Iteration: 31, ELBO: -41407.1, tau: 0.004656168, beta: 0.03485079, relative difference: 2.761558e-05
#> Iteration: 32, ELBO: -41404.27, tau: 0.004656116, beta: 0.0349643, relative difference: 6.829299e-05
#> Iteration: 33, ELBO: -41403.12, tau: 0.004656197, beta: 0.03632297, relative difference: 2.777162e-05
#> Iteration: 34, ELBO: -41402.25, tau: 0.004656151, beta: 0.03659401, relative difference: 2.085768e-05
#> Iteration: 35, ELBO: -41399.7, tau: 0.00465615, beta: 0.0367209, relative difference: 6.171548e-05
#> Iteration: 36, ELBO: -41398.89, tau: 0.004656235, beta: 0.03813469, relative difference: 1.951381e-05
#> Iteration: 37, ELBO: -41396.67, tau: 0.004656176, beta: 0.03834549, relative difference: 5.366795e-05
#> Iteration: 38, ELBO: -41395.9, tau: 0.004656242, beta: 0.03965539, relative difference: 1.860799e-05
#> Iteration: 39, ELBO: -41395.27, tau: 0.004656188, beta: 0.03994257, relative difference: 1.512592e-05
#> Iteration: 40, ELBO: -41394.46, tau: 0.004656182, beta: 0.04012693, relative difference: 1.97483e-05
#> Iteration: 41, ELBO: -41392.49, tau: 0.004656193, beta: 0.0404769, relative difference: 4.739802e-05
#> Iteration: 42, ELBO: -41391.96, tau: 0.004656257, beta: 0.0417793, relative difference: 1.278454e-05
#> Iteration: 43, ELBO: -41391.52, tau: 0.004656197, beta: 0.04200306, relative difference: 1.065772e-05
#> Iteration: 44, ELBO: -41391.11, tau: 0.004656187, beta: 0.04214314, relative difference: 9.97081e-06
#> After 44 iterations stage 2 ends!
#> Factor 1 retained!
#> Iteration: 1, ELBO: -40975, tau: 0.004752181, beta: 2.082172, relative difference: 0.04005003
#> After 1 iterations stage 1 ends!
#> Iteration: 1, ELBO: -40945.35, tau: 0.004755508, beta: 2.103296, relative difference: 0.04074471
#> Iteration: 2, ELBO: -40851.42, tau: 0.004904824, beta: 0.9394679, relative difference: 0.002293882
#> Iteration: 3, ELBO: -40337.62, tau: 0.005692732, beta: 0.2163247, relative difference: 0.0125774
#> Iteration: 4, ELBO: -40210.05, tau: 0.005976268, beta: 0.1468454, relative difference: 0.003162459
#> Iteration: 5, ELBO: -40160.58, tau: 0.00599527, beta: 0.1236717, relative difference: 0.00123037
#> Iteration: 6, ELBO: -40130.99, tau: 0.005999016, beta: 0.1122164, relative difference: 0.0007368561
#> Iteration: 7, ELBO: -40111.32, tau: 0.00600085, beta: 0.1061432, relative difference: 0.0004900061
#> Iteration: 8, ELBO: -40097.29, tau: 0.006002023, beta: 0.1034686, relative difference: 0.000349781
#> Iteration: 9, ELBO: -40086.46, tau: 0.006002823, beta: 0.1026805, relative difference: 0.0002700955
#> Iteration: 10, ELBO: -40078, tau: 0.006003424, beta: 0.1036829, relative difference: 0.0002110526
#> Iteration: 11, ELBO: -40070.93, tau: 0.006003855, beta: 0.105444, relative difference: 0.0001764187
#> Iteration: 12, ELBO: -40064.97, tau: 0.006004209, beta: 0.1083375, relative difference: 0.0001486744
#> Iteration: 13, ELBO: -40059.62, tau: 0.006004478, beta: 0.1119039, relative difference: 0.0001337229
#> Iteration: 14, ELBO: -40055.02, tau: 0.00600472, beta: 0.116613, relative difference: 0.0001147329
#> Iteration: 15, ELBO: -40050.66, tau: 0.006004884, beta: 0.1216548, relative difference: 0.0001087517
#> Iteration: 16, ELBO: -40046.91, tau: 0.006005059, beta: 0.127906, relative difference: 9.380948e-05
#> Iteration: 17, ELBO: -40043.29, tau: 0.006005156, beta: 0.134277, relative difference: 9.020971e-05
#> Iteration: 18, ELBO: -40039.96, tau: 0.006005276, beta: 0.1416863, relative difference: 8.317924e-05
#> Iteration: 19, ELBO: -40036.99, tau: 0.006005356, beta: 0.1496436, relative difference: 7.413818e-05
#> Iteration: 20, ELBO: -40034.39, tau: 0.006005405, beta: 0.1578692, relative difference: 6.493461e-05
#> Iteration: 21, ELBO: -40031.78, tau: 0.006005428, beta: 0.1661624, relative difference: 6.541195e-05
#> Iteration: 22, ELBO: -40029.36, tau: 0.006005482, beta: 0.1753365, relative difference: 6.028283e-05
#> Iteration: 23, ELBO: -40027.22, tau: 0.006005496, beta: 0.1846911, relative difference: 5.340042e-05
#> Iteration: 24, ELBO: -40025.11, tau: 0.006005491, beta: 0.1940252, relative difference: 5.27701e-05
#> Iteration: 25, ELBO: -40023.29, tau: 0.006005507, beta: 0.2039036, relative difference: 4.557246e-05
#> Iteration: 26, ELBO: -40021.61, tau: 0.006005471, beta: 0.2133716, relative difference: 4.205264e-05
#> Iteration: 27, ELBO: -40019.91, tau: 0.006005443, beta: 0.2227326, relative difference: 4.245581e-05
#> Iteration: 28, ELBO: -40018.53, tau: 0.006005437, beta: 0.2323958, relative difference: 3.433869e-05
#> Iteration: 29, ELBO: -40016.91, tau: 0.006005358, beta: 0.2409607, relative difference: 4.061319e-05
#> Iteration: 30, ELBO: -40015.59, tau: 0.006005393, beta: 0.250586, relative difference: 3.284857e-05
#> Iteration: 31, ELBO: -40014.25, tau: 0.006005325, beta: 0.2592651, relative difference: 3.364351e-05
#> Iteration: 32, ELBO: -40013.13, tau: 0.006005315, beta: 0.2681352, relative difference: 2.775883e-05
#> Iteration: 33, ELBO: -40011.92, tau: 0.006005257, beta: 0.2762826, relative difference: 3.048414e-05
#> Iteration: 34, ELBO: -40011.08, tau: 0.006005261, beta: 0.2848773, relative difference: 2.088641e-05
#> Iteration: 35, ELBO: -40010.21, tau: 0.006005147, beta: 0.2917749, relative difference: 2.181973e-05
#> Iteration: 36, ELBO: -40009.22, tau: 0.006005101, beta: 0.2982882, relative difference: 2.465201e-05
#> Iteration: 37, ELBO: -40008.26, tau: 0.006005108, beta: 0.3051575, relative difference: 2.395932e-05
#> Iteration: 38, ELBO: -40007.56, tau: 0.006005103, beta: 0.3120786, relative difference: 1.760662e-05
#> Iteration: 39, ELBO: -40006.75, tau: 0.006005014, beta: 0.3176339, relative difference: 2.01958e-05
#> Iteration: 40, ELBO: -40006.13, tau: 0.006005005, beta: 0.3232548, relative difference: 1.553132e-05
#> Iteration: 41, ELBO: -40005.58, tau: 0.006004947, beta: 0.3279908, relative difference: 1.376891e-05
#> Iteration: 42, ELBO: -40004.82, tau: 0.006004897, beta: 0.3320841, relative difference: 1.895344e-05
#> Iteration: 43, ELBO: -40004.12, tau: 0.006004923, beta: 0.3367596, relative difference: 1.740847e-05
#> Iteration: 44, ELBO: -40003.7, tau: 0.006004916, beta: 0.3412813, relative difference: 1.061965e-05
#> Iteration: 45, ELBO: -40003.24, tau: 0.00600485, beta: 0.3446567, relative difference: 1.146337e-05
#> Iteration: 46, ELBO: -40002.64, tau: 0.006004842, beta: 0.3480187, relative difference: 1.497478e-05
#> Iteration: 47, ELBO: -40002.19, tau: 0.006004861, beta: 0.3517664, relative difference: 1.122406e-05
#> Iteration: 48, ELBO: -40001.6, tau: 0.006004837, beta: 0.3550417, relative difference: 1.486838e-05
#> Iteration: 49, ELBO: -40001.2, tau: 0.006004866, beta: 0.3588804, relative difference: 9.788869e-06
#> After 49 iterations stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject) + Y_mean
#> The main data matrix Y has no missing entries!

# Root-mean-square-error
rmse <- sqrt(mean(Y_obs - Y_hat)^2)
rmse
#> [1] 0.0515441

# Standard deviation of Y_obs
sd(as.vector(Y_obs))
#> [1] 18.50556
```

## Development

The package is developed by Zhiwei Wang (<zhiwei.wang@connect.ust.hk>).

## Contact Information

Please feel free to contact Zhiwei Wang (<zhiwei.wang@connect.ust.hk>),
Prof. Mingxuan Cai (<mingxcai@cityu.edu.hk>), or Prof. Can Yang
(<macyang@ust.hk>) with any inquiries.
