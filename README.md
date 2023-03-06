
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
library(rpart)

# Simulate data
N <- 100
M <- 100
K_true <- 2L

PVE_Z <- 0.8
PVE_Y <- 0.5

X1 <- runif(N, min = -10, max = 10)
X2 <- runif(N, min = -10, max = 10)
X <- cbind(X1, X2)

FX1 <- X1 / 2 - X2
FX2 <- (X1^2 - X2^2 + 2 * X1 * X2) / 10
FX <- cbind(FX1, FX2)

beta1 <- var(FX1) * (1 / PVE_Z - 1)
Z1 <- mvrnorm(n = 1, mu = FX1, Sigma = beta1 * diag(N))
beta2 <- var(FX2) * (1 / PVE_Z - 1)
Z2 <- mvrnorm(n = 1, mu = FX2, Sigma = beta2 * diag(N))
Z <- cbind(Z1, Z2)

W <- matrix(rnorm(M * K_true), nrow = M, ncol = K_true)

Y <- Z %*% t(W)
Y_var <- var(as.vector(Y))
epsilon <- sqrt(Y_var * (1 / PVE_Y - 1))
Y_obs <- Y + matrix(rnorm(N * M, sd = epsilon), nrow = N, ncol = M)
Y_mean <- mean(Y_obs)

# Create MFAIR object
mfairObject <- createMFAIR(Y_obs - Y_mean, X, K_max = K_true)

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject)
#> Iteration: 1, ELBO: -43170.38, tau: 0.00313005, beta: 0.2874784, difference: Inf
#> After 2 iterations stage 1 ends!
#> Iteration: 1, ELBO: -41696.03, tau: 0.004642825, beta: 0.06469098, difference: 0.03415181
#> Iteration: 2, ELBO: -41624.25, tau: 0.004650448, beta: 0.05657293, difference: 0.001721528
#> Iteration: 3, ELBO: -41578.73, tau: 0.004652098, beta: 0.05127436, difference: 0.001093435
#> Iteration: 4, ELBO: -41546.8, tau: 0.004652959, beta: 0.04688818, difference: 0.0007681498
#> Iteration: 5, ELBO: -41524.18, tau: 0.004653569, beta: 0.04425139, difference: 0.0005442874
#> Iteration: 6, ELBO: -41506.31, tau: 0.004653987, beta: 0.04161936, difference: 0.0004304285
#> Iteration: 7, ELBO: -41493, tau: 0.004654337, beta: 0.04008187, difference: 0.0003206757
#> Iteration: 8, ELBO: -41481.48, tau: 0.004654583, beta: 0.03828924, difference: 0.0002776407
#> Iteration: 9, ELBO: -41472.76, tau: 0.004654817, beta: 0.03741257, difference: 0.0002101555
#> Iteration: 10, ELBO: -41464.79, tau: 0.004654974, beta: 0.03615787, difference: 0.0001922829
#> Iteration: 11, ELBO: -41457.91, tau: 0.00465514, beta: 0.03559209, difference: 0.0001657973
#> Iteration: 12, ELBO: -41452.74, tau: 0.004655269, beta: 0.03521355, difference: 0.0001247932
#> Iteration: 13, ELBO: -41447.68, tau: 0.004655354, beta: 0.03437566, difference: 0.0001219774
#> Iteration: 14, ELBO: -41443.15, tau: 0.004655463, beta: 0.03405664, difference: 0.0001093549
#> Iteration: 15, ELBO: -41439.24, tau: 0.004655548, beta: 0.03387444, difference: 9.442968e-05
#> Iteration: 16, ELBO: -41435.81, tau: 0.004655615, beta: 0.03365024, difference: 8.262388e-05
#> Iteration: 17, ELBO: -41432.15, tau: 0.004655675, beta: 0.03341724, difference: 8.830401e-05
#> Iteration: 18, ELBO: -41429.84, tau: 0.004655749, beta: 0.03361785, difference: 5.573319e-05
#> Iteration: 19, ELBO: -41427.31, tau: 0.004655766, beta: 0.03317627, difference: 6.119996e-05
#> Iteration: 20, ELBO: -41424.58, tau: 0.004655823, beta: 0.03307976, difference: 6.578433e-05
#> Iteration: 21, ELBO: -41422.94, tau: 0.004655878, beta: 0.03328862, difference: 3.979912e-05
#> Iteration: 22, ELBO: -41421, tau: 0.004655885, beta: 0.03294401, difference: 4.66231e-05
#> Iteration: 23, ELBO: -41418.9, tau: 0.00465593, beta: 0.03290331, difference: 5.080613e-05
#> Iteration: 24, ELBO: -41417.22, tau: 0.004655972, beta: 0.03309614, difference: 4.054556e-05
#> Iteration: 25, ELBO: -41415.45, tau: 0.004655988, beta: 0.03311052, difference: 4.276809e-05
#> Iteration: 26, ELBO: -41414.43, tau: 0.004656019, beta: 0.0332732, difference: 2.450408e-05
#> Iteration: 27, ELBO: -41413.14, tau: 0.004656017, beta: 0.03303393, difference: 3.123153e-05
#> Iteration: 28, ELBO: -41411.68, tau: 0.004656048, beta: 0.03303158, difference: 3.526986e-05
#> Iteration: 29, ELBO: -41410.51, tau: 0.004656078, beta: 0.03319705, difference: 2.827204e-05
#> Iteration: 30, ELBO: -41407.36, tau: 0.004656092, beta: 0.03324507, difference: 7.598194e-05
#> Iteration: 31, ELBO: -41406.27, tau: 0.004656188, beta: 0.03467596, difference: 2.634995e-05
#> Iteration: 32, ELBO: -41403.43, tau: 0.004656133, beta: 0.03480447, difference: 6.858272e-05
#> Iteration: 33, ELBO: -41402.31, tau: 0.004656216, beta: 0.03620671, difference: 2.710306e-05
#> Iteration: 34, ELBO: -41401.5, tau: 0.004656167, beta: 0.03650764, difference: 1.955337e-05
#> Iteration: 35, ELBO: -41399.09, tau: 0.004656163, beta: 0.03664485, difference: 5.811014e-05
#> Iteration: 36, ELBO: -41398.2, tau: 0.00465624, beta: 0.03799111, difference: 2.150632e-05
#> Iteration: 37, ELBO: -41396.02, tau: 0.004656188, beta: 0.03830291, difference: 5.27422e-05
#> Iteration: 38, ELBO: -41395.26, tau: 0.004656251, beta: 0.03962186, difference: 1.824912e-05
#> Iteration: 39, ELBO: -41394.66, tau: 0.004656198, beta: 0.03992401, difference: 1.45007e-05
#> Iteration: 40, ELBO: -41392.74, tau: 0.004656192, beta: 0.04011209, difference: 4.638525e-05
#> Iteration: 41, ELBO: -41392.22, tau: 0.004656258, beta: 0.04136164, difference: 1.260637e-05
#> Iteration: 42, ELBO: -41391.44, tau: 0.0046562, beta: 0.04156937, difference: 1.883329e-05
#> Iteration: 43, ELBO: -41391.01, tau: 0.004656208, beta: 0.04197014, difference: 1.040398e-05
#> Iteration: 44, ELBO: -41389.28, tau: 0.004656194, beta: 0.04212405, difference: 4.192996e-05
#> Iteration: 45, ELBO: -41388.85, tau: 0.004656267, beta: 0.04336824, difference: 1.026675e-05
#> Iteration: 46, ELBO: -41388.21, tau: 0.004656218, beta: 0.04360438, difference: 1.546563e-05
#> Iteration: 47, ELBO: -41386.64, tau: 0.00465623, beta: 0.04400889, difference: 3.79818e-05
#> Iteration: 48, ELBO: -41386.1, tau: 0.004656286, beta: 0.04524735, difference: 1.308068e-05
#> Iteration: 49, ELBO: -41384.71, tau: 0.004656238, beta: 0.04565636, difference: 3.358408e-05
#> Iteration: 50, ELBO: -41383.92, tau: 0.004656283, beta: 0.04679467, difference: 1.916733e-05
#> After 51 iterations stage 2 ends!
#> Factor 1 retained!
#> Iteration: 1, ELBO: -40972.63, tau: 0.004751977, beta: 2.297962, difference: Inf
#> After 2 iterations stage 1 ends!
#> Iteration: 1, ELBO: -40799.14, tau: 0.004984733, beta: 0.725531, difference: 0.004234403
#> Iteration: 2, ELBO: -40295.22, tau: 0.005794689, beta: 0.193989, difference: 0.01235132
#> Iteration: 3, ELBO: -40202.34, tau: 0.005982072, beta: 0.1398687, difference: 0.002304846
#> Iteration: 4, ELBO: -40156.31, tau: 0.005995614, beta: 0.1198, difference: 0.001145041
#> Iteration: 5, ELBO: -40128.48, tau: 0.005998863, beta: 0.1096819, difference: 0.0006929496
#> Iteration: 6, ELBO: -40109.75, tau: 0.006000561, beta: 0.1043514, difference: 0.0004668111
#> Iteration: 7, ELBO: -40096.26, tau: 0.006001666, beta: 0.1021556, difference: 0.0003363525
#> Iteration: 8, ELBO: -40085.76, tau: 0.006002427, beta: 0.101702, difference: 0.0002617623
#> Iteration: 9, ELBO: -40077.52, tau: 0.006003003, beta: 0.1029533, difference: 0.0002055841
#> Iteration: 10, ELBO: -40070.6, tau: 0.006003415, beta: 0.1049133, difference: 0.0001726344
#> Iteration: 11, ELBO: -40064.76, tau: 0.006003756, beta: 0.1079678, difference: 0.0001459019
#> Iteration: 12, ELBO: -40059.51, tau: 0.006004016, beta: 0.11167, difference: 0.0001308579
#> Iteration: 13, ELBO: -40054.95, tau: 0.006004246, beta: 0.1164117, difference: 0.0001139653
#> Iteration: 14, ELBO: -40050.63, tau: 0.006004411, beta: 0.1216059, difference: 0.0001077248
#> Iteration: 15, ELBO: -40046.75, tau: 0.00600458, beta: 0.1279506, difference: 9.696821e-05
#> Iteration: 16, ELBO: -40043.2, tau: 0.006004699, beta: 0.1349067, difference: 8.868589e-05
#> Iteration: 17, ELBO: -40039.99, tau: 0.006004792, beta: 0.1424245, difference: 7.999283e-05
#> Iteration: 18, ELBO: -40037.13, tau: 0.006004857, beta: 0.1503036, difference: 7.149488e-05
#> Iteration: 19, ELBO: -40034.25, tau: 0.006004902, beta: 0.1584762, difference: 7.210457e-05
#> Iteration: 20, ELBO: -40031.73, tau: 0.006004978, beta: 0.1677794, difference: 6.272856e-05
#> Iteration: 21, ELBO: -40029.42, tau: 0.006004981, beta: 0.1769218, difference: 5.785599e-05
#> Iteration: 22, ELBO: -40027.33, tau: 0.006004991, beta: 0.1863014, difference: 5.214015e-05
#> Iteration: 23, ELBO: -40025.37, tau: 0.006004978, beta: 0.195626, difference: 4.909271e-05
#> Iteration: 24, ELBO: -40023.4, tau: 0.006004964, beta: 0.2049647, difference: 4.910207e-05
#> Iteration: 25, ELBO: -40021.75, tau: 0.006004981, beta: 0.2148892, difference: 4.135256e-05
#> Iteration: 26, ELBO: -40020.29, tau: 0.006004929, beta: 0.2241837, difference: 3.647724e-05
#> Iteration: 27, ELBO: -40018.51, tau: 0.006004878, beta: 0.2329631, difference: 4.428697e-05
#> Iteration: 28, ELBO: -40016.96, tau: 0.006004932, beta: 0.2431397, difference: 3.882921e-05
#> Iteration: 29, ELBO: -40015.65, tau: 0.006004895, beta: 0.2529303, difference: 3.262754e-05
#> Iteration: 30, ELBO: -40014.35, tau: 0.006004821, beta: 0.2617873, difference: 3.26281e-05
#> Iteration: 31, ELBO: -40013.36, tau: 0.006004802, beta: 0.2706927, difference: 2.461082e-05
#> Iteration: 32, ELBO: -40012.37, tau: 0.006004716, beta: 0.2784238, difference: 2.494008e-05
#> Iteration: 33, ELBO: -40011.21, tau: 0.006004673, beta: 0.2858503, difference: 2.877593e-05
#> Iteration: 34, ELBO: -40010.37, tau: 0.006004692, beta: 0.293891, difference: 2.108943e-05
#> Iteration: 35, ELBO: -40009.38, tau: 0.006004601, beta: 0.3005346, difference: 2.471472e-05
#> Iteration: 36, ELBO: -40008.66, tau: 0.006004603, beta: 0.3075263, difference: 1.796591e-05
#> Iteration: 37, ELBO: -40007.76, tau: 0.006004527, beta: 0.3133752, difference: 2.265214e-05
#> Iteration: 38, ELBO: -40007.13, tau: 0.006004543, beta: 0.3197471, difference: 1.568382e-05
#> Iteration: 39, ELBO: -40006.33, tau: 0.006004455, beta: 0.3247015, difference: 1.997731e-05
#> Iteration: 40, ELBO: -40005.78, tau: 0.006004468, beta: 0.3301181, difference: 1.376341e-05
#> Iteration: 41, ELBO: -40005.04, tau: 0.0060044, beta: 0.3344323, difference: 1.854854e-05
#> Iteration: 42, ELBO: -40004.53, tau: 0.006004413, beta: 0.3391524, difference: 1.272821e-05
#> After 43 iterations stage 2 ends!
#> Factor 2 retained!

# Prediction based on the low-rank approximation
Y_hat <- predict(mfairObject) + Y_mean
#> The main data matrix Y has no missing entries!

# Root-mean-square-error
rmse <- sqrt(mean(Y_obs - Y_hat)^2)
rmse
#> [1] 0.05206072

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
