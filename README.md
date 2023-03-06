
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mfair: Matrix Factorization with Auxiliary Information in R

<!-- badges: start -->
<!-- badges: end -->

Methods for matrix factorization to leverage auxiliary information based
on the paper MFAI. The name of the package `mfair` comes from “Matrix
Factorization with Auxiliary Information in R”.

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

# Create MFAIR object
mfairObject <- createMFAIR(Y_obs, X, K_max = K_true)

# Fit the MFAI model
mfairObject <- fitGreedy(mfairObject)
#> Iteration: 1, ELBO: -43170.78, tau: 0.003129733, beta: 0.2875156, difference: Inf
#> After 2, iterations, stage 1 ends!
#> Iteration: 1, ELBO: -41695.68, tau: 0.004643328, beta: 0.06469208, difference: 0.03416902
#> Iteration: 2, ELBO: -41623.89, tau: 0.004650919, beta: 0.05657566, difference: 0.001721655
#> Iteration: 3, ELBO: -41578.36, tau: 0.004652564, beta: 0.05127748, difference: 0.001093858
#> Iteration: 4, ELBO: -41546.41, tau: 0.004653425, beta: 0.04689145, difference: 0.0007684995
#> Iteration: 5, ELBO: -41523.78, tau: 0.004654034, beta: 0.04425461, difference: 0.0005445599
#> Iteration: 6, ELBO: -41505.9, tau: 0.004654452, beta: 0.04162255, difference: 0.0004306376
#> Iteration: 7, ELBO: -41492.59, tau: 0.004654802, beta: 0.0400849, difference: 0.0003208454
#> Iteration: 8, ELBO: -41481.06, tau: 0.004655048, beta: 0.03829216, difference: 0.0002777901
#> Iteration: 9, ELBO: -41472.34, tau: 0.004655282, beta: 0.03741575, difference: 0.0002102705
#> Iteration: 10, ELBO: -41464.36, tau: 0.004655439, beta: 0.03616089, difference: 0.0001923893
#> Iteration: 11, ELBO: -41457.48, tau: 0.004655605, beta: 0.03559528, difference: 0.0001658761
#> Iteration: 12, ELBO: -41452.3, tau: 0.004655734, beta: 0.03521653, difference: 0.0001248625
#> Iteration: 13, ELBO: -41447.24, tau: 0.004655819, beta: 0.03437844, difference: 0.0001220469
#> Iteration: 14, ELBO: -41442.71, tau: 0.004655929, beta: 0.03405954, difference: 0.0001094025
#> Iteration: 15, ELBO: -41438.8, tau: 0.004656014, beta: 0.03387705, difference: 9.447153e-05
#> Iteration: 16, ELBO: -41435.37, tau: 0.004656081, beta: 0.03365256, difference: 8.267427e-05
#> Iteration: 17, ELBO: -41431.71, tau: 0.00465614, beta: 0.03341967, difference: 8.832937e-05
#> Iteration: 18, ELBO: -41429.4, tau: 0.004656214, beta: 0.03361983, difference: 5.576383e-05
#> Iteration: 19, ELBO: -41426.86, tau: 0.004656232, beta: 0.033178, difference: 6.123741e-05
#> Iteration: 20, ELBO: -41424.14, tau: 0.004656289, beta: 0.03308155, difference: 6.580065e-05
#> Iteration: 21, ELBO: -41422.49, tau: 0.004656344, beta: 0.03328998, difference: 3.9821e-05
#> Iteration: 22, ELBO: -41420.55, tau: 0.00465635, beta: 0.03294512, difference: 4.665062e-05
#> Iteration: 23, ELBO: -41418.45, tau: 0.004656395, beta: 0.0329044, difference: 5.081705e-05
#> Iteration: 24, ELBO: -41416.77, tau: 0.004656437, beta: 0.03309683, difference: 4.057031e-05
#> Iteration: 25, ELBO: -41415, tau: 0.004656453, beta: 0.0331112, difference: 4.277825e-05
#> Iteration: 26, ELBO: -41413.98, tau: 0.004656485, beta: 0.03327355, difference: 2.45174e-05
#> Iteration: 27, ELBO: -41412.69, tau: 0.004656483, beta: 0.03303405, difference: 3.125276e-05
#> Iteration: 28, ELBO: -41411.23, tau: 0.004656514, beta: 0.03303172, difference: 3.529304e-05
#> Iteration: 29, ELBO: -41410.05, tau: 0.004656543, beta: 0.03319731, difference: 2.82907e-05
#> Iteration: 30, ELBO: -41406.91, tau: 0.004656558, beta: 0.03324535, difference: 7.599398e-05
#> Iteration: 31, ELBO: -41405.82, tau: 0.004656654, beta: 0.03467611, difference: 2.636311e-05
#> Iteration: 32, ELBO: -41402.98, tau: 0.004656599, beta: 0.03480453, difference: 6.859678e-05
#> Iteration: 33, ELBO: -41401.85, tau: 0.004656682, beta: 0.03620674, difference: 2.711581e-05
#> Iteration: 34, ELBO: -41399.38, tau: 0.004656633, beta: 0.03650761, difference: 5.977194e-05
#> Iteration: 35, ELBO: -41398.5, tau: 0.004656701, beta: 0.03786035, difference: 2.125324e-05
#> Iteration: 36, ELBO: -41397.78, tau: 0.004656648, beta: 0.03812993, difference: 1.745273e-05
#> Iteration: 37, ELBO: -41395.59, tau: 0.004656645, beta: 0.03830297, difference: 5.290113e-05
#> Iteration: 38, ELBO: -41394.95, tau: 0.004656717, beta: 0.03962129, difference: 1.535812e-05
#> Iteration: 39, ELBO: -41394.24, tau: 0.004656658, beta: 0.03982721, difference: 1.712653e-05
#> Iteration: 40, ELBO: -41392.31, tau: 0.004656659, beta: 0.04009094, difference: 4.654273e-05
#> Iteration: 41, ELBO: -41391.79, tau: 0.004656723, beta: 0.04134094, difference: 1.2546e-05
#> Iteration: 42, ELBO: -41391.01, tau: 0.004656665, beta: 0.04154184, difference: 1.884652e-05
#> Iteration: 43, ELBO: -41390.59, tau: 0.004656673, beta: 0.04193877, difference: 1.036143e-05
#> Iteration: 44, ELBO: -41388.85, tau: 0.00465666, beta: 0.04208768, difference: 4.183659e-05
#> Iteration: 45, ELBO: -41388.43, tau: 0.004656733, beta: 0.04332399, difference: 1.02012e-05
#> Iteration: 46, ELBO: -41387.79, tau: 0.004656684, beta: 0.0435541, difference: 1.547325e-05
#> Iteration: 47, ELBO: -41386.22, tau: 0.004656696, beta: 0.04395472, difference: 3.80693e-05
#> Iteration: 48, ELBO: -41385.68, tau: 0.004656752, beta: 0.04519142, difference: 1.297569e-05
#> Iteration: 49, ELBO: -41384.28, tau: 0.004656704, beta: 0.04559305, difference: 3.368592e-05
#> Iteration: 50, ELBO: -41383.5, tau: 0.004656749, beta: 0.04673016, difference: 1.906052e-05
#> After 51, iterations, stage 2 ends!
#> Factor 1 retained!
#> Iteration: 1, ELBO: -40972.13, tau: 0.00475246, beta: 2.298006, difference: Inf
#> After 2, iterations, stage 1 ends!
#> Iteration: 1, ELBO: -40802.62, tau: 0.004979535, beta: 0.7394195, difference: 0.004137369
#> Iteration: 2, ELBO: -40297.54, tau: 0.005788384, beta: 0.1951493, difference: 0.01237845
#> Iteration: 3, ELBO: -40202.95, tau: 0.005981478, beta: 0.1402107, difference: 0.002347298
#> Iteration: 4, ELBO: -40156.7, tau: 0.005995332, beta: 0.1199751, difference: 0.001150348
#> Iteration: 5, ELBO: -40128.77, tau: 0.005998612, beta: 0.1097859, difference: 0.0006955289
#> Iteration: 6, ELBO: -40109.99, tau: 0.006000318, beta: 0.1044153, difference: 0.0004681309
#> Iteration: 7, ELBO: -40096.47, tau: 0.006001429, beta: 0.1021949, difference: 0.0003370875
#> Iteration: 8, ELBO: -40085.96, tau: 0.006002191, beta: 0.1017245, difference: 0.0002621967
#> Iteration: 9, ELBO: -40077.7, tau: 0.006002769, beta: 0.102963, difference: 0.0002058863
#> Iteration: 10, ELBO: -40070.78, tau: 0.006003183, beta: 0.1049155, difference: 0.0001728181
#> Iteration: 11, ELBO: -40064.92, tau: 0.006003525, beta: 0.1079626, difference: 0.0001460474
#> Iteration: 12, ELBO: -40059.68, tau: 0.006003785, beta: 0.1116605, difference: 0.0001309789
#> Iteration: 13, ELBO: -40055.11, tau: 0.006004015, beta: 0.1164004, difference: 0.0001140288
#> Iteration: 14, ELBO: -40050.79, tau: 0.006004181, beta: 0.121591, difference: 0.0001078003
#> Iteration: 15, ELBO: -40046.9, tau: 0.00600435, beta: 0.1279356, difference: 9.704721e-05
#> Iteration: 16, ELBO: -40043.35, tau: 0.006004469, beta: 0.1348944, difference: 8.870651e-05
#> Iteration: 17, ELBO: -40040.15, tau: 0.006004563, beta: 0.142411, difference: 8.00368e-05
#> Iteration: 18, ELBO: -40037.28, tau: 0.006004628, beta: 0.1502927, difference: 7.154605e-05
#> Iteration: 19, ELBO: -40034.39, tau: 0.006004673, beta: 0.1584706, difference: 7.217982e-05
#> Iteration: 20, ELBO: -40031.91, tau: 0.006004749, beta: 0.1677841, difference: 6.192241e-05
#> Iteration: 21, ELBO: -40029.56, tau: 0.006004746, beta: 0.1768282, difference: 5.864487e-05
#> Iteration: 22, ELBO: -40027.48, tau: 0.006004762, beta: 0.1862452, difference: 5.210258e-05
#> Iteration: 23, ELBO: -40025.51, tau: 0.006004748, beta: 0.1955567, difference: 4.922745e-05
#> Iteration: 24, ELBO: -40023.54, tau: 0.006004737, beta: 0.2049094, difference: 4.91173e-05
#> Iteration: 25, ELBO: -40021.88, tau: 0.006004753, beta: 0.2148355, difference: 4.14681e-05
#> Iteration: 26, ELBO: -40020.42, tau: 0.006004702, beta: 0.2241468, difference: 3.653553e-05
#> Iteration: 27, ELBO: -40018.65, tau: 0.00600465, beta: 0.232939, difference: 4.43018e-05
#> Iteration: 28, ELBO: -40017.09, tau: 0.006004704, beta: 0.2431226, difference: 3.885474e-05
#> Iteration: 29, ELBO: -40015.78, tau: 0.006004667, beta: 0.2529214, difference: 3.270609e-05
#> Iteration: 30, ELBO: -40014.47, tau: 0.006004593, beta: 0.2617964, difference: 3.271024e-05
#> Iteration: 31, ELBO: -40013.49, tau: 0.006004574, beta: 0.2707242, difference: 2.470786e-05
#> Iteration: 32, ELBO: -40012.49, tau: 0.006004488, beta: 0.2784829, difference: 2.500751e-05
#> Iteration: 33, ELBO: -40011.34, tau: 0.006004446, beta: 0.2859345, difference: 2.8694e-05
#> Iteration: 34, ELBO: -40010.49, tau: 0.006004462, beta: 0.2939734, difference: 2.11581e-05
#> Iteration: 35, ELBO: -40009.5, tau: 0.006004372, beta: 0.3006351, difference: 2.477152e-05
#> Iteration: 36, ELBO: -40008.78, tau: 0.006004375, beta: 0.3076478, difference: 1.80085e-05
#> Iteration: 37, ELBO: -40007.88, tau: 0.006004299, beta: 0.3135156, difference: 2.256986e-05
#> Iteration: 38, ELBO: -40007.25, tau: 0.006004313, beta: 0.319882, difference: 1.572768e-05
#> Iteration: 39, ELBO: -40006.45, tau: 0.006004226, beta: 0.3248473, difference: 2.002087e-05
#> Iteration: 40, ELBO: -40005.9, tau: 0.00600424, beta: 0.3302791, difference: 1.367787e-05
#> Iteration: 41, ELBO: -40005.23, tau: 0.00600417, beta: 0.3345829, difference: 1.678143e-05
#> Iteration: 42, ELBO: -40004.49, tau: 0.006004161, beta: 0.3389327, difference: 1.841276e-05
#> Iteration: 43, ELBO: -40003.99, tau: 0.006004192, beta: 0.3437831, difference: 1.259482e-05
#> After 44, iterations, stage 2 ends!
#> Factor 2 retained!
```

## Development

The package is developed by Zhiwei Wang (<zhiwei.wang@connect.ust.hk>).

## Contact Information

Please feel free to contact Zhiwei Wang (<zhiwei.wang@connect.ust.hk>),
Prof. Mingxuan Cai (<mingxcai@cityu.edu.hk>), or Prof. Can Yang
(<macyang@ust.hk>) with any inquiries.
