setGeneric(
  name = "fitGreedy",
  def = function(object, ...) {
    standardGeneric("fitGreedy")
  }
)

setMethod(
  f = "fitGreedy",
  signature = "MFAIR",
  definition = function(object, ...) {
    if (object@Y_missing) {
      object <- fitGreedyMissing(object, ...)
    } else {
      object <- fitGreedyFully(object, ...)
    }
  }
)

fitGreedyFully <- function(object,
                           K_max = NULL,
                           learning_rate = 0.1, minsplit = 10, minbucket = round(minsplit / 3), maxdepth = 2,
                           iter_max = 5e+3,
                           tol_stage1 = 0.1, tol_stage2 = 1e-5,
                           verbose = TRUE, save_models = TRUE) {
  # Check K_max
  if (!is.null(K_max)) {
    object@K_max <- K_max
  }
  if (object@K_max > object@N || object@K_max > object@N) {
    stop("The maximum rank allowed can not be larger than the rank of the main data matrix!")
  }

  for (k in 1:object@K_max) {
    # Initialize
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
  }
}
