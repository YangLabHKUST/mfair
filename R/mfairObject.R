#' Define the matrixORdgCMatrix class as the union of matrix and Matrix::dgCMatrix.
#'
#' @import Matrix
#'
setClassUnion(
  name = "matrixORdgCMatrix",
  members = c("matrix", "dgCMatrix")
)

#' Each MFAIR object has a number of slots which store information. Key slots to access are listed below.
#'
#' @slot Y A matrix or Matrix::dgCMatrix. The main data matrix of N samples and M features.
#' @slot X A data.frame. The auxiliary information data frame of N samples and C covariates.
#' @slot Y_sparse Logical. Whether the main data matrix Y is stored in the sparse mode.
#' @slot Y_center Logical. Whether the main data matrix Y is centered.
#' @slot Y_mean Numeric. Mean of the main data matrix Y if centered. Zero if not.
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs integer. Total number of observed entries in Y.
#' @slot N An integer. Number of rows (samples) of Y, also the number of rows (samples) of X.
#' @slot M An integer. Number of columns (features) of Y.
#' @slot C An integer. Number of columns (auxiliary covariates) of X.
#' @slot K_max An integer. Please note that increasing K_max does not ensure that the actual K also increases since K_max is just an upper bound, and the model will automatically infer K below K_max under the default setting. If you want to fit the model with larger rank K, please set the `null_check` argument as FALSE, or make sure that K_max is large enough and the `tol_snr` argument in the fitting function `fitGreedy()` is small enough simultaneously in the fitting function `fitGreedy()`.
#' @slot K An integer. The inferred rank of Y.
# #' @slot Z An N * K matrix. Estimated factor matrix, corresponding to the posterior mean of Z in the MFAI model.
# #' @slot a_sq A matrix containing posterior variance of Z with k-th column corresponding to the k-th factor. For fully observed Y, all N samples of one specific factor share the same posterior variance, then a_sq is a 1 * K matrix. For Y with missing data, samples of one specific factor have different posterior variances, then a_sq is an N * K matrix.
# #' @slot W An M * K matrix. Estimated loading matrix, corresponding to the posterior mean of W in the MFAI model.
# #' @slot b_sq A matrix containing posterior variance of W with k-th column corresponding to the k-th loading. For fully observed Y, all M features of one specific loading share the same posterior variance, then b_sq is a 1 * K matrix. For Y with missing data, features of one specific loading have different posterior variances, then b_sq is an M * K matrix.
#' @slot Z A list of length K. Each element is a vector of length N, representing the posterior mean of one factor in the MFAI model.
#' @slot a_sq A list of length K with each element representing the posterior variance of one factor in the MFAI model.  For fully observed Y, all N samples of one specific factor share the same posterior variance, then each element is a numeric value. For Y with missing data, the samples have different posterior variances, then each element is a vector of length N.
#' @slot W A list of length K. Each element is a vector of length M, representing the posterior mean of one loading in the MFAI model.
#' @slot b_sq A list of length K with each element representing the posterior variance of one loading in the MFAI model. For fully observed Y, all M features of one specific loading share the same posterior variance, then each element is a numeric value. For Y with missing data, the features have different posterior variances, then each element is a vector of length M.
# #' @slot tau Numeric. A vector of length K, containing the precision parameter for each pair of factor/loading.
# #' @slot beta Numeric. A vector of length K, containing the precision parameter for each factor Z_k.
#' @slot tau A list of length K. Each elements is a numeric value representing the precision parameter for the combination of each pair of factor and loading.
#' @slot beta A list of length K. Each elements is a numeric value representing the precision parameter for each factor Z_k.
# #' @slot FX An N * K matrix representing the prior mean of Z, corresponding to F(X) in the MFAI model.
# #' @slot tree_0 An 1 * K matrix containing tree_0 with k-th column corresponding to the k-th factor. Tree_0 is defined as the mean of mu vector in each factor.
#' @slot FX A list of length K. Each element is a vector of length N, representing the prior mean of one factor in the MFAI model.
#' @slot tree_0 A list of length K. Each element is a numeric value representing tree_0 in one factor, which is defined as the mean of mu vector.
#' @slot tree_lists A list of length K, containing K fitted functions and each function is represented as a list of trees, i.e., the k-th list corresponds to function F_k(.) in the MFAI model.
#' @slot initialization A list. Initialization of the fitted model.
#' @slot learning_rate Numeric. The learning rate in the gradient boosting part.
#' @slot tree_parameters A list of options that control details of the rpart algorithm.
#' @slot project Character. Name of the project (for record keeping).
#'
#' @return MFAIR class.
#' @export
setClass(
  # Set the name for the class
  "MFAIR",

  # Define the slots
  slots = c(
    Y = "matrixORdgCMatrix",
    X = "data.frame",
    Y_sparse = "logical",
    Y_center = "logical",
    Y_mean = "numeric",
    Y_missing = "logical",
    n_obs = "integer",
    N = "integer",
    M = "integer",
    C = "integer",
    K_max = "integer",
    K = "integer",
    # Z = "matrix",
    # a_sq = "matrix",
    # W = "matrix",
    # b_sq = "matrix",
    Z = "list",
    a_sq = "list",
    W = "list",
    b_sq = "list",
    # tau = "numeric",
    # beta = "numeric",
    tau = "list",
    beta = "list",
    # FX = "matrix",
    # tree_0 = "matrix",
    FX = "list",
    tree_0 = "list",
    tree_lists = "list",
    initialization = "list",
    learning_rate = "numeric",
    tree_parameters = "list",
    project = "character"
  ),

  # Assign the default prototypes
  prototype = list(
    K_max = 1L,
    project = "MFAIR"
  )
)

#' MFAIRSingleFactor object contains the key information about the fitted single factor MFAI model.
#'
#' @slot Y_missing Logical. Whether the main data matrix Y is partially observed.
#' @slot n_obs Integer. Total number of observed entries in Y.
#' @slot mu An vector of length N representing the inferred loading, corresponding to the posterior mean of z in the single factor MFAI model.
#' @slot a_sq Numeric. The posterior variance of the loading z. For fully observed Y, all N elements of the loading share the same posterior variance, then a_sq is a single number. For Y with missing data, the elements have different posterior variances, then a_sq is a vector of length N.
#' @slot nu An vector of length M representing the inferred factor, corresponding to the posterior mean of w in the single factor MFAI model.
#' @slot b_sq Numeric. The posterior variance of the factor w. For fully observed Y, all M elements of the factor share the same posterior variance, then b_sq is a single number. For Y with missing data, the elements have different posterior variances, then b_sq is a vector of length M.
#' @slot tau Numeric. Precision parameter this pair of loading/factor.
#' @slot beta Numeric. Precision parameter for this loading z.
#' @slot FX An vector of length N representing the prior mean of z, corresponding to F(X) in the single factor MFAI model.
#' @slot tree_0 Numeric. Tree_0 is defined as the mean of mu vector.
#' @slot tree_list A list containing multiple decision trees, corresponding to function F(.) in the single factor MFAI model.
#' @slot project Character. Name of the project (for record keeping).
#'
#' @return MFAIRSingleFactor class.
#' @export
setClass(
  # Set the name for the class
  "MFAIRSingleFactor",

  # Define the slots
  slots = c(
    Y_missing = "logical",
    n_obs = "integer",
    mu = "numeric",
    a_sq = "numeric",
    nu = "numeric",
    b_sq = "numeric",
    tau = "numeric",
    beta = "numeric",
    FX = "numeric",
    tree_0 = "numeric",
    tree_list = "list",
    project = "character"
  ),

  # Assign the default prototypes
  prototype = list(
    project = "MFAIRSingleFactor"
  )
)
