#' Project a matrix with given indices and store the result in the sparse mode.
#'
#' @import Matrix
#'
#' @param Y A matrix to be projected.
#' @param obs_indices A matrix containing 1-based indices of the observed entries in the matrix Y. The first column represents row and the second column represents column.
#'
#' @return A dgCMatrix containing the projection result.
#' @export
#'
projSparse <- function(Y, obs_indices) {
  N <- nrow(Y)
  M <- ncol(Y)
  Y <- Matrix::sparseMatrix(
    i = obs_indices[, 1], j = obs_indices[, 2],
    x = Y[obs_indices],
    dims = c(N, M),
    symmetric = FALSE, triangular = FALSE,
    index1 = TRUE,
    repr = "C"
  )
}
