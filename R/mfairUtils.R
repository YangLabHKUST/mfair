#' Defined the matrixORdgCMatrix class as the union of matrix and Matrix::dgCMatrix
#' @importFrom Matrix dgCMatrix
#'
setClassUnion(name = "matrixORdgCMatrix", members = c("matrix", "dgCMatrix"))
