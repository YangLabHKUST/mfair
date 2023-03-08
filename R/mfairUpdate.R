#' Append the fitted factor to the MFAIR object in the greedy algorithm.
#'
#' @param object An MFAIR object containing the first few factors.
#' @param object_sf An MFAIRSingleFactor needed to be appended.
#'
#' @return MFAIR object containing the information about the new fitted single factor.
#' @export
#'
appendMFAIR <- function(object, object_sf) {
  # Save the information about the fitted single factor MFAI model
  object@Z <- cbind(object@Z, object_sf@mu)
  object@a_sq <- cbind(object@a_sq, object_sf@a_sq)
  object@W <- cbind(object@W, object_sf@nu)
  object@b_sq <- cbind(object@b_sq, object_sf@b_sq)

  object@tau <- c(object@tau, object_sf@tau)
  object@beta <- c(object@beta, object_sf@beta)

  object@FX <- cbind(object@FX, object_sf@FX)

  # Save the tree list
  object@tree_lists <- append(object@tree_lists, list(object_sf@tree_list))

  # Update the inferred rank of the data
  object@K <- as.integer(object@K + 1)

  return(object)
}

#' Update the k-th factor of the MFAIR object in the backfitting algorithm.
#'
#' @param object An MFAIR object containing the initial information about the K-factor MFAI model.
#' @param object_sf A MFAIRSingleFactor containing the information about the newly fitted single factor MFAI model.
#' @param k Integer. Which fator to be updated.
#'
#' @return MFAIR object containing the information about the new fitted single factor.
#' @export
#'
updateMFAIR <- function(object, object_sf, k) {
  # Update the information about the k-factor
  object@Z[, k] <- object_sf@mu
  object@a_sq[, k] <- object_sf@a_sq
  object@W[, k] <- object_sf@nu
  object@b_sq[, k] <- object_sf@b_sq

  object@tau[k] <- object_sf@tau
  object@beta[k] <- object_sf@beta

  object@FX[, k] <- object_sf@FX

  # Save the tree list
  object@tree_lists[[k]] <- append(object@tree_lists[[k]], list(object_sf@tree_list))

  return(object)
}
