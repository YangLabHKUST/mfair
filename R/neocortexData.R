#' Human brain gene expression data.
#'
#' A list containing the bulk gene expression of human brain and tissue sample information.
#'
#' @format A list containing a gene expression matrix and a tissue sample information data frame.
#' \describe{
#'   \item{expression}{Bulk gene expression matrix of 886 tissue samples and 2,000 genes with highest differential stability.}
#'   \item{sample_info}{A data frame containing the information of 886 bulk tissue samples, the four columns correspond to sample ID, neocortex area, hemisphere, and time periods respectively.}
#' }
#' @source \url{https://hbatlas.org/pages/data}
#' @name neocortex
"neocortex"
