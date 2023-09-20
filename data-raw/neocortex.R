### Code to prepare `neocortex` dataset goes here

# Bulk gene expression data (microarray platform) used in the MFAI paper

# Set the path for raw data
RAW_DATA_PATH <- "data-raw"

library(googledrive)
library(tidyverse)

## Download the raw data from Google drive

drive_deauth()
drive_user()
public_file <- drive_get(as_id("1iFN98KTFXFkJKhf4Zc3721fxTdzk2fQO"))
drive_download(public_file,
  overwrite = TRUE,
  path = paste0(RAW_DATA_PATH, "/all_exp.RData")
)
public_file <- drive_get(as_id("1XhHLSdt-A7xFFjdhX5GrT84aT6qrD0Mn"))
drive_download(public_file,
  overwrite = TRUE,
  path = paste0(RAW_DATA_PATH, "/gene_symbol.rda")
)

# Load gene expression data matrix
load(paste0(RAW_DATA_PATH, "/all_exp.RData"))
# Column names contain tissue sample information: sampleID | brainRegion | LeftorRightbrain
head(colnames(all_exp))
# The first row represents the time stage
head(all_exp[1, ])
# Each remaining row represents one gene
head(rownames(all_exp[-1, ]))

# Load gene symbol
load(paste0(RAW_DATA_PATH, "/gene_symbol.rda"))
# Gene symbols match with the rows in the gene expression data matrix
head(gene_symbol)
rownames(all_exp)[-1] <- gene_symbol

## Exclude period 1&2 since most of the 16 brain regions sampled in future periods have not differentiated.

idx <- which((all_exp[1, ] != 1) & (all_exp[1, ] != 2))
exp_ex_period12 <- all_exp[-1, idx]
stage_ex_period12 <- all_exp[1, idx]

## Extract sample information

sample_info_ex_period12 <- colnames(exp_ex_period12)
sample_info_ex_period12 <- strsplit(sample_info_ex_period12, split = "_")
sample_info_ex_period12 <- t(sapply(sample_info_ex_period12,
  FUN = function(x) {
    x
  }
))
sample_info_ex_period12 <- as.data.frame(sample_info_ex_period12)
colnames(sample_info_ex_period12) <- c("ID", "Region", "Hemisphere")
sample_info_ex_period12 <- cbind(sample_info_ex_period12, Stage = stage_ex_period12)

# There are 17 brain regions in total
length(unique(sample_info_ex_period12[, "Region"]))
unique(sample_info_ex_period12[, "Region"])

# There are 13 time periods in total
length(unique(sample_info_ex_period12[, "Stage"]))
unique(sample_info_ex_period12[, "Stage"])

## Select 11 neocortex areas

neocortex_areas <- c("OFC", "DFC", "VFC", "MFC", "M1C", "S1C", "IPC", "A1C", "STC", "ITC", "V1C")
idx <- sample_info_ex_period12[, "Region"] %in% neocortex_areas
exp_preprocess <- t(exp_ex_period12[, idx])
# head(exp_preprocess)
sample_info_preprocess <- sample_info_ex_period12[idx, ]
sample_info_preprocess[, "Region"] <- factor(sample_info_preprocess[, "Region"],
  levels = neocortex_areas
)
sample_info_preprocess[, "Hemisphere"] <- factor(sample_info_preprocess[, "Hemisphere"],
  levels = c("L", "R")
)
# head(sample_info_preprocess)

## Select genes with reproducible spatial patterns (differential stability)

# Convert long table to wide table
long2wide <- function(x) {
  dat <- tibble::as_tibble(data.frame(
    ID = sample_info_preprocess[, "ID"],
    Region = sample_info_preprocess[, "Region"],
    Exp = x
  ))
  dat %>%
    pivot_wider(
      names_from = "Region",
      values_from = "Exp",
      values_fn = function(x) {
        mean(x, na.rm = TRUE) # Take the average if multiple values
      }
    ) %>%
    select(-ID) -> dat

  return(dat)
}

# Please note that this step can take several minutes
exp_wide <- apply(exp_preprocess, MARGIN = 2, FUN = long2wide)
# head(exp_wide[[1]])

# Function used to compute the differential stability
computeDS <- function(x, method) {
  cor_mat <- cor(t(x), method = method, use = "pairwise.complete.obs")
  # Averaged correlation
  ds <- mean(cor_mat[lower.tri(cor_mat, diag = FALSE)], na.rm = TRUE)
  return(ds)
}
# methods_all <- c("pearson", "kendall", "spearman")
ds <- sapply(exp_wide, FUN = computeDS, method = "pearson")

# Select the top 2,000 genes
gene_idx <- which(rank(-ds) <= 2000)

neocortex <- list(
  expression = exp_preprocess[, gene_idx],
  sample_info = sample_info_preprocess
)

# Save the compressed data
usethis::use_data(neocortex, overwrite = TRUE, compress = "xz")
# # Compress the data
# tools::resaveRdaFiles(paths = "data/neocortex.rda")
