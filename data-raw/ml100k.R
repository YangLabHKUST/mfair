### code to prepare `ml100k` dataset goes here

## Movie rating data
u_data <- read.table(
  "https://files.grouplens.org/datasets/movielens/ml-100k/u.data",
  sep = "\t", header = FALSE
)
# user id | item id | rating | timestamp
# head(u_data)
# dim(u_data)

# User, range(u_data[, 1])
N <- (range(u_data[, 1]))[2]
# Movie, range(u_data[, 2])
M <- (range(u_data[, 2]))[2]

u_data_matrix <- Matrix::sparseMatrix(
  i = u_data[, 1], j = u_data[, 2],
  x = u_data[, 3],
  dims = c(N, M),
  symmetric = FALSE, triangular = FALSE,
  index1 = TRUE,
  repr = "C"
)
# head(u_data_matrix)

## User information
u_user <- read.table(
  "https://files.grouplens.org/datasets/movielens/ml-100k/u.user",
  sep = "|", header = FALSE
)
# user id | age | gender | occupation | zip code
# head(u_user)
# dim(u_user)

u_user <- u_user[, -c(1, 5)]
u_user[, 2] <- factor(u_user[, 2])
u_user[, 3] <- factor(u_user[, 3])
colnames(u_user) <- c("Age", "Gender", "Occupation")
# head(u_user)
# dim(u_user)

# Movie genre information
u_item <- read.csv(
  "https://files.grouplens.org/datasets/movielens/ml-100k/u.item",
  sep = "|", header = FALSE
)
# head(u_item)
# dim(u_item)

# all_genres <- read.csv(
#   "https://files.grouplens.org/datasets/movielens/ml-100k/u.genre",
#   sep = "|", header = FALSE
# )
# all_genres

u_item <- u_item[, c(6:24)]
colnames(u_item) <- c(
  "Unknown", "Action", "Adventure", "Animation",
  "Children‘s", "Comedy", "Crime", "Documentary",
  "Drama", "Fantasy", "Film-Noir", "Horror",
  "Musical", "Mystery", "Romance", "Sci-Fi",
  "Thriller", "War", "Western"
)
# head(u_item)
# dim(u_item)

ml100k <- list(
  rating = u_data_matrix,
  user = u_user,
  genre = u_item
)

# Save the compressed data
usethis::use_data(ml100k, overwrite = TRUE, compress = "xz")
# # Compress the data
# tools::resaveRdaFiles(paths = "data/ml100k.rda")
