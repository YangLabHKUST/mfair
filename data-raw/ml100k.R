### code to prepare `ml100k` dataset goes here

## Movie rating data
u_data <- read.table("https://files.grouplens.org/datasets/movielens/ml-100k/u.data",
  sep = "\t", header = FALSE
)
# u_data <- read.table(system.file("extdata/ml-100k", "u.data", package = "mfair"),
#   sep = "\t", header = FALSE
# )
# head(u_data)
# dim(u_data)

# range(u_data[, 1])
N <- (range(u_data[, 1]))[2]
# range(u_data[, 2])
M <- (range(u_data[, 2]))[2]

u_data_matrix <- matrix(NA, N, M)
for (i in 1:dim(u_data)[1]) {
  u_data_matrix[u_data[i, 1], u_data[i, 2]] <- u_data[i, 3]
}
# head(u_data_matrix)

## User information
u_user <- read.table("https://files.grouplens.org/datasets/movielens/ml-100k/u.user",
  sep = "|", header = FALSE
)
# u_user <- read.table(system.file("extdata/ml-100k", "u.user", package = "mfair"),
#   sep = "|", header = FALSE
# )
# user id | age | gender | occupation | zip code
# head(u_user)
# dim(u_user)

u_user <- u_user[, -c(1, 5)]
u_user[, 2] <- factor(u_user[, 2])
u_user[, 3] <- factor(u_user[, 3])
colnames(u_user) <- c("Age", "Gender", "Occupation")
# head(u_user)

# Movie genre information
u_item <- read.csv("https://files.grouplens.org/datasets/movielens/ml-100k/u.item",
  sep = "|", header = FALSE
)
# u_item <- read.csv(system.file("extdata/ml-100k", "u.item", package = "mfair"),
#   sep = "|", header = FALSE
# )
# head(u_item)
# dim(u_item)

u_item <- u_item[, c(6:24)]
colnames(u_item) <- c("Unknown", "Action", "Adventure", "Animation", "Children‘s", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

ml100k <- list(rating = u_data_matrix, user = u_user, genre = u_item)

# Save the data
usethis::use_data(ml100k, overwrite = TRUE)
