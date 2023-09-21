### Code to prepare `ml10m` dataset goes here

library(Matrix)
library(stringr)

raw_data <- tempfile()
download.file(
  "https://files.grouplens.org/datasets/movielens/ml-10m.zip", raw_data
)

## Movie rating data

# UserID::MovieID::Rating::Timestamp
ratings <- read.table(
  text = readLines(unzip(raw_data, "ml-10M100K/ratings.dat")),
  sep = ":", header = FALSE,
  colClasses = c(
    "integer", "NULL", "integer", "NULL",
    "numeric", "NULL", "integer"
  )
)
# head(ratings)
# dim(ratings)
all_UserID <- paste0("user_", sort(unique(ratings[, 1])))
all_MovieID <- paste0("movie_", sort(unique(ratings[, 2])))
ratings[, 1] <- paste0("user_", ratings[, 1])
ratings[, 2] <- paste0("movie_", ratings[, 2])

N <- length(all_UserID)
M <- length(all_MovieID)

rating_matrix <- Matrix::sparseMatrix(
  i = match(ratings[, 1], all_UserID), # Row indices
  j = match(ratings[, 2], all_MovieID), # Column indices
  x = ratings[, 3],
  dims = c(N, M),
  symmetric = FALSE, triangular = FALSE,
  index1 = TRUE
)
# head(rating_matrix)

# # Check missing entries
# sum(colSums(rating_matrix) == 0)
# which(colSums(rating_matrix) == 0)
# sum(rowSums(rating_matrix) == 0)
# which(rowSums(rating_matrix) == 0)

## Movies file description

# MovieID::Title::Genres
movies <- stringr::str_split_fixed(
  readLines(unzip(raw_data, "ml-10M100K/movies.dat")),
  pattern = "::", n = 3
)
# head(movies)
movies[, 1] <- paste0("movie_", movies[, 1])
movies <- movies[movies[, 1] %in% all_MovieID, ]

all_genres <- c(
  "Action", "Adventure", "Animation", "Children's",
  "Comedy", "Crime", "Documentary", "Drama",
  "Fantasy", "Film-Noir", "Horror", "Musical",
  "Mystery", "Romance", "Sci-Fi", "Thriller",
  "War", "Western"
)

# Create the movie-genre matrix for rows (# of movies) and columns (# of genres)
movie_genre_matrix <- t(sapply(
  movies[, 3],
  FUN = function(x) {
    as.integer(all_genres %in%
                 unlist(strsplit(x, split = "|", fixed = TRUE))
    )
  },
  USE.NAMES = FALSE
))
colnames(movie_genre_matrix) <- all_genres

# # Check missing entries
# sum(colSums(movie_genre_matrix) == 0)
# which(colSums(movie_genre_matrix) == 0)
# sum(rowSums(movie_genre_matrix) == 0)
# which(rowSums(movie_genre_matrix) == 0)

ml10m <- list(
  rating = rating_matrix,
  genre = movie_genre_matrix
)

# Save the compressed data
usethis::use_data(ml10m, overwrite = TRUE, compress = "xz")
# # Compress the data
# tools::resaveRdaFiles(paths = "data/ml10m.rda")
