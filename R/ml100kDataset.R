#' MovieLens 100K dataset.
#'
#' A list containing the movie rating, user information, and movie genres of the MovieLens 100K dataset.
#'
#' @format A list containing a movie rating matrix, a user information data frame, and a movie genres data frame.
#' \describe{
#'   \item{rating}{Movie rating matrix of 1,682 movies and 943 users (0–5 star rating).}
#'   \item{user}{A data frame containing the information of 943 users, the three columns correspond to age, gender, and occupation respectively.}
#'   \item{genre}{A binary data frame containing the genre information of 1,682 movies, each column corresponds to one specific genre.}
#' }
#' @source \url{https://grouplens.org/datasets/movielens/100k/}
#' @name ml100k
"ml100k"
