  # Sketch: implementing the naive version of https://arxiv.org/pdf/1901.04992.pdf

# Step 1
cfof <- function(data, ratio = .1, distance = "euclidean") {
  # check data, ratio
  # get reverse neighborhood sizes for each datum for each k
  # for each datum, find minimal k* so that it is in at least ratio * nrow(data)
  #    of the k*-neighborhoods
  # return data and k*/n
  # possibly: do this for more than 1 ratio-value at once?
}

#--------------------------------------------------------------------------------

# Step 2

cfof <- function(data, ratio = .1, distance = "euclidean") {
  # check data, ratio
  neighborhood_sizes <- get_neighborhood_sizes(data, distance = distance)
  # for each datum, find minimal k* so that neighborhood size is at least ratio * nrow(data)
  # return data and k* (possibly: for more than 1 ratio-value)
}

get_neighborhood_sizes <- function(data, distance) {
  # get data distances
  # for each row x in data
  #   for each k
  #     how many k-neighborhoods is x part of?
  # return matrix (n x max(k)) with neighborhood sizes
}

#--------------------------------------------------------------------------------

# Step 3:
# figure out computation of reverse neighborhood sizes,
# write down your reasoning

cfof <- function(data, ratio = .1, distance = "euclidean") {
  # check data, ratio
  neighborhood_sizes <- get_neighborhood_sizes(data, distance = distance)
  # for each datum, find minimal k so that neighborhood size is at least ratio * nrow(data)
}

get_neighborhood_sizes <- function(data, distance) {
  distance_matrix <- distance_function(data, distance = distance)
  # for each row x in data, for each k, how many k-neighborhoods is x part of?
  # x is in the k-neighborhood of y if it has one of the k smallest distances to y
  # count for how many y this is true
  # --> compute row-wise ranks of distances,
  #     then count in each column k how often rank is <= k
  distance_ranks <- apply(distance_matrix, MARGIN = 1, FUN = rank)
  n <- nrow(data)
  # matrix: rows correspond to x, columns to k:
  neighborhood_sizes <- matrix(NA_integer_, nrow = n, ncol = n)
  for (k in seq_len(n)) {
    # sum of logical vector == no of TRUE entries in vector
    neighborhood_sizes[, k] <- colSums(distance_ranks <= k)
  }
  neighborhood_sizes
}

#--------------------------------------------------------------------------------

# Step 4:
# - refactor: encapsulate distance computation outside neighboorhood size computation

cfof <- function(data, ratio = .1, distance = "euclidean") {
  # check data, ratio
  distance_matrix <- get_distance_matrix(data, distance = distance)
  neighborhood_sizes <- get_neighborhood_sizes(distance_matrix)
  # for each datum, find minimal k so that neighborhood size is at least ratio * nrow(data)
}

get_neighborhood_sizes <- function(distance_matrix) {
  # for each row x in data, for each k, how many k-neighborhoods is x part of
  # x is in the k-neighborhood of y if it has one of the k smallest distances to y
  # count for how many y this is true
  # --> compute row-wise ranks of distances,
  #     then count how often rank is at most k in each column k
  distance_ranks <- apply(distance_matrix, 1, rank)
  n <- nrow(distance_matrix)
  # matrix: rows correspond to x, columns to k:
  neighborhood_sizes <- matrix(NA_integer_, nrow = n, ncol = n)
  for (k in seq_len(n)) {
    neighborhood_sizes[, k] <- colSums(distance_ranks <= k)
  }
  neighborhood_sizes
}

get_distance_matrix <- function(data, distance) {
  # determine distance_function based on distance
  # compute distance matrix
}

#--------------------------------------------------------------------------------
# Step 5:
# - write down specs for inputs, outputs (could/should have done that sooner),
# but usually can't do it at once, first have to understand problem a bit better...
# - implement input checks
# - should also at this point, at the latest, have some code snippets in
# a separate file to check that the code actually works and does the right thing

#' Compute naive version of "concentration-free outlier factor"
#' @param data a data.frame or matrix. rows are instances, columns are features.
#' @param ratio proportion of data set size that the k-neighborhood membership
#'   count has to exceed. (can be a vector)
#' @param distance what type of distance to compute. defaults to euclidean
#' @return a matrix that contains, for each row in data, "the smallest
#'   neighborhood size k′for which the object is a neighbor of at least n*ratio
#'   other objects". rows are data instances, columns correspond to ratios.
cfof <- function(data, ratio = .1, distance = c("euclidean", "manhattan", "cosine")) {
  data <- prepare_data(data)
  checkmate::assert_numeric(
    ratio,
    lower = 0, upper = 1, any.missing = FALSE, min.len = 1
  )
  distance <- match.arg(distance)

  # check data, ratio
  distance_matrix <- get_distance_matrix(data, distance)
  neighborhood_sizes <- get_neighborhood_sizes(distance_matrix)
  # for each ratio:
  #   for each datum:
  #     find minimal k so that neighborhood size is at least ratio * nrow(data)
}

# @param distance_matrix
# @return
get_neighborhood_sizes <- function(distance_matrix) {
  # for each row x in data, for each k, how many k-neighborhoods is x part of
  # x is in the k-neighborhood of y if it has one of the k smallest distances to y
  # count for how many y this is true
  # --> compute row-wise ranks of distances, then check columns how often rank is <= k
  distance_ranks <- apply(distance_matrix, 1, rank)
  n <- nrow(distance_matrix)
  # matrix: rows correspond to x, columns to k:
  neighborhood_sizes <- matrix(NA_integer_, nrow = n, ncol = n)
  for (k in seq_len(n)) {
    neighborhood_sizes[, k] <- colSums(distance_ranks <= k)
  }
  neighborhood_sizes
}

get_distance_matrix <- function(data, distance) {
  distances <-
    switch(distance,
      euclidean = dist(data, method = "euclidean"), # built in
      manhattan = dist(data, method = "manhattan"), # built in
      cosine = dist_cosine(data)
    )
  # dist() returns special "dist"-object instead of matrix, so homogenize output:
  as.matrix(distances)
}

# cosine distance: d(u, v) = 1 - u' v / (||u||_2 ||v||_2)
# yields 0 for parallel, 1 for orthogonal, 2 for diametral.
dist_cosine <- function(data) {
  # data contains *row* vectors, so need crossproduct of transposed data:
  crossprods <- tcrossprod(data)
  lengths <- sqrt(rowSums(data^2))
  norms <- outer(lengths, lengths)
  1 - crossprods / norms
  # alternative: 1 - t(crossprods / lengths) / lengths # no need for norms...
}

# homogenize data input and check suitability
# things to worry about here:
#   - missing values (disallow completely / exclude incomplete columns?)
#   - dealing with non-numeric variables (convert to numeric / disallow completely?)
#   - empty inputs with 0 rows or columns (disallow / abort and return empty object?)
# things to consider in documentation / for more refined implementation:
#   - distance won't work well for data on different scales --> standardize/rescale?
#   - warn about factor variables being converted to integer
prepare_data <- function(data) {
  checkmate::assert_multi_class(data, c("data.frame", "matrix"))
  data <- data.matrix(data)
  checkmate::assert_matrix(data, any.missing = FALSE, min.rows = 1, min.cols = 1)
  data
}

#--------------------------------------------------------------------------------
# Further steps:

# a) Step back to think about failure modes, weaknesses of the design above,
# code smells etc. The earlier you do so, the easier things are to repair. E.g.:
# - creating n-by-n matrices (distance_matrix, neighborhood_sizes)
#   very bad if data is big -- can this be avoided?
# - cosine distance = speculative generality?
#   (distance arg / get_distance_matrix in general might be
#   unnecessary, since dist has enough options ..?)
# Refactor to improve.
#
# b) set up tooling:
# write simple, obvious data examples / tests
# - where you KNOW DEFINITELY what the output should be for the given inputs
# - for *all* the functions that are already (close to being) executable
# write visualization functions for fast iteration, easy checking during further
# development
#
# c) flesh out further, based on failing tests etc (obviously...)

