#' Compute naive version of "concentration-free outlier factor"
#'
#' The concentration-free outlier factor is the smallest neighborhood size k in
#' terms of "k nearest neighbors" for which the object is a neighbor of at least
#' a certain ratio of other objects. "Loosely speaking, it corresponds to a
#' measure of how many nearest neighbors have to be taken into account in order
#' for a point to be close to a sufficient fraction of the data." (Angiulli,
#' 2019) See reference below for details.
#'
#' @param data a data.frame or matrix. rows are instances, columns are features.
#'   factor variables are turned into dummy variables and then treated as
#'   numeric features.
#' @param ratio proportion of data set size that the k-neighborhood membership
#'   count has to exceed. (can be a vector, must be greater than 1/nrow(data))
#' @param distance what type of distance to compute. defaults to euclidean, see
#'   ?dist
#' @param rescale_data center and scale all data columns to have mean 0, sd 1?
#' @return a matrix that contains, for each row in data, "the smallest
#'   neighborhood size k' for which the object is a neighbor of at least
#'   n*`ratio`` other objects". Rows are data instances, columns correspond to
#'   different ratios.
#' @references
#'   Angiulli, F. (2019)
#'   CFOF: A Concentration Free Measure for Anomaly Detection
#'   https://arxiv.org/pdf/1901.04992.pdf
#' @author Fabian Scheipl
cfof <- function(data,
                 ratio = 0.1,
                 distance = c("euclidean", "maximum", "manhattan"),
                 rescale_data = TRUE) {

  # input checks & homogenization
  data_prepped <- prepare_data(data, rescale_data)
  n_data <- nrow(data_prepped)
  checkmate::assert_numeric(
    ratio,
    # k = 1 and k = n are not informative, so put outside range for ratio:
    lower = 2 / n_data, upper = 1 - 1 / n_data,
    any.missing = FALSE, min.len = 1
  )
  distance <- match.arg(distance)

  #-------------------------------------------------------

  distance_matrix <- as.matrix(stats::dist(data_prepped, method = distance))

  counts <- count_neighborhoods(distance_matrix)

  cfof_matrix <- vapply(ratio,
    FUN = get_cfof,
    FUN.VALUE = double(n_data),
    counts = counts
  )
  colnames(cfof_matrix) <- paste0("cfof_", ratio)

  cfof_matrix / n_data
}

# homogenize data input and check suitability.
# !! converts factor variables in data.frames into dummy variables !!
prepare_data <- function(data, rescale_data) {
  if (inherits(data, "data.frame")) {
    data <- stats::model.matrix(~ -1 + ., data)
  }
  checkmate::assert_matrix(data, any.missing = FALSE, min.rows = 1, min.cols = 1)
  checkmate::assert_flag(rescale_data)

  if (rescale_data) {
    data <- scale(data)
  }
  data
}


#' @return n x n matrix with entries i,j: "how many neighborhoods of size j is
#'   object i in"
count_neighborhoods <- function(distance_matrix) {
  # for each row x in data, for each k, how many k-neighborhoods is x part of
  # x is in the k-neighborhood of y if it has one of the k smallest distances to y
  # count for how many y this is true
  # --> compute **row**-wise ranks of distances,
  # then count, for each column, in **how many** rows the rank is at most k

  # need to transpose result of apply so that rows represent the row-wise ranks!!
  distance_ranks <- t(
    apply(distance_matrix,
      MARGIN = 1,
      FUN = rank,
      ties.method = "random"
    )
  )
  # ties = "max", "average" would result in systematically too small
  # neighborhoods (ranks too high)
  # ties = "min" in systematically too large neighborhoods (ranks too low)
  # ties = "first" would yield smaller neighborhoods for objects higher up in
  # data set.

  n <- nrow(distance_ranks)
  # matrix rows correspond to x, columns to k:
  counts <- vapply(seq_len(n),
    FUN = function(k) colSums(distance_ranks <= k),
    FUN.VALUE = double(n)
  )
  counts
}

#' find minimal neighborhood size k so that counts of neighborhoods is at least
#' ratio * nrow(data)
#' @param counts output of `count_neighborhoods`
get_cfof <- function(ratio, counts) {
  threshold <- ratio * ncol(counts)
  apply(counts,
    MARGIN = 1,
    FUN = function(this_row) which(this_row >= threshold)[1]
  )
}
