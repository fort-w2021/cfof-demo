source("cfof.R")

#' ### Generate some diverse example data:

# simple data where all points have same distance to each other
triangle <- rbind(
  c(sin(0), cos(0)),
  c(sin(2 / 3 * pi), cos(2 / 3 * pi)),
  c(sin(4 / 3 * pi), cos(4 / 3 * pi))
)
# plot(triangle)

set.seed(1212)
two_cluster <- rbind(
  mvtnorm::rmvnorm(50, mean = c(0, 0)),
  mvtnorm::rmvnorm(50, mean = c(10, 10))
)
# plot(two_cluster)

set.seed(1212)
two_cluster_het <- rbind(
  mvtnorm::rmvnorm(50, mean = c(0, 0), sigma = matrix(c(.5, 0, 0, .5), 2)),
  mvtnorm::rmvnorm(50, mean = c(10, 10), sigma = matrix(c(2, 1, 1, 3), 2)),
  c(0, 10),
  c(12, 2)
)

set.seed(1212)
two_cluster_5d <- rbind(
  mvtnorm::rmvnorm(5, mean = rep(0, 20)),
  mvtnorm::rmvnorm(5, mean = rep(10, 20))
)
# plot(two_cluster_5d)

set.seed(1337)
outlier <- rbind(
  mvtnorm::rmvnorm(25, mean = c(0, 0), sigma = matrix(c(1, .9, .9, 1), 2)),
  c(5, 5)
)
plot(outlier)
#' ... similar to Fig 1 in CFOF paper

#-------------------------------------------------------------------------------

#' ### example for code snippets
#' ... used during development of `count_neighborhoods`, e.g.:

distance_matrix <- round(as.matrix(dist(outlier)), 3)
count_neighborhoods()

cfof(outlier, ratio = c(.2, .3))

#-------------------------------------------------------------------------------
#' #### tooling:
#' visualization function for fast checking of correctness/plausibility


# remotes::install_github("thomasp85/patchwork")
library(ggplot2)
library(patchwork)


plot_cfof <- function(data, ratio = .1) {
  plotdata <- cbind(
    data.frame(data),
    cfof(data, ratio = ratio)
  )
  colnames(plotdata) <- c("x1", "x2", "cfof")
  ggplot(plotdata) +
    geom_point(aes(x = x1, y = x2, color = cfof)) +
    scale_color_viridis_c() +
    ggtitle(match.call())
}

plot_cfof(outlier, .1) +
  plot_cfof(outlier, .3) +
  plot_cfof(outlier, .5)

plot_cfof(two_cluster_het, .02) +
  plot_cfof(two_cluster_het, .05) +
  plot_cfof(two_cluster_het, .10) +
  plot_cfof(two_cluster_het, .15)

#-------------------------------------------------------------------------------

#' ### missing
#' - formal unit tests


