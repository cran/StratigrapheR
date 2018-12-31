#' @title Scatterplot Matrices for large dataset
#'
#' @description Makes a matrix of plots with a given data set of large
#' size using hexagonal binning (similar result than the pairs function)
#'
#' @param dat data frame of all parameters to be compared
#' @param bins numeric vector giving number of bins in both vertical and
#' horizontal directions. Set to 60 by default.
#' @param base_size size of the base texts (axis, titles, ...)
#' @param cor_size size of the text for the correlation coefficient. Has
#' different units (?) than base_size
#' @examples
#' fastPairs(iris[1:4],bins=10)
#'
#' @import ggplot2
#' @importFrom GGally ggpairs putPlot ggally_cor
#' @export

fastPairs <- function(dat, bins = 60, base_size = 12, cor_size = 6)
{
  theme_set(theme_gray(base_size = base_size))

  p <- ggpairs(dat, lower="blank", upper = "blank")
  seq <- 1:ncol(dat)
  for (x in seq)
    for (y in seq)
      if (y > x)
        p <- putPlot(p, ggplot(dat, aes_string(x=names(dat)[x],
                                               y=names(dat)[y]))
                     + geom_hex(bins=bins), y, x)
  p

  for (x in seq)
    for (y in seq)
      if (x > y)
        p <- putPlot(p, ggally_cor(dat,
                                   aes_string(x=names(dat)[x],
                                              y=names(dat)[y]),
                                   size = cor_size), y, x)
  p
}

