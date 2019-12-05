#' @title Find the "next" or "previous" values in a matrix.
#'
#' @description Find the "next" or "previous" values in a matrix.
#'
#' @param m a matrix of values
#' @param n a positive integer of length 1, giving the number of
#' positions to lead or lag by
#' @param default value used for non-existent rows. Defaults to NA.
#'
#' @examples
#' m <- matrix(1:120, ncol = 12)
#'
#' mat.lag(m)
#' mat.lead(m)
#'
#' @export
#' @importFrom dplyr lag lead

mat.lag <- function(m, n = 1L, default = NA)
{
  m <- as.matrix(m)

  if(n >= 0 & n < 1) return(m)

  col <- ncol(m)

  res <- lag(m, n, default = default)

  replace <- matrix(rep(default, n * col), ncol = col)

  res[1:n,] <- replace

  return(res)
}

#' @rdname mat.lag
#' @export

mat.lead <- function(m, n = 1L, default = NA)
{
  m <- as.matrix(m)

  if(n >= 0 & n < 1) return(m)

  col <- ncol(m)
  row <- nrow(m)

  res <- lead(m, n, default = default)

  replace <- matrix(rep(default, n * col), ncol = col)

  res[(row - n + 1):row,] <- replace

  return(res)
}

