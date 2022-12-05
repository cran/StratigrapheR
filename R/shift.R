#' @title Circular shift
#'
#' @description Circular shift; the order of points will be lagged as if the
#' beginning is preceded by the end.
#'
#' @param x a vector (characters, numerics, integers,...), data.frame or list
#' @param i a vector of ids to divide x in various groups; the shift will occur
#' on the subgroups defined by identical i ids.
#' @param n a positive integer of length 1, giving the number of
#' positions to shift by (positive values generate lag)
#' @param p the index position or row that will become the first one
#' (overrides n)
#' @param names whether the names of the elements or rows should also shift
#' @return the same object than the input, but with a shifted order
#' @examples
#' # Simple use ----------------------------------------------------------------
#'
#' shift(x = c(6,8,10,12,2,4), n = 2)
#'
#' #> [1]  2  4  6  8 10 12
#'
#' vector        <- rep(1:4, 3)
#' names(vector) <- rep(c("P1", "P2", "P3"), each = 4)
#' split(vector, f = names(vector))
#'
#' #> $P1
#' #> P1 P1 P1 P1
#' #>  1  2  3  4
#' #>
#' #> $P2
#' #> P2 P2 P2 P2
#' #>  1  2  3  4
#' #>
#' #> $P3
#' #> P3 P3 P3 P3
#' #>  1  2  3  4
#'
#' sh <- shift(x = vector, i = names(vector), p = c(1,2,3))
#' split(sh, f = names(sh))
#'
#' #> $P1
#' #> P1 P1 P1 P1
#' #>  1  2  3  4
#' #>
#' #> $P2
#' #> P2 P2 P2 P2
#' #>  2  3  4  1
#' #>
#' #> $P3
#' #> P3 P3 P3 P3
#' #>  3  4  1  2
#'
#' # Applications to litholog generation ---------------------------------------
#'
#' l <- c(1,2,3)
#' r <- c(0,1,2)
#' h  <- c(4,3,4)
#' i   <- c("B1","B2","B3")
#'
#' basic.litholog <- litholog(l,r,h,i)
#'
#' whiteSet(xlim = c(0,4), ylim = c(0,3),
#'          xaxs = "r", yaxs = "r",   # This gives a little room to the graph
#'          ytick = 1, ny = 10)
#'
#' multigons(basic.litholog$i, basic.litholog$xy, basic.litholog$dt,
#'           forget = "B1", lwd = 2)
#'
#' openbed <- subset(basic.litholog, basic.litholog == "B1")
#'
#' openbed <- shift(openbed, -1)
#'
#' lines(openbed$xy, openbed$dt, lwd = 2)
#'
#' @importFrom dplyr lead lag
#' @export

shift <- function(x, n = 1L, p = - n + 1L, i = NA, names = T)
{

  n <- - p + 1

  internal_shift <- function(x, n, names = names)
  {

    if(inherits(x, "data.frame")) {

      len.s <- nrow(x)
      namex <- rownames(x)

    } else {

      len.s <- length(x)
      namex <- names(x)

    }

    s <- seq_len(len.s)

    n.shift <- fmod(n,len.s)

    shifted   <- lag(s, n.shift)
    shift.inv <- lead(s, len.s - n.shift)

    shifted[is.na(shifted)] <- shift.inv[!is.na(shift.inv)]

    if(inherits(x, "data.frame")) {
      res <- x[shifted,,drop = FALSE]
    } else {
      res <- x[shifted]
    }

    if(!is.null(namex) & isTRUE(names)){

      if(inherits(x, "data.frame")) {
        rownames(res) <- namex[shifted]
      } else {
        names(res) <- namex[shifted]
      }

    } else {

      if(inherits(x, "data.frame")) {
        rownames(res) <- namex
      } else {
        names(res) <- namex
      }

    }

    return(res)

  }

  if(!is.na(i[1])){

    if(any(is.na(i))) stop("If 'i' is provided there should be no NA value")

    efx <- split(x, f = i)

    reorder1 <- match(unique(i), names(efx))

    efx <- efx[reorder1]

    if(inherits(x, "data.frame")) len.s <- nrow(x) else len.s <- length(x)

    reorder2 <- order(unlist(split(seq(len.s), f = i)[reorder1]))

    lu <- length(unique(i))

    if(length(n) == 1) n <- rep(n, lu)

    if(length(n) != lu) {
      stop(paste0("The 'n' and 'p' parameter should be of length 1, or of same ",
                  "length than the unique amount of elements in i (", lu, ")"))
    }

    out1 <- mapply(internal_shift, efx, n, names = names, SIMPLIFY = F)

    if(inherits(x, "data.frame")) {

      names.all <- unlist(lapply(out1, row.names))

      out2 <- do.call(rbind, out1)

      rownames(out2) <- names.all

      output <- out2[reorder2,,drop = F]

    } else {

      names.all <- unlist(lapply(out1, names))

      out2 <- unlist(out1)

      names(out2) <- names.all

      output <- out2[reorder2]

    }


  } else {

    if(length(n) != 1) {
      stop("If 'i' is not provided, 'n' and 'p' should be of length 1")
    }

    output <- internal_shift(x, n, names = names)

  }

  return(output)

}
