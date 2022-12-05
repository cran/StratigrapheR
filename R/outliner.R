#' @title Find the points of a litholog that are along a given vertical line
#'
#' @description Through interpolation, this function identifies all the points
#' of a litholog that are at a given intensity value, along a vertical line.
#'
#' @param log a "litholog()"-like data frame
#' @param xy the intensity value for the vertical line
#' @param add whether to have the interpolated points added to the litholog
#'
#' @return a data frame of the extracted vertical line or the litholog with
#' points along this line wherever the log intersects the vertical line, with
#' its i (bed identification), dt (depth/time), and xy (intensity).
#'
#' @examples
#' l <- c(1,2,3,4,5)  # left boundary of the bed interval (upper or lower)
#' r <- c(0,1,2,3,4)  # right boundary of the bed interval (upper or lower)
#' h <- c(4,3,5,3,4) # hardness (arbitrary)
#' i <- c("B1","B2","B3","B4","B5") # Bed name
#'
#' olog <- litholog(l,r,h,i) # Generate data frame of the polygons
#'                           # making the litholog
#'
#' log <- weldjoint(olog, c(1 ,4, 5), oufti99,
#'                  sym = c("1sin", "liquefaction", "1sin"),
#'                  ymax  = c(NA, 0.2, 0.2),
#'                  xmin  = c(0, 1, 0),
#'                  xmax  = c(4, 1.5, 3))
#'
#' log_line <- outliner(log, 2)
#'
#' plot.new()
#' plot.window(xlim = c(0,5), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(log$i, log$xy, log$d)
#'
#' points(log_line$xy, log_line$dt, pch = 19, col = "red")
#'
#' @export

outliner <- function(log, xy, add = F)
{

  if(!is.litholog(log)) stop("This is not an appropriate 'litholog' object")

  if(!(inherits(xy, "numeric") | inherits(xy, "integer"))) {
    stop("The 'xy' parameter should be numeric or integer")
  }

  wlog <- log

  first_line <- !duplicated(wlog$i)
  last_line  <- rev(!duplicated(rev(wlog$i)))

  nl <- sum(last_line)

  ref_seq <- seq(nrow(wlog) + nl)

  relast.pos <- which(first_line) + seq(nl) - 1

  norm_seq             <- rep(0, length(ref_seq))
  norm_seq[relast.pos] <- 1

  ref_seq2             <- ref_seq - cumsum(norm_seq)
  ref_seq2[relast.pos] <- which(last_line)

  wlog <- wlog[ref_seq2,]

  low   <- which(wlog$xy < xy)
  equal <- which(wlog$xy == xy)
  high  <- which(wlog$xy > xy)

  wlog$pos <- NA

  if(length(low) != 0)   wlog$pos[low]   <- -1
  if(length(equal) != 0) wlog$pos[equal] <- 0
  if(length(high) != 0)  wlog$pos[high]  <- 1

  wlog$initial_order <- seq(nrow(wlog))

  wlog$across_lag <- (wlog$pos == 1 & lag(wlog$pos == -1) & wlog$i == lag(wlog$i)) |
    (wlog$pos == -1 & lag(wlog$pos == 1) & wlog$i == lag(wlog$i))

  wlog$across_ref <- lead(wlog$across_lag)

  wlog$across_lag[is.na(wlog$across_lag)]  <- F
  wlog$across_ref[is.na(wlog$across_ref)]  <- F

  across1 <- wlog[wlog$across_ref,]
  across2 <- wlog[wlog$across_lag,]

  if(nrow(across1) != 0){

    across <- rbind(across1, across2)[seq_mult(2 * nrow(across1), 2, inv = T), ]

    # Nombre impair de points.

    across$group <- rep(seq(nrow(across)/2), each = 2)

    approx_list <- split(across, f = across$group)

    approx_fun <- function(df) approx(x = df$xy, y = df$dt, xout = xy)$y
    list_test  <- function(df) length(unique(df$i)) == 1

    of2 <- !duplicated(across$group)

    name_interpol <- across$i[of2]

    interpol <- data.frame(i = across$i[of2], xy = rep(xy, length(approx_list)))

    interpol$is.1bed <- unlist(lapply(approx_list, list_test))

    approx_list <- approx_list[interpol$is.1bed]
    interpol    <- interpol[interpol$is.1bed,]

    interpol$dt <- unlist(lapply(approx_list, approx_fun))

    interpol$initial_order <- (across$initial_order[of2] +
                                 across$initial_order[!of2]) / 2

    df1 <- interpol[,c(1,2,4,5)]

  } else {

    df1 <- data.frame(matrix(nrow = 0, ncol = 4))

    names(df1) <- c("i", "dt", "xy", "initial_order")

  }

  flog <- wlog[!norm_seq,c(1,2,3,5)]

  if(isTRUE(add)){

    out <- rbind(df1, flog)

  } else {

    df2 <- flog[flog$xy == xy,]

    out <- rbind(df1, df2)

  }

  out <- out[order(out$initial_order),]

  row.names(out) <- NULL

  res <- out[,c(1,3,2)]

  return(res)

}






