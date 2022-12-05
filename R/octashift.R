#' @title Shifts the order of polygon points
#'
#' @description  Shifts the order of polygon points based on octagon-like
#' reference
#'
#' @param x,y the coordinates of the polygons
#' @param i the identification of the polygons if there are multiple ones
#' @param pos an integer from 1 to 8 identifying a points, based on the
#' formalism of the \code{\link{octapos}} function
#' @param clockwise whether to have the points in the polygon be ordered
#' clockwise (T), counterclockwise (F). If NA (which is the default), this will
#' not be addressed
#'
#' @return a data frame with $x, $y and $i of the polygons as columns
#'
#' @examples
#' xy <- c(-3,-4,-3,0,-1,-2,-1,0,1,2,1,3,4,5,4,3)
#' dt <- c(1,1.5,2,1,1,1.5,2,2,1,1.5,2,1,1,1.5,2,2)
#' id <- c(rep("B1",3), rep("B2",5), rep("B3",3), rep("B4",5))
#'
#' out <- octashift(xy, dt, id, pos = 3, clockwise = TRUE)
#'
#' par(mfrow = c(2,1))
#'
#' plot.new()
#' plot.window(xlim = range(xy) + c(-1, 1), ylim = range(dt) + 0.5 * c(-1, 1))
#'
#' axis(1)
#' axis(2)
#'
#' multilines(i = id, x = xy, y = dt)
#'
#' plot.new()
#' plot.window(xlim = range(xy) + c(-1, 1), ylim = range(dt) + 0.5 * c(-1, 1))
#'
#' axis(1)
#' axis(2)
#'
#' multilines(i = out$i, x = out$x, y = out$y)
#'
#' @export

octashift <- function(x, y, i, pos, clockwise = NA)
{

  df <- data.frame(x = x, y = y, i = i)

  gen <- is.clockwise(x = x, y = y, i = i, get.pos = T)

  clock  <- gen$is.clockwise
  df$is.pos <- gen$pos[,pos,drop = T]

  if(!is.na(clockwise[1])){

    if(length(clockwise) == 1) clockwise <- rep(clockwise, length(unique(i)))

    change <- !(clock == clockwise)

    if(any(change)){

      l.inter  <- split(df, f = df$i)
      reorder  <- match(unique(df$i), names(l.inter))
      l.inter  <- l.inter[reorder]
      l.change <- l.inter[change]

      changed <- lapply(l.change, function(x) x[rev(seq(nrow(x))),])

      l.inter[change] <- changed

      df            <- do.call(rbind, l.inter)
      row.names(df) <- NULL

    }

  }

  if(length(pos) != 1) stop("The pos parameter should be of length 1")

  if(!(pos %in% 1:8)) stop("The pos parameter should be 1,2,3,4,5,6,7, or 8")

  l.pos <- split(df$is.pos, df$i)
  l.p   <- unlist(lapply(l.pos, which))
  l.p   <- l.p[match(unique(df$i), names(l.p))]

  out <- shift(df, i = df$i, p = l.p)

  return(out[,1:3])

}



