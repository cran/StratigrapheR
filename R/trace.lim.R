#' @title Visualize lim objects
#'
#' @description Visualize lim objects as lines for each interval. The lines are
#' time series with the dt (depth/time) being the boundaries of the interval,
#' and an xy intensity is defined as values attributed to the interval.
#'
#' @param lim a list of n left (1st element) and n right (2ndt element)
#' interval limits, of n interval IDs, and of n interval boundary rules (e.g.
#' "[]").
#' @param l the left interval limits (numerical vector of length n).
#' @param r the right interval limits (numerical vector of length n).
#' @param id the interval IDs (numerical or character vector of length n,
#' the default is 1 for each interval). They can be similar for different
#' intervals.
#' @param b the interval boundaries rules: "[]"
#' (or "closed") to include both boundaries points, "][" (or "()" and "open") to
#' exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
#' include only the left boundary point, and "]]" (or "(]", "left-open",
#' "right-closed") to include only the right boundary point. The notation is
#' simplified to "[]", "[[", "]]" and "][" only.
#' @param xy the intensity attributed to each interval.
#' @param order whether to order the intervals.
#' @param decreasingly whether the order to set is decreasing.
#' @param output whether to output the results.
#' @param plot whether to plot the results.
#' @param dt the boundaries of the intervals as provided by \code{trace.lim}.
#' @param int the id of each interval as provided by \code{trace.lim}.
#' @param include whether the boundaries of the intervals are included in them,
#' as provided by \code{trace.lim}.
#' @param link whether to link all the intervals into one line.
#' @param point whether to add points to the boundaries of each interval.
#' @param style the style of the lines.
#' @param close the style of the points for closed boundaries.
#' @param open the style of the points for open boundaries.
#' @param add whether to add the plot to an existing plot.
#' @param hz whether dt stands for the horizontal axis (the x axis, i.e., the
#' abscissa): in that case it is set a TRUE (this is the default value). Any
#' other value will associate dt with the vertical axis (y axis, i.e., the
#' ordinate)
#' @param gen general parameters for \code{\link{plot}}
#'
#' @return \code{trace.lim} returns a list of 'dt' values (dt stands for
#' depth/time, which corresponds to the boundaries of intervals), 'xy' values
#' (the "intensity" of each interval), 'int' which is an id for each interval,
#' id which is the ids defined in the lim objects (these ids can be similar for
#' different intervals, and therefore define groups of intervals), and 'include'
#' which are boolean (T/F) values whether a boundary of the interval is included
#' in the interval.
#'
#' @seealso generalities on lim data: \code{\link{as.lim}}
#'
#' @examples
#' lim <- as.lim(l = c(0,6,4,6,50), r = c(1,5,6,9,8),
#'               b = c("[[", "]]", "[[", "]]", "[["))
#'
#' xy <- c(1,2,3,4,5)
#'
#' trace <- trace.lim(lim = lim, xy = xy, plot = FALSE)
#'
#' trace
#'
#' plot_lim(dt = trace$dt, xy = trace$xy,
#'          int = trace$int, include = trace$include)
#'
#' @export

trace.lim <- function(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]", xy = 0,
                      order = F, decreasingly = F,
                      output = T, plot = T,
                      link = F, point = T,
                      style = list(),
                      close = list(pch = 19),
                      open = list(pch = 21, bg = "white"),
                      add = F, gen = list(xlab = "dt", ylab = "xy"))
{

  lim <- as.lim(lim = lim, l = l, r = r, id = id, b = b)

  if(isTRUE(order)) lim <- order.lim(lim)

  df <- as.data.frame(lim)

  df$xy <- xy

  df$int <- seq(nrow(df))

  df.l <- df
  df.r <- df

  df.l$dt <- df.l$l
  df.r$dt <- df.r$r

  df.l$include <- F
  df.l$include[df.l$b == "[[" | df.l$b == "[]"] <- T

  df.r$include <- F
  df.r$include[df.r$b == "]]" | df.r$b == "[]"] <- T

  df2 <- rbind(df.l, df.r)

  df3 <- df2[seq_mult(l = nrow(df2), mult = 2, inv = T),]

  out <- list(dt = df3$dt, xy = df3$xy, int = df3$int,
              id = df3$id, include = df3$include)

  if(isTRUE(plot)){

    plot_lim(dt = out$dt, xy = out$xy, int = out$int, include = out$include,
             link = link, point = point,
             style = style, open = open, close = close,
             add = add, gen = gen)

  }

  if(isTRUE(output)) return(out)

}

#' @rdname trace.lim
#' @export

plot_lim <- function(dt, xy, int, include,
                     link = F, point = T,
                     style = list(),
                     close = list(pch = 19),
                     open = list(pch = 21, bg = "white"),
                     add = F, hz = T, gen = list(xlab = "dt", ylab = "xy"))
{

  if(!isTRUE(add)){

    if(isTRUE(hz)){
      plot.list <- merge_list(list(x = 1, y = 1, type = "n"),
                              gen,
                              list(xlim = range(dt), ylim = range(xy)),
                              list(xlab = "dt", ylab = "xy"))
    } else {
      plot.list <- merge_list(list(x = 1, y = 1, type = "n"),
                              gen,
                              list(xlim = range(xy), ylim = range(dt)),
                              list(xlab = "xy", ylab = "dt"))
    }

    do.call(plot, plot.list)

  }

  if(!isTRUE(link)){

    if(isTRUE(hz)){
      para.line <- merge_list(list(i = int,x = dt, y = xy), style)
    } else {
      para.line <- merge_list(list(i = int,x = xy, y = dt), style)
    }

    do.call(multilines, para.line)

  } else {

    if(isTRUE(hz)){
      para.line <- merge_list(list(x = dt, y = xy), style)
    } else {
      para.line <- merge_list(list(x = xy, y = dt), style)
    }

    do.call(lines, para.line)

  }

  if(isTRUE(point)){

    if(isTRUE(hz)){
      pos.close <- list(x = dt[include], y = xy[include])
      pos.open  <- list(x = dt[!include], y = xy[!include])
    } else {
      pos.close <- list(x = xy[include], y = dt[include])
      pos.open  <- list(x = xy[!include], y = dt[!include])
    }

    para.close <- merge_list(pos.close, close, list(pch = 19))
    para.open  <- merge_list(pos.open, open, list(pch = 21, bg = "white"))

    do.call(graphics::points, para.close)
    do.call(graphics::points, para.open)

  }

}



