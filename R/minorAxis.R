#' @title Adds an axis of with minor ticks to a plot
#'
#' @description Adds an axis with minor ticks to a plot, but with the
#' possibility to have no superposition of minor ticks on major ticks, allowing
#' to export a clean plot in vector format. It is based on the
#' minor.tick function in the Hmisc package.
#'
#' @param side an integer (here 1,2,3 or 4) specifying which side of the
#' plot the axis is to be drawn on. The axis is placed as follows: 1=below,
#' 2=left, 3=above and, 4=right.
#' @param n the number of intervals defined by the minor ticks
#' @param at.maj the positions at which major tick-marks are to be drawn.
#' By default (when NULL) tickmark locations are computed, see the "Details"
#' part in the ?axis help page.
#' @param at.min the positions at which minor tick-marks are to be drawn.
#' This parameter overrides n.
#' @param tick.ratio ratio of lengths of minor tick marks to major tick
#' marks. The length of major tick marks is retrieved from par("tcl") unless
#' specified otherwise.
#' @param labels.maj this can either be a logical value specifying
#' whether (numerical) annotations are to be made at the major tickmarks, or a
#' character or expression vector of labels to be placed at the major
#' tickpoints.
#' @param extend whether to add minor ticks even outside the major ticks
#' (T) or not (F)
#' @param line,pos,outer,font,lty,lwd,lwd.ticks,col,col.ticks,hadj,padj,tcl,...
#' see the ?axis function help page for the other parameters
#'
#' @seealso Set a plot environment with minorAxis: \code{\link{whiteSet}},
#' \code{\link{blackSet}} and \code{\link{greySet}}
#'
#' This function is based on \code{\link{every_nth}}, which suppresses values
#' every multiple of a given number.
#'
#' @examples
#' plot(c(0,1), c(0,1), axes = FALSE, type = "n")
#'
#' minorAxis(1, n = 10)
#'
#' minorAxis(3, n = 10,extend=FALSE)
#' @export

minorAxis <- function(side, n = NULL, at.maj = NULL, at.min = NULL,
                      tick.ratio = 0.75, labels.maj = TRUE, line = NA, pos = NA,
                      outer= FALSE, font = NA, lty = "solid", lwd = 1,
                      lwd.ticks = lwd, col = NULL, col.ticks = NULL, hadj = NA,
                      padj = NA, extend = TRUE, tcl = NA, ...)
{
  # Copy axis() if n and at.min are NA

  if(is.null(n) & is.null(at.min)){

    axis(side, at = at.maj, labels = labels.maj, line = line, pos = pos,
         outer = outer, font = font, lty = lty, lwd = lwd,
         lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks, hadj = hadj,
         padj = padj, tcl = tcl,...)

  } else {

    if(!is.null(at.maj)){

      # Check regularity ----

      mina <- min(at.maj)
      maxa <- max(at.maj)
      difa <- maxa - mina
      na   <- difa / (length(at.maj) - 1)

      sia <- seq(mina,maxa,by = na)

      if(!isTRUE(all.equal(sort(sia),sort(at.maj))) & is.null(at.min)) {
        stop("at.maj is irregular, use at.min for minor ticks")
      }

    }

    # Define range of plot ----

    if(side == 1 | side == 3){
      range    <- par("usr")[1:2]
    } else if (side == 2 | side == 4) {
      range    <- par("usr")[3:4]
    }

    # Define the positions of major ticks ----

    if(is.null(at.min))
    {

      if(!(is.numeric(n) & length(n) == 1)){
        stop("n should be a numeric of length one")
      }

      if(!is.null(at.maj)){

        tick.pos <- c(mina,maxa,difa/na)

      } else {

        if(side == 1 | side == 3){
          tick.pos <- par("xaxp")
        } else if (side == 2 | side == 4) {
          tick.pos <- par("yaxp")
        }

      }

      # Define the position of minor ticks ----

      nat.int <- (tick.pos[2] - tick.pos[1])/tick.pos[3]

      distance.between.minor <- nat.int/n

      p <- seq(tick.pos[1], tick.pos[2], by = distance.between.minor)
      q <- every_nth(p,n,empty=FALSE)

      if(is.null(at.maj)) at.maj <- seq(tick.pos[1], tick.pos[2], nat.int)

      # Extend outside of major ticks range ? ----

      if(!extend) {

        tick.seq <- q

      } else {

        possible.low.minors <- tick.pos[1] - (n:1) * distance.between.minor
        low.candidates <- which(possible.low.minors >= range[1])

        low.laureates <- possible.low.minors[low.candidates]

        possible.hi.minors <- tick.pos[2] + (1:n) * distance.between.minor
        hi.candidates <- which(possible.hi.minors <= range[2])

        hi.laureates <- possible.hi.minors[hi.candidates]

        tick.seq <- sort(c(low.laureates,q,hi.laureates))

      }

    } else {

      tick.pos <- c(mina,maxa,na)

      tick.seq <- at.min

    }

    limits <- c(min(c(tick.pos[1:2],tick.seq)),max(c(tick.pos[1:2],tick.seq)))

    # Define the length of ticks ----

    if(is.na(tcl)) maj.tcl <- par()$tcl else if (!is.na(tcl)) maj.tcl <- tcl

    min.tcl <- maj.tcl*tick.ratio

    # Plot the axes ----

    axis(side, at = limits, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lty = lty, lwd = lwd, lwd.ticks = 0,
         col = col,...)

    axis(side, at = at.maj, labels = labels.maj, tick = TRUE, line = line,
         pos = pos, outer = outer, font = font, lty = lty,
         lwd = 0, lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks,
         hadj = hadj, padj = padj, tcl = maj.tcl,...)

    axis(side, at = tick.seq, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lwd = 0, lwd.ticks = lwd.ticks, col = col,
         col.ticks = col.ticks, tcl = min.tcl,...)

  }
}
