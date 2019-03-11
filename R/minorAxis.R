#' @title Adds an axis with minor ticks to a plot
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
#' @param range the range of the axis
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
#' plot(c(0,1), c(0,1), axes = FALSE, type = "n", xlab = "", ylab = "")
#'
#' minorAxis(1, n = 10, range = c(0.12,0.61))
#'
#' minorAxis(3, n = 10, extend=FALSE)
#' @export

minorAxis <- function(side, n = NULL, at.maj = NULL, at.min = NULL, range = NULL,
                      tick.ratio = 0.5, labels.maj = TRUE, line = NA, pos = NA,
                      outer = FALSE, font = NA, lty = "solid", lwd = 1,
                      lwd.ticks = lwd, col = NULL, col.ticks = NULL, hadj = NA,
                      padj = NA, extend = TRUE, tcl = NA, ...)
{

  if(side == 1 | side == 3){
    tick.pos <- par("xaxp")
  } else if (side == 2 | side == 4) {
    tick.pos <- par("yaxp")
  }

  # Define the positions of major ticks ----

  if(is.null(at.maj)) {

    # nat.int <- (tick.pos[2] - tick.pos[1])/tick.pos[3]

    at.maj <- seq(tick.pos[1], tick.pos[2],
                  by = (tick.pos[2] - tick.pos[1])/tick.pos[3])

  }

  # Define range, exclude at.maj values if necessary ----

  if(length(range) != 0){

    eff.range <- range

    r1 <- at.maj - min(range)
    r2 <- at.maj - max(range)

    p1 <- which.min(abs(r1))
    p2 <- which.min(abs(r2))

    if(!(abs(r1[p1]/min(range)) <  1.5e-8) & r1[p1] < 0) p1 <- p1 + 1
    if(!(abs(r2[p2]/max(range)) <  1.5e-8) & r2[p2] > 0) p2 <- p2 - 1

    at.maj <- at.maj[p1:p2]

  } else {

    if(side == 1 | side == 3){
      eff.range     <- par("usr")[1:2]
    } else if (side == 2 | side == 4) {
      eff.range     <- par("usr")[3:4]
    }

  }

  # Define limits ----

  if(!extend) {

    if(!is.null(at.min) & length(range) == 0){
      limits <- c(min(c(at.min, at.maj)), max(c(at.min, at.maj)))
    } else {
      limits <- c(min(at.maj), max(at.maj))
    }

  } else {

    limits <- eff.range

  }

  # Standard axis when n and at.min are not given ----

  if(is.null(n) & is.null(at.min)){

    axis(side, at = limits, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lty = lty, lwd = lwd, lwd.ticks = 0,
         col = col,...)

    axis(side, at = at.maj, labels = labels.maj, tick = TRUE, line = line,
         pos = pos, outer = outer, font = font, lty = lty,
         lwd = 0, lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks,
         hadj = hadj, padj = padj, tcl = maj.tcl,...)

  } else {

    # Work the minor ticks: check regularity ----

    mina <- min(at.maj)
    maxa <- max(at.maj)
    difa <- maxa - mina
    na   <- difa / (length(at.maj) - 1)

    if(is.null(at.min))
    {
      # n realm ----

      # Checks----

      sia <- seq(mina,maxa,by = na)

      if(!isTRUE(all.equal(sort(sia),sort(at.maj)))) {
        stop("at.maj is irregular, use at.min for minor ticks (not n)")
      }

      if(!(is.numeric(n) & length(n) == 1)){
        stop("n should be a numeric of length one")
      }

      # Work it ----

      tick.pos <- c(mina,maxa,difa/na)

      nat.int  <- (tick.pos[2] - tick.pos[1])/tick.pos[3]

      # Define the position of minor ticks ----

      distance.between.minor <- nat.int/n

      p <- seq(min(at.maj), max(at.maj), by = distance.between.minor)
      q <- sort(every_nth(p,n,empty=FALSE))

      # Extend outside of major ticks range if necessary ----

      if(!extend) {

        tick.seq <- q

      } else {

        possible.low.minors <- min(at.maj) - (n:1) * distance.between.minor
        possible.hi.minors  <- max(at.maj) + (1:n) * distance.between.minor

        r3 <- possible.low.minors - min(eff.range)
        r4 <- possible.hi.minors  - max(eff.range)

        p3 <- which.min(abs(r3))
        p4 <- which.min(abs(r4))

        if(!(abs(r3[p3]/min(eff.range)) <  1.5e-8) & r3[p3] < 0) p3 <- p3 + 1
        if(!(abs(r4[p4]/max(eff.range)) <  1.5e-8) & r4[p4] > 0) p4 <- p4 - 1

        if(p3 < length(possible.low.minors + 1)){
          low.candidates <- seq(p3, length(possible.low.minors), 1)
          low.laureates  <- possible.low.minors[low.candidates]
        } else {
          low.laureates  <- NULL
        }

        if(p4 > 0){
          hi.candidates  <- seq(1, p4, 1)
          hi.laureates   <- possible.hi.minors[ hi.candidates]
        } else {
          hi.laureates  <- NULL
        }

        tick.seq <- c(low.laureates,q,hi.laureates)

      }

    } else {

      # at.min realm ----

      tick.pos <- c(mina,maxa,na)

      tick.seq <- sort(at.min)

      if(length(range) != 0){

        r3 <- tick.seq - min(eff.range)
        r4 <- tick.seq - max(eff.range)

        p3 <- which.min(abs(r3))
        p4 <- which.min(abs(r4))

        if(!(abs(r3[p3]/min(eff.range)) <  1.5e-8) & r3[p3] < 0) p3 <- p3 + 1
        if(!(abs(r4[p4]/max(eff.range)) <  1.5e-8) & r4[p4] > 0) p4 <- p4 - 1

        tick.seq  <- tick.seq [p3:p4]

      }

    }

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
