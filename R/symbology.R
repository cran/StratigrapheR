#' @title Draws the symbols of a collection
#'
#' @description Draws all the required symbols from a collection of them
#'
#' @param collection a collection object (e.g. oufti)
#' @param sym the name of the symbols in the collection
#' @param x,y numeric vectors of coordinates where the object should be
#' drawn.
#' @param xfac the x size factor.
#' @param yfac the y size factor.
#' @param xadj value specifying the x adjustment of the drawing.
#' @param yadj value specifying the y adjustment of the drawing.
#' @param col the polygons background color. If density is specified with
#' a positive value this gives the color of the shading lines.
#' @param border the lines color.
#' @param density the density of shading lines, in lines per inch. The
#' default value of NULL means that no shading lines are drawn.
#' @param angle the slope of shading lines, given as an angle in degrees
#' (counter-clockwise)
#' @param lty,lwd the border line type and width, see ?par for details.
#' @param scol,slty,slwd the colour, type and width of the shading lines.
#'
#' @seealso Similar functions: \code{\link{centresvg}}, \code{\link{framesvg}}
#' and \code{\link{placesvg}}
#'
#' Collections available in StratigrapheR: \code{\link{oufti99}}
#'
#' @examples
#' # Create a data frame for all the required information ----
#'
#' a <- data.frame(name = c("ammonite", "marcassite",
#'                          "nodule.point", "ammonite"),
#'                 x = c(1,3,5,1),
#'                 y = c(1,3,5,5),
#'                 col = c(NA, "grey90",
#'                         "grey50", "grey90"))
#'
#' # Draw them all in a single line of code ----
#'
#' plot.new()
#' plot.window(xlim = c(0,6), ylim = c(0, 6))
#'
#' axis(1)
#' axis(2, las = 1)
#'
#' symbology(oufti99, a$name, a$x, a$y, col = a$col)
#'
#' @export

symbology <- function(collection, sym, x, y,
                      xfac = 1,
                      yfac = 1,
                      xadj = 0,
                      yadj = 0,
                      col = NA,
                      border = "black",
                      density = NA,
                      angle = 45,
                      lty = par("lty"),
                      lwd = par("lwd"),
                      scol = border,
                      slty = lty,
                      slwd = lwd)
{

  if(!is.collection(collection)) {
    stop("This is not an appropriate 'collection' object")
  }

  sym.in.coll <- names(collection)

  in.coll <- sym %in% sym.in.coll

  if(!all(in.coll)) {

    not.sym <- sort(unique(sym[!in.coll]))

    warning("The following 'sym' symbols are not in the collection: \n - ",
            paste(not.sym, collapse = "\n - "))

  }

  homo <- homogenise(l = list(sym = sym,
                              x = x,
                              y = y,
                              xfac = xfac,
                              yfac = yfac,
                              xadj = xadj,
                              yadj = yadj,
                              col = col,
                              border = border,
                              density = density,
                              angle = angle,
                              lty = lty,
                              lwd = lwd,
                              scol = scol,
                              slty = slty,
                              slwd = slwd))

  df <- as.data.frame(homo)

  adf <- df[in.coll,]

  acoll <- collection[match(adf$sym, sym.in.coll)]

  l.adf <- split(adf, seq(nrow(adf)))

  svg_fun <- function(svg, args)
  {
    centresvg(object = svg,
              x = args$x,
              y = args$y,
              xfac = args$xfac,
              yfac = args$yfac,
              xadj = args$xadj,
              yadj = args$yadj,
              col = args$col,
              border = args$border,
              density = args$density,
              angle = args$angle,
              lty = args$lty,
              lwd = args$lwd,
              scol = args$scol,
              slty = args$slty,
              slwd = args$slwd)
  }

  mapply(svg_fun, acoll, l.adf)

  return(invisible())

}





