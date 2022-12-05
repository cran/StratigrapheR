#' @title Create a list of symbols
#'
#' @description From a file containing SVG files, extracts all the SVGs into
#' a list of symbols that can be used in lithologs.
#'
#' @param dir the file directory where the SVG files are
#' @param collection an object similar to the output of collection()
#' @param col the background colour of the symbols
#' @param cex the size of the text in the plot
#' @param as.pdf whether to output the plot as a pdf
#' @param name the name of the pdf document to plot the symbols of a collection
#' @param dir the file where the document will be saved (by default a temporary
#'  directory, tempdir())
#' @param ext the extension of the document: ".pdf" by default, but
#' ".svg" works also.
#' @param width the width of the drawing area (in inches)
#' @param height the height of the drawing area (in inches)
#' @param track whether to generate different files for each rerun of pdfDisplay
#' with identical 'name'. The name will be followed by '_(i)' where i is the
#' version number. With this you avoid closing your pdf file at each rerun if
#' your pdf reader is not able to deal with (to my knowledge only SumatraPDF is
#' able)
#' @param openfile should the pdf file be opened (for the moment works
#' only in Windows). Use SumatraPDF as default pdf reader to be able to write
#' over current file
#'
#' @return a collection is a list of pointsvg-objects
#' (see \code{\link{pointsvg}})
#'
#' @examples
#' # To show you how to import, we first have to have a svg file to import. The
#' # following lines of code will create a svg in a temporary files:
#'
#' # 1. Create temporary file
#' svg.file.directory <- tempfile(pattern = "ammonite",
#'                                fileext = ".svg")
#' # 2. Write the svg in the file
#' writeLines(example.ammonite.svg, svg.file.directory)
#'
#' print(paste("An example .svg file was created at ", svg.file.directory,
#'             sep = ""))
#'
#' coll <- collection(dirname(svg.file.directory))
#'
#' is.collection(coll)
#'
#' \dontrun{
#' plot_collection(coll, cex = 1.5)
#'
#' plot_collection(oufti99, name = "Oufti99")}
#'
#' @export

collection <- function(dir = getwd())
{

  files <- list.files(path = dir, pattern = ".\\.svg$")

  nc <- nchar(files)

  names <- substr(files, 1, nc-4)

  files.dir <- paste(dir, files, sep = "/")

  list.coll <- lapply(files.dir, pointsvg, keep.ratio = T)

  names(list.coll) <- names

  ll <- lapply(list.coll, nrow)

  if(any(ll == 0)){

    no.fig <- names[which(ll == 0)]

    warning("The following svg is made of no usable figure: \n - ",
            paste0(no.fig, ".svg", collapse = "\n - "))

  }

  return(list.coll)

}

#' @rdname collection
#' @export

is.collection <- function(collection)
{

  ret <- T

  if(!inherits(collection, "list")) {

    if(ret) return(F) else print(F)

  }

  if(!all(unlist(lapply(collection, is.pointsvg)))){

    if(ret) return(F) else print(F)

  }

  return(T)

}

#' @rdname collection
#' @export

plot_collection <- function(collection, col = "grey90", cex = 2,
                            as.pdf = T, name = "symbols",
                            ext = ".pdf", dir = tempdir(),
                            width = 7, height = 0.8 * width / 5.6,
                            track = T, openfile = T)
{

  if(!is.collection(collection)) {
    stop("This is not an appropriate 'collection' object")
  }

  l <- length(collection)

  opar <- par("mar")

  on.exit(par(mar = opar))

  nam <- names(collection)

  xrange <- unlist(lapply(collection, function(x) max(x$x) - min(x$x)))
  yrange <- unlist(lapply(collection, function(x) max(x$y) - min(x$y)))

  exa <- xrange / yrange

  exa.base <- 6

  exa[exa > exa.base] <- exa.base

  exa[exa < 1] <- 1


  g <- function()
  {

    par(mar = c(0,0,0,0))

    layout(matrix(seq_len(l), l, 1, byrow = TRUE))

    for(i in seq_len(l)){

      nlegend(t = nam[i], xt = 3.8, xmin = -3.2, xmax = 8, ymax = 1,
              asp = 1, cex = cex)

      centresvg(collection[[i]], 0, 0,
                xfac = exa[i]/2, yfac = exa[i]/2,
                col = col)

    }

  }

  if(isTRUE(as.pdf)){

    pdfDisplay(g(), name = name, ext = ext, dir = dir,
               width = width, height = l * height,
               track = track, openfile = openfile)

  } else {

    g()

  }

}





