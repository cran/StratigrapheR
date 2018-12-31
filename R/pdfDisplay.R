#' @title Generates PDF and SVG figures
#'
#' @description Takes an ensemble of figures, represented by a function g(), and
#' generates a PDF (or SVG if specified). The PDF can be visualised immediatly
#' on the default PDF reader, preferentially one that can be refreshed without
#' closing it (e.g. SumatraPDF, but it only works in Windows).
#'
#' @param g the plot function to be exported and looked at
#' @param width the width of the drawing area (in inches)
#' @param height the height of the drawing area (in inches)
#' @param name the name of the document
#' @param dir the file where the document will be saved (by default the
#' working directory, getwd())
#' @param ext the extension of the document: ".pdf" by default, but
#' ".svg" works also.
#' @param openfile should the pdf file be opened (for the moment works
#' only in Windows). Use SumatraPDF as default pdf reader to be able to write
#' over current file
#' @param pargs list of arguments transmitted to the par() function
#' @param output whether to output the output of g() or not
#' @param warn whether you want to be annoyed by the \code{openfile}
#' warning
#'
#' @details This a fragile function, I believe it is especially sensible to OS
#' changes. The openfile argument will only work in Windows (I'm not even sure
#' it will work in all Windows versions).
#'
#' Also the width and height you provide will not exactly be respected. I could
#' not find a pdf printing function that respects dimensions scrupulously for
#' R base graphics.
#'
#' @return the output of the g() function if output = TRUE
#'
#' @examples
#' g1   <- function() plot(1,1)
#'
#' temp <- tempfile()
#' dir.create(temp)
#'
#' pdfDisplay(g1(),8.27,11.69,
#'            "TestGraph", dir = temp, openfile = TRUE,
#'            pargs = list(mar = c(6,6,6,6), ps = 24,lwd = 4))
#'
#' @importFrom grDevices cairo_pdf dev.off
#' @export

pdfDisplay <- function(g, width, height, name, ext=".pdf", dir = getwd(),
                       openfile = FALSE, pargs = list(), output = F, warn = T)
{
  close <- T

  on.exit(if(close) dev.off())

  filename <- paste(dir, "/", name, ext, sep="")

  cairo_pdf(filename = filename, width = width, height = height,
            bg = FALSE, onefile = TRUE)

  # pdf(file = filename, width = width, height = height, bg = F)

  if(length(pargs) != 0) par(pargs)

  res <- g

  dev.off()

  close <- F

  if(openfile) {

    if(warn){
      warning(paste("The openfile argument should only work in windows ",
                    "(I think): this should be optimised for other OS. ",
                    "Also it works better with SumatraPDF as a default",
                    " PDF reader, but the latter only works in windows",
                    sep = ""))
    }

    system2("open", shQuote(filename))

   }

  if(!is.null(res) & output) return(res)
}

