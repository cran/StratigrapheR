#' @title Converts line, rect, polygon and polyline class SVG objects into
#' data frames
#'
#' @description Converts 'line', 'rect', 'polygon' and 'polyline' class SVG
#' objects into data frames. \strong{ONLY THESE CLASSES OF OBJECTS CAN BE
#' IMPORTED.} If you have bezier or spline curves, they will be stored as 'path'
#' class objects that cannot be imported here. The same goes for 'rect' objects
#' that are transformed (rotation, etc...).
#'
#' @param file a .svg file
#' @param standard whether to standardise (centre to (0,0), rescale so
#' that extreme points are at -1 and 1) (T or F)
#' @param keep.ratio if the object is to be  standardised, whether to
#' keep the x/y ratio (T or F)
#' @param round whether to round the coordinates (T or F)
#' @param xdigits the number of digits after the decimal to round to for
#' x values
#' @param ydigits the number of digits after the decimal to round to for
#' y values
#' @param xinverse whether to inverse the plotting for x values (T or F)
#' @param yinverse whether to inverse the plotting for y values (T or F)
#' @param warn whether you want to be annoyed
#' @return A data.frame with x and y coordinates, ids for each object, and a
#' type, either line (L) or polygon (P)
#' @details This function is quite empirical. There is no guarantee it is bug
#' free. If you have .svg files that should work but do not, you can email me:
#' \email{sébastien.wouters@@doct.uliege.be}
#' @seealso Plot the drawing: \code{\link{placesvg}},
#'
#' Plot the drawing and change the coordinates :\code{\link{centresvg}} and
#' \code{\link{framesvg}}
#'
#' Change the drawing: \code{\link{changesvg}} and \code{\link{clipsvg}}
#' @examples
#' # To show you how to import, we first have to have a svg file to import. The
#' #following lines of code will create a svg in a temporary files:
#'
#' svg.file.directory <- tempfile(fileext = ".svg") # Creates temporary file
#' writeLines(example.ammonite.svg, svg.file.directory) # Writes svg in the file
#'
#' print(paste("An example .svg file was created at ", svg.file.directory,
#'             sep = ""))
#'
#' # The pointsvg function allows to import simple svg drawings into R
#'
#' ammonite.drawing <- pointsvg(file = svg.file.directory) # Provide file
#'
#' plot(c(-2,2), c(-2,2), type = "n")
#'
#' placesvg(ammonite.drawing)
#'
#' # If you want to import your own .svg file uncomment the following line:
#'
#' # pointsvg(file.choose())
#'
#' @importFrom stringr str_match str_match_all
#' @importFrom utils read.table
#' @export

pointsvg <-function(file, standard = TRUE, keep.ratio = FALSE, round = TRUE,
                    xdigits = 4, ydigits = 4, xinverse = FALSE, yinverse = TRUE,
                    warn = T)
{
  if(!isTRUE(warn) | !isTRUE(warn)) stop("Argument 'warn' should be T or F")

  a <- readLines(file)

  line <- grep(a, pattern = ".*<.* class=.*")
  type <- gsub(".*<\\s*| class=.*", "", a[line])

  able <- c("polygon", "polyline", "line", "rect")

  loc.miss <- !(type %in% able)

  if(any(loc.miss)) {

    type.miss <- unique(type[loc.miss])
    line.miss <- line[loc.miss]

    warning(paste("Elements of the .svg file are of the following class(es): ",
                  paste(type.miss, collapse = ", "), " at line(s) ",
                  paste(line.miss, collapse = ", "), " in the .svg file",
                  ". These cannot be uploaded in R with this function.",
                  " Use a vector graphics editor to change these elements and",
                  " generate point-based objects only (polygon, polyline, ",
                  "line and rect). You can open your .svg file with a text ",
                  "editor to get familiar with .svg files and classes. To ",
                  "convert curvy objects to ",
                  "polygon or polyline the general idea is to select all the ",
                  "nodes of your object in a vector graphics editor. Then ",
                  "to add nodes between the initial nodes, and to convert the ",
                  "lines between these new nodes to lines. We advise: ",
                  "\n  - for CorelDRAW users to look for the 'shape', 'add ",
                  "nodes' and 'convert to lines' tools. \n  - for Inkscape ",
                  "users to look for the 'node', 'insert new node' and 'make ",
                  "selected segments lines' tools. \n  - for Adobe users to ",
                  "look for the 'convert anchor point' and 'straight lines'",
                  " tools",sep = ""))
  }

  find <- type %in% able

  gline <- line[find]
  gtype <- type[find]

  tran <- grep(a, pattern = ".*transform=.*")

  if(length(tran) != 0){

    tran.match <- match(tran, gline)

    if(any(!is.na(tran.match))){

      tran.type <- unique(gtype[tran.match])

      warning(paste("Some of the elements of the following type were ",
                    "transformed (rotation, translation, ...): ",
                    paste(tran.type, collapse = ", "), ", at line(s) ",
                    paste(gline[tran.match], collapse = ", "),
                    ". They cannot be imported using this function. We advise ",
                    "to convert these objects into polygons using a vector ",
                    "graphics editor.",sep = ""))

    }

    gline <- gline[-tran.match]
    gtype <- gtype[-tran.match]

  }

  # Extract objects ----

  accu           <- data.frame(matrix(ncol = 4,nrow = 0))
  colnames(accu) <- c("x","y","i","type")

  # j <- 1

  for(j in seq_len(length(gline)))
  {

    ji <- gline[j]
    ti <- gtype[j]

    tline <- a[ji]



    if(ti == 'line'){

      # LINE ----

      pd  <- unlist(str_match_all(tline,"[a-zA-Z0-9]+= *\"-*[0-9]+\""))

      line.id <- sub("= *\"[0-9]+\"", "", pd)

      if(warn & !all(c("x1", "y1", "x2", "y2") %in% line.id)){

        warning(paste("Strange format detected for 'line' class object. ",
                      "If this leads to incorrect output, the best you can do ",
                      "is to identify the 'line' object at line ", ji,
                      " in the .svg file. The abnormality is the absence of",
                      " at least one of the arguments 'x1', 'y1', 'x2' or ",
                      "'y2'. You can modify the object in a vector graphics  ",
                      "editor to fit a format this function can understand. ",
                      "You can also send the maintainer an email explaining ",
                      "the problem and providing the problematic .svg file to ",
                      "help improve StratigrapheR.", sep = ""))

      }

      if(warn & !all(line.id %in% c("x1", "y1", "x2", "y2"))){

        warning(paste("Strange format detected for 'line' class object. ",
                      "If this leads to incorrect output, the best you can do ",
                      "is to identify the 'line' object at line ", ji,
                      " in the .svg file. The abnormalities are arguments ",
                      "other than 'x1', 'y1', 'x2' and 'y2'. You can ",
                      "modify the object in a vector graphics editor to fit a ",
                      "format this function can understand. You can also send ",
                      " the maintainer an email explaining the problem and ",
                      "providing the problematic .svg file to help improve ",
                      "StratigrapheR.", sep = ""))

      }

      line.xy <- as.numeric(gsub("\"", "", str_match(pd, "\"[0-9]+\"")))
      line.xy <- as.list(line.xy)

      names(line.xy) <- line.id

      pe <- data.frame(x = c(line.xy$x1, line.xy$x2),
                       y = c(line.xy$y1, line.xy$y2))

      tr <- "L"

    } else if (ti == "polygon" | ti == "polyline"){

      # POLY -line and -gon ----

      poly.t <- as.character(str_match(tline, "points= *\"[[0-9]+,[0-9]+ ]*\""))

      if(warn & length(poly.t) == 0){

        warning(paste("Strange format detected for ", ti, " class object. ",
                      "If this leads to incorrect output, the best you can do ",
                      "is to identify the object at line ", ji,
                      " in the .svg file. The abnormality is the lack of the ",
                      "argument 'points' providing the x and y coordinates in ",
                      "a 'points=\"X1,Y1 X2,Y2 \"' format. You can modify the ",
                      "object in a vector graphics editor to fit a format ",
                      "this function can understand. You can also send the ",
                      "maintainer an email explaining the problem and ",
                      "providing the problematic .svg file to help improve ",
                      "StratigrapheR.", sep = ""))

      }

      pd <- unlist(str_match_all(tline,"-*[0-9]+,-*[0-9]+"))
      pe <- read.table(text=pd,sep=",")
      colnames(pe) <- c("x","y")

      if(ti == "polygon") tr <- "P" else if(ti == "polyline") tr <- "L"

    } else if (ti == "rect"){

      # RECT ----

      pd  <- unlist(str_match_all(tline,"[a-zA-Z0-9]+= *\"-*[0-9]+\""))

      rect.id <- sub("= *\"[0-9]+\"", "", pd)

      if(warn & !all(c("x", "y", "height", "width") %in% rect.id)){

        warning(paste("Strange format detected for 'rest' class object. ",
                      "If this leads to incorrect output, the best you can do ",
                      "is to identify the 'rect' object at line ", ji,
                      " in the .svg file. The abnormalities are arguments ",
                      "other than 'x', 'y', 'height' and 'width'. You can ",
                      "modify the object in a vector graphics editor to fit a ",
                      "format this function can understand. You can also send ",
                      " the maintainer an email explaining the problem and ",
                      "providing the problematic .svg file to help improve ",
                      "StratigrapheR.", sep = ""))

      }

      if(warn & !all(rect.id %in% c("x", "y", "height", "width"))){

        warning(paste("Strange format detected for rect class object. ",
                      "If this leads to incorrect output, the best you can do ",
                      "is to identify the rect object at line ", ji,
                      " in the .svg file. The abnormalities are arguments ",
                      "other than 'x', 'y', 'height' and 'width'. You can ",
                      "modify the object in a vector graphics editor to fit a ",
                      "format this function can understand. You can also send ",
                      " the maintainer an email explaining the problem and ",
                      "providing the problematic .svg file to help improve ",
                      "StratigrapheR.", sep = ""))

      }

      rect.xy <- as.numeric(str_match(pd, "[0-9]+"))
      rect.xy <- as.list(rect.xy)

      names(rect.xy) <- rect.id

      x1 <- rect.xy$x
      x2 <- rect.xy$x + rect.xy$width
      y1 <- rect.xy$y
      y2 <- rect.xy$y + rect.xy$height

      pe <- data.frame(x = c(x1, x1, x2, x2), y = c(y1, y2, y2, y1))

      tr <- "P"

    }

    pn  <- nrow(pe)

    pid <- data.frame(id = rep(paste(tr,j,sep = ""),pn), type = rep(tr,pn),
                      stringsAsFactors = F)

    pf           <- cbind(pe,pid)
    accu <- rbind(accu,pf)

  }

  # Correct if necessary ----

  res <- changesvg(accu, standard = standard, keep.ratio = keep.ratio,
                   round = round, xdigits = xdigits, ydigits = ydigits,
                   xinverse = xinverse, yinverse = yinverse)

  return(res)

}
