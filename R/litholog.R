#' @title Create/check lithologs
#'
#' @description Creates and checks basic coordinates of polygons to draw a
#' simple litholog with rectangles
#'
#' @param l,r the height of each delimitation (upper and lower; l and r
#' stand for left and right boundaries of the interval, their order does not
#' matter)
#' @param h the hardness of each bed
#' @param i the id of each bed: it should be different for each bed
#' @param object an R object to test whether it is a litholog, as outputted by
#' the \code{litholog} function
#'
#' @return A table of ids (i), depth (dt) and xy value (i.e. hardness, or simply
#' the x position if your litholog is vertical) of rectangles for each bed. This
#' order of column variable (i, dt, xy) is checked by is.litholog
#'
#' @examples
#' l <- c(1,2,3)  # left boundary of the bed interval (upper or lower)
#' r <- c(0,1,2)  # right boundary of the bed interval (upper or lower)
#' h  <- c(4,3,4) # hardness (arbitrary)
#' i   <- c("B1","B2","B3") # Bed name
#'
#' basic.litholog <- litholog(l,r,h,i) # Generate data frame of the polygons
#'                                     # making the litholog
#'
#' is.litholog(basic.litholog)
#'
#' whiteSet(xlim = c(0,4), ylim = c(0,3), ytick = 1, ny = 10) # Plot background
#' multigons(basic.litholog$i, basic.litholog$xy, basic.litholog$dt) # Draw log
#'
#' @seealso For a more detailed explanation of how to make a litholog:
#' \code{\link{StratigrapheR}}
#'
#' How to prepare the plot background for the litholog: \code{\link{whiteSet}}
#'
#' How to draw the litholog: \code{\link{multigons}}
#'
#' How to add the names of the beds in the litholog: \code{\link{bedtext}}
#'
#' How to plot in pdf: \code{\link{pdfDisplay}}
#'
#' To add personalised boundaries between beds: \code{\link{weldlog}}
#'
#' To have open beds at the extremities of the log. More generaly to transform a
#' polygon into a polyline and control the part that is not drawn:
#' \code{\link{multilines}} and \code{\link{shift}}
#'
#' To add details and drawings: \code{\link{centresvg}} and
#' \code{\link{framesvg}}
#'
#' Go further with interval data (between two boundaries, as there often is
#' in stratigraphy): \code{\link{as.lim}} and related functions.
#'
#' Complementary functions: \code{\link{infobar}} and \code{\link{ylink}}
#' @importFrom dplyr arrange
#' @export


litholog <- function(l, r, h, i)
{
  i <- as.character(i)

  d <- data.frame(i, l, r, h, stringsAsFactors = F)
  d["k"] <- 1:nrow(d)

  na <- c(which(is.na(l)), which(is.na(r)), which(is.na(h)),
          which(is.na(i)))

  if(length(na) > 0) d <- d[-na,]

  d["dta"] <- d$l
  d["xya"] <- rep(0,nrow(d))

  d["dtb"] <- d$l
  d["xyb"] <- d$h

  d["dtc"] <- d$r
  d["xyc"] <- d$h

  d["dtd"] <- d$r
  d["xyd"] <- rep(0,nrow(d))

  da <- d[c(1,5,6,7)]
  db <- d[c(1,5,8,9)]
  dc <- d[c(1,5,10,11)]
  dd <- d[c(1,5,12,13)]

  cn  <- c("i","gloVar.k","dt","xy")

  colnames(da) <- cn
  colnames(db) <- cn
  colnames(dc) <- cn
  colnames(dd) <- cn

  da["p"] <- "A"
  db["p"] <- "B"
  dc["p"] <- "C"
  dd["p"] <- "D"

  df <- rbind(da,db,dc,dd)

  df <- arrange(df,gloVar.k,desc(p))

  res <- df[,-c(2,5)]

  return(res)
}

#' @rdname litholog
#' @export

is.litholog <- function(object)
{

  ret <- T

  if(!inherits(object, "data.frame")) {

    if(ret) return(F) else print(F)

  }

  if(ncol(object) != 3) {

    if(ret) return(F) else print(F)

  }

  if(!identical(names(object), c("i", "dt", "xy"))){

    if(ret) return(F) else print(F)

  }

  if(nrow(object) == 0){

    if(ret) return(T) else print(T)

  }

  if(!(inherits(object$dt, "numeric") | inherits(object$dt, "integer")) |
     !(inherits(object$xy, "numeric") | inherits(object$xy, "integer")) |
     !(inherits(object$i, "numeric") | inherits(object$i, "integer") | inherits(object$i, "character"))){

    if(ret) return(F) else print(F)

  }

  if(ret) return(T) else print(T)

}







