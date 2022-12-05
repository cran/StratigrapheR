#' @title Check (bedding) joint objects
#'
#' @description Check whether a data.frame complies with the criteria to be a
#' valid bedding joint to be integrated in a litholog.
#'
#' @param joint the data.frame to test
#' @param warn whether to have a warning explaining why the candidate joint
#' is invalid
#'
#' @examples
#' # Plots for visualisation ----
#'
#' opar <- par("mfrow")
#' par(mfrow = c(2,1))
#'
#' plot.new()
#' plot.window(xlim = range(oufti99$'1sin'$x),
#'             ylim = range(oufti99$'1sin'$y))
#' title("oufti99$'1sin'")
#' placesvg(oufti99$'1sin')
#'
#' plot.new()
#' plot.window(xlim = range(oufti99$ammonite$x),
#'             ylim = range(oufti99$ammonite$y), asp = 1)
#' title("oufti99$ammonite")
#' placesvg(oufti99$ammonite)
#'
#' par(mfrow = opar)
#'
#' # Exemplification of is.joint ----
#'
#' is.joint(oufti99$'1sin')
#'
#' is.joint(oufti99$ammonite)
#'
#' @export

is.joint <- function(joint, warn = F)
{

  ret <- T

  if(!inherits(joint, "data.frame")){

    if(isTRUE(warn)) warning("Joint should be a data.frame")

    if(ret) return(F) else print(F)

  }

  if(!(inherits(joint[,1], "numeric") | inherits(joint[,1], "integer") |
       inherits(joint[,2], "numeric") | inherits(joint[,2], "integer"))){

    if(isTRUE(warn)) {
      warning(paste0("Joint should be a data.frame with the ",
                     "first two columns made of integers or numerics"))
    }

    if(ret) return(F) else print(F)

  }

  if(is.pointsvg(joint)) {

    if(length(unique(joint$id)) > 1){

      if(isTRUE(warn)) warning("Joint should be made of only one polyline")

      if(ret) return(F) else print(F)

    }

    if(any(joint$type != "L")){

      if(isTRUE(warn)) warning("Joint should be made of only one polyline")

      if(ret) return(F) else print(F)

    }

  }

  first.line <- joint[1,]
  last.line  <- joint[nrow(joint),]

  max.x <- max(joint[,1])
  min.x <- min(joint[,1])

  if(first.line$x == last.line$x){

    if(isTRUE(warn)) {
      warning(paste0("The first and last points of the joint should",
                     " not have the same x value"))
    }

    if(ret) return(F) else print(F)

  }

  if(!(first.line[1,1] == max.x | first.line[1,1] == min.x)){

    if(isTRUE(warn)){
      warning(paste0("The first and last points of the joint should",
                     " not have the same x value"))
    }

    if(ret) return(F) else print(F)

  }

  if(!(last.line[1,1] == max.x  | last.line[1,1] == min.x)){

    if(isTRUE(warn)){
      warning(paste0("The first and last points of the joint should ",
                     "be at the left or right extreme"))
    }

    if(ret) return(F) else print(F)

  }

  if(max(joint[,2]) == min(joint[,2])){

    if(isTRUE(warn)){
      warning("The joint should not be a straight horizontal line")
    }

    if(ret) return(F) else print(F)

  }

  if(ret) return(T) else print(T)

}






