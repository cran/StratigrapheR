#' @title Change the dimensions of bedding joints
#'
#' @param joint the bedding joint to be modified
#' @param yinv,xinv whether to inverse the plotting for x and y values (T or F)
#' @param yleft,yright the depth/height/time value for the extreme point at the
#' right or left of the joint (yleft overruns yright, which overruns ymin and
#' ymax)
#' @param ymin,ymax the extreme values for the y axis (in case of conflict with
#' yleft and/or yright, defaults to the smallest exaggeration)
#' @param xmin,xmax the extreme values for the x axis
#'
#' @examples
#' # Create an initial litholog ----
#'
#' l <- c(-2,-1,0,1,2)
#' r <- c(-1,0,1,2,3)
#' h   <- c(4,3,4,3,4)
#' i <- c("B1","B2","B3","B4","B5")
#' log  <- litholog(l, r, h, i)
#'
#' # Get a custom bedding joint to specific dimensions using changejoint() ----
#'
#' liq <- changejoint(oufti99$liquefaction,
#'                    yleft = 0, ymax = 0.3,
#'                    xmin = 1, xmax = 2)
#'
#' nlog <- weldlog(log, dt = 0, seg = list(liq = liq), j = c("liq"))
#'
#' # Plots for visualisation ----
#'
#' plot.new()
#' plot.window(xlim = c(0,5), ylim = c(-2,3))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(nlog$i, nlog$xy, nlog$dt)
#'
#' @export

changejoint <- function(joint,
                        yinv = F, xinv = F,
                        yleft = NA, yright = NA,
                        ymin = NA, ymax = NA,
                        xmin = NA, xmax = NA)
{

  # checks ----

  if(!is.joint(joint))stop("This is not an appropriate 'joint' object")

  lengths <- lapply(list(yleft = yleft, yright = yright,
                         ymin = ymin, ymax = ymax,
                         xmin = xmin, xmax = xmax),
                    length)

  if(any(unlist(lengths) != 1)){
    stop(paste0("The 'yleft', 'yright', 'ymin', 'ymax', 'xmin' and 'xmax' ",
                "parameters should be of length 1"))
  }

  test.num <- function(x) {
    !(inherits(x, "integer") | inherits(x, "numeric") | is.na(x))
  }

  if(test.num(yleft))  stop("The 'yleft' parameter should be numeric, integer or NA")
  if(test.num(yright)) stop("The 'yright' parameter should be numeric, integer or NA")
  if(test.num(ymin)) stop("The 'ymin' parameter should be numeric, integer or NA")
  if(test.num(ymax)) stop("The 'ymax' parameter should be numeric, integer or NA")
  if(test.num(xmin)) stop("The 'xmin' parameter should be numeric, integer or NA")
  if(test.num(xmax)) stop("The 'xmax' parameter should be numeric, integer or NA")

  if(isTRUE(yleft > ymax)) stop("yleft should be smaller or equal to ymax")
  if(isTRUE(yleft < ymin)) stop("yleft should be greater or equal to ymin")
  if(isTRUE(yright > ymax)) stop("yright should be smaller or equal to ymax")
  if(isTRUE(yright < ymin)) stop("yright should be greater or equal to ymin")

  if(is.na(xmin)) xmin <- min(joint[,1])
  if(is.na(xmax)) xmax <- max(joint[,1])

  if(isTRUE(ymin >= ymax)) stop("ymin should be smaller than ymax")
  if(isTRUE(xmin >= xmax)) stop("xmin should be smaller than xmax")

  # Invert joint if required ----

  if(isTRUE(xinv)) joint[,1] <- -joint[,1]
  if(isTRUE(yinv)) joint[,2] <- -joint[,2]

  # Determine parameter for framesvg ----

  n <- nrow(joint)

  if(!is.na(yright) | !is.na(yleft)){

    initial.ymin <- min(joint[,2])
    initial.ymax <- max(joint[,2])
    range.x      <- c(joint[1,1], c(joint[n, 1]))

    left.is.min <- which.max(range.x) == 2

    if(left.is.min){

      initial.yleft  <- joint[1,2]
      initial.yright <- joint[n,2]

    } else {

      initial.yleft  <- joint[n,2]
      initial.yright <- joint[1,2]

    }

  }

  if(!is.na(yright) & is.na(yleft)) {

    yside         <- yright
    initial.yside <- initial.yright

  } else if(!is.na(yleft)) {

    yside         <- yleft
    initial.yside <- initial.yleft

  }

  if(!is.na(yright) | !is.na(yleft)){

    norm.ymax <- initial.ymax - initial.yside
    norm.ymin <- initial.ymin - initial.yside

    exa.up   <- (ymax - yside)/(norm.ymax)
    exa.down <- (ymin - yside)/(norm.ymin)

    if(is.na(exa.up) & is.na(exa.down)){

      exa <- 1

    } else {

      if(is.na(exa.up)){
        exa <- exa.down
      } else if(is.na(exa.down)){
        exa <- exa.up
      } else if(abs(exa.up) < abs(exa.down)){
        exa <- exa.up
      } else {
        exa <- exa.down
      }

    }

    if(is.infinite(exa)) exa <- 1

    ymax.output <- ((initial.ymax - initial.yside) * abs(exa)) + yside
    ymin.output <- ((initial.ymin - initial.yside) * abs(exa)) + yside

  } else {

    ymax.output <- ymax
    ymin.output <- ymin

  }

  # Change the joint ----

  res <- framesvg(joint,
                  xmax = xmax, xmin = xmin,
                  ymax = ymax.output, ymin = ymin.output,
                  standard = T, keep.ratio = F,
                  plot = F, output = T)

  return(res)

}


