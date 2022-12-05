#' @title Changes boundaries segments in basic lithologs
#'
#' @description Adds personalised segments to bed boundaries of lithologs from
#' "litholog()"-like data frames
#'
#' @param log a "litholog()"-like data frame on which the new segments
#' need to be welded.
#' @param dt the position of the n boundaries to change.
#' @param collection a collection object (e.g. oufti)
#' @param sym the name of the symbols in the collection. This should be a symbol
#' that can be considered as a bedding joint; see \code{\link{is.joint}}.
#' @param yinv,xinv whether to inverse the plotting for x and y values (T or F)
#' @param yleft,yright the depth/height/time value for the extreme point at the
#' right or left of the joint (yleft overruns yright, which overruns ymin and
#' ymax)
#' @param ymin,ymax the extreme values for the y axis (in case of conflict with
#' yleft and/or yright, defaults to the smallest exaggeration)
#' @param xmin,xmax the extreme values for the x axis
#' @param add.dt whether to automatically add the dt value to the dt of
#' the segments (with the add.dt value when it is not zero)
#' @param tolerance the order of tolerance for errors, i.e. the number of
#' decimals considered as being meaningful for matching dt to log
#'
#' @return a "litholog()"-like data frame, with new bed boundaries
#'
#' @examples
#' # Generate litholog ----
#'
#' l <- c(0,1,2,3,4)
#' r <- c(1,2,3,4,5)
#' h   <- c(4,3,4,3,4)
#' i <- c("B1","B2","B3","B4","B5")
#' log  <- litholog(l, r, h, i)
#'
#' # Modify the boundaries of the litholog ----
#'
#' nlog <- weldjoint(log, c(1,2,3,4), oufti99,
#'                   sym = c("1sin", "stylolith", "3sin", "liquefaction"),
#'                   ymax  = c(NA, NA, NA, 0.2),
#'                   xmin  = c(0,0,0,1),
#'                   xmax  = c(4,4,4,1.5))
#'
#' # Visualise
#'
#' par(mfrow = c(1,2))
#'
#' plot.new()
#' plot.window(xlim = c(0,5), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(log$i, log$xy, log$dt)
#'
#' plot.new()
#' plot.window(xlim = c(0,5), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(nlog$i, nlog$xy, nlog$dt)
#'
#' @export

weldjoint <- function(log, dt, collection, sym,
                      yinv = F, xinv = F,
                      yleft = 0, yright = NA,
                      ymin = NA, ymax = NA,
                      xmin = 0, xmax = max(log$xy),
                      add.dt = 0, tolerance = 8)
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

  check.joint <- unlist(lapply(collection[match(sort(unique(sym)), sym.in.coll)],
                               is.joint))

  if(any(!check.joint)){

    false.joint <- names(check.joint)[!check.joint]

    warning("The following 'sym' symbols are not suitable 'joint' objects \n - ",
            paste(false.joint, collapse = "\n - "))

  }

  homo <- homogenise(i = dt,
                     l = list(sym = sym,
                              yinv = yinv, xinv = xinv,
                              yleft = yleft, yright = yright,
                              ymin = ymin, ymax = ymax,
                              xmin = xmin, xmax = xmax))

  df <- as.data.frame(homo)

  adf <- df[check.joint,]

  ajoint <- collection[match(adf$sym, sym.in.coll)]

  names(ajoint) <- paste(names(ajoint), seq_len(length(ajoint)), sep = "_")

  l.adf <- split(adf, seq(nrow(adf)))

  joint_fun <- function(joint, args)
  {
    changejoint(joint = joint,
                yinv = args$yinv,
                xinv = args$xinv,
                yleft = args$yleft,
                yright = args$yright,
                ymin = args$ymin,
                ymax = args$ymax,
                xmin = args$xmin,
                xmax = args$xmax)
  }

  corr.joint <- mapply(joint_fun, ajoint, l.adf, SIMPLIFY = F)

  nlog <- weldlog(log = log, dt = dt,
                  seg = corr.joint, j = names(ajoint),
                  tolerance = tolerance)

  return(nlog)

}








