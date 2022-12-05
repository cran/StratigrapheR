#' @title Changes profiles in basic lithologs
#'
#' @description Adds profiles (hardness, weathering, grain-size, facies, etc.)
#' to lithologs from "litholog()"-like data frames
#'
#' @param log a "litholog()"-like data frame on which the new profile needs to
#' be welded.
#' @param gap The xy value delimiting the litholog in the parts that will
#' remain, and the parts that will be replaced by the profile. This should be
#' comparable to the most inward values of the profile. What side of the
#' litholog will remain depends on the ext parameter.
#' @param dt,xy the depth and intensity values for the profile
#' @param i the beds ids for the points of the profile (is optional; this is
#' useful for complex profiles, which can go back and forth in depth).
#' @param ext the most outward values of the profile; defaults to infinity Inf,
#' for "left-side" profiles, set to -Inf.
#' @param tolerance the order of tolerance for errors, i.e. the number of
#' decimals considered as being meaningful for matching dt to log
#' @param correct this parameter applies in a very specific case, when i is
#' provided, and when the order of points is not straightforward (going from low
#' to high values, or vice versa). If correct is TRUE, the ambiguous parts
#' (in ambiguous beds) of the profile will be reversed.
#'
#' @return A litholog object, i.e., a table of bed id (i), depth (dt) and xy
#' values (the x position if your litholog is vertical).
#'
#' @examples
#' # Make an initial log ----
#'
#' r <- c(1,2,3,4,5)  # left boundary of the bed interval (upper or lower)
#' l <- c(0,1,2,3,4)  # right boundary of the bed interval (upper or lower)
#' h <- c(4,3,5,3,4) # hardness (arbitrary)
#' i <- c("B1","B2","B3","B4","B5") # Bed name
#'
#' log <- litholog(l,r,h,i) # Generate data frame of the polygons
#' # making the litholog
#'
#' # Define the profile ----
#'
#' # Depths (dt), intensity (xy), and ids (id) of profile points
#' dt <- c(0,1,1,1,2,2,2,3,3,3,4,4,5)
#' xy <- c(5,4,3,4,3,3,6,5,3,4,3,5,4)
#' id <- c("B1","B1","B1","B2","B2","B3","B3","B3","B3","B4","B4","B5","B5")
#'
#' # Weld profile to litholog ----
#'
#' nlog <- weldprofile(log = log, gap = 3, dt = dt, xy = xy, i = id, ext = Inf)
#'
#' # Visualisation ----
#'
#' opar <- par()$mfrow
#' par(mfrow = c(1,3))
#'
#' plot.new()
#' plot.window(xlim = c(0,6), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(log$i, log$xy, log$dt)
#'
#' plot.new()
#' plot.window(xlim = c(0,6), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' lines(xy, dt, type = "o", pch = 19)
#'
#' plot.new()
#' plot.window(xlim = c(0,6), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(nlog$i, nlog$xy, nlog$dt)
#'
#' par(mfrow = opar)
#'
#' @importFrom dplyr arrange lag
#' @export

weldprofile <- function(log, gap, dt, xy, i = NA, ext = Inf,
                        tolerance = 8, correct = T)
{

  # Test and round litholog ----

  olog <- log

  if(!is.litholog(olog)) stop("This is not an appropriate 'litholog' object")

  log$dt <- round(log$dt, tolerance)
  dt     <- round(dt, tolerance)

  # Test the other parameters ----

  test <- c(gap, ext, xy, dt)

  if(!(inherits(test, "numeric") | inherits(test, "integer"))) {
    stop(paste0("The 'gap', 'ext', 'xy', and 'dt'  parameters should",
                " be numerics or integers"))
  }

  if(length(dt) != length(xy)){
    stop("The 'xy' and 'dt'parameters should be of same length")
  }

  if(gap == ext) {
    stop("The 'gap' and 'ext' parameters should be distinct numbers")
  }

  # Identify the direction of the gap ----

  if(ext > gap) plus <- T else if (gap > ext) plus <- F else stop("WTF?")

  if(plus) {

    if(any(log$xy > ext)) {
      warning("There are xy values in the litholog higher than 'ext'")
    }

    if(any(xy > ext)) {
      warning("There are xy values in the profile higher than 'ext'")
    }

    if(any(xy < gap)) stop("There are xy values in the profile lower than 'gap'")

  } else if(!plus){

    if(any(log$xy < ext)) {
      warning("There are xy values in the litholog lower than 'ext'")
    }

    if(any(xy < ext)) {
      warning("There are xy values in the profile lower than 'ext'")
    }

    if(any(xy > gap)) stop("There are xy values in the profile higher than 'gap'")

  }

  # Add the points at the gap, test if cutting occurs twice on all beds ----

  ilog <- log[,1:3]

  llog <- outliner(ilog, gap, add = T)

  gap_points     <- llog$i[llog$xy == gap]

  beds.in.rem <- unique(llog$i) %in% unique(gap_points)

  if(any(!beds.in.rem)){

    beds.not.in.rem <- unique(log$i)[!beds.in.rem]

    stop(paste0("The following beds are not found intersecting the gap line: ",
                paste(beds.not.in.rem, collapse = ", ")))

  }

  gap_list       <- split(gap_points, f = gap_points)
  gap_length     <- unlist(lapply(gap_list, length))

  bad_cut <- gap_length != 2

  if(any(bad_cut)){

    bad_cut_names <- names(bad_cut)[bad_cut]

    stop("The following beds are not intersecting ",
         "the gapline exactly two times: ",
         paste0(bad_cut_names, collapse = ", "))

  }

  # Adapt the order of points in the litholog ----

  if(plus) {

    shlog <- octashift(x = llog$xy, y = llog$dt, i = llog$i,
                       pos = 6, clockwise = T)

  } else if (!plus){

    shlog <- octashift(x = llog$xy, y = llog$dt, i = llog$i,
                       pos = 2, clockwise = T)

  }

  glog <- data.frame(i = shlog$i, dt = shlog$y, xy = shlog$x)

  # Divide the adapted log into two groups of points that should come before and
  # after the profile ----

  glog$order <- seq(nrow(glog))

  if(plus) {

    glog$extract <- glog$xy > gap

  } else if(!plus){

    glog$extract <- glog$xy < gap

  }

  left <- glog[!glog$extract,]

  unames <- unique(left$i)
  inames <- seq(length(unames))

  left$main_i     <- match(left$i, unames)
  left$gloVar.sec <- seq(nrow(left))

  change1 <- duplicated(paste(left$i, left$xy, sep = "_")) & left$xy == gap

  list_seq <- split(seq(nrow(left)), f = left$i)
  change2  <- unlist(lapply(list_seq, function(x) x[1]))

  left$change          <- 0
  left$change[change1] <- 1
  left$change[change2] <- -1
  left$change[1]       <- 0

  left$is.down <- cumsum(left$change)

  left$add.main <- NA

  left$add.main[left$is.down == 0] <- 0.1
  left$add.main[left$is.down == 1] <- 0.9

  if(any(is.na(left$add.main))) stop("Problem in the code, debug needed")

  left$gloVar.main <- left$main_i + left$add.main

  # If profile has attributed beds; welding occurs using those, with larger
  # personalisation possibilities. Details about the 'correct' parameter and the
  # reordering of points (clockwise) are critical ----

  if(!any(is.na(i))) {

    if(length(i) != length(dt)) {
      stop("The 'i' parameter should be of same length than 'xy' and 'dt'")
    }

    if(any(is.na(i))) stop("If 'i' is provided, there should be no NA values")

    uni    <- unique(i)
    uni.in <- uni %in% unique(left$i)

    if(any(!uni.in)){

      uni.out <- uni[!uni.in]

      stop("The following beds from the profile are not encountered ",
           "in the litholog: ",
           paste0(uni.out, collapse = ", "))
    }

    proline <- data.frame(dt = dt, xy = xy, i = i)

    direct  <- !is.unsorted(proline$dt)
    reverse <- !is.unsorted(rev(proline$dt))

    if((direct & plus) | (reverse & !plus)) {
      proline <- proline[rev(seq(nrow(proline))),]
    }

    if(!direct & !reverse){

      sproline <- split(proline, f = proline$i)

      ldirect  <- unlist(lapply(sproline, function(x) !is.unsorted(x$dt)))
      lreverse <- unlist(lapply(sproline, function(x) !is.unsorted(rev(x$dt))))

      if((any(ldirect) & plus) | (any(lreverse) & !plus)){

        if(plus) {
          to.flip1  <- sproline[ldirect]
        } else if(!plus) {
          to.flip1  <- sproline[lreverse]
        }

        flipped1 <- lapply(to.flip1, function(x) x[rev(seq(nrow(x))),])

      } else {

        flipped1 <- list()

      }

      if(any(!ldirect & !lreverse) & isTRUE(correct)){

        to.flip2  <- sproline[!ldirect & !lreverse]

        flipped2 <- lapply(to.flip2, function(x) x[rev(seq(nrow(x))),])

      } else {

        flipped2 <- list()

      }

      remaining <- sproline[lreverse]

      lproline <- merge_list(remaining, flipped1, flipped2)
      proline  <- do.call(rbind, lproline)

    }

    unames <- unique(left$i)
    inames <- seq(length(unames))

    proline$gloVar.main <- match(proline$i, unames) + 0.5
    proline$gloVar.sec  <- seq(nrow(proline))

    merged <- rbind(left[, c(1:3, 11, 7)], proline[, c(3,1,2,4,5)])

    res <- arrange(merged, gloVar.main, gloVar.sec)

    res$text <- paste(res$i, res$dt, res$xy, sep = "_")
    res$lagt <- lag(res$text)
    res$dup  <- duplicated(res$text)

    res <- res[!(res$dup & res$text == res$lagt),]

  }

  # If profiles has no beds attributed, profile needs to be non-repeating,
  # at least at the boundaries ----

  if(any(is.na(i))){

    gapline <- glog[glog$xy == gap,]

    range.pro <- range(gapline$dt)

    if(min(dt) != range.pro[1] | max(dt) != range.pro[2]) {

      stop(paste0("The profile dt range should be from ", range.pro[1],
                  " to ", range.pro[2]))

    }

    proline <- data.frame(dt = dt, xy = xy)

    direct  <- !is.unsorted(proline$dt)
    reverse <- !is.unsorted(rev(proline$dt))

    if(direct) proline <- proline[rev(seq(nrow(proline))),]

    if(!direct & !reverse) stop("The 'dt' values should be ordered")

    test_dt <- dt[dt %in% unique(gapline$dt)]

    if(any(duplicated(test_dt))) {
      stop(paste0("If the parameter 'i' has NA values, there should be",
                  " no duplicate 'dt' values at the bed boundaries"))
    }

    interpol    <- data.frame(dt = gapline$dt)
    interpol$xy <- approx(x = dt, y = xy, xout = gapline$dt, ties = mean)$y
    interpol$i  <- gapline$i

    gaplist <- split(gapline$dt, f = gapline$i)

    l <- unlist(lapply(gaplist, min), use.names = F)
    r <- unlist(lapply(gaplist, max), use.names = F)

    lim1 <- as.lim(l = l, r = r, id = names(gaplist), b = "[[")
    lim2 <- as.lim(l = l, r = r, id = names(gaplist), b = "]]")

    proline1 <- proline
    proline2 <- proline

    proline1$i <- in.lim(proline1$dt, lim1)$id
    proline2$i <- in.lim(proline2$dt, lim2)$id

    proline <- rbind(proline2, proline1)

    proline <- proline[!is.na(proline$i),]

    right <- rbind(proline, interpol)

    right <- right[rev(order(right$dt)),]

    unames <- unique(left$i)
    inames <- seq(length(unames))

    right$gloVar.main <- match(right$i, unames) + 0.5
    right$gloVar.sec  <- seq(nrow(right))

    merged <- rbind(left[, c(1:3, 11, 7)], right[, c(3,1,2,4,5)])

    res <- arrange(merged, gloVar.main, gloVar.sec)

    res <- res[!duplicated(paste(res$i, res$dt, res$xy, sep = "_")),]

  }

  out <- data.frame(i = res$i, dt = res$dt, xy = res$xy)

  return(out)

}




