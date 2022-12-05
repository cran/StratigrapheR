#' @title Extract the profile of a litholog
#'
#' @description Extract the induration, grain-size, lithology, facies, or any
#' other information coded in the profile variations of a litholog.
#'
#' @param log a "litholog()"-like data frame
#' @param gap the most inward values of the profile, i.e. the minimum values
#' expected in the signal
#' @param ext the most outward values of the profile; defaults to infinity Inf,
#' for "left-side" profiles, set to -Inf
#' @param down.xy,up.xy the xy values to give the the lower and upper parts of
#' the signal.
#'
#' @return a data frame of the extracted profile, with its i (bed
#' identification), dt (depth/time), and xy (intensity).
#'
#' @examples
#' l <- c(1,2,3,4,5)  # left boundary of the bed interval (upper or lower)
#' r <- c(0,1,2,3,4)  # right boundary of the bed interval (upper or lower)
#' h <- c(4,3,4,2,3) # hardness (arbitrary)
#' i <- c("B1","B2","B3","B4","B5") # Bed name
#'
#' log <- litholog(l,r,h,i) # Generate data frame of the polygons
#'                          # making the litholog
#'
#' # Extract the profile of the litholog, with the upper and lower values set
#' # at a value of 2 ----
#'
#' pro <- profiler(log, gap = 2, up.xy = 2, down.xy = 2)
#'
#' opar <- par()$mfrow
#'
#' par(mfrow = c(1,2))
#'
#' # Draw the litholog ----
#'
#' plot.new()
#' plot.window(xlim = c(0,4), ylim = c(0,5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(log$i, log$xy, log$dt,
#'           col = c("grey80","grey20","grey80","grey20","grey80")) # Draw log
#'
#' # Draw the profile ----
#'
#' plot(pro$xy, pro$dt, type = "l", xlab = "hardness", ylab = "", axes = FALSE)
#'
#' axis(1)
#'
#' par(mfrow = opar)
#'
#' @export

profiler <- function(log, gap, ext = Inf, down.xy = NA, up.xy = NA)
{

  if(!is.litholog(log)) stop("This is not an appropriate 'litholog' object")

  test <- c(gap, ext)

  if(!(inherits(test, "numeric") | inherits(test, "integer"))) {
    stop("The 'gap' and 'ext' parameters should be numerics or integers")
  }

  if(gap == ext) {
    stop("The 'gap' and 'ext' parameters should be distinct numbers")
  }

  if(ext > gap) plus <- T else if (gap > ext) plus <- F else stop("WTF?")

  if(plus) {

    if(any(log$xy >= ext)) warning("There are xy values higher than 'ext'")

    extract <- which(log$xy >= gap & log$xy <= ext)

  } else if(!plus){

    if(any(log$xy <= ext)) warning("There are xy values lower than 'ext'")

    extract <- which(log$xy <= gap & log$xy >= ext)

  }

  exlog <- log[extract,]

  if(any(unique(exlog$i) != unique(log$i))){
    warning("Not all beds are included from this extraction")
  }

  # Deal with different sortings ---------------------------------------------------

  exlog.list <- split(exlog, f = exlog$i)

  # Counteract split reorder ?

  # Reorder based on the first and last point of the profile of each bed

  ref.list1 <- lapply(exlog.list, function(x) c(first(x$dt), last(x$dt)))

  rev.ref.list1 <- unlist(lapply(ref.list1, function(x) x[1] > x[2]))

  rev.list <- exlog.list[rev.ref.list1]

  reversed.list <- lapply(rev.list, function(x) x[rev(seq(nrow(x))),])

  exlog.list[rev.ref.list1] <- reversed.list

  # Reorder beds ----

  ref.list2 <- lapply(exlog.list, function(x) c(first(x$dt), last(x$dt)))

  bed.order <- order(unlist(lapply(ref.list2, function(x) x[1])))

  if(is.unsorted(unlist(ref.list2[bed.order]))){

    warning("Beds are overlapping in dt, the profile will be ordered ",
            "as is in the log")

    oi <- unique(log$i)

    exlog.list <- exlog.list[match(names(exlog.list), oi)]

  } else {

    exlog.list <- exlog.list[bed.order]

  }

  outlog <- do.call(rbind, exlog.list)

  is.generally.unsorted <- function(x) !(!is.unsorted(x) | !is.unsorted(rev(x)))

  if(is.generally.unsorted(outlog$dt)){

    warning(paste0("The extracted signal shows back and forth (unsorting) ",
                   "in the stratigraphic/time (dt) dimension"))

  }

  # Careful about the upper and lower points if they are added ----------------------
  # This implies that the order of points should always be similar, from top to bottom or opposite

  range <- c(1, nrow(outlog))

  if(!is.na(down.xy) &
     (inherits(down.xy,"numeric") | inherits(down.xy,"integer"))){

    pos.down <- which.min(outlog$dt[range])

    down.line    <- outlog[range[pos.down],]

    if(down.line$xy != down.xy){

      down.line$xy <- down.xy

      if(pos.down == 1){

        outlog <- rbind(down.line, outlog)

      } else if(pos.down == 2){

        outlog <- rbind(outlog, down.line)

      }

    }
  }

  range <- c(1, nrow(outlog))

  if(!is.na(up.xy) & (inherits(up.xy,"numeric") | inherits(up.xy,"integer"))){

    pos.up <- which.max(outlog$dt[range])

    up.line    <- outlog[range[pos.up],]

    if(up.line$xy != up.xy){

      up.line$xy <- up.xy

      if(pos.up == 1){

        outlog <- rbind(up.line, outlog)

      } else if(pos.up == 2){

        outlog <- rbind(outlog, up.line)

      }

    }

  }

  res <- outlog[,1:3]

  return(res)

}




