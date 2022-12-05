#' @title Identify whether the points of a polygon are ordered clockwise
#'
#' @param x,y the coordinates of the polygons
#' @param i the identification of the polygons if there are multiple ones
#' @param get.pos get the output as a list of the result and of the output of
#' octapos()
#'
#' @return logical values for each polygon: TRUE for clockwise, FALSE for
#' counterclockwise, NA for ambiguous order, typically in lines or the polygons
#' whose lines cross each other (although sometimes such polygons are still
#' giving T or F values, as this function only consider certain reference
#' points, for more details see \code{\link{octapos}}).
#'
#' @examples
#' # Generate some polygons ----
#'
#' x1 <- c(0,1,0.5)
#' y1 <- c(3,3,4)
#' i1 <- rep("P1", 3)
#' s1 <- 1:3
#'
#' x2 <- c(3,3.5,4)
#' y2 <- c(3,4,3)
#' i2 <- rep("P2", 3)
#' s2 <- 1:3
#'
#' x3 <- c(0,0.5,1)
#' y3 <- c(1, 1.5,2)
#' i3 <- rep("P3", 3)
#' s3 <- 1:3
#'
#' x4 <- c(3,4,4,3)
#' y4 <- c(1,2,1,2)
#' i4 <- rep("P4", 4)
#' s4 <- 1:4
#'
#' x5 <- c(1,2,3,3,2,1)
#' y5 <- c(-0.5,0.4,-0.5,0.5,-0.4,0.5)
#' i5 <- rep("P5", 6)
#' s5 <- 1:6
#'
#' x6 <- c(1,2,3,3,2,1)
#' y6 <- c(-2,-1,-2,-1,-2.5,-1)
#' i6 <- rep("P6", 6)
#' s6 <- 1:6
#'
#' x <- c(x1, x2, x3, x4, x5, x6)
#' y <- c(y1, y2, y3, y4, y5, y6)
#' i <- c(i1, i2, i3, i4, i5, i6)
#' s <- c(s1, s2, s3, s4, s5, s6)
#'
#' # Test whether they are clockwise or not ----
#'
#' is.clockwise(x = x, y = y, i = i)
#'
#' # Visualise the result ----
#'
#' plot.new()
#' plot.window(xlim = c(-0.5,5.5), ylim = c(-2.5,4.5))
#'
#' axis(1)
#' axis(2)
#'
#' multigons(i = i, x = x, y = y)
#'
#' center.x   <- c(0.5, 3.5, 0.5, 3.5, 2, 2)
#' center.y   <- c(3.4, 3.4, 2, 1.5, 0, -1.5)
#' center.lab <- c("P1", "P2", "P3", "P4", "P5", "P6")
#'
#' text(x = center.x, y = center.y, labels = center.lab)
#'
#' text(x = x, y = y, labels = s)
#'
#' @importFrom dplyr lag lead
#' @export

is.clockwise <- function(x, y, i = rep("A1", length(x)), get.pos = F)
{

  # Find the points of reference
  octa <- octapos(x = x, y = y, i = i)

  # Find if there are polygons having only two points of reference
  # despite having more than two points

  octa$tot    <- rowSums(octa)
  octa$is.tot <- octa$tot != 0

  ini  <- data.frame(x = x, y = y, i = i)
  df   <- cbind(ini, octa)
  ref1 <- df[,1:11]
  ref  <- ref1

  sdf <- split(df, f = df$i)

  nrow.sdf <- unlist(lapply(sdf, nrow))
  nref.sdf <- unlist(lapply(sdf, function(x) sum(x$is.tot)))

  false.line <- nref.sdf == 2 & nrow.sdf > 2

  # If such polygons exist, rotate them along the line so defined, and see if
  #  new reference points can be found

  if(any(false.line)){

    false.line.groups <- df$i %in% names(nref.sdf)[false.line]

    ndf <- df[false.line.groups,]

    indf <- sdf[false.line]

    ildf <- lapply(indf, function(x) x[x$is.tot,])

    # Identify the point of pivot, which is going to be the left side point

    pivot.id <- lapply(ildf, function(d) {

      low.x <- which.min(d$x)

      if(low.x == 1) high.x <- 2 else high.x <- 1

      return(c(low.x, high.x))

    })

    match1 <- match(ndf$i, names(pivot.id))

    pivot.df           <- t(data.frame(pivot.id))
    colnames(pivot.df) <- c("low.x", "high.x")

    pdf <- pivot.df[match1,]

    ndf <- cbind(ndf, pdf)

    # Compute the slope and angle of the line

    line.ndf  <- ndf[ndf$is.tot,]
    sline.ndf <- split(line.ndf, f = line.ndf$i)

    slopes <- unlist(lapply(sline.ndf, function(d) {
      (d$y[d$high.x[1]]-d$y[d$low.x[1]])/(d$x[d$high.x[2]]-d$x[d$low.x[2]])
    }))

    angles <- atan(slopes)

    ndf$slopes <- slopes[match1]
    ndf$angles <- angles[match1]

    # Rotate the points along the pivot, using the angle

    rot.list <- split(ndf, f = ndf$i)

    # x <- rot.list[[1]]

    rot <- function(x)
    {

      coords <- as.matrix(x[,1:2])
      theta  <- -x$angles[1]

      pivot.pos <- which(x$is.tot)

      pivot  <- c(x$x[pivot.pos[x$low.x[1]]],
                  x$y[pivot.pos[x$low.x[1]]])

      rmat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
                     byrow = TRUE, ncol = 2)

      res <- t(pivot + rmat %*% (t(coords) - pivot))

      return(res)

    }

    back  <- unlist(split(seq(nrow(ndf)), f = ndf$i))
    oback <- order(back)

    rotated <- lapply(rot.list, rot)

    rot.df <- as.data.frame(do.call("rbind", rotated)[oback, ])

    colnames(rot.df) <- c("xr", "yr")

    ndf <- cbind(ndf, rot.df)

    octa2 <- octapos(x = ndf$xr, y = ndf$yr, i = ndf$i)

    ref2 <- cbind(ndf[,1:3], octa2)

    ref[false.line.groups,] <- ref2

  }

  # Establish the order of reference points ----

  m8_pos <- as.matrix(ref[,4:11])
  m8_num <- matrix(rep(1:8, nrow(ref)), ncol = 8, byrow = T)
  m8_in  <- m8_num * m8_pos

  m8_sum <- rowSums(m8_in)

  m8_mean <- m8_sum/rowSums(m8_pos)

  cond_0    <- m8_sum == 0
  cond_18   <- m8_pos[,1] & m8_pos[,8] & !m8_pos[,2] & !m8_pos[,7]
  cond_218  <- m8_pos[,1] & m8_pos[,8] & m8_pos[,2] & !m8_pos[,7] & !m8_pos[,3]
  cond_187  <- m8_pos[,1] & m8_pos[,8] & m8_pos[,7] & !m8_pos[,2] & !m8_pos[,6]

  cond_3218  <- m8_pos[,3] & m8_pos[,2] & m8_pos[,1] & m8_pos[,8]
  cond_2187  <- m8_pos[,2] & m8_pos[,1] & m8_pos[,8] & m8_pos[,7]
  cond_1876  <- m8_pos[,1] & m8_pos[,8] & m8_pos[,7] & m8_pos[,6]

  m8_mean[cond_0]    <- NA
  m8_mean[cond_18]   <- 0.5
  m8_mean[cond_218]  <- 1
  m8_mean[cond_187]  <- 8
  m8_mean[cond_3218] <- 1.5
  m8_mean[cond_2187] <- 0.5
  m8_mean[cond_1876] <- 7.5

  ref$mean <- m8_mean

  ref <- ref[!is.na(ref$mean),]

  fin <- split(ref$mean, f = ref$i)

  back2  <- unlist(split(seq(nrow(ref)), f = ref$i))
  oback2 <- order(back2)

  fin.min  <- unlist(lapply(fin, which.min))
  fin.max  <- unlist(lapply(fin, which.max))
  fin.n    <- unlist(lapply(fin, length))

  ref$ro1  <- unlist(mapply(lead, fin, fin.min - 1))[oback2]
  ref$roi1 <- unlist(mapply(lag, fin, fin.n - fin.min + 1))[oback2]

  ref$ro1[is.na(ref$ro1)] <- ref$roi1[!is.na(ref$roi1)]

  ref$ro2  <- unlist(mapply(lead, fin, fin.max - 1))[oback2]
  ref$roi2 <- unlist(mapply(lag, fin, fin.n - fin.max + 1))[oback2]

  ref$ro2[is.na(ref$ro2)] <- ref$roi2[!is.na(ref$roi2)]

  end <- split(ref, f = ref$i)

  clockwise        <- unlist(lapply(end, function(x) !is.unsorted(x$ro1)))
  counterclockwise <- unlist(lapply(end, function(x) !is.unsorted(rev(x$ro2))))

  res <- clockwise

  res[!clockwise & !counterclockwise] <- NA
  res[clockwise & counterclockwise]   <- NA

  res <- res[match(unique(i), names(res))]

  if(isTRUE(get.pos)) res <- list(is.clockwise = res, pos = octa[,1:8])

  return(res)

}



