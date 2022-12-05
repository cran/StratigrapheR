#' @title Identify points in a polygon in reference to an octagon
#'
#' @description Identify points in a polygon as if they were constituting a
#' reference octagon, having two sides horizontal and two sides vertical: there
#' are eight points, starting from the right side of the upper horizontal side,
#' and following each other in a clockwise order.
#'
#' @param x,y the coordinates of the polygons
#' @param i the identification of the polygons if there are multiple ones
#' @param pos which reference points to compute
#'
#' @return a data frame with as much columns as positions, labelled from pos1 to
#' pos8, or a vector if only one position is required
#'
#' @examples
#' # Define polygons (in this case, two octagons) ----
#'
#' dt1 <- c(0,0,0.33,0.67,1,1,0.67,0.33) - 0.5
#' xy1 <- c(0.33,0.67,1,1,0.67,0.33,0,0) - 0.5
#'
#' dt2 <- rev(0.5 * (c(0,0,0.33,0.67,1,1,0.67,0.33) - 0.5))
#' xy2 <- rev(0.5 * (c(0.33,0.67,1,1,0.67,0.33,0,0) -0.5))
#'
#' dt <- c(dt1, dt2)
#' xy <- c(xy1, xy2)
#' gr <- rep(c("B2","A3"), each = 8)
#'
#' # Compute the position to the octagon reference ----
#'
#' octa <- octapos(x = xy, y = dt, i = gr)
#'
#' # Plot base----
#'
#' plot.new()
#' plot.window(xlim = c(-0.5,0.5), ylim = c(-0.5,0.5))
#' axis(1)
#' axis(2, las = 1)
#' title(xlab = "x", ylab = "y")
#' polygon(x = xy1, y = dt1)
#' points(x = xy1, y = dt1, pch = 19)
#' polygon(x = xy2, y = dt2)
#' points(x = xy2, y = dt2, pch = 19)
#'
#' # Plot the positions ----
#'
#' one <- rep(NA, nrow(octa))
#'
#' one[octa$pos1] <- 1
#' one[octa$pos2] <- 2
#' one[octa$pos3] <- 3
#' one[octa$pos4] <- 4
#' one[octa$pos5] <- 5
#' one[octa$pos6] <- 6
#' one[octa$pos7] <- 7
#' one[octa$pos8] <- 8
#'
#' text(0.8 * xy, 0.8 * dt, one)
#'
#' @importFrom dplyr arrange desc
#' @export

octapos <- function(x, y, i = "I1", pos = 1:8)
{

  pos <- as.integer(pos)

  in8 <- pos %in% as.integer(1:8)

  if(any(!in8)) {
    stop("The 'pos' values should be integers from 1 to 8, 1 and 8 included")
  }

  df <- data.frame(gloVar.x = x, gloVar.y = y, i = i, ini = seq(length(x)))

  df$gloVar.polygons <- match(df$i, df[!duplicated(df$i),3])

  if(1 %in% pos | 5 %in% pos){
    df1 <- arrange(df, gloVar.polygons, gloVar.y, gloVar.x)
    l1  <- split(df1$ini, f = df1$gloVar.polygons)
  }

  if(2 %in% pos | 6 %in% pos){
    df2 <- arrange(df, gloVar.polygons, gloVar.x, gloVar.y)
    l2  <- split(df2$ini, f = df2$gloVar.polygons)
  }

  if(3 %in% pos | 7 %in% pos){
    df3 <- arrange(df, gloVar.polygons, gloVar.x, desc(gloVar.y))
    l3  <- split(df3$ini, f = df3$gloVar.polygons)
  }

  if(4 %in% pos | 8 %in% pos){
    df4 <- arrange(df, gloVar.polygons, gloVar.y, desc(gloVar.x))
    l4  <- split(df4$ini, f = df4$gloVar.polygons)
  }

  if(1 %in% pos){
    df$pos1 <- F
    df$pos1[unlist(lapply(l1, last),  use.names = F)] <- T
  }

  if(2 %in% pos){
    df$pos2 <- F
    df$pos2[unlist(lapply(l2, last),  use.names = F)] <- T
  }

  if(3 %in% pos){
    df$pos3 <- F
    df$pos3[unlist(lapply(l3, last),  use.names = F)] <- T
  }

  if(4 %in% pos){
    df$pos4 <- F
    df$pos4[unlist(lapply(l4, first), use.names = F)] <- T
  }

  if(5 %in% pos){
    df$pos5 <- F
    df$pos5[unlist(lapply(l1, first), use.names = F)] <- T
  }

  if(6 %in% pos){
    df$pos6 <- F
    df$pos6[unlist(lapply(l2, first), use.names = F)] <- T
  }

  if(7 %in% pos){
    df$pos7 <- F
    df$pos7[unlist(lapply(l3, first), use.names = F)] <- T
  }

  if(8 %in% pos){
    df$pos8 <- F
    df$pos8[unlist(lapply(l4, last),  use.names = F)] <- T
  }

  out <- df[,-1:-5]

  return(out)

}



