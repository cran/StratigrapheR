% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.lim.R
\name{as.lim}
\alias{as.lim}
\alias{is.lim}
\alias{are.lim.nonunique}
\alias{are.lim.nonadjacent}
\alias{are.lim.distinct}
\alias{are.lim.ordered}
\alias{order.lim}
\title{Create / Check / Manipulate lim objects}
\usage{
as.lim(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]")

is.lim(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]")

are.lim.nonunique(lim = NULL, l = NULL, r = NULL, check.lim = TRUE)

are.lim.nonadjacent(lim = NULL, l = NULL, r = NULL, b = "[]", check.lim = TRUE)

are.lim.distinct(lim = NULL, l = NULL, r = NULL, check.lim = TRUE)

are.lim.ordered(
  lim = NULL,
  l = NULL,
  r = NULL,
  id = 1L,
  decreasingly = FALSE,
  dependently = FALSE,
  check.lim = TRUE
)

order.lim(
  lim = NULL,
  l = NULL,
  r = NULL,
  id = 1L,
  b = "[]",
  decreasingly = FALSE
)
}
\arguments{
\item{lim}{a list of n left (1st element) and n right (2ndt element)
interval limits, of n interval IDs, and of n interval boundary rules (e.g.
"[]").}

\item{l}{the left interval limits (numerical vector of length n).}

\item{r}{the right interval limits (numerical vector of length n).}

\item{id}{the interval IDs (numerical or character vector of length n,
the default is 1 for each interval). They can be similar for different
intervals.}

\item{b}{the interval boundaries rules: "[]"
(or "closed") to include both boundaries points, "][" (or "()" and "open") to
exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
include only the left boundary point, and "]]" (or "(]", "left-open",
"right-closed") to include only the right boundary point. The notation is
simplified to "[]", "[[", "]]" and "][" only.}

\item{check.lim}{whether to check if the object is a lim object.}

\item{decreasingly}{whether the order to check for or to set is decreasing.}

\item{dependently}{whether the intervals themselves should be ordered
relatively to the other.}
}
\description{
Functions to create and check limits of intervals (what
we define here as a 'lim' object), with control of specified properties.
Basically we define an interval by its left and right boundaries, by an id
and by a rule of boundary inclusion.
}
\details{
\code{as.lim}: creates a lim object

\code{is.lim}: checks if arguments qualify as a lim object

\code{are.lim.nonunique}: checks if there are no intervals of identical l and
r

\code{are.lim.nonadjacent}: checks if there are no pairs of intervals having
at least one similar boundary

\code{are.lim.distinct}: checks if the intervals are not overlapping

\code{are.lim.ordered}: checks if the intervals are ordered (in l and r, and
if dependently is TRUE, relative to the other intervals of same id)

\code{order.lim}: orders l and r parts of the intervals (use \code{simp.lim}
for more advanced ordering)
}
\examples{
example <- as.lim(l = c(0,1,2), r = c(0.5,2.1,2.5), id = "I")

is.lim(lim = example)

are.lim.nonunique(l = c(0,1,2),r = c(0.5,1.5,2.5))

are.lim.nonunique(l = c(0,1,2),r = c(0.5,1.5,2))

are.lim.nonadjacent(l = c(0,1,2),r = c(0.5,1.5,2.5))

are.lim.nonadjacent(l = c(0,1,1.5),r = c(0.5,1.5,2))

are.lim.ordered(l = c(0,1,2),r = c(0.5,1.5,2.5))

are.lim.ordered(l = c(0,1,2.5),r = c(0.5,1.5,2))

are.lim.ordered(l = c(0,1,2),r = c(0.5,1.5,2.5),dependently = TRUE)

are.lim.ordered(l = c(0,2,1),r = c(0.5,2.5,1.5),dependently = TRUE)

are.lim.distinct(l = c(0,1,2),r = c(0.5,1.5,2.5))

are.lim.distinct(l = c(0,1,2),r = c(0.5,3.5,2.5))

order.lim(l = c(0,6,4,6,50), r = c(1,5,6,9,8),
          b = c("[[", "]]", "[[", "]]", "[["))

}
\seealso{
To find which values are in which interval: \code{\link{in.lim}}

To simplify intervals by merging overlapping parts: \code{\link{simp.lim}}

To extract the part outside of intervals: \code{\link{flip.lim}}

To make intervals with boundaries in between given values:
\code{\link{mid.lim}}

To discretise intervals: \code{\link{tie.lim}}

To simplify boundary rules into "[]", "[[", "]]" and "][":
 \code{\link{rebound}}

To plot interval data as lines: \code{\link{trace.lim}} and
\code{\link{plot_lim}}

To plot interval data as rectangles: \code{\link{infobar}}
}
