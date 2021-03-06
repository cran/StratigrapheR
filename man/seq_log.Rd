% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_log.R
\name{seq_log}
\alias{seq_log}
\title{Gives the repartition of values for a log 10 scale between a given interval}
\usage{
seq_log(x1, x2, divide = FALSE)
}
\arguments{
\item{x1}{the first value of the interval}

\item{x2}{the second value of the interval (can be higher or lower,
but never equal to x1)}

\item{divide}{whether to divide the result for major values (1,10,100)
and minor values (2,3,...,20,30,...)}
}
\value{
the repartition of values for a log 10 scale between x1 and x2
}
\description{
Gives the repartition of values for a log 10 scale between a given interval
}
\examples{
x1 <- 101
x2 <- 0.29

seq_log(x1, x2)
seq_log(x1, x2, divide = TRUE)

}
\seealso{
\code{\link{convertAxis}}
}
