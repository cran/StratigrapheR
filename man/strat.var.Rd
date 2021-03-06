% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strat.var.R
\name{strat.var}
\alias{strat.var}
\title{Compute the realtive thickness variations of sections}
\usage{
strat.var(dt, initial = NULL, ref = 1, events = NULL, sections = NULL)
}
\arguments{
\item{dt}{a matrix of depth (or time) of the different tie points. Columns
are for the sections, rows for each tie point. No NA values are accepted,
if necessary, tie-points have to be estimated, using for instance the
\code{\link{strat.mean}} function}

\item{initial}{which tie-points are originally present in the sections (if
NULL, by default all the values are considered as originally present)}

\item{ref}{the column index for the section which acts as a reference (by
default, it is set to 1, for the first columns)}

\item{events}{the name of the tie points}

\item{sections}{the name of the sections}
}
\description{
Based on tie-points, this function computes the relative
thickness variations of different sections compared to a reference section or
composite sections
}
\examples{
dt     <- tie.points.example[,2:6]
events <- tie.points.example[,1]

extended <- strat.mean(dt = dt, events = events)

strat.var(extended$dt, extended$initial)

}
