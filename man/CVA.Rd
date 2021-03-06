% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CVA.R
\name{CVA}
\alias{CVA}
\title{Coefficient of variation analysis}
\usage{
CVA(Time_series,MaxIETD,xlabel,ylabel)
}
\arguments{
\item{Time_series}{A dataframe. The first column contains the time and day of a rainfall pulse and the second one the depth
of rainfall in each time step. The date must be as POSIXct class.}

\item{MaxIETD}{The maximum value of IETD to be analyzed (in hours). Default value 24.}

\item{xlabel}{Label of the x-axis of the figure IETD vs CV.}

\item{ylabel}{Label of the y-axis of the figure IETD vs CV.}
}
\value{
A list with a figure of IETD vs CV, a dataframe with the values of that figure, and the computed value of IETD.
}
\description{
This function computes the inter-event time definition (IETD) based on the coefficient of variation analysis.
}
\details{
This method assumes that inter-event times (b) are represented well by a exponential distribution. Since
by definition b>= IETD, IETD is computed as the value whose resulting coefficient of variation (CV) of b equal to unity \insertCite{Restrepo-Posada1982,Adams2000}{IETD}.
This analysis is done by testing several values of IETD and analyzing the resulting CV. The computed IETD is obtained via interpolation from the figure of
IETD vs CV.
}
\note{
To review the concepts of b and IETD, go to the details of  \code{\link{drawre}} function.
}
\examples{
CVA (Time_series=hourly_time_series)
}
\references{
\insertAllCited{}
}
\author{
Luis F. Duque <lfduquey@gmail.com> <l.f.duque-yaguache2@newcastle.ac.uk>
}
