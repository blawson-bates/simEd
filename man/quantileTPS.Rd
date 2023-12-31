% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tps.R
\name{quantileTPS}
\alias{quantileTPS}
\title{Sample Quantiles of Time-Persistent Statistics (TPS)}
\usage{
quantileTPS(times = NULL, numbers = NULL, probs = c(0, 0.25, 0.5, 0.75, 1))
}
\arguments{
\item{times}{A numeric vector of non-decreasing time observations}

\item{numbers}{A numeric vector containing the values of the
time-persistent statistic between the time observation}

\item{probs}{A numeric vector of probabilities with values in [0,1]}
}
\value{
a vector of the sample quantiles of the time-persistent function provided
}
\description{
Computes the sample quantiles of a time-persistent
function, corresponding to the given probabilities.
}
\details{
\if{html}{\out{<div class="sourceCode">}}\preformatted{   The lengths of \code{times} and \code{numbers} either must be
the same, or \code{times} may have one more entry than \code{numbers}
(interval endpoints vs. interval counts).  The sample quantiles are calculated
by determining the length of time spent in each state, sorting these times,
then calculating the quantiles associated with the values in the \code{prob}
vector in the same fashion as one would calculate quantiles associated with
a univariate discrete probability distribution.
}\if{html}{\out{</div>}}
}
\examples{
 times  <- c(1,2,3,4,5)
 counts <- c(1,2,1,1,2)
 meanTPS(times, counts)
 sdTPS(times, counts)
 quantileTPS(times, counts)

 output <- ssq(seed = 54321, maxTime = 100, saveNumInSystem = TRUE)
 utilization <- meanTPS(output$numInSystemT, output$numInSystemN)
 sdServerStatus <- sdTPS(output$numInSystemT, output$numInSystemN)
 quantileServerStatus <- quantileTPS(output$numInSystemT, output$numInSystemN)

 # compute and graphically display quantiles of number in system vs time
 output <- ssq(maxArrivals = 60, seed = 54321, saveAllStats = TRUE)
 quantileSys <- quantileTPS(output$numInSystemT, output$numInSystemN)
 plot(output$numInSystemT, output$numInSystemN, type = "s", bty = "l",
     las = 1, xlab = "time", ylab = "number in system")
 labels <- c("0\%", "25\%", "50\%", "75\%", "100\%")
 mtext(text = labels, side = 4, at = quantileSys, las = 1, col = "red")
 abline(h = quantileSys, lty = "dashed", col = "red", lwd = 2)

}
\author{
Barry Lawson (\email{blawson@bates.edu}), \cr
Larry Leemis (\email{leemis@math.wm.edu}), \cr
Vadim Kudlay (\email{vkudlay@nvidia.com})
}
\keyword{utilities}
