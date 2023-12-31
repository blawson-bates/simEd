% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thinning.R
\name{thinning}
\alias{thinning}
\title{Thinning Algorithm Visualization}
\usage{
thinning(
  maxTime = 24,
  intensityFcn = function(x) (5 - sin(x/0.955) - (4 * cos(x/3.82)))/0.5,
  majorizingFcn = NULL,
  majorizingFcnType = NULL,
  seed = NA,
  maxTrials = Inf,
  plot = TRUE,
  showTitle = TRUE,
  plotDelay = plot * -1
)
}
\arguments{
\item{maxTime}{maximum time of the non-homogeneous Poisson process.
(The minimum time is assumed to be zero.)}

\item{intensityFcn}{intensity function corresponding to rate of arrivals across
time.}

\item{majorizingFcn}{majorizing function.  Default value is NULL,
corresponding to a constant majorizing function that is
1.01 times the maximum value of the intensity function.
May alternatively be provided as a user-specified function,
or as a data frame requiring additional notation as
either piecewise-constant or piecewise-linear.  See
examples.}

\item{majorizingFcnType}{used to indicate whether a majorizing function that
is provided via data frame is to be interpreted as
either piecewise-constant (\code{"pwc"}) or
piecewise-linear (\code{"pwl"}).  If the majorizing
function is either the default or a user-specified
function (closure), the value of this parameter is
ignored.}

\item{seed}{initial seed for the uniform variates used during
generation.}

\item{maxTrials}{maximum number of accept-reject trials; infinite by default.}

\item{plot}{if TRUE, visual display will be produced.  If FALSE,
generated event times will be returned without visual display.}

\item{showTitle}{if TRUE, display title in the main plot.}

\item{plotDelay}{wait time, in seconds, between plots; -1 (default) for
interactive mode, where the user is queried for input
to progress.}
}
\value{
returns a vector of the generated random event times
}
\description{
This function animates the "thinning" approach the
generation of the random event times for a non-homogeneous
Poisson process with a specified intensity function, given a
majorizing function that dominates the intensity function.
}
\details{
There are three modes for visualizing Lewis and Shedler's thinning
algorithm for generating random event times for a non-homogeneous
Poisson process with a particular intensity function:
\itemize{
\item interactive advance (\code{plotDelay = -1}), where
pressing the 'ENTER' key advances to the next step
(an accepted random variate) in the algorithm,
typing 'j #' jumps ahead # steps,
typing 'q' quits immediately,
and typing 'e' proceeds to the end;
\item automatic advance (\code{plotDelay} > 0); or
\item final visualization only (\code{plotDelay = 0}).
}
As an alternative to visualizing, event times can be generated
}
\examples{

nhpp <- thinning(maxTime = 12, seed = 8675309, plotDelay = 0)
nhpp <- thinning(maxTime = 24, seed = 8675309, plotDelay = 0)

\donttest{
nhpp <- thinning(maxTime = 48, seed = 8675309, plotDelay = 0)

# thinning with custom intensity function and default majorizing function
intensity <- function(x) { 
    day <- 24 * floor(x/24)
    return(80 * (dnorm(x, day + 6,    2.5) + 
                 dnorm(x, day + 12.5, 1.5) + 
                 dnorm(x, day + 19,   2.0)))
}
nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)

# thinning with custom intensity and constant majorizing functions
major <- function(x) { 25 }
nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity,
                 majorizingFcn = major)

# piecewise-constant data.frame for bounding default intensity function
fpwc <- data.frame(
    x = c(0, 2, 20, 30, 44, 48),
    y = c(5, 5, 20, 12, 20,  5)
)
nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwc, majorizingFcnType = "pwc")

# piecewise-linear data.frame for bounding default intensity function
fpwl <- data.frame(
    x = c(0, 12, 24, 36, 48),
    y = c(5, 25, 10, 25, 5)
)
nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwl, majorizingFcnType = "pwl")

# piecewise-linear closure/function bounding default intensity function
fclo <- function(x) {
    if (x <= 12) (5/3)*x + 5
    else if (x <= 24) 40 - (5/4)*x
    else if (x <= 36) (5/4)*x - 20
    else 85 - (5/3) * x
}
nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fclo)

# thinning with fancy custom intensity function and default majorizing
intensity <- function(x) { 
    day <- 24 * floor(x/24)
    return(80 * (dnorm(x, day + 6,    2.5) + 
                 dnorm(x, day + 12.5, 1.5) + 
                 dnorm(x, day + 19,   2.0)))
}
nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)

# piecewise-linear data.frame for bounding custom intensity function
fpwl <- data.frame(
    x = c(0,  6,  9, 12, 16, 19, 24, 30, 33, 36, 40, 43, 48),
    y = c(5, 17, 12, 28, 14, 18,  7, 17, 12, 28, 14, 18,  7)
)
nhpp <- thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity,
          majorizingFcn = fpwl, majorizingFcnType = "pwl")

# thinning with simple custom intensity function and custom majorizing
intensity <- function(t) {
    if      (t < 12) t
    else if (t < 24) 24 - t
    else if (t < 36) t - 24
    else             48 - t
}
majorizing <- data.frame(
    x = c(0, 12, 24, 36, 48),
    y = c(1, 13,  1, 13,  1))
times <- thinning(plotDelay = 0, intensityFcn = intensity,
    majorizingFcn = majorizing , majorizingFcnType = "pwl", maxTime = 48)
}

}
\references{
Lewis, P.A.W. and Shedler, G.S. (1979). Simulation of non-homogeneous Poisson
processes by thinning. \emph{Naval Research Logistics}, \strong{26}, 403–413.
}
\concept{non-homogeneous Poisson process}
