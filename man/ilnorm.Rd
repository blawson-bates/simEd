% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ilnorm.R
\name{ilnorm}
\alias{ilnorm}
\title{Visualization of Random Variate Generation for the Log-Normal Distribution}
\usage{
ilnorm(
  u = runif(1),
  meanlog = 0,
  sdlog = 1,
  minPlotQuantile = 0,
  maxPlotQuantile = 0.95,
  plot = TRUE,
  showCDF = TRUE,
  showPDF = TRUE,
  showECDF = TRUE,
  show = NULL,
  maxInvPlotted = 50,
  plotDelay = 0,
  sampleColor = "red3",
  populationColor = "grey",
  showTitle = TRUE,
  respectLayout = FALSE,
  restorePar = TRUE,
  ...
)
}
\arguments{
\item{u}{vector of uniform(0,1) random numbers, or NULL to show population
figures only}

\item{meanlog}{Mean of distribution on log scale (default 0)}

\item{sdlog}{Standard deviation of distribution on log scale (default 1)}

\item{minPlotQuantile}{minimum quantile to plot}

\item{maxPlotQuantile}{maximum quantile to plot}

\item{plot}{logical; if \code{TRUE} (default), one or more plots will appear
(see parameters below); otherwise no plots appear}

\item{showCDF}{logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf
plot is suppressed}

\item{showPDF}{logical; if \code{TRUE} (default), PDF plot
appears, otherwise PDF plot is suppressed}

\item{showECDF}{logical; if \code{TRUE} (default), ecdf plot appears,
otherwise ecdf plot is suppressed}

\item{show}{octal number (0-7) indicating plots to display;  4: CDF, 2: PDF,
1: ECDF; sum for desired combination}

\item{maxInvPlotted}{number of inversions to plot across CDF before switching to
plotting quantiles only}

\item{plotDelay}{delay in seconds between CDF plots}

\item{sampleColor}{Color used to display random sample from distribution}

\item{populationColor}{Color used to display population}

\item{showTitle}{logical; if \code{TRUE} (default), displays a title in the
first of any displayed plots}

\item{respectLayout}{logical; if \code{TRUE} (default), respects existing
settings for device layout}

\item{restorePar}{logical; if \code{TRUE} (default), restores user's previous
par settings on function exit}

\item{...}{Possible additional arguments. Currently, additional arguments not considered.}
}
\value{
A vector of Log-Normal random variates
}
\description{
Generates random variates from the Log-Normal distribution by inversion.
Optionally graphs the population cumulative distribution function and
associated random variates, the population probability density
function and a histogram of the random variates, and the empirical
cumulative distribution function versus the population cumulative
distribution function.
}
\details{
Generates random variates from the Log-Normal distribution, and optionally,
illustrates
\itemize{
\item the use of the inverse-CDF technique,
\item the effect of random sampling variability in relation to the PDF and CDF.
}
When all of the graphics are requested,
\itemize{
\item the first graph illustrates the use of the inverse-CDF technique by
graphing the population CDF and the transformation of the random numbers
to random variates,
\item the second graph illustrates the effect of random sampling variability
by graphing the population PDF and the histogram associated with the
random variates, and
\item the third graph illustrates effect of random sampling variability by
graphing the population CDF and the empirical CDF associated with the
random variates.
}
All aspects of the random variate generation algorithm are output in red by
default, which can be changed by specifying \code{sampleColor}.
All aspects of the population distribution are output in gray by default,
which can be changed by specifying \code{populationColor}.

The log-normal distribution has density

\if{html}{\out{<div class="sourceCode">}}\preformatted{\deqn{f(x) = \frac{1}{\sqrt{2 \pi} \sigma x}
                 e^{-(\log{x} - \mu)^2 / (2 \sigma^2)} }{%
          f(x) = 1/(\sqrt(2 \pi) \sigma x) e^-((log x - \mu)^2 / (2 \sigma^2))}
}\if{html}{\out{</div>}}

where \eqn{\mu}{\mu} and \eqn{\sigma}{\sigma} are the mean and standard deviation
of the logarithm.

The mean is
\eqn{E(X) = \exp(\mu + 1/2 \sigma^2)}{E(X) = exp(\mu + 1/2 \sigma^2)},
the median is
\eqn{med(X) = \exp(\mu)}{med(X) = exp(\mu)},
and the variance is
\eqn{Var(X) = \exp(2\times \mu +\sigma^2)\times (\exp(\sigma^2)-1)}{%
   Var(X) = exp(2*\mu + \sigma^2)*(exp(\sigma^2) - 1)}
and hence the coefficient of variation is
\eqn{sqrt(\exp(\sigma^2)-1)}{sqrt(exp(\sigma^2) - 1)}
which is approximately \eqn{\sigma}{\sigma} when small
(e.g., \eqn{\sigma < 1/2}{\sigma < 1/2}).

The algorithm for generating random variates from the log-normal distribution is
synchronized (one random variate for each random number) and monotone in u.
This means that the variates generated here might be useful in some variance
reduction techniques used in Monte Carlo and discrete-event simulation.

Values from the u vector are plotted in the cdf plot along the vertical axis
as colored dots.  A horizontal, dashed, colored line extends from the dot to
the population cdf.  At the intersection, a vertical, dashed colored line
extends downward to the horizontal axis, where a second colored dot, denoting
the associated log-normal random variate is plotted.

This is not a particularly fast variate generation algorithm because it uses
the base R \code{qlnorm} function to invert the values contained in \code{u}.

All of the elements of the \code{u} vector must be between 0 and 1.
Alternatively, \code{u} can be \code{NULL} in which case plot(s) of the
theoretical PDF and cdf are displayed according to plotting parameter
values (defaulting to display of both the PDF and cdf).

The \code{show} parameter can be used as a shortcut way to denote plots to
display.  The argument to \code{show} can be either:
\itemize{
\item a binary vector of length three, where the entries from left to right
correspond to \code{showCDF}, \code{showPDF}, and \code{showECDF},
respectively.  For each entry, a 1 indicates the plot should be
displayed, and a 0 indicates the plot should be suppressed.
\item an integer in [0,7] interpreted similar to the Unix chmod command.  That
is, the integer's binary representation can be transformed into a
length-three vector discussed above (e.g., 6 corresponds to c(1,1,0)).
See examples.
}
Any valid value for \code{show} takes precedence over existing individual
values for \code{showCDF}, \code{showPDF}, and \code{showECDF}.

If \code{respectLayout} is \code{TRUE}, the function respects existing
settings for device layout.  Note, however, that if the number of plots
requested (either via \code{show} or via \code{showCDF}, \code{showPMF}, and
\code{showECDF}) exceeds the number of plots available in the current layout
(as determined by \code{prod(par("mfrow"))}), the function will display all
requested plots but will also display a warning message indicating that the
current layout does not permit simultaneous viewing of all requested plots.
The most recent plot with this attribute can be further annotated after the call.

If \code{respectLayout} is \code{FALSE}, any existing user settings for device
layout are ignored.  That is, the function uses \code{par} to explicitly set
\code{mfrow} sufficient to show all requested plots stacked vertically to
align their horizontal axes, and then resets row, column, and margin settings
to their prior state on exit.

The \code{minPlotQuantile} and \code{maxPlotQuantile} arguments are present in
order to compress the plots horizontally.   The random variates generated are
not impacted by these two arguments.  Vertical, dotted, black lines are
plotted at the associated quantiles on the plots.

\code{plotDelay} can be used to slow down or halt the variate generation for
classroom explanation.

In the plot associated with the PDF, the maximum plotting height is
associated with 125\\% of the maximum height of PDF. Any histogram cell
that extends above this limit will have three dots appearing above it.
}
\examples{
 ilnorm(0.5, meanlog = 5, sdlog = 0.5)

 set.seed(8675309)
 ilnorm(runif(10), 8, 2, showPDF = TRUE)

 set.seed(8675309)
 ilnorm(runif(10), 8, 2, showECDF = TRUE)

 set.seed(8675309)
 ilnorm(runif(10), 8, 2, showPDF = TRUE, showECDF = TRUE, sampleColor = "blue3")

 set.seed(8675309)
 ilnorm(runif(10), 8, 2, showPDF = TRUE, showCDF = FALSE)

 ilnorm(runif(100), 8, 2, showPDF = TRUE, minPlotQuantile = 0.02, maxPlotQuantile = 0.98)

 # plot the PDF and CDF without any variates
 ilnorm(NULL, 8, 2, showPDF = TRUE, showCDF = TRUE)

 # plot CDF with inversion and PDF using show
 ilnorm(runif(10), 8, 2, show = c(1,1,0))
 ilnorm(runif(10), 8, 2, show = 6)

 # plot CDF with inversion and ECDF using show, using vunif
 ilnorm(vunif(10), 8, 2, show = c(1,0,1))
 ilnorm(vunif(10), 8, 2, show = 5)

 # plot CDF with inversion, PDF, and ECDF using show
 ilnorm(vunif(10), 8, 2, show = c(1,1,1))
 ilnorm(vunif(10), 8, 2, show = 7)

 # plot three different CDF+PDF+ECDF horizontal displays,
 # with title only on the first display
 oldpar <- par(no.readonly = TRUE)
 par(mfrow = c(3,3))  # 3 rows, 3 cols, filling rows before columns
 set.seed(8675309)
 ilnorm(runif(20), 8, 2, show = 7, respectLayout = TRUE, restorePar = FALSE)
 ilnorm(runif(20), 8, 2, show = 7, respectLayout = TRUE, restorePar = FALSE, showTitle = FALSE)
 ilnorm(runif(20), 8, 2, show = 7, respectLayout = TRUE, restorePar = TRUE,  showTitle = FALSE)
 par(oldpar)

 # display animation of all components
 ilnorm(runif(10), 8, 2, show = 7, plotDelay = 0.1)

 # display animation of CDF and PDF components only
 ilnorm(runif(10), 8, 2, show = 5, plotDelay = 0.1)

 if (interactive()) {
   # interactive -- pause at each stage of inversion
   ilnorm(runif(10), 8, 2, show = 7, plotDelay = -1)
 }

}
\seealso{
\code{\link[=rlnorm]{stats::rlnorm}}

\code{\link[=runif]{stats::runif}},
\code{\link[=vunif]{simEd::vunif}}
}
\author{
Barry Lawson (\email{blawson@bates.edu}), \cr
Larry Leemis (\email{leemis@math.wm.edu}), \cr
Vadim Kudlay (\email{vkudlay@nvidia.com})
}
\concept{random variate generation}
\keyword{distribution}
\keyword{dynamic}
\keyword{hplot}
