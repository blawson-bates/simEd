#' Visualization of Random Variate Generation for the <%= distro %> Distribution
#'
#' @description
#'  Generates random variates from the <%= distro %> distribution by inversion.
#'  Optionally graphs the population cumulative distribution function and
#'  associated random variates, the population probability <%= massDen %>
#'  function and a histogram of the random variates, and the empirical
#'  cumulative distribution function versus the population cumulative
#'  distribution function.
#'
#' @param u vector of uniform(0,1) random numbers, or NULL to show population
#'        figures only
#' @param minPlotQuantile minimum quantile to plot
#' @param maxPlotQuantile maximum quantile to plot
#' @param plot logical; if \code{TRUE} (default), one or more plots will appear
#'        (see parameters below); otherwise no plots appear
#' @param showCDF logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf 
#'        plot is suppressed
#' @param show<%= PXF %> logical; if \code{TRUE} (default), <%= PXF %> plot appears,
#'        otherwise <%= PXF %> plot is suppressed
#' @param showECDF logical; if \code{TRUE} (default), ecdf plot appears,
#'        otherwise ecdf plot is suppressed
#' @param show octal number (0-7) indicating plots to display;  4: CDF, 2: <%= PXF %>, 
#'        1: ECDF; sum for desired combination
#' @param maxInvPlotted number of inversions to plot across CDF before switching to 
#'        plotting quantiles only
#' @param plotDelay delay in seconds between CDF plots
#' @param sampleColor Color used to display random sample from distribution
#' @param populationColor Color used to display population
#' @param showTitle logical; if \code{TRUE} (default), displays a title in the 
#'        first of any displayed plots
#' @param respectLayout logical; if \code{TRUE} (default), respects existing 
#'        settings for device layout
#' @param ... Possible additional arguments. Currently, additional arguments not considered.
#'
#' @returns A vector of <%= distro %> random variates
#'
#' @details
#'  Generates random variates from the <%= distro %> distribution, and optionally,
#'  illustrates
#'  \itemize{
#'    \item the use of the inverse-CDF technique,
#'    \item the effect of random sampling variability in relation to the <%= PXF %> and CDF.
#'  }
#'  When all of the graphics are requested,
#'  \itemize{
#'    \item the first graph illustrates the use of the inverse-CDF technique by
#'        graphing the population CDF and the transformation of the random numbers
#'        to random variates,
#'    \item the second graph illustrates the effect of random sampling variability
#'        by graphing the population <%= PXF %> and the histogram associated with the
#'        random variates, and
#'    \item the third graph illustrates effect of random sampling variability by
#'        graphing the population CDF and the empirical CDF associated with the
#'        random variates.
#'  }
#'  All aspects of the random variate generation algorithm are output in red by
#'  default, which can be changed by specifying \code{sampleColor}.
#'  All aspects of the population distribution are output in gray by default,
#'  which can be changed by specifying \code{populationColor}.
#'
#' @keywords hplot dynamic
#'
#' @concept random variate generation
#'
#' @examples
#'  <%= ifunct %>(0.5, <%= arglong %>)
#'
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(10), <%= argshort %>, show<%= PXF %> = TRUE)
#'
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(10), <%= argshort %>, showECDF = TRUE)
#'
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(10), <%= argshort %>, show<%= PXF %> = TRUE, showECDF = TRUE, sampleColor = "blue3")
#'
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(10), <%= argshort %>, show<%= PXF %> = TRUE, showCDF = FALSE)
#'
#'  <%= ifunct %>(runif(100), <%= argshort %>, show<%= PXF %> = TRUE, minPlotQuantile = 0.02, maxPlotQuantile = 0.98)
#'
#'  # plot the <%= PXF %> and CDF without any variates
#'  <%= ifunct %>(NULL, <%= argshort %>, show<%= PXF %> = TRUE, showCDF = TRUE)
#'
#'  # plot CDF with inversion and <%= PXF %> using show
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = c(1,1,0))
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 6)
#'
#'  # plot CDF with inversion and ECDF using show, using vunif
#'  <%= ifunct %>(vunif(10), <%= argshort %>, show = c(1,0,1))
#'  <%= ifunct %>(vunif(10), <%= argshort %>, show = 5)
#'
#'  # plot CDF with inversion, <%= PXF %>, and ECDF using show
#'  <%= ifunct %>(vunif(10), <%= argshort %>, show = c(1,1,1))
#'  <%= ifunct %>(vunif(10), <%= argshort %>, show = 7)
#'
#'  # plot three different CDF+<%= PXF %>+ECDF horizontal displays,
#'  # with title only on the first display
#'  oldpar <- par(no.readonly = TRUE)
#'  par(mfrow = c(3,3))  # 3 rows, 3 cols, filling rows before columns
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE, showTitle = FALSE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE, showTitle = FALSE)
#'  par(oldpar)
#'
#'  # display animation of all components
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 7, plotDelay = 0.1)
#'
#'  # display animation of CDF and PMF components only
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 5, plotDelay = 0.1)
#'
#'  \dontrun{
#'  # interactive -- pause at each stage of inversion
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 7, plotDelay = -1)
#'  }
#'
#' @importFrom grDevices dev.hold dev.flush recordPlot replayPlot adjustcolor 
#' @importFrom stats quantile
