#' Random Variate Generation for the <%= distro %> Distribution
#'
#' @description
#'  Generates random variates from the <%= distro %> distribution by inversion.
#'  Optionally graphs the population cumulative distribution function and
#'  associated random variates, the population probability density function
#'  and a histogram of the random variates, and the empirical cumulative
#'  distribution function versus the population cumulative distribution function.
#'
#' @param u
#'    Desired uniform distrubution elements in (0, 1) (default runif(1)). \cr
#'    If NULL, show only theorhetical graphs with no generation
#' @param minPlotQuantile Minimum quantile to plot
#' @param maxPlotQuantile Maximum quantile to plot
#' @param plot Whether to plot distribution
#' @param showCDF Whether to plot cumulative distribution (CDF)
#' @param show<%= PXF %> Whether to plot probability density/mass (<%= PXF %>)
#' @param showECDF Whether to plot empirical vs theorhetical CDF
#' @param show 4: CDF, 2: <%= PXF %>, 1: ECDF; sum for desired combo
#' @param maxInvPlotted Max number of inversions to plot before switching to quantile view
#' @param plotDelay Delay in seconds between CDF plots
#' @param animateAll Should animate all graphs (otherwise, CDF)
#' @param empColor Color of empirical data display
#' @param theoColor Color of theoretical data display
#' @param showTitle Whether to title the plot
#' @param respectLayout Should respect implemented plot setting
#' @param ... Possible additional arguments. Currently, additional arguments not considered.
#'
#' @return A vector of <%= distro %> distributed random variates
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
#'  default, though this can be changed by specifying \code{empColor}.
#'  All aspects of the population distribution are output in black.
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
#'  <%= ifunct %>(runif(10), <%= argshort %>, show<%= PXF %> = TRUE, showECDF = TRUE, empColor = "blue3")
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
#'  # plot CDF with inversion and ECDF using show
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = c(1,0,1))
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 5)
#'
#'  # plot CDF with inversion, <%= PXF %>, and ECDF using show
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = c(1,1,1))
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = 7)
#'
#'  # plot three different CDF+<%= PXF %>+ECDF vertical displays
#'  par(mfcol = c(3,3))  # 3 rows, 3 cols, filling columns before rows
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE)
#'
#'  # plot three different CDF+<%= PXF %>+ECDF horizontal displays,
#'  # with title only on the first display
#'  par(mfrow = c(3,3))  # 3 rows, 3 cols, filling rows before columns
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE, showTitle = FALSE)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7, respectLayout = TRUE, showTitle = FALSE)
#'
#'  # exhibit use of the respectLayout = FALSE option (default)
#'  par(mfrow = c(3,3))  # this will be ignored below since respectLayout = FALSE
#'  par(mar = c(5.1, 4.1, 4.1, 2.1))
#'  set.seed(8675309)
#'  <%= ifunct %>(runif(20), <%= argshort %>, show = 7)
#'  par("mfrow")  # will have been reset to c(3,3)
#'  par("mar")    # will have been reset to c(5.1, 4.1, 4.1, 2.1)
#'
#'
#'  # overlay visual exploration of ks.test results
#'  set.seed(54321)
#'  vals <- <%= ifunct %> (runif(10), <%= argshort %>, showECDF = TRUE)
#'  D <- as.numeric(ks.test(vals, "p<%= funct %>", <%= argshort %>)$statistic)
#'  for (x in seq(0.75, 1.25, by = 0.05)) {
#'   y <- p<%= funct %>(x, <%= argshort %>)
#'   segments(x, y, x, y + D, col = "darkgreen", lwd = 2, xpd = NA)
#'  }
#'
#'  # allow animation of CDF component only
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = c(1,1,0), plotDelay = 0.1, animateAll = FALSE)
#'  <%= ifunct %>(runif(10), <%= argshort %>, show =        7, plotDelay = 0.1, animateAll = FALSE)
#'
#'  # allow animation of all component
#'  <%= ifunct %>(runif(10), <%= argshort %>, show = c(1,1,0), plotDelay = 0.1, animateAll = TRUE)
#'  <%= ifunct %>(runif(10), <%= argshort %>, show =        7, plotDelay = 0.1)
#'
#'  # pause at each stage of inversion
#'  <%= ifunct %>(runif(10), <%= argshort %>, show =        7, plotDelay = -1)
#'
#' @keywords hplot dynamic distribution
#'
#' @concept random variate generation
#'
#' @author
#'    Barry Lawson (\email{blawson@@richmond.edu}), \cr
#'    Larry Leemis (\email{leemis@@math.wm.edu}), \cr
#'    Vadim Kudlay (\email{vadim.kudlay@@richmond.edu})
#'
#' @seealso \code{\link[=runif]{stats::runif}}
