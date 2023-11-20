################################################################################
#  ibinom - IDF Visualization Function for Binom Distribution
# ------------------------------------------------------------------------------
#  The ibinom R function visualizes the binomial idf evaluated at a
#  provided uniform[0,1) u. This will graph the idf in action (via dashed lines
#  back across the cdf). Note that the u argument can be a scalar or vector.
#  If a vector, multiple dashed lines will be displayed, and the return type will
#  be a vector. The function also gives the option of displaying a discrete
#  histogram of the variates generated, with theoretical binomial superimposed
#  as spike/dots.
# ------------------------------------------------------------------------------
#' @templateVar distro   Binomial
#' @templateVar ifunct   ibinom
#' @templateVar funct    binom
#' @templateVar PXF      PMF
#' @templateVar arglong  size = 7, prob = 0.4,
#' @templateVar argshort 10, 0.3
#' @templateVar minPQ    0
#' @templateVar maxPQ    1
#'
#' @template i-1
#' @template -binom
#' @template i-2
#' @export
################################################################################
ibinom <- function(u = runif(1), size, prob,
                   minPlotQuantile = 0,
                   maxPlotQuantile = 1,
                   plot            = TRUE,
                   showCDF         = TRUE,
                   showPMF         = FALSE,
                   showECDF        = FALSE,
                   show            = NULL,
                   maxInvPlotted     = 50,
                   plotDelay       = 0,
                   animateAll      = plotDelay > 0 || plotDelay == -1,
                   empColor        = "red3",
                   theoColor       = "grey",
                   showTitle       = TRUE,
                   respectLayout   = FALSE, ...)
{
  #############################################################################

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  warnVal <- options("warn")          # save current warning setting...
  oldpar  <- par(no.readonly = TRUE)  # save current par settings

 #############################################################################

  options(warn = -1)          # suppress warnings

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1)) stop("must have 0 < u < 1")
  if(length(u) == 0) u <- NULL

  checkVal(size, type = "i", minex = 0)
  checkVal(prob, min = 0, max = 1)

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, max = 1)

  options(warn = 1)                   # set to immediate warnings

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  # Define getter functions
  getDensity  <- function(d)  dbinom(d, size, prob)  #d
  getDistro   <- function(d)  pbinom(d, size, prob)  #p
  getQuantile <- function(d)  qbinom(d, size, prob)  #q

  titleStr <- paste("Binomial (",
                    "n = ", round(size, 3), ", ",
                    "p = ", round(prob, 3)
                    , ")\n", sep = "")

  #############################################################################

  out <- PlotDiscrete(
    u                = u,
    minPlotQuantile  = minPlotQuantile,
    maxPlotQuantile  = maxPlotQuantile,
    plot             = plot,
    showCDF          = showCDF,
    showPMF          = showPMF,
    showECDF         = showECDF,
    show             = show,
    maxInvPlotted    = maxInvPlotted,
    plotDelay        = plotDelay,
    animateAll       = animateAll,
    empColor         = empColor,
    theoColor        = theoColor,
    showTitle        = showTitle,
    respectLayout    = respectLayout,
    getDensity       = getDensity,
    getDistro        = getDistro,
    getQuantile      = getQuantile,
    hasCDF           = !missing(showCDF),
    hasPMF           = !missing(showPMF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )

  # reseting par and warning settings
  options(warn = warnVal$warn)
  if (!all(oldpar$mfrow == par()$mfrow) || !all(oldpar$mfcol == par()$mfcol))
    par(oldpar)

  return(out)
}
