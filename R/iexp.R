################################################################################
#  iexp - IDF Visualization Function for Exponential Distribution
# ------------------------------------------------------------------------------
#  The iexp R function visualizes the exponential idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical exponential distribution superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Exponential
#' @templateVar ifunct   iexp
#' @templateVar funct    exp
#' @templateVar PXF      PDF
#' @templateVar arglong  rate = 3
#' @templateVar argshort 2
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.99
#'
#' @template i-1
#' @template -exp
#' @template i-2
#' @export
################################################################################
iexp <- function(u = runif(1), rate = 1,
                  minPlotQuantile = 0,
                  maxPlotQuantile = 0.99,
                  plot            = TRUE,
                  showCDF         = TRUE,
                  showPDF         = FALSE,
                  showECDF        = FALSE,
                  show            = NULL,
                  maxInvPlotted   = 50,
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

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if (length(u) == 0)  u <- NULL

  checkVal(rate, minex = 0)
  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, maxex = 1)

  options(warn = 1)                   # set to immediate warnings

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  # Define getter functions
  getDensity  <- function(d)  dexp(d, rate)  #d
  getDistro   <- function(d)  pexp(d, rate)  #p
  getQuantile <- function(d)  qexp(d, rate)  #q

  titleStr <- paste("Exp (", sym$lambda, " = ", round(rate, 3), ")\n", sep = "")

  #############################################################################

  out <- PlotContinuous(
    u                = u,
    minPlotQuantile  = minPlotQuantile,
    maxPlotQuantile  = maxPlotQuantile,
    plot             = plot,
    showCDF          = showCDF,
    showPDF          = showPDF,
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
    hasPDF           = !missing(showPDF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )


  # reseting par and warning settings
  options(warn = warnVal$warn)
  if (!all(oldpar$mfrow == par()$mfrow) || !all(oldpar$mfcol == par()$mfcol))
    par(oldpar)

  return(out)
}
