################################################################################
#  igeom - IDF Visualization Function for Geometric Distribution
# ------------------------------------------------------------------------------
#  The igeom R function visualizes the geometric idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical geometric distribution superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Geometric
#' @templateVar ifunct   igeom
#' @templateVar funct    geom
#' @templateVar PXF      PMF
#' @templateVar arglong  prob = 0.25
#' @templateVar argshort 0.4
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-1
#' @template -geom
#' @template i-2
#' @export
################################################################################
igeom <- function(u = runif(1), prob,
                  minPlotQuantile = 0,
                  maxPlotQuantile = 0.95,
                  plot            = TRUE,
                  showCDF         = TRUE,
                  showPMF         = FALSE,
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

  checkVal(prob, minex = 0, maxex = 1)
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
  getDensity  <- function(d)  dgeom(d, prob)  #d
  getDistro   <- function(d)  pgeom(d, prob)  #p
  getQuantile <- function(d)  qgeom(d, prob)  #q

  titleStr <- paste("Geom (p = ", round(prob, 3), ")\n", sep = "")

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
  if (!all(oldpar$mfrow == par()$mfrow) || !all(oldpar$mfcol == par()$mfcol))
    par(oldpar)

  return(out)
}
