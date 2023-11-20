################################################################################
#  it - IDF Visualization Function for Student T Distribution
# ------------------------------------------------------------------------------
#  The it R function visualizes the Student t idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical Student t distribution superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Student T
#' @templateVar ifunct   it
#' @templateVar funct    t
#' @templateVar PXF      PDF
#' @templateVar arglong  df = 5, ncp = 10
#' @templateVar argshort 4
#' @templateVar minPQ    0.01
#' @templateVar maxPQ    0.99
#'
#' @template i-1
#' @template -t
#' @template i-2
#' @export
################################################################################
it <- function (u = runif(1), df, ncp,
                minPlotQuantile = 0.01,
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

  checkVal(df, minex = 0, define = "degrees of freedom")
  if (!missing(ncp))
    checkVal(ncp, min = -37.62, max = 37.62, define = "non-centrality param")

  checkQuants(minPlotQuantile, maxPlotQuantile, minex = 0, maxex = 1)

  options(warn = 1)                   # set to immediate warnings

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  no.ncp <- missing(ncp)

  # Define getter functions
  getDensity  <- function(d) if (no.ncp) dt(d, df) else dt(d, df, ncp) #d
  getDistro   <- function(d) if (no.ncp) pt(d, df) else pt(d, df, ncp) #p
  getQuantile <- function(d) if (no.ncp) qt(d, df) else qt(d, df, ncp) #q

  titleStr <- paste("t (", sym$nu, " = ", round(df, 3),
                    (if(!missing(ncp)) paste(",", sym$sigma, "=", round(ncp,3))
                    ), ")\n", sep = "")

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
