################################################################################
#  iunif - IDF Visualization Function for Uniform Distribution
# ------------------------------------------------------------------------------
#  The \code{iunif} R function visualizes the uniform idf evaluated at a
#  provided uniform(0,1) u. This will graph the idf in action (via dashed lines
#  back across the cdf). Note that the u argument can be a scalar or vector.
#  If a vector, multiple dashed lines will be displayed, and the return type
#  will be a vector. The function also gives the option of displaying a histogram
#  of the variates generated, with theoretical uniform distribution superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Uniform
#' @templateVar distrolc uniform
#' @templateVar ifunct   iunif
#' @templateVar funct    unif
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  min = -10, max = 10
#' @templateVar argshort 0, 10
#' @templateVar minPQ    0
#' @templateVar maxPQ    1
#'
#' @template i-cont
#' @template -unif
#' @template i-2
#' @export
################################################################################
iunif <- function(u = runif(1), min = 0, max = 1,
                  minPlotQuantile = 0,
                  maxPlotQuantile = 1,
                  plot            = TRUE,
                  showCDF         = TRUE,
                  showPDF         = TRUE, 
                  showECDF        = TRUE, 
                  show            = NULL,
                  maxInvPlotted   = 50,
                  plotDelay       = 0,
                  sampleColor     = "red3",
                  populationColor = "grey",
                  showTitle       = TRUE,
                  respectLayout   = FALSE, ...)
{
  #############################################################################

  # using on.exit w/ par per CRAN suggestion (add 22 Nov 2023)
  oldpar <- par(no.readonly = TRUE)  # save current par settings (add 22 Nov 2023)
  on.exit(par(oldpar))               # add (22 Nov 2023)

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  #warnVal <- options("warn")          # save current warning setting... (del 22 Nov 2023)

  #############################################################################

  #options(warn = -1)          # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if(length(u) == 0)  u <- NULL

  checkVal(min, maxex = Inf, minex = -Inf)
  checkVal(max, maxex = Inf, minex = -Inf)
  if (min >= max)
    stop("'min' and 'max' must each be numeric values such that 'min' < 'max'")

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, max = 1)

  #options(warn = 1)                   # set to immediate warnings (del 22 Nov 2023)

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime") {
      # mod 23 Nov 2023
      #warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0",
              immediate. = TRUE)
    }
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  # Define getter functions
  getDensity  <- function(d)  dunif(d, min, max)  #d
  getDistro   <- function(d)  punif(d, min, max)  #p
  getQuantile <- function(d)  qunif(d, min, max)  #q

  # using plotmath for [nothing here]; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  titleStr <- as.expression(bquote(bold(
                    "Uniform (" ~ "a" ~ "=" ~ .(round(min, 3)) * ","
                                ~ "b" ~ "=" ~ .(round(max, 3)) ~ ")"
              )))

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
    sampleColor      = sampleColor,
    populationColor  = populationColor,
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

  # resetting par and warning settings
  #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)

  ## using on.exit for par RE CRAN suggest (del 22 Nov 2023)
  #if (!all(oldpar$mfrow == par()$mfrow)) {
  #  # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
  #  par(mfrow = oldpar$mfrow)
  #}

  if (!is.null(out)) return(out)
}
