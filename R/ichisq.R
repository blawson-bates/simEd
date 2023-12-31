###############################################################################
#  ichisq - IDF Visualization Function for Chi-Squared Distribution
# ------------------------------------------------------------------------------
#  The ichisq R function visualizes the Chi-Squared idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical chi squared superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Chi-Squared
#' @templateVar distrolc chi-squared
#' @templateVar ifunct   ichisq
#' @templateVar funct    chisq
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  df = 3, ncp = 2
#' @templateVar argshort 3
#' @templateVar minPQ    0.01
#' @templateVar maxPQ    0.99
#'
#' @template i-cont
#' @template -chisq
#' @template i-2
#' @export
################################################################################
ichisq <- function (u = runif(1), df, ncp = 0,
                minPlotQuantile = 0.01,
                maxPlotQuantile = 0.99,
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
                respectLayout   = FALSE, 
                restorePar      = TRUE,  # add 23 Nov 2023
                ...)
{
  #############################################################################

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  #warnVal <- options("warn")          # save current warning setting... (del 22 Nov 2023)

  #############################################################################

  #options(warn = -1)          # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if (length(u) == 0)  u <- NULL

  checkVal(df,  minex = 0, define = "degrees of freedom")
  checkVal(ncp, min = 0,   define = "non-centrality param")

  checkQuants(minPlotQuantile, maxPlotQuantile, minex = 0, maxex = 1)

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

  no.ncp <- missing(ncp)

  # Define getter functions
  getDensity  <- function(d)
      if (no.ncp)  dchisq(d, df)  else  dchisq(d, df, ncp)  #d
  getDistro   <- function(d)
      if (no.ncp)  pchisq(d, df)  else  pchisq(d, df, ncp)  #p
  getQuantile <- function(d)
      if (no.ncp)  qchisq(d, df)  else  qchisq(d, df, ncp)  #q

  # using plotmath for x_0, gamma; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (no.ncp) {
    titleStr <- as.expression(bquote(bold(
                    "Chi-Squared (" ~ "n =" ~ .(round(df, 3)) ~ ")"
                )))
  } else {
    titleStr <- as.expression(bquote(bold(
                    "Chi-Squared (" ~ "n"    ~ "=" ~ .(round(df,  3)) * ","
                                    ~ lambda ~ "=" ~ .(round(ncp, 3)) ~ ")"
                )))
  }

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
    restorePar       = restorePar,    # add 23 Nov 2023
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
