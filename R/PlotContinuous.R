################################################################################
## PlotContinuous  -  i* continuous plotting
## -----------------------------------------------------------------------------
#' i* Plotting Function for Continuous Distributions
#'
#' @description
#'   Performs displays for i* functions (i.e. \code{iexp}), in which the parameters
#'   are leveraged to plot the CDF, PDF, and ECDF of the distribution.
#'   Used internally in continuous i* functions, but can be used externally
#'   to define other distributions with definable density, distribution, and
#'   quantile functions.
#'
#' @param u
#'    Desired uniform distrubution elements in (0, 1) (default runif(1)). \cr
#'    If NULL, show only theorhetical graphs with no generation
#' @param minPlotQuantile Minimum quantile to plot
#' @param maxPlotQuantile Maximum quantile to plot
#' @param plot Whether to plot distribution
#' @param showCDF Whether to plot cumulative distribution (CDF)
#' @param showPDF Whether to plot probability density (PDF)
#' @param showECDF Whether to plot empirical vs theorhetical CDF
#' @param show 4: CDF, 2: PDF, 1: ECDF; sum for desired combo
#' @param maxInvPlotted Max number of inversions to plot before switching to quantile view
#' @param plotDelay Delay in seconds between plots. If -1, use interactive mode
#' @param animateAll Should animate all graphs (otherwise, CDF)
#' @param empColor Color of empirical data display
#' @param theoColor Color of theorhetical data display
#' @param showTitle Whether to title the plot
#' @param respectLayout Should respect implemented plot setting
#' @param getDensity  A density function for the distribution (i.e. \code{dunif} for uniform)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param getDistro   A distribution function for the distribution (i.e. \code{punif} for uniform)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param getQuantile A quantile function for the distribution (i.e. \code{qunif} for uniform)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param hasCDF   Tells function if \code{showCDF} was specified. Used for determining
#'    priority of individual show- parameters and the main show parameter
#' @param hasPDF   Tells function if \code{showPDF} was specified. Used for determining
#'    priority of individual show- parameters and the main show parameter
#' @param hasECDF  Tells function if \code{showECDF} was specified. Used for determining
#'    priority of individual show- parameters and the main show parameter
#' @param titleStr String/Language of text to be displayed as the title
#'
#' @details
#'  Generates random variates using the inputted getDistro function and, optionally,
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
#'   The functionality of this function is a generalization of the functionality
#'   continuous i* functions such as \code{ibeta}, which should be considered for
#'   more detail on these parameters. Plotting functionality for i* functions is
#'   provided by \code{PlotContinuous} as well as its discrete distribution
#'   counterpart, \code{PlotDiscrete}.
#'
#' @return A vector of random variates distributed according to the provided
#'   distribution function
#'
#' @seealso \code{\link[=runif]{stats::runif}}
#' 
#' @importFrom grDevices dev.hold dev.flush recordPlot replayPlot adjustcolor 
#' @importFrom stats quantile
#'
#' @template signature
#' @concept  random variate generation
#' @export
################################################################################
PlotContinuous <- function(
    u               = runif(1),
    minPlotQuantile = 0.05,
    maxPlotQuantile = 0.95,
    plot            = TRUE,
    showCDF         = TRUE,
    showPDF         = FALSE,
    showECDF        = FALSE,
    show            = NULL,
    maxInvPlotted   = 50,
    plotDelay       = 0,
    animateAll      = TRUE,
    empColor        = "red3",
    theoColor       = "grey",
    showTitle       = TRUE,
    respectLayout   = FALSE,
    getDensity,
    getDistro,
    getQuantile,
    hasCDF,
    hasPDF,
    hasECDF,
    titleStr        = ""
) {
    #############################################################################
    # Run base parameter checking
    #############################################################################
    warnVal <- options("warn")          # save current warning setting...
    oldpar  <- par(no.readonly = TRUE)  # save current par settings
    options(warn = -1)  # suppress warnings

    checkVal(plot,     "l")
    checkVal(showCDF,  "l")
    checkVal(showPDF,  "l")
    checkVal(showECDF, "l")

    if (missing(hasCDF))
          hasCDF <- missing(show)
    else  checkVal(hasCDF,   "l")
    if (missing(hasPDF))
          hasPDF <- missing(show)
    else  checkVal(hasPDF,   "l")
    if (missing(hasECDF))
          hasECDF <- missing(show)
    else  checkVal(hasECDF,  "l")

    # parse show as appropriate
    showResults <- ParseShow(
      showBools   = c(showCDF, showPDF, showECDF),
      show        = show,
      ignoreBools = missing(showCDF) && missing(showPDF) && missing(showECDF)
    )
    showCDF  <- showResults[1]
    showPDF  <- showResults[2]
    showECDF <- showResults[3]

    checkVal(maxInvPlotted, "i", minex = 2)
    checkVal(animateAll,  "l")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
        stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1")

    if (!is.color(c(empColor)))   stop("'empColor' must be a valid color")
    if (!is.color(c(theoColor)))  stop("'theoColor' must be a valid color")

    checkVal(showTitle,     "l")
    checkVal(respectLayout, "l")
    checkVal(getDensity,  "f")
    checkVal(getDistro,   "f")
    checkVal(getQuantile, "f")

    options(warn = 1)  # set to immediate warnings

    #############################################################################
    # Initialize important variables
    #############################################################################

    # compute the plot min and max X
    fromX    <- getQuantile(minPlotQuantile)
    toX      <- getQuantile(maxPlotQuantile)

    # Generate smoothed exact CDF distribution
    x        <- seq(fromX, toX, by = (toX-fromX)/1000)
    exactCDF <- getDistro(x)

    # Generate smoothed exact CDF distribution
    exactPDF <- getDensity(seq(fromX, toX, by = (toX-fromX)/100))

    # Global variables to be used in later functions
    xVals          <- NULL # Will store all inverse values of u
    yThresh        <- NULL # Will store y threshhold for plotting
    pdfYMax        <- NULL # Will store maximum Y for PDF plots
    currSub        <- NULL # Will store a subset of xVals for animation
    xValsPlotted   <- NULL # Will store direct hash for x values plotted on
    yValsPlotted   <- NULL
    yValsPlotMax   <- 1
    uXLoc          <- NULL # Will Store location of points
    plot1          <- NULL # Will store state of CDF plot
    currSub        <- NULL # Will store growing subset of u to plot inversions

    histInfo       <- NULL # Will store histogram info for plotting on CDF/PDF
    maxStackHeight <- NULL # Histogram data for highest bar
    maxHistDensity <- NULL # Histogram data for max density

    # Return inverted values function
    ReturnVals <- function() {
      options(warn = warnVal$warn)
      par(oldpar)
      if (is.null(xVals))  return(invisible(xVals))  else  return(xVals)
    }

    # Bind colors to [0, 255]
    BindColor <- function(val){ return(max(0, min(val, 255))/255) }

    #############################################################################


    #############################################################################
    # Builds density histogram based on inputted set of data
    #############################################################################

    MakeHist <- function(xSubset) {

      # if subset empty don't plot any variates; only PDF and/or CDF
      if (is.null(xSubset)) { maxHistDensity <<- 0; return() }

      options(warn = -1)  # suppress warnings

      maxBins  <- 500
      histInfo <<- NULL

      fdBinSize <- 2 * IQR(xSubset) / (length(xSubset) ^ (1/3))
      fdNumBins <- (max(xSubset) - min(xSubset)) / fdBinSize

      # grab onto the histogram data for use in cdf and/or pdf plots;
      # start this here so we can use hist's rightmost bin point in xlim computation;
      # note that hist() can fail on Byzantine cases for any of "fd", "scott",
      # "sturges", so we'll use try/catch in that order;
      # also note that in some cases "fd" freaks out -- e.g.,
      #    > set.seed(123456); vals = igamma(runif(20),0.01,scale=0.03,showHist=T)
      #    > histInfo = hist(vals,breaks="fd",plot=FALSE)
      #    > length(histInfo$breaks)
      #    [1] 275065
      # In some _really_ bizarre cases, using "fd" really-really freaks out when
      # the IQR (see http://bit.ly/2lbSN34) is extremely small.  As a specific
      # example:
      #
      #    > n = 10; set.seed(9923527)
      #    > qvals = qgamma(runif(n), shape = 0.01, scale = 0.003)
      #    > binSize = 2 * IQR(qvals) / (10)^(1/3)
      #    > binSize
      #    [1] 7.105208e-19
      #    > numBins = (max(qvals) - min(qvals)) / binSize
      #    > numBins
      #    [1] 77600938
      # (!!)
      # So rather than wait for hist() to try to tell us the number of $breaks,
      # let's pre-compute for "fd", and immediately move on if more than, say, 500.
      # Note, however, that for hist's "fd", when IQR(xVals) returns 0, it defaults
      # to mad(xVals, constant = 2), and if that's 0, returns 1.  So we also need
      # to make sure that the bin size > 0 below before trying our "fd" condition.

      if (fdBinSize > 0 && fdNumBins <= maxBins) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "fd", plot = FALSE),
          error = function(err) { return(NULL) })}

      # if "fd" fails, histInfo will be NULL, so try "scott"
      if (is.null(histInfo) || length(histInfo$breaks) > maxBins) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "scott", plot = FALSE),
          error = function(err) { return(NULL) })}

      # if "scott" also fails, histInfo will be NULL, so try "sturges"
      if (is.null(histInfo)) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "sturges", plot = FALSE), error = function(err) {
            print(err); stop('inorm: Error internally using hist()') })}

      if (length(histInfo$breaks) > maxBins)
        stop(paste('inorm: Error using hist() -- more than', maxBins, 'bins'))

      maxHistDensity <<- max(histInfo$density)

      options(warn = 1)  # reset to our inorm state: immediate warnings

    }

    #############################################################################


    #############################################################################
    # Handles special case where inputted u is null or special assumptions should
    #   taken for plotting preferences
    #############################################################################

    if (is.null(u)) {
      # this corresponds to just showing the pdf and/or cdf distribution
      if (showECDF == TRUE)
        warning("ignoring showECDF = TRUE since u is NULL; plotting CDF")
      if (plot == FALSE) {
        warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)")
        plot <- TRUE
      }
      if (showCDF == TRUE || showECDF == TRUE) {
        # may ask to show CDF via either showCDF or showECDF, but we
        # plot the distro-only (when no variates) CDF via showECDF logic below
        showCDF  <- FALSE
        showECDF <- TRUE
      }
      # by default, if u is null and showPDF is missing (using default FALSE),
      # let's set showPDF to TRUE -- the user can override by giving F explicitly
      if (missing(showPDF))  showPDF <- TRUE

      plotDelay <- 0
      pdfYMax <- max(getDensity(x))
    } else {
      xVals <- getQuantile(u)  # generate the variates
      MakeHist(xVals)          # generate initial histogram with values

      # if the majority of density is packed into the first histogram bin, set
      # the y range max to be no bigger than 75% > than max pdf value
      pdfYMax <- max(exactPDF * 1.75, maxHistDensity)
    }

    # if no plot, or "equal" min/max quantile values, just return the variates...
    if (plot == FALSE)  ReturnVals()

    if (showCDF == FALSE && showPDF == FALSE && showECDF == FALSE) {
      if (plot == TRUE)
        warning("ignoring plot since showCDF, showPDF, and showECDF are all FALSE")
      if (is.null(xVals))  ReturnVals()
    }

    if (round(fromX, digits = 7) == round(toX, digits = 7)) {
      warning(paste("politely declining to plot:",
                    "essentially equal min/max quantile values"))
      if (is.null(xVals))  ReturnVals()
    }

    #############################################################################


    #############################################################################
    # Handles plotting initial setup
    #############################################################################

    # use sum to determine how many plot "shows" are TRUE ==> set # rows
    minPlots  <- sum(showCDF, showPDF, showECDF)

    # try to respect user's previously defined par(mfrow) or par(mfcol) if
    # sufficient to show all plots; if not, display a warning message
    if (respectLayout) {
      userPlots <- prod(par("mfrow"))
      if (userPlots < minPlots)
        warning(paste(
          'Cannot display the requested ', minPlots,
          ' plots simultaneously because layout is set for ', userPlots,
          ' plot', if (userPlots > 1) 's. ' else '. ',
          'Please use \'par\' to set layout appropriately, e.g., ',
          'par(mfrow = c(', minPlots, ', 1)) or ',
          'par(mfcol = c(1, ', minPlots, ')).', sep = ""))
    }
    # Otherwise, force layout to show only these plots
    else if (plotDelay == -1)  par(mfrow = c(3, 1))
    else  par(mfrow = c(minPlots, 1))

    # Number of plots needed to fill based on par specifications
    numPlotSlots  <- max(prod(par("mfrow")), prod(par("mfcol")))
    # Number of plot slots that have been filled so far
    numPlotted <- 0

    # set default margins for plots
    botMar <- if (minPlots > 1) 4.1 else 5.1
    par(mar = c(botMar, 4.1, 2.1, 1.5))

    # set color for plotting variates / estimated curves
    colorTrans <- adjustcolor(empColor, alpha.f = 0.5 * max(0.5, (1 - length(u)/(length(u)+1000))))

    colorOutline  <- empColor   # Color of circle outline on inversion endpoints
    lineSideColor <- empColor   # Color for horizontal lines; can change later

    # Scale points relative to length of u
    pointCex <- if (length(u) < 50) 0.8 else 0.8 * (50/length(u))

    # set line width and cex paramters for plotting
    lwd <- max(2, round(log10(length(xVals))))  # increases with num xVals
    plot_cex <- par("cex")  # R automatically lowers if three plots...

    # Note that the simulation illustrates random number inversion/tracking
    if (!is.null(xVals))
      titleStr <- paste(titleStr, "random variate generation", sep = "")

    #############################################################################


    #############################################################################
    # Formats a plot for output, adding axes and labels
    #############################################################################

    FormatPlot <- function(xtext = "", ytext = "", isCDF = TRUE) 
    {
      # draw our own axes (R's pretty() sometimes gives a min tick or max tick
      # value that is outside the min/max quantile bounds);
      # add the vertical axis
      axis(2, las = 1)
      mtext(ytext, side = 2, line = 2.5, cex = plot_cex)

      ticks <- par("xaxp")

      # remove any tick value that R includes that's outside fromX:toX bounds
      tickVals <- seq(ticks[1], ticks[2], by = (ticks[2] - ticks[1])/ticks[3])
      if (tickVals[1] < fromX)
        tickVals <- tickVals[-1]                    # remove first
      if (tickVals[length(tickVals)] > toX)
        tickVals <- tickVals[-length(tickVals)]     # remove last

      # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
      # min/max quantile, even if contrary to R's pretty() decision;
      # and a 2nd axis with line but no ticks, extending thru the max/min x vals
      axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
      axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
      mtext(xtext, side = 1, line = 2, cex = plot_cex)

      # draw vertical dotted lines corresponding to the min/max quantile values
      maxY <- if (isCDF) 1 else pdfYMax
      if (maxPlotQuantile < 1)
        segments(toX,   0, toX,   maxY, lty = "dotted", xpd = NA)
      if (minPlotQuantile > 0)
        segments(fromX, 0, fromX, maxY, lty = "dotted", xpd = NA)
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the cdf...
    #############################################################################

    TryInitPlotCDF <- function()
    {
      if (showCDF == FALSE && plotDelay != -1)  return()

      # totally empty plot allows us to know in advance what the x ticks will
      # be according to R's call to pretty()... use tick info for any/all plots...
      # ylim upper is 1, unless showing pdf (at top) but not cdf...
      firstPlotYMax <- if (showCDF == FALSE && showPDF == TRUE) pdfYMax else 1
      plot(NA, NA, xlim = c(fromX, toX), ylim = c(0, firstPlotYMax),
          xlab = "", ylab = "", bty = "n", las = 1)

      numPlotted <<- numPlotted + 1

      if (showTitle)  title(titleStr, cex.main = 0.975)

      # plot the cdf (for now in lightgray)
      lines(x, exactCDF, lwd = lwd, col = theoColor)
      FormatPlot(xtext = "x", ytext = "F(x)", isCDF = TRUE)

      if (is.null(u))  return()

      # try to find appropriate x-value to draw u's and u-points...
      # by trial and error, decreasing by -0.02*plot_range seems to work...
      uXLoc <<- fromX - (0.02 * (toX - fromX))

      # NOTES: on plotting histogram density rectangles in vertical stack: height
      # of pch=20 is strheight("x")*0.75*(2/3), and use "rect" of that height --
      # see ?points (pch 'Details' & 'pch values');
      # strheight("x") changes with different display();
      # from experimentation, it appears as though it will be OK to plot about 16
      # _full_ rect heights down, starting from the bottom of the horizontal
      # axis, and not impede on the hist() histogram below...
      # so use this to compute the height of one "stacked rect", which will allow
      # us to plot one large rectangle representing a stack of "stacked rects",
      # such that we don't extend beyond this 16-full-heights vertical range...
      # (side note: the default R bounding box for points is a square of side 0.01
      #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)
      pch20Height     <- strheight("x") * 0.75 * (2 / 3)
      maxInStack      <- 16
      maxStackHeight <<- maxInStack * pch20Height

      # now draw the inverting...
      xValsPlotted   <<- rep(0, length(histInfo$counts))
      if (length(u) > 50) {
        yThresh      <<- max(100, 1000/length(u))
        yValsPlotted <<- rep(0, yThresh)
      }
    }

    #############################################################################
    # Update CDF plot by adding next random variate inversion
    #############################################################################

    TryPlotCDFInversion <- function(i, val, col = empColor, isQuantile = FALSE)
    {
      if (showCDF == FALSE && plotDelay != -1)  return()

      # Consider values being passed in instead of indeces
      inv.y <- if (!missing(i))  u[i]      else  val
      inv.x <- if (!missing(i))  xVals[i]  else  getQuantile(val)

      # Override global trans/outline colors if requested
      if (col != empColor) {
        empColor     <- col[1]
        colorTrans   <- col[min(2, length(col))]
        colorOutline <- col[min(3, length(col))]
      }

      # find the low limit of the bin holding xVal
      xBinIdx <- which(inv.x < histInfo$breaks)[1] - 1

      # draw the u-value point
      DrawPoint(uXLoc, inv.y, col = colorTrans, bg = colorOutline, cex = pointCex)

      # bgl 17 Dec 2020
      # if plotting quantile lines, make sure to use darker color for clarity
      #lineSideColor <- colorTrans
      lineSideColor <- if (isQuantile) empColor else colorTrans
      lwd <- if (isQuantile) 1.5 else 1

      # draw the dashed segment from u to the cdf, and cdf to horizontal:
      if (minPlotQuantile <= inv.y && inv.y <= maxPlotQuantile) {
        # if u is within min/max quantile bounds, draw horizontal and vertical
        segments(uXLoc, inv.y, inv.x, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
        segments(inv.x, inv.y, inv.x, 0,     lty = "dashed", lwd = lwd, col = lineSideColor)
        #segments(inv.x, inv.y, inv.x, 0,     lty = "dashed", col = colorTrans)
      } else if (inv.y > maxPlotQuantile) {
        # if u is greater than max quantile, draw horizontal to max x axis val
        segments(uXLoc, inv.y, toX, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
      }
      else  {
        # if u is less than min quantile, draw horizontal to min x axis val,
        segments(uXLoc, inv.y, fromX, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
      }

      if (length(u) == 1)
        text(0.75*(fromX + toX), 0.1, 
            paste(round(inv.y, 3), sym$arrow, round(inv.x, 3)),
            col = empColor, cex = ScaleFont(15))

      # for this variate value, track the number in hist bin plotted
      xValsPlotted[xBinIdx] <<- xValsPlotted[xBinIdx] + 1

      # draw the variate on the line only if within min/max quantile bounds
      if (minPlotQuantile <= inv.y && inv.y <= maxPlotQuantile)
        DrawPoint(inv.x, 0, col = colorTrans, bg = colorOutline, cex = pointCex)
    }

    FormatCDF <- function() 
    {
      if (showCDF == FALSE && plotDelay != -1)  return()

      if (length(u) > 1) {
        # now draw rectangles beneath the horizontal axis showing upside-down
        # density of points; just draw a single rectange of the max stack height
        # (computed above) scaled by the histogram's bin density
        xAxisYLoc <- -0.04
        for (i in 1:length(histInfo$density))
        {
          rectXMin    <- histInfo$breaks[i]
          rectXMax    <- histInfo$breaks[i+1]
          rectDensity <- histInfo$density[i]

          # avoid drawing if zero density or completely outside limits
          if (rectDensity == 0 || rectXMin > toX || rectXMax < fromX) next

          # don't draw beyond the lower/upper limits of the xaxis
          if (rectXMin < fromX) rectXMin <- fromX
          if (rectXMax > toX)   rectXMax <- toX

          rect(rectXMin, xAxisYLoc - (rectDensity/maxHistDensity * maxStackHeight),
               rectXMax, xAxisYLoc, border = NA, col = colorTrans, xpd = NA)
        }

        # If length > 100, darken u groups that are especially dense
        if (length(u) > 100) {

          # Record quantile threshholds to assess darkness of lines
          # freqYMax <- max(yValsPlotted)
          freqYMin <- quantile(yValsPlotted, 15:19*0.05)

          for (i in (1:150)/150) {  # Use 1/150 as Y increment
            yIndex <- ceiling(yThresh * i)
            if (yValsPlotted[yIndex] > min(freqYMin[1])) {

              # Initialize shade to 1 and decrease for each threshhold surpassed
              shade <- 1
              for (j in 1:length(freqYMin))
                if (yValsPlotted[yIndex] >= freqYMin[j])  shade <- shade - 0.15

              lineSideColor <- adjustcolor(0.8, shade, shade, shade)

              # Draw segment
              segments(uXLoc, i, min(getQuantile(i), toX), i, lwd = 1,
                lty = "dashed", col = lineSideColor)
            }
          }
        }
      }

      # redraw the cdf curve back over top of dashed variate generation lines...
      lines(x, exactCDF, lwd = lwd, col = adjustcolor(theoColor, 1, 0.9, 0.9, 0.9))

      FormatPlot(xtext = "x", ytext = "F(x)", isCDF = TRUE)
      # re-draw the one "u" atop all u-points
      text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot_cex
    }

    TryPlotCDFQuant <- function(i)
    {
      if (showCDF == FALSE)  return()

      TryInitPlotCDF()

      quants <- quantile(u[1:i], seq(0, 1, 0.25))

      # bgl 17 Dec 2020: remove "..." b/w quantile lines
      #for (j in 2:length(quants)) {
      #  y0 <- quants[j-1]
      #  x0 <- getQuantile(y0)
      #  y1 <- quants[j]
      #  x1 <- getQuantile(y1)
      #
      #  if (y1 - y0 > 0.05)
      #    text(labels = sym$dots, x = 0, y = (y1 + y0)/2, srt = 90)
      #  if (x1 - x0 > abs(toX - fromX)/20)
      #    text(labels = sym$dots, x = (x1 + x0)/2, y = 0.05)
      #}

      darkCol  <- rep("red3")
      TryPlotCDFInversion(val = quants[1], col = darkCol, isQuantile = TRUE)
      TryPlotCDFInversion(val = quants[2], col = darkCol, isQuantile = TRUE)
      TryPlotCDFInversion(val = quants[3], col = darkCol, isQuantile = TRUE)
      TryPlotCDFInversion(val = quants[4], col = darkCol, isQuantile = TRUE)
      TryPlotCDFInversion(val = quants[5], col = darkCol, isQuantile = TRUE)

      for (j in 1:i) {
        DrawPoint(uXLoc, u[j], col = colorTrans, bg = colorOutline, cex = pointCex)
        DrawPoint(xVals[j], 0, col = colorTrans, bg = colorOutline, cex = pointCex)
      }
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the pdf / histogram of variates
    #############################################################################

    TryPlotPDF <- function(xSubset)
    {
      if (showPDF == FALSE)  return()

      plot(NA, NA, las = 1, bty = "n",
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "",
           xlim = c(fromX, toX), ylim = c(0, pdfYMax))

      if (showCDF == FALSE && showTitle)  title(titleStr, cex.main = 0.975)

      numPlotted <<- numPlotted + 1

      if (!is.null(xSubset))  # plotting variates
      {
        # draw the histogram, respecting the appropriate clip region
        clip(fromX, toX, 0, pdfYMax)
        plot(histInfo, freq = FALSE, col = empColor, add = TRUE)
        do.call("clip", as.list(par("usr"))) # reset clip

        # for any histogram bins that are cut off by pdfYMax clipping,
        # draw "..." above the bin indicating that it continues north
        for (i in 1:length(histInfo$density))
        {
          rectDensity <- histInfo$density[i]
          if (rectDensity > pdfYMax) {
            rectXMin    <- max(histInfo$breaks[i], fromX)
            rectXMax    <- min(histInfo$breaks[i+1], toX)
            rectCenter  <- rectXMin + (rectXMax - rectXMin) / 2
            text(rectCenter, pdfYMax, "...", pos = 3, srt = 90, xpd = TRUE)
          }
        }
      }

      # superimpose the theoretical (use xAxisMax here so the curve will
      # extend through to the end of the axis)
      curve(getDensity(x), xlim = c(fromX, toX), add = TRUE, lwd = 2, col = theoColor)
      FormatPlot(xtext = "x", ytext = "f(x)", isCDF = FALSE)
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the ecdf with superimposed cdf
    #############################################################################

    TryPlotECDF <- function(xSubset){

      if (showECDF == FALSE)  return()

      plot(NA, NA, xlim = c(fromX,toX), ylim = c(0,1),
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

      if (showCDF == FALSE && showPDF == FALSE && showTitle)  title(titleStr, cex.main = 0.975)

      numPlotted <<- numPlotted + 1

      clip(fromX, toX, -0.1, 1.1)  # need to go lower than y=0 for the y=0 lwd

      # not plotting variates
      if (is.null(xVals))
        lines(x, exactCDF, lwd = 2, col = "black", lty = "solid")

      else {
        # plot the cdf -- ensure doesn't plot outside bounds;  draw wider
        # so ecdf can clearly be seen overlayed, and in light gray, with
        # dashed black over top
        lwd <- 3
        cdf_lwd <- 8
        lines(x, exactCDF, lwd = cdf_lwd, col = "grey")
        lines(x, exactCDF, lwd = 1, col = "black", lty = "dashed")

        # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
        # plot.ecdf() function doesn't want to draw the first and last horizontals
        # all the way from/to fromX/toX... so just draw it ourselves...
        ecdfFcn   <- ecdf(xSubset)
        ecdfKnots <- knots(ecdfFcn)

        clip(fromX, toX, -0.1, 1)
        plot.ecdf(ecdfFcn, verticals = TRUE, pch = "", add = TRUE,
                  lwd = lwd, col = empColor, col.01line = NA)
        segments(fromX, 0, ecdfKnots[1], 0,
                 lwd = lwd, col = empColor, lend = 1)

        segments(ecdfKnots[length(ecdfKnots)], 0.99, toX, 1,
                 lwd = lwd, col = empColor, lend = 1)
      }

      do.call("clip", as.list(par("usr"))) # reset clip
      FormatPlot(xtext = "x", ytext = "F(x)", isCDF = TRUE)

    }

    #############################################################################


    ##############################################################################
    ##  TryPausePlot
    ## --------------------------------------------------------------------------
    ## Handles pausing of plot at regular intervals depending on plotDelay.
    ## Also handles user input and processing for plotDelay == -1
    ##############################################################################

    TryPausePlot <- function(xSubset) {

      if (plotDelay == 0)  return()

      # Flush display, pause, then hold
      dev.flush()
      if (plotDelay > 0)  Sys.sleep(plotDelay)
      dev.hold()

      if (plotDelay == -1) {
        input <- " "

        # While the input is not just an enter, keep looping request for input
        while (plotDelay == -1 && input != "") {
          #input <- readline("\nHit 'ENTER' to proceed, 'q' to end, or 'help' for more: ")
          input <- readline("Hit 'ENTER' to proceed, 'q' to end, or 'help' for more: ")

          if (input == "q") {
            plotDelay <<- 0
          }

          else if (input == "help") {
            message("'latest'          = shows latest inversion")
            message("'showPDF'         = toggles parameter for next run")
            message("'showCDF'         = toggles parameter for next run")
            message("'showECDF'        = toggles parameter for next run")
            message("'q'               = change plotDelay to 0")}

          else if (input == "q")          plotDelay <<- 0
          else if (input == "showCDF")    showCDF   <<- !showCDF
          else if (input == "showPDF")    showPDF   <<- !showPDF
          else if (input == "showECDF")   showECDF  <<- !showECDF

          # Shows job statistics if requested
          else if (input == "latest")
            message(" - Mapped ", u[length(xSubset)],
                    " ", sym$arrow, " ", xSubset[length(xSubset)])

          if (numPlotSlots < showCDF + showPDF + showECDF)
            message(showCDF + showPDF + showECDF, " plots but only ", numPlotSlots,
                    " slots; some of the plots may no longer appear")
        }
      }
    }

    ##############################################################################


    ####################################################################################
    ##  CurrResetPlot
    ## --------------------------------------------------------------------------------
    ##  Pad the current plot components to fill up the graph;
    ##  This keeps everything in animation consistently located
    ####################################################################################
    CurrResetPlot <- function(force = FALSE) {

      if (numPlotted == 0 && force == FALSE)  return()

      while (numPlotted %% numPlotSlots > 0) {
        numPlotted <<- numPlotted + 1
        plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n",
             xlab = "", ylab = "", bty = "n", las = 1, type = "s")
      }

      numPlotted <<- numPlotted %% numPlotSlots
    }
    ####################################################################################


    #############################################################################
    # Handle plotting of the visuals
    #############################################################################

    if(plotDelay != 0 || respectLayout == FALSE)
      dev.flush(dev.hold() - 1L) # Resets hold level and initializes to 1
    
    TryInitPlotCDF()             # Initialize the CDF plot if necessary

    plot1 <- recordPlot()
    
    xtimes <- PlotTimer() # Initialize timer plot for execution times
    
    for (i in 1:length(u)){

      # Message of progress; will be taken out later
      if (length(u) > 2000 && i %% 1000 == 0)
        message("Processed ", i, " random variables")

      # If full animation is enabled, get subset and compute histogram for output
      if (plotDelay != 0 && animateAll) {
        currSub <- xVals[0:i]
        MakeHist(currSub)
      }

      if (plotDelay != 0) {
        if (showCDF == TRUE && i >= maxInvPlotted && animateAll) {
          ## Too many inversions to plot; switching to quantiles
          TryPlotCDFQuant(i)
        }
        else if (i < maxInvPlotted) {
          numPlotted <- numPlotted + 1
        }
      }
      
      # Plot the next u value inversion in CDF plot
      TryPlotCDFInversion(i)

      if (plotDelay != 0) {
        if (animateAll) {

          if (i < maxInvPlotted)  plot1 <- recordPlot()
          if (showCDF == TRUE)    FormatCDF()
          else  CurrResetPlot(TRUE)

          TryPlotPDF(currSub)
          TryPlotECDF(currSub)
        }
        
        TryPausePlot(currSub)
        
        CurrResetPlot()

        if (plotDelay == 0 && i < maxInvPlotted - 1)
          numPlotted <- 1

        if (animateAll && i < maxInvPlotted - 1)
            replayPlot(plot1)
      }
    } # for (i in 1:length(u))

    MakeHist(xVals)                  # Generate final hist (consider optimizing)

    if (showCDF == TRUE) {
      if (length(u) >= maxInvPlotted) {
        CurrResetPlot(TRUE)
        TryPlotCDFQuant(length(u))
      } else {
        if (animateAll)  replayPlot(plot1)
        TryPlotCDFInversion(length(u)) # Plot last inversion onto CDF if shown
      }
      FormatCDF()                      # Format CDF for final output if shown
    } else CurrResetPlot(TRUE)

    TryPlotPDF(xVals)                # Plot and format PDF  output if shown
    TryPlotECDF(xVals)               # Plot and format ECDF output if shown
    dev.flush()                      # Render the final plot state

    #############################################################################
    # reset display, margins, and warnings
    #############################################################################

    ReturnVals()                     # return a value/vector of F^-1(u)
}
