################################################################################
## PlotDiscrete  -  i* discrete plotting
## -----------------------------------------------------------------------------
#' i* Plotting Function for Discrete Distributions
#'
#' @description
#'   Performs displays for i* functions (i.e. \code{ibinom}), in which the parameters
#'   are leveraged to plot the CDF, PMF, and ECDF of the distribution.
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
#' @param showPMF Whether to plot probability density (PMF)
#' @param showECDF Whether to plot empirical vs theorhetical CDF
#' @param show 4: CDF, 2: PMF, 1: ECDF; sum for desired combo
#' @param maxInvPlotted Max number of inversions to plot before switching to quantile view
#' @param plotDelay Delay in seconds between plots. If -1, use interactive mode
#' @param animateAll Should animate all graphs (otherwise, CDF)
#' @param empColor Color of empirical data display
#' @param theoColor Color of theorhetical data display
#' @param showTitle Whether to title the plot
#' @param respectLayout Should respect implemented plot setting
#' @param getDensity  A density function for the distribution (i.e. \code{dpois} for Poisson)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param getDistro   A distribution function for the distribution (i.e. \code{ppois} for Poisson)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param getQuantile A quantile function for the distribution (i.e. \code{qpois} for Poisson)
#'    \cr (REQUIRED w/ NO DEFAULTS)
#' @param hasCDF   Tells function if \code{showCDF} was specified. Used for determining
#'    priority of individual show- parameters and the main show parameter
#' @param hasPMF   Tells function if \code{showPMF} was specified. Used for determining
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
#'   discrete i* functions such as \code{ipois}, which should be considered for
#'   more detail on these parameters. Plotting functionality for i* functions is
#'   provided by \code{PlotContinuous} as well as its discrete distribution
#'   counterpart, \code{PlotDiscrete}.
#'
#' @return A vector of random variates distributed according to the provided
#'   distribution function
#'
#' @seealso \code{\link[=runif]{stats::runif}}
#' @importFrom grDevices dev.hold dev.flush recordPlot replayPlot adjustcolor 
#' @importFrom stats quantile
#' 
#' @template signature
#' @concept  random variate generation
#' @export
################################################################################
PlotDiscrete <- function(
    u               = runif(1),
    minPlotQuantile = 0.05,
    maxPlotQuantile = 0.95,
    plot            = TRUE,
    showCDF         = TRUE,
    showPMF         = FALSE,
    showECDF        = FALSE,
    show            = NULL,
    maxInvPlotted   = 50,
    plotDelay       = 0,
    animateAll      = TRUE,
    empColor        = "red3",
    theoColor       = "grey3",
    showTitle       = TRUE,
    respectLayout   = FALSE,
    getDensity,
    getDistro,
    getQuantile,
    hasCDF,
    hasPMF,
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
    checkVal(showPMF,  "l")
    checkVal(showECDF, "l")

    if (missing(hasCDF))
          hasCDF <- missing(show)
    else  checkVal(hasCDF,   "l")
    if (missing(hasPMF))
          hasPMF <- missing(show)
    else  checkVal(hasPMF,   "l")
    if (missing(hasECDF))
          hasECDF <- missing(show)
    else  checkVal(hasECDF,  "l")

    # parse show as appropriate
    showResults <- ParseShow(
      showBools   = c(showCDF, showPMF, showECDF),
      show        = show,
      ignoreBools = missing(showCDF) && missing(showPMF) && missing(showECDF)
    )
    showCDF  <- showResults[1]
    showPMF  <- showResults[2]
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
    rangeX   <- toX - fromX

    # Generate smoothed exact CDF distribution
    exactCDF <- getDistro(fromX:toX)

    # compute the plot min and max Y
    fromY <- if (fromX == 0)  0  else  getDistro(fromX-0.01)
    toY   <- exactCDF[rangeX + 1]

    # Generate smoothed exact CDF distribution
    exactPMF <- getDensity(fromX:toX)

    # Ranges for plotting boundaries
    xAxisMin <- if (fromY > 0)  fromX - (0.02 * rangeX)  else  fromX
    xAxisMax <- if (toY   < 1)  toX   + (0.02 * rangeX)  else  toX

    # Global variables to be used in later functions
    xVals          <- NULL # Will be set of all xValues after inversion
    xValsPlotted   <- NULL # Will store direct hash for x values plotted on
    xValsPlotMax   <- 1    # Keeps running tally of the maximum plot count

    pmfYMax        <- NULL # Will store maximum Y for PMF plots
    currSub        <- NULL # Will store a subset of xVals for animation
    maxStepY       <- NULL # Will store maximum step height on discrete CDF
    vertOffset     <- NULL # Used for generated spacing in CDF plotting
    uXLoc          <- NULL # Will Store location of points

    plot1          <- NULL # Will store state of CDF plot

    histInfo       <- NULL # Will store histogram info for plotting on CDF/PMF
    maxStackHeight <- NULL # Histogram data for highest bar
    maxHistDensity <- NULL # Histogram data for max density

    # Return inverted values function
    ReturnVals <- function() {
      options(warn = warnVal$warn)
      par(oldpar)
      if (is.null(xVals))  return(invisible(xVals))  else  return(xVals)
    }

    # Bind colors to [0, 255] scale
    BindColor <- function(val){ return(max(0, min(val, 255))/255) }

    #############################################################################


    #############################################################################
    # Builds density histogram based on inputted set of data
    #############################################################################

    MakeHist <- function(xSubset) {

      # if subset empty don't plot any variates; only PMF and/or CDF
      if (is.null(xSubset)) { maxHistDensity <<- 0; return() }

      options(warn = -1)  # suppress warnings

      maxBins  <- 500
      histInfo <<- NULL

      fdBinSize <- 2 * IQR(xSubset) / (length(xSubset) ^ (1/3))
      fdNumBins <- (max(xSubset) - min(xSubset)) / fdBinSize

      # grab onto the histogram data for use in cdf and/or pmf plots;
      # start this here so we can use hist's rightmost bin point in xlim computation;
      # note that hist() can fail on Byzantine cases for any of "fd", "scott",
      # "sturges", so we'll use try/catch in that order;
      # also note that in some cases "fd" freaks out -- e.g.,
      #    > set.seed(123456); vals = igamma(runif (20),0.01,scale=0.03,showHist=T)
      #    > histInfo = hist(vals,breaks="fd",plot=FALSE)
      #    > length(histInfo$breaks)
      #    [1] 275065
      # In some _really_ bizarre cases, using "fd" really-really freaks out when
      # the IQR (see http://bit.ly/2lbSN34) is extremely small.  As a specific
      # example:
      #
      #    > n = 10; set.seed(9923527)
      #    > qvals = qgamma(runif (n), shape = 0.01, scale = 0.003)
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
      # this corresponds to just showing the pmf and/or cdf distribution
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
      # by default, if u is null and showPMF is missing (using default FALSE),
      # let's set showPMF to TRUE -- the user can override by giving F explicitly
      if (missing(showPMF))  showPMF <- TRUE

      plotDelay <- 0
    } else {
      xVals <- getQuantile(u)  # generate the variates
      MakeHist(xVals)          # generate initial histogram with values
    }

    # if no plot, or "equal" min/max quantile values, just return the variates...
    if (plot == FALSE)  ReturnVals()

    if (showCDF == FALSE && showPMF == FALSE && showECDF == FALSE) {
      if (plot == TRUE)
        warning("ignoring plot since showCDF, showPMF, and showECDF are all FALSE")
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
    minPlots <- sum(showCDF, showPMF, showECDF)

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
    # Otherwise, force layout to show plots
    else if (plotDelay == -1)  par(mfrow = c(3, 1))
    else  par(mfrow = c(minPlots, 1))

    # Number of plots needed to fill based on par specifications
    numPlotSlots  <- max(prod(par("mfrow")), prod(par("mfcol")))
    # Number of plot slots that have been filled so far
    numPlotted <- 0

    # set default margins for plots
    botMar <- if (minPlots > 1) 4.1 else 5.1
    par(mar = c(botMar, 4.1, 2.1, 1.5))   # 0.5 for right??

    # set color tones for plotting variates
    colorTrans <- adjustcolor(empColor, alpha.f = if (length(u) < 150) 0.5 else 0.35)

    # Color of circle outline on inversion endpoints
    colorOutline <- if (length(u) < 150) empColor else colorTrans

    # Scale points relative to length of u
    pointCex <- if (length(u) < 50) 0.8 else 0.8 * (50/length(u))

    # set line width and cex paramters for plotting
    lwd      <- max(2, round(log10(length(xVals))))  # increases with num xVals
    plot.cex <- par("cex")              # R automatically lowers if three plots

    if (!is.null(xVals)) {

      estimPMF <- table(xVals) / length(xVals)

      # if the majority of density is packed into the first histogram bin:
      # set the y range max to be <= 75% larger than the maximum pmf value
      pmfYMax <- max(exactPMF, estimPMF)
      if (max(estimPMF) > max(exactPMF) * 1.5) pmfYMax <- max(exactPMF) * 1.5

    } else {

      estimPMF <- NULL
      pmfYMax <- max(exactPMF)

    }

    # totally empty plot allows us to know in advance what the x ticks will
    # be according to R's call to pretty()... use tick info for any/all plots...
    # ylim upper is 1, unless showing pmf (at top) but not cdf...
    firstPlotYMax <- if (showCDF == FALSE && showPMF == TRUE) pmfYMax else 1
    ticks <- par("xaxp")

    # remove any tick value that R includes that's outside fromX:toX bounds
    tickVals <- seq(ticks[1], ticks[2], by = ((ticks[2] - ticks[1])/ticks[3]))
    if (ticks[1] < fromX) { tickVals <- tickVals[-1] }
    if (ticks[2] > toX)   { tickVals <- tickVals[-length(tickVals)] }
    if (rangeX <= 5) { tickVals <- fromX:toX }  # avoid R's decimals...

    #############################################################################


    #############################################################################
    # Formats a plot for output, adding axes and labels
    #############################################################################

    FormatPlot <- function(xtext = "", ytext = "", isCDF = TRUE) {

      # draw our own axes (R's pretty() sometimes gives a min tick or max tick
      # value that is outside the min/max quantile bounds);
      # add the vertical axis
      # axis(2, las = 1)
      mtext(ytext, side = 2, line = 3, cex = plot.cex)

      # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
      # min/max quantile, even if contrary to R's pretty() decision;
      # and a 2nd axis with line but no ticks, extending thru the max/min x vals
      # axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
      axis(1, at = c(xAxisMin, xAxisMax), labels = FALSE, tcl = 0)
      mtext(xtext, side = 1, line = 3, cex = plot.cex)

      clip(xAxisMin, xAxisMax, -0.1, 1.1)  # need to go lower than y=0 for y=0 lwd

      # draw vertical dotted lines corresponding to the min/max quantile values
      maxY <- if (isCDF) 1 else pmfYMax
      if (maxPlotQuantile < 1)
        segments(xAxisMax, 0, xAxisMax, maxY, lty = "dotted", xpd = NA)
      if (minPlotQuantile > 0)
        segments(xAxisMin, 0, xAxisMin, maxY, lty = "dotted", xpd = NA)

      do.call("clip", as.list(par("usr"))) # reset clip
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the cdf...
    #############################################################################

    PlotTheoCDF <- function(col, lwd, strk = NULL, swd = 1, scol = "black") {

      clip(xAxisMin, xAxisMax, -0.1, 1.1)

      # plot the cdf (for now in lightgray)
      lines(fromX:toX, exactCDF, type = "s", lwd = lwd, col = col)
      if (!is.null(strk))
        lines(fromX:toX, exactCDF, type = "s", lwd = swd, lty = strk, col = scol)

      # plotting the discrete cdf using "s" omits the first vertical -- draw it
      segments(fromX, fromY, fromX, exactCDF[1], lwd = lwd, col = col)
      if (!is.null(strk))
        segments(fromX, fromY, fromX, exactCDF[1], lwd = swd, lty = strk, col = scol)

      # when minPlotQuantile causes fromX > 0, draw incoming stub of horizontal
      # and intersecting vertical
      if (fromX > 0) {
        segments(xAxisMin, fromY, fromX, fromY, lwd = lwd, col = col)
        if (!is.null(strk))
          segments(xAxisMin, fromY, fromX, fromY, lwd = swd, lty = strk, col = scol)
      }

      # draw outgoing stub of horizontal and intersecting vertical
      segments(toX, toY, xAxisMax, toY, lwd = lwd, col = col)
      if (!is.null(strk))
        segments(toX, toY, xAxisMax, toY, lwd = swd, lty = strk, col = scol)

      do.call("clip", as.list(par("usr"))) # reset clip
    }

    #############################################################################
    # Handle plotting of the cdf...
    ############################################################################

    PlotInvPoint <- function(x, y) {
      points(x, y, pch = 20, col = colorTrans, cex = pointCex, xpd = NA)
      points(x, y, pch = 21, col = colorOutline, cex = pointCex, xpd = NA)
    }

    TryPlotCDFInversion <- function(i, val, col = empColor){

      if ((showCDF == FALSE && plotDelay != -1) || is.null(u))  return()

      clip(xAxisMin, xAxisMax, -0.1, 1.1)

      inv.y <- if (!missing(i))  u[i]      else  val
      inv.x <- if (!missing(i))  xVals[i]  else  getQuantile(val)

      xValIdx <- inv.x + 1  # to handle 0-variate's index in R

      # Override global trans/outline colors is requested
      if (col != empColor) {
        empColor     <- col[1]
        colorTrans   <- col[min(2, length(col))]
        colorOutline <- col[min(3, length(col))]
      }

      # draw the u value point
      PlotInvPoint(uXLoc, inv.y)

      if (length(u) == 1)
        text(0.75*(fromX + toX), 0.1, paste(round(inv.y, 3), sym$arrow, round(inv.x, 3)),
            col = empColor, cex = ScaleFont(15))

      lineSideColor <- colorTrans

      # draw the dashed segment from u to the cdf;  unlike continuous case,
      # have to check fromX / xVals[i] / toX rather than quantiles because
      # many quantiles map to same xVals[i]
      if (fromX <= inv.x && inv.x <= toX) {

        segments(uXLoc, inv.y, inv.x, inv.y, lty = "dashed", col = lineSideColor)

        # draw the dashed segment from cdf to bottom of the cdf riser
        segments(inv.x, inv.y, inv.x, maxStepY[xValIdx],
                      lty = "dashed", col = empColor)

        maxStepY[xValIdx] <<- max(inv.y, maxStepY[xValIdx])
      }

      else if (inv.x > toX) { # if u > max quantile, draw hline to max x
        segments(uXLoc, inv.y, xAxisMax, inv.y, lty = "dashed", col = empColor)
      } else { # if u < min quantile, draw hline to min x val
        segments(uXLoc, inv.y, xAxisMin, inv.y, lty = "dashed", col = empColor)
      }
      do.call("clip", as.list(par("usr"))) # reset clip

      # for this variate value, track the number plotted
      xValsPlotted[xValIdx] <<- xValsPlotted[xValIdx] + 1
      xValsPlotMax <<- max(xValsPlotMax, xValsPlotted[xValIdx])
      vertPos <- xValsPlotted[xValIdx]

      clip(-20, 1, -0.1, 1.1)

      # draw the variate in a vertical stack only if w/in fromX/toX bounds
      if (fromX <= inv.x && inv.x <= toX) {
        yVal <- 0 - (vertOffset * (vertPos-1))
        PlotInvPoint(inv.x, yVal)
      }
      do.call("clip", as.list(par("usr"))) # reset clip
    }

    TryInitPlotCDF <- function() {

      if (showCDF == FALSE && plotDelay != -1)  return()

      # totally empty plot allows us to know in advance what the x ticks will
      # be according to R's call to pretty()... use tick info for any/all plots...
      # ylim upper is 1, unless showing pmf (at top) but not cdf...
      firstPlotYMax <- if (showCDF == FALSE && showPMF == TRUE) pmfYMax else 1
      plot(NA, NA, xlim = c(fromX, toX), ylim = c(0, firstPlotYMax),
           xlab = "", ylab = "", bty = "n", las = 1)

      numPlotted <<- numPlotted + 1

      FormatPlot("x", "F(x)")
      PlotTheoCDF(col = theoColor, lwd = lwd)
      if (showTitle)  title(titleStr, cex.main = 0.975)

      if (is.null(u))  return()

      # try to find appropriate x-value to draw u's & u-points...
      # by trial & error, decreasing by -0.02 * plot.range seems to work...
      uXLoc <<- xAxisMin - (0.02 * (xAxisMax - xAxisMin))

      # NOTES: on plotting generated variates in a vertical stack: height of
      # pch=20 is strheight("x")*0.75*(2/3) -- see ?points (pch 'Details' &
      # 'pch values');
      # strheight("x") changes with different display();
      # from experimentation, it appears as though it will be OK to plot about 18
      # of these _full_ point heights down, starting from the bottom of the
      # horizontal axis, and not impede on the histogram below...
      # so use this to compute the stack height, which will allow us to plot
      # points on top of one another if necessary so that we don't extend beyond
      # this 18-full-heights vertical range...
      # (side note: the default R bounding box for points is a square of side 0.01
      #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)

      maxInStack    <- 18
      maxXValsCount <- max(table(xVals))      # max count (for scaling vert stacking)
      vertOffset    <<- strheight("x") * 0.75 * (2 / 3)

      if (maxXValsCount > maxInStack) {
        maxStackHeight <<- maxInStack * vertOffset
        vertOffset     <<- maxStackHeight / maxXValsCount
      }

      # now draw the inverting...
      xValsPlotted <<- rep(0, getQuantile(0.999) + 1)
      maxStepY     <<- rep(0, toX + 1)
    }

    FormatCDF <- function(){

      if (showCDF == FALSE && plotDelay != -1)  return()

      # redraw the cdf back over top of dashed variate generation lines...
      PlotTheoCDF(col = theoColor, lwd = lwd)

      if (!is.null(u))
        text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot.cex
    }

    TryPlotCDFQuant <- function(i){

      if (showCDF == FALSE)  return()

      TryInitPlotCDF()

      quants <- quantile(u[1:i], seq(0, 1, 0.25))

      for (j in 2:length(quants)) {
        y0 <- quants[j-1]
        x0 <- getQuantile(y0)
        y1 <- quants[j]
        x1 <- getQuantile(y1)

        if (y1 - y0 > 0.05)
          text(labels = sym$dots, x = 0, y = (y1 + y0)/2, srt = 90)
        if (x1 - x0 > abs(toX - fromX)/20)
          text(labels = sym$dots, x = (x1 + x0)/2, y = 0.05)
      }

      TryPlotCDFInversion(val = quants[1], col = "black")
      TryPlotCDFInversion(val = quants[2], col = "grey")
      TryPlotCDFInversion(val = quants[3], col = "black")
      TryPlotCDFInversion(val = quants[4], col = "grey")
      TryPlotCDFInversion(val = quants[5], col = "black")

      xValsPt <- rep(0, max(xVals[1:i]))
      for (j in 1:i) {
        PlotInvPoint(uXLoc,    u[j])

        xValIdx <- xVals[j] + 1
        xValsPt[xValIdx] <- xValsPt[xValIdx] + 1
        vertPos <- xValsPt[xValIdx]
        yVal <- 0 - (vertOffset * (vertPos-1))

        PlotInvPoint(xVals[j], yVal)
      }
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the PMF / histogram of variates
    #############################################################################

    TryPlotPMF <- function(xSubset){

      if (showPMF == FALSE)  return()

      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, pmfYMax),
           xlab = "", ylab = "", bty = "n", las = 1)

      if (showCDF == FALSE)  title(titleStr, cex.main = 0.975)

      numPlotted <<- numPlotted + 1

      # plot the theoretical -- start w/ black spikes behind, adding points below
      points(fromX:toX, exactPMF, pch = "", type = "h", col = theoColor)

      if (!is.null(xSubset))  # no variates to plot
      {
        if (length(xSubset) < length(xVals))
          estimPMF <- table(xSubset) / length(xSubset)

        # overlay the estimated pmf using table w/ type='h' (discrete);
        # NB: (a) we want to avoid printing outside of the [fromX, toX] bounds,
        #     (b) table does not necessarily create an entry per x value,
        #     (c) access the count of a table value a la estim["27"] not estim[27]
        #     (d) "..." doesn't center above the lwd=3 line, so need an adjustment
        #     (e) "..." can push on title when PMF is 1st plot, so stop drawing short
        for (xVal in fromX:toX)
        {
          xProb <- estimPMF[as.character(xVal)]
          if (!is.na(xProb)) {     # no xVal entry will return NA
            if (xProb > pmfYMax) {
              points(xVal, pmfYMax * 0.95, type = "h", col = empColor, lwd = 3)
              # put "..." above if height is cut off by plot max
              text(xVal, pmfYMax * 0.95, labels = "...", srt = 90,
                   adj = c(-0.2, 0.075), xpd = TRUE, cex = 1.25)
            } else {
              points(xVal, xProb, type = "h", col = empColor, lwd = 3)
            }
          }
        }
      }

      # superimpose the theoretical points
      points(fromX:toX, exactPMF, pch = 20)
      FormatPlot("x", "f(x)", isCDF = FALSE)
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the ecdf with superimposed cdf
    #############################################################################

    TryPlotECDF <- function(xSubset){

      if (showECDF == FALSE)  return()

      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, 1),
           xlab = "", ylab = "", bty = "n", las = 1)

      if (showCDF == FALSE && showPMF == FALSE)  title(titleStr, cex.main = 0.975)

      numPlotted <<- numPlotted + 1

      if (is.null(xSubset)) {
        # not plotting variates
        lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s",
              lwd = 2, lty = "solid", col = "black")

        # draw 1st vertical, cdf "extensions", & dotted quantile lines...
        fromY <- if (fromX == 0) 0 else exactCDF[fromX]
        segments(fromX, fromY, fromX, exactCDF[fromX + 1],
                 lwd = 2, col = "black", lty = "solid", xpd = NA)

      } else {
        # plot the cdf -- ensure doesn't plot outside bounds;  draw wider
        # so ecdf can clearly be seen overlayed, and in light gray, with
        # dashed black over top
        lwd <- 3
        cdf.lwd <- 8
        x <- NULL

        # according to R documentation, different plots can react differently
        # to clip... seems we need to call again here for plot.ecdf to respect
        PlotTheoCDF(col = theoColor, lwd = cdf.lwd, strk = "dashed")

        # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
        # plot.ecdf() function doesn't want to draw the first and last horizontals
        # all the way from/to fromX/toX... so just draw it ourselves...
        ecdfVals    <- ecdf(xSubset)         # a function!
        ecdfKnots   <- knots(ecdfVals)
        firstKnot   <- ecdfKnots[1]
        lastKnot    <- ecdfKnots[length(ecdfKnots)]

        # Plot empirical ecdf and extend it
        clip(xAxisMin, xAxisMax, -0.1, 1.1) # need to go < 0 for lwd

        plot.ecdf(ecdfVals, verticals = TRUE, pch = "", add = TRUE,
                  lwd = lwd, xlim = c(fromX, toX), col = empColor, col.01line = NA)

        # segments(0, firstKnot, fromX, firstKnot, lwd = lwd, col = empColor)
        segments(toX, lastKnot, xAxisMax, lastKnot, lwd = lwd, col = empColor)

        do.call("clip", as.list(par("usr"))) # reset clip

      }

      FormatPlot("x", "F(x)")    # add the vertical & horizontal axes

      # draw the title
      if (showTitle && showCDF == FALSE && showPMF == FALSE)
        title(titleStr, cex.main = 0.975)

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

          if (input == "q")  plotDelay <<- 0

          else if (input == "help") {
            message("'latest'          = shows latest inversion")
            message("'showPMF'         = toggles parameter for next run")
            message("'showCDF'         = toggles parameter for next run")
            message("'showECDF'        = toggles parameter for next run")
            message("'q'               = change plotDelay to 0")}

          else if (input == "q")          plotDelay <<- 0
          else if (input == "showCDF")    showCDF   <<- !showCDF
          else if (input == "showPMF")    showPMF   <<- !showPMF
          else if (input == "showECDF")   showECDF  <<- !showECDF

          # Shows job statistics if requested
          else if (input == "latest")
            message(" - Mapped ", u[length(xSubset)], " ", 
                    sym$arrow, " ", xSubset[length(xSubset)])

          if (numPlotSlots < showCDF + showPMF + showECDF)
            message(showCDF + showPMF + showECDF, " plots but only ", numPlotSlots,
                    " slots; some of the plots may no longer appear")
        }
      }
    }
    ##############################################################################


    ####################################################################################
    ##  ResetPlot
    ## --------------------------------------------------------------------------------
    ##  Pad the current plot components to fill up the graph;
    ##  This keeps everything in animation consistently located
    ####################################################################################
    ResetPlot <- function(force = FALSE) {

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
        if (showCDF == TRUE && i >= maxInvPlotted && animateAll)
          TryPlotCDFQuant(i)
        else if (i < maxInvPlotted)
          numPlotted <- numPlotted + 1
      }
      # Plot the next u value inversion in CDF plot
      TryPlotCDFInversion(i)

      if (plotDelay != 0) {
        if (animateAll) {

          if (i < maxInvPlotted)  plot1 <- recordPlot()
          if (showCDF == TRUE)  FormatCDF()
          else  ResetPlot(TRUE)

          TryPlotPMF(currSub)
          TryPlotECDF(currSub)
        }

        TryPausePlot(currSub)

        ResetPlot()

        if (plotDelay == 0 && i < maxInvPlotted - 1)
          numPlotted <- 1

        if (animateAll && i < maxInvPlotted - 1)
            replayPlot(plot1)
      }
    } # for (i in 1:length(u))

    MakeHist(xVals)                  # Generate final hist (consider optimizing)

    if (showCDF == TRUE) {
      if (length(u) >= maxInvPlotted) {
        ResetPlot(TRUE)
        TryPlotCDFQuant(length(u))
      } else {
        if (animateAll)  replayPlot(plot1)
        TryPlotCDFInversion(length(u)) # Plot last inversion onto CDF if shown
      }
      FormatCDF()                      # Format CDF for final output if shown
    } else ResetPlot(TRUE)

    TryPlotPMF(xVals)                # Plot and format PMF  output if shown
    TryPlotECDF(xVals)               # Plot and format ECDF output if shown
    dev.flush()                      # Render the final plot state

    #############################################################################
    # reset display, margins, and warnings
    #############################################################################

    ReturnVals()                     # return a value/vector of F^-1(u)
}
