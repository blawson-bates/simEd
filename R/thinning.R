# pwc data.frame for bounding default intensity function
#fpwc <- data.frame(
#   x = c(0, 2, 20, 30, 44, 48),
#   y = c(5, 5, 20, 12, 20,  5)
#)
#nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fpwc, majorizingFcnType="pwc")
#nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwc, majorizingFcnType="pwc")
#nhpp <- thinning(maxTime = 12, plotDelay = 0, majorizingFcn = fpwc, majorizingFcnType="pwc")

# pwl data.frame for bounding default intensity function
#fpwl <- data.frame(
#   x = c(0, 12, 24, 36, 48),
#   y = c(5, 25, 10, 25, 5)
#)
#nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fpwl, majorizingFcnType="pwl")
#nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwl, majorizingFcnType="pwl")
#nhpp <- thinning(maxTime = 12, plotDelay = 0, majorizingFcn = fpwl, majorizingFcnType="pwl")

# pwl closure/function bounding default intensity function
#fclo <- function(x) {
#    if (x <= 12) (5/3)*x + 5
#    else if (x <= 24) 40 - (5/4)*x
#    else if (x <= 36) (5/4)*x - 20
#    else 85 - (5/3) * x
#}
#nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fclo)
#nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fclo)
#nhpp <- thinning(maxTime = 12, plotDelay = 0, majorizingFcn = fclo)

#intensity <- function(x) { 
#    day <- 24 * floor(x/24)
#    return(80 * (dnorm(x, day + 6,    2.5) + 
#                 dnorm(x, day + 12.5, 1.5) + 
#                 dnorm(x, day + 19,   2.0)))
#}
#nhpp <- thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity)
#nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)
#nhpp <- thinning(maxTime = 12, plotDelay = 0, intensityFcn = intensity)

# pwl data.frame for bounding custom intensity function
#fpwl <- data.frame(
#   x = c(0,  6,  9, 12, 16, 19, 24, 30, 33, 36, 40, 43, 48),
#   y = c(5, 17, 12, 28, 14, 18,  7, 17, 12, 28, 14, 18,  7)
#)
#nhpp <- thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity,
#            majorizingFcn = fpwl, majorizingFcnType = "pwl")
#nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity,
#            majorizingFcn = fpwl, majorizingFcnType = "pwl")
#nhpp <- thinning(maxTime = 12, plotDelay = 0, intensityFcn = intensity,
#            majorizingFcn = fpwl, majorizingFcnType = "pwl")

#nhpp <- NULL
#nrep <- 100
#for (i in 1:nrep) {
#    cat('.')
#    nhpp <- c(nhpp, 
#        thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity,
#                 majorizingFcn = fpwl, majorizingFcnType = "pwl", plot = FALSE))
#}
#cat('\n')
#dev.new()
#histvals <- hist(nhpp, breaks = 48, plot = FALSE)
#plot(histvals$mids, histvals$counts / nrep, las = 1, bty = "l", type = "h")
#curve(intensity, add = TRUE, col = "red")


## -------------------------------------------------------------------------
#' Thinning 
#'
#' @description This function animates the generation of a non-homogeneous
#'              Poisson process via the "thinning" approach, given a specified
#'              intensity function and majorizing function.
#'        
#' @param maxTime        Maximum time of the generated non-homogeneous Poisson process.
#' @param intensityFcn   Intensity function corresponding to rate of arrivals across
#'                       time.
#' @param majorizingFcn  Majorizing function.  Default value is NULL,
#'                       corresponding to a constant majorizing function.  May
#'                       alternatively be provided as a user-specified function,
#'                       or as a data frame requiring additional notation as 
#'                       either piecewise-constant or piecewise-linear.  See
#'                       examples.
#' @param majorizingFcnType Used to indicate whether a majorizing function that
#'                          is provided via data frame is to be interpreted as
#'                          either piecewise-constant ("pwc") or piecewise-linear
#'                          ("pwl").  If the majorizing function is either the
#'                          default or a user-specified function (closure), the
#'                          value of this parameter is ignored.
#' @param seed           Initial seed for the uniform variates used during
#'                       generation.
#' @param maxTrials      Maximum number of accept-reject trials; infinite by default.
#' @param plot           If TRUE, visual display will be produced.  If FALSE,
#'                       generated variates will returned without visual display.
#' @param showTitles     Should the title be displayed in the main plot
#' @param plotDelay      Wait time in seconds between plots; -1 (default) for
#'                       interactive mode, where the user is queried for input
#'                       to progress.
#' @param sf_ratio       Multiplied for font scaling ratio relative to screen x/y ratio. 
#'                       Set to c(2, 1) by default with ideal screening at a 2:1 ratio
#'
#' @return   Returns the generated variates pulled from the actual distribution
#'
#' @concept  random variate generation
#' 
#' @importFrom shape Arrows
#' @importFrom grDevices dev.list dev.new
#'
#' @examples
#' 
#' nhpp <- thinning()
#' nhpp <- thinning(maxTime = 48, seed = 8675309, plotDelay = 0)
#' nhpp <- thinning(maxTime = 8, seed = 8675309, plotDelay = 0.01)
#'
#' # piecewise-constant data.frame for bounding default intensity function
#' fpwc <- data.frame(
#'     x = c(0, 2, 20, 30, 44, 48),
#'     y = c(5, 5, 20, 12, 20,  5)
#' )
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, 
#'                  majorizingFcn = fpwc, majorizingFcnType = "pwc")
#'
#' # piecewise-linear data.frame for bounding default intensity function
#' fpwl <- data.frame(
#'     x = c(0, 12, 24, 36, 48),
#'     y = c(5, 25, 10, 25, 5)
#' )
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, 
#'                  majorizingFcn = fpwl, majorizingFcnType = "pwl")
#'
#' # piecewise-linear closure/function bounding default intensity function
#' fclo <- function(x) {
#'     if (x <= 12) (5/3)*x + 5
#'     else if (x <= 24) 40 - (5/4)*x
#'     else if (x <= 36) (5/4)*x - 20
#'     else 85 - (5/3) * x
#' }
#' nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fclo)
#'
#' # thinning with custom intensity function and default majorizing
#' intensity <- function(x) { 
#'     day <- 24 * floor(x/24)
#'     return(80 * (dnorm(x, day + 6,    2.5) + 
#'                  dnorm(x, day + 12.5, 1.5) + 
#'                  dnorm(x, day + 19,   2.0)))
#' }
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)
#'
#' # piecewise-linear data.frame for bounding custom intensity function
#' fpwl <- data.frame(
#'     x = c(0,  6,  9, 12, 16, 19, 24, 30, 33, 36, 40, 43, 48),
#'     y = c(5, 17, 12, 28, 14, 18,  7, 17, 12, 28, 14, 18,  7)
#' )
#' nhpp <- thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity,
#'           majorizingFcn = fpwl, majorizingFcnType = "pwl")
#'
#' @export
################################################################################
thinning <- function(
                maxTime           = 24,
                intensityFcn      = function(x) (5 - sin(x / 0.955) - 
                                                (4 * cos(x / 3.82))) / 0.5,
                majorizingFcn     = NULL,
                majorizingFcnType = NULL,
                seed              = NA,
                maxTrials         = Inf,
                plot              = TRUE, 
                showTitles        = TRUE,
                plotDelay         = plot * -1,
                sf_ratio          = c(1, 3)
) {
  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  {
    checkVal(seed, "i", min = 1, na = TRUE, null = TRUE)
    checkVal(maxTime, "i", min = 1)

    #intensityFcn <- function(x) { 
    #    #day <- 24 * floor(x/24)
    #    day <- 24 * as.integer(x/24)
    #    return(80 * (dnorm(x, day + 6,    2.5) + 
    #                 dnorm(x, day + 12.5, 1.5) + 
    #                 dnorm(x, day + 19,   2.0)))
    #}
    
    checkVal(intensityFcn, "f")
    
    checkVal(maxTrials, "i", min = 1)
    checkVal(plot, "l")
    checkVal(showTitles, "l")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
      stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
    
    if (any(is.na(sf_ratio)) || length(sf_ratio) < 2) {
      stop("sf_ratio must be a list of two values")
    }
  }

  #############################################################################

  numBins = maxTime #* 2
  binWidth = maxTime / numBins
  bins = rep(0, numBins)
  breaks = (0:numBins * binWidth)
  mids = (0:numBins * binWidth) + (binWidth / 2)

  #############################################################################
  
  # Diagnostic boxed to show range of subplot windows
  showBoxes = FALSE
  
  #plotxvals <- seq(0, 200, length=400)
  plotxvals <- seq(0, maxTime, length.out=500)
  
  if(is.null(majorizingFcn)) {
    # Override majorizing function if NULL
    majorizingFcn <- function(x) 1.01 * max(sapply(plotxvals, intensityFcn))
    useDefaultMajorizingFcn <- TRUE
  } else {
    # checkMajorizing (utils.R) also checks "pwc" or "pwl" for type
    pieces        <- checkMajorizing(majorizingFcn, majorizingFcnType, c(0, maxTime))
    if (typeof(majorizingFcn) == "closure") {
        majorizingFcn <- pieces$majorizingFcn
    } else {
        majorizingFcn <- pieces$mappingFcn
    }
    inversionFcn  <- pieces$inversionFcn
    mappingFcn    <- pieces$mappingFcn  # NA if type != "pwl"
    pwl_xvals     <- pieces$pwl_xvals   # NA if type != "pwl"
    pwl_yvals     <- pieces$pwl_yvals   # NA if type != "pwl"
    useDefaultMajorizingFcn <- FALSE
  }
  
  # Maximum function of pdf, majorizing function, and both
  # Y components of plots
  a.ys <- sapply(plotxvals, intensityFcn)
  if (typeof(majorizingFcn) == "closure") {
    m.ys <- sapply(plotxvals, majorizingFcn)
  } else {
    m.ys <- sapply(plotxvals, mappingFcn)
  }
  
  # Maximum function of intensityFcn, majorizing function, and both
  if (plot) {
    #a.maxv <- max(sapply(plotxvals, intensityFcn))
    #m.maxv <- max(sapply(plotxvals, majorizingFcn))
    #maxv   <- max(m.maxv, a.maxv)
    a.maxv <- max(a.ys)
    m.maxv <- max(m.ys)
    maxv   <- max(m.maxv, a.maxv)
  }
  

  #if (min(sapply(plotxvals, majorizingFcn) - sapply(plotxvals, intensityFcn)) < 0)
  if (min(m.ys - a.ys) < 0)
    message(" - Maximizing function is not always greater than intensity function")
  
  color_u = "thistle1"
  color_T = "yellow"
  
  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function() return(NA)

  sf <- function(n) ScaleFont(n, f = 2, r = sf_ratio)   # font-scaling function

  # Construct title for the plot
  titleText <- "Thinning Model";

  ####################################################################################
  ##  Define graphical components
  ####################################################################################
  # Ranges for Plots (Keep bounds in (10, 190))
  # -------------   xmin, xmax, ymin, ymax ------------
  mplotRange    <- c( 20,  180,   65,  145)     # Main Plot range
  histplotRange <- c( 20,  180,    5,   70)     # Histogram Plot Range
  varplotRange  <- c( 20,  180,  145,  180)     # Variable Plot Range
  
  overplotRange <- c(
    min(mplotRange[1], histplotRange[1], varplotRange[1]),
    max(mplotRange[2], histplotRange[2], varplotRange[2]),
    min(mplotRange[3], histplotRange[3], varplotRange[3]),
    max(mplotRange[4], histplotRange[4], varplotRange[4])
  ) + c(-5,5,-5,5)

  ####################################################################################

  ####################################################################################
  ### -----------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   -----------------  ###
  ####################################################################################
  
  xvals <- c(0)
  mvals <- c()
  hvals <- c()
  
###################### BGL
#  DrawThinningPlot <- function(vpair, times, accepted, AccXs, AccYs, RejXs, RejYs) {
  DrawThinningPlot <- function(tupair, times, accepted, AccXs, AccYs, RejXs, RejYs) 
  {
        vpair <- tupair
###################### BGL

    # Range displacement for background components
    cbRange <- mplotRange + c(-5, 5, 2, 5)
    ScalePlots(mplotRange)

    # Draw border around cycle region
    if (showBoxes) {
      TogglePlot(cbRange)
      DrawBorder("grey", "white")
    }
    
    # Initialize main plot
    TogglePlot(mplotRange, initPlot = FALSE, mar = c(3,3,1,1))
    
    max_x = max(AccXs[length(AccXs)], RejXs[length(RejXs)])
    if (xvals[length(xvals)] <= max_x) {
      #xvals <<- seq(0, ceiling(max_x/25)*25, length.out = 500)
      #  print(xvals)
      xvals <<- seq(0, maxTime, length.out = 500)   # CHANGED!!
      mvals <<- sapply(xvals, majorizingFcn)
      hvals <<- sapply(xvals, intensityFcn) 
    }

    # Plot the intensity and majorizing functions
    #plot(xvals, mvals, ylab="density", ylim = c(0, maxv), 
    #     type ="l", bty = "n", col="red3", las = TRUE)
    #plot(xvals, mvals, ylab="density", ylim = c(0, maxv), 
    #     type ="l", bty = "n", col="red3", las = 1, xaxt = "n")
    plot(xvals, mvals, xlim = c(0, maxTime), ylim = c(0, maxv), 
         type ="l", bty = "n", col="red3", las = 1, xaxt = "n")

    axisLabels = seq(0, maxTime, by = (maxTime / 24) * 4)
    for (i in 1:length(axisLabels)) {
        if (as.integer(axisLabels[i]) != axisLabels[i]) {
            axisLabels[i] = 
                format(round(as.numeric(axisLabels[i]), 2), nsmall = 2)
        }
    }
    axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)

    lines(xvals, hvals, ylab="density", type ="l", col="green4")
    
    # Plot approproate points of accepted and rejected values
    points(AccXs, AccYs, col = "green4")
    points(RejXs, RejYs, col = "red3", pch = 4)
    points(AccXs, rep(0, length(AccXs)), col = "green4", pch = "|")
    
    latest = c(times[2], vpair[2]) 
    
    # Plot segments for most recent point generated
    segments(latest[1], latest[2], y1 = 0, lty = 2)
    segments(latest[1], latest[2], x1 = 0, lty = 2)
    
    points(latest[1], latest[2], col = if(accepted) "green4" else "red3", pch = 19, cex = 1.5)
    
    TogglePlot(mplotRange + c(-5, -5, 10, 0))
    if (showBoxes) DrawBorder(color_u)
  
############# BGL
    #TextBox(
    #  text = bquote(.(round(latest[2], 2)) * lambda*.("*")),
    #  mw = 0, mh = 15 + 175 * latest[2]/maxv,
    #  hw = 12, hh = 15,
    #  bg = color_u,
    #  size = sf(20)
    #)
    TextBox(
      text = bquote(.(round(latest[2], 3))),
      mw = 0, mh = 15 + 175 * latest[2]/maxv,
      hw = 12, hh = 15,
      bg = "orange",
      size = sf(20)
    )
############# BGL
    
    TogglePlot(mplotRange + c(10, 1, 2, 0))
    if (showBoxes) DrawBorder(color_T)
    
    TextBox(
############# BGL
      #text = paste(round(latest[1], 2)),
      text = paste(round(latest[1], 3)),
############# BGL
      mw = 12 + 176 * latest[1]/xvals[length(xvals)], mh = 0,
      hw = 12, hh = 12,
      bg = color_T,
      size = sf(20)
    )
    
    # Label for majorizing function
    TextBox(  
      text = bquote(lambda*.("*")),
      mw = 205, mh = 22 + 175 * majorizingFcn(xvals[length(xvals)])/maxv,
      hw = 2, hh = 10,
      col = "red3",
      size = sf(20)
    )
    
    # Label for intensity function
      #text = bquote(""~lambda),
    TextBox(  
      text = bquote("    "~lambda),
      mw = 195, mh = 25 + 175 * intensityFcn(xvals[length(xvals)])/maxv,
      hw =  2, hh = 10,
      col = "green4",
      size = sf(20)
    )
    
    # Draw Histogram Components
    
    ebRange  <- histplotRange + c(-5, 5, 0, 5)

    ScalePlots(histplotRange)
    
    # Toggle to subplot and draw border 
    if (showBoxes) {
      TogglePlot(ebRange)
      DrawBorder("grey", "white")
    }

    # Toggle to hist plot and initiate coordinate-bound plot
############# BGL
    #TogglePlot(histplotRange, initPlot = FALSE, mar = c(3,3,1,1))
    TogglePlot(histplotRange, initPlot = FALSE, mar = c(3,3,1.2,1))
############# BGL
    
    if (length(AccXs) == 0) {
      #h_vals <- c(a.maxv)
      h_vals <- NULL
############### BGL
      #plot(NULL, xlim = c(plotxvals[1], xvals[length(xvals)]),
           #ylim = c(0, a.maxv), las = TRUE, bty="n")
      plot(NA, NA,    # CHANGED!!
           xlim = c(plotxvals[1], xvals[length(xvals)]),
           ylim = c(0, maxv), 
           xpd = NA, xlab = "", ylab = "",
           las = 1, bty = "n", xaxt = "n")

      axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)

############### BGL

    } else {

      plot(NA, NA,   # CHANGED!!
        xlim = c(0, xvals[length(xvals)]),
        ylim = c(0,max(maxv, bins / binWidth)), 
        bty = "n", las = 1, main = "", xlab = "", ylab = "", xaxt = "n")
      axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)
      for (i in 1:length(breaks)) {
        if (i < length(breaks)) {
            rect(breaks[i], 0, breaks[i+1], bins[i] / binWidth, 
                col = "lightgray")
        }
      }
    }
    
    #hv_max <- max(h_vals$counts)
    
################# BGL
    #lines(xvals, intensityFcn(xvals) * (hv_max), ylab="density",
    lines(xvals, intensityFcn(xvals), ylab="density",
          type ="l", col=adjustcolor("green4", alpha.f=1.0), lwd = 1.5)
################# BGL
      
    #segments(latest[1], max(h_vals$counts), y1 = 0, lty = 2, col = if(accepted) "green4" else "red3")

  }

  ####################################################################################

  ####################################################################################
  ##  DrawLinePlot (and associated function-scope holders)
  ## --------------------------------------------------------------------------------
  ##  Initialized the generated value plot depending on specifications
  ##  @param xn      Point just generated; flashes as light blue
  ##  @param period  How long should the generated list be
  ####################################################################################
############### BGL
  #DrawVarMap <- function(vpair, times, numAcc, numRej) {
  DrawVarMap <- function(etpair, u, numAcc, numRej) {
    vpair <- etpair
############### BGL

    varplotRange <- varplotRange + c(-5, 5, 0,0)  # Extends a bit to go with others

    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(varplotRange)

    TogglePlot(varplotRange)
    DrawBorder("grey", "white")
    
    TogglePlot(varplotRange, initPlot = FALSE, mar = c(0,0,0,0))
    
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
         xlab = "", ylab = "", bty = "n", las = 1, type = "s")
    # Special plot call to use 's' option
    
    numTotal <- numAcc + numRej
   
    TextBox(
      mw = 25, mh = 100,
      hw = 25, hh = 100,
      bg = "cadetblue1"
    )
     
    TextBox(
      text = paste("Trial", numTotal),
      mw = 25, mh = 140,
      hw = 25, hh =  40,
      size = sf(30),
      border = NA
    )
    
    TextBox(
      text = paste("Accept", numAcc),
      mw = 25, mh =  75,
      hw = 20, hh =  25,
      size = sf(20),
      col  = "green4",
      bg   = "white"
    )
    TextBox( 
      text = paste("Reject", numRej),
      mw = 25, mh =  25,
      hw = 20, hh =  25,
      size = sf(20),
      col  = "red3",
      bg   = "white"
    )
    
############ BGL
    #TextBox( 
    #  text = bquote( u[1]~"~"~ U (0, 1) *", "*u[2] ~"~"~ U (0, lambda*.("*"))),
    #  mw = 100, mh = 150,
    #  hw =  20, hh =  50,
    #  size = sf(23)
    #)
    # lambda *.("*") (T[.(numTotal-1)])

    TextBox( 
      text = bquote( e[.(numTotal)]~"~"~ Exp(lambda*.("*"))),
      mw =  75, mh = 150,
      hw =  20, hh =  50,
      size = sf(23)
    )
    TextBox( 
      text = bquote(T[.(numTotal)] ~ "=" ~ T[.(numTotal-1)] + e[.(numTotal)]),
      mw =  125, mh = 150,
      hw =  20, hh =  50,
      size = sf(23)
    )

    #TextBox( 
    #  text = bquote(T[.(numTotal-1)] ~"-"~ .("")^{log(u[1])} / .("")[lambda *.("*")]),
    #  mw = 125, mh = 50,
    #  hw =  20, hh = 30,
    #  size = sf(23)
    #)
  
    TextBox( 
      text = bquote(u[.(numTotal)] ~ "=" ~ U(0,lambda*.("*")(T[.(numTotal)]))),
      mw = 100, mh =  50,
      hw = 20, hh =  50,
      size = sf(25)
    )

    Arrows(50, 100, 140, 100)
    
    #TextBox( 
    #  text = bquote(T[.(numTotal-1)] ~ "=" ~ .(format(round(times[1], 2), nsmall = 2))),
    #  mw = 75, mh =  50,
    #  hw = 25, hh =  50,
    #  size = sf(25),
    #  bg = "snow2"
    #)

    TextBox( 
      text = bquote(
        .(format(round(vpair[1], 3), nsmall = 3))
      ),
      mw = 162.5, mh = 150,
      hw = 12.5, hh =  50,
      bg = "gray90",
      size = sf(23)
    )
    TextBox( 
      text = bquote(
        .(format(round(vpair[2], 3), nsmall = 3))
      ),
      mw = 187.5, mh = 150,
      hw = 12.5, hh =  50,
      bg = "yellow",
      size = sf(23)
    )
    
    
    #TextBox(
    #  text = bquote(T[.(numTotal)] ~"="~ .(format(round(times[2], 2), nsmall = 2))),
    #  mw = 175, mh =  50,
    #  hw =  25, hh =  50,
    #  bg = color_T,
    #  size = sf(25)
    #)
    TextBox(
      text = bquote(.(format(round(u, 3), nsmall = 3))),
      mw = 175, mh =  50,
      hw =  25, hh =  50,
      bg = "orange",
      size = sf(25)
    )
############ BGL

  }

  ####################################################################################
  
  
  ####################################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------------
  ##  The equivalent of main() from a C program, to be used for visualization
  ####################################################################################
  main <- function()
  {
    # -----------------------------------------------------------------------
    # Initialization of main-global variables
    # -----------------------------------------------------------------------
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed))  simEd::set.seed(seed)
    
    # Initialize streamlines pausing functionality
    pauseData <<- SetPausePlot(
      plotDelay = plotDelay,
      prompt    = "Hit 'ENTER' to proceed and 'q' to end: "
    )

    PauseCurrPlot <- function(pause.data, progress) {
      
      out <- PausePlot(
        pauseData = pause.data,
        currStep  = progress+1
      )
      
      return(out)
    }

    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    RejXs <- c()
    RejYs <- c()
    AccXs <- c()
    AccYs <- c()
    
    # Force layout to show only outputted plots if possible
    if (plot) {
      if(is.null(dev.list())) 
        dev.new(width=5, height=6) 
      par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)
      dev.flush(dev.hold()-1L)
    }

    # xtimes <- PlotTimer() # Initialize timer plot for execution times
    # ttimes <- PlotTimer() # Initialize timer plot for total recording
    # ptimes <- PlotTimer(start = FALSE) # Initialize timer plot for pause times

    currtime <- 0  # CHANGED!!
#print(majorizingFcn)
#print(typeof(majorizingFcn))
#print(mappingFcn)
#print(typeof(mappingFcn))
#for (i in 1:10) {
#  print(mappingFcn(i))
#}
    e1 <- vexp(1, rate = majorizingFcn(0), stream = 1)

    oldtime  <- currtime
    currtime <- currtime + e1
    
    while (length(AccXs) + length(RejXs) < maxTrials && 
           currtime < maxTime)
    {  
      # (0) Initialize t = 0
      # (1) Generate e ∼ Exp(λ*(t))
      # (2) Set t ← t + e
      # (3) Generate u ∼ U(0, λ*(t))
      # (4) If u ≤ λ(t) then deliver t.
      # (5) Goto Step (1).
      
##############BGL
      #v1 <- vunif(1, 0, 1, stream = 1)
      #v2 <- vunif(1, 0, majorizingFcn(currtime), stream = 2)
      #currtime <- currtime - (log(v1)/majorizingFcn(currtime))
      #accepted <- (v2 <= intensityFcn(currtime)/majorizingFcn(currtime))
      
      # this is the right way _only_ if \lambda* is constant;
      # o/w should do unit SPP inversion of \Lamba*

      umax <- if (typeof(majorizingFcn) == "closure") majorizingFcn(currtime)
              else mappingFcn(currtime) 
      u1 <- vunif(1, 0, umax, stream = 2)
      accepted <- (u1 <= intensityFcn(currtime))

      #cat(paste("rate = ", majorizingFcn(0), "\n"))
      #cat(paste("e = ", e1, "\n"))
      #cat(paste("t = ", currtime, "\n"))
      #cat(paste("u = ", u1, "\n"))

      if(accepted) {
        AccXs <- c(AccXs, currtime)
        #AccYs <- c(AccYs, v2)
        AccYs <- c(AccYs, u1)

        currT = AccXs[length(AccXs)]
        whichBin = ceiling(currT / binWidth)
        bins[whichBin] <<- bins[whichBin] + 1
        #print(paste("currT = ", currT))
        #print(paste("whichBin = ", whichBin))
        #print(bins)
        #print(mids)
        #print(sum(bins))
      } else {
        RejXs <- c(RejXs, currtime)
        #RejYs <- c(RejYs, v2)
        RejYs <- c(RejYs, u1)
      }
##############BGL

      
      last_run <- (length(AccXs) + length(RejXs) == maxTrials || currtime >= maxTime)
      
      if (plot && ((pauseData$plotDelay != 0) || last_run)) 
      {
        ResetPlot() # Clear out the plot for the next flush
        TogglePlot(c(10,190,0,190))
        if (showBoxes) DrawBorder("black") # Clear out the plot for the next flush
        if (showTitles) title(titleText, line = -1, cex.main = 1.2)
        
##############BGL
        # Draw all of the mandatory components
        #vpair <- c(v1, v2)
        #times <- c(oldtime, currtime)
        etpair <- c(e1, currtime)
        tupair <- c(currtime, u1)
##############BGL
        
        
        TogglePlot(overplotRange)
        DrawBorder("grey", "white")
        
##############BGL
        #DrawThinningPlot(vpair, times, accepted, AccXs, AccYs, RejXs, RejYs)
        DrawThinningPlot(tupair, c(0, currtime), accepted, AccXs, AccYs, RejXs, RejYs)

        #DrawVarMap(vpair, times, length(AccXs), length(RejXs))
        DrawVarMap(etpair, u1, length(AccXs), length(RejXs))
##############BGL
        
        if (last_run && pauseData$plotDelay == 0) 
          pauseData$plotDelay <<- 0.01

        # showTimes <- last_run
        # ttimes <- PlotTimer(ttimes, c(100, 200, 35, 70), c(-17,  0, -8,  0), showPlot = showTimes)  # Displays total timer
        # ptimes <- PlotTimer(ptimes, c(100, 145,  0, 30), c(-15,  0,  0,  0), showPlot = showTimes)  # Displays pause timer
        # xtimes <- PlotTimer(xtimes, c(155, 200,  0, 30), c(-15, -2,  0,  0), start = FALSE, showPlot = showTimes)  # Displays execution timer
       
      }
      
      if (plot) {
        if (pauseData$passUntil == -2) {
          # If a jump is just finished, make sure the output is flushed for the current pause
          dev.flush(dev.hold())
          dev.hold()
        }
        # Lazily pause current plot
        pauseData <<- PauseCurrPlot(pauseData, length(AccXs))
      }
      
      # ptimes <- ToggleTimer(ptimes)   # Pauses pause timer
      # xtimes <- ToggleTimer(xtimes)   # Resumes execution timer
      
      e1 <- vexp(1, rate = majorizingFcn(currtime), stream = 1)
      oldtime  <- currtime
      currtime <- currtime + e1
    }

############## BGL 24 Nov 200
    # Needed to copy this "last run" code here because I moved where
    # the updated of e1 and currtime happen... so as not to continue in
    # while loop past maxTime... but then final plot wasn't happening
    # at the end

    if (plot) {
        ResetPlot() # Clear out the plot for the next flush
        TogglePlot(c(10,190,0,190))
        if (showBoxes) DrawBorder("black") # Clear out the plot for the next flush
        if (showTitles) title(titleText, line = -1, cex.main = 1.2)
        
        # Draw all of the mandatory components
        etpair <- c(e1, currtime)
        tupair <- c(currtime, u1)

        TogglePlot(overplotRange)
        DrawBorder("grey", "white")
        
        #DrawThinningPlot(vpair, times, accepted, AccXs, AccYs, RejXs, RejYs)
        DrawThinningPlot(tupair, c(0, currtime), accepted, AccXs, AccYs, RejXs, RejYs)

        #DrawVarMap(vpair, times, length(AccXs), length(RejXs))
        DrawVarMap(etpair, u1, length(AccXs), length(RejXs))
        
        if (last_run && pauseData$plotDelay == 0) 
        pauseData$plotDelay <<- 0.01

############## BGL 24 Nov 200

      # Lazily pause current plot one last time to ensure full flush
      pauseData <- PauseCurrPlot(pauseData, length(AccXs))
      dev.flush(dev.hold())
    }
      
    #return(AccYs)
    return(AccXs)


  } # mainvis
  ####################################################################################

  # ********************************************************************
  # * CALL THE MAIN FUNCTION, executing the simulation, return result. *
  # ********************************************************************
  # if (!plotDelay || plot) return(mainvis())
  return(main())

}
