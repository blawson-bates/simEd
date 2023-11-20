## -------------------------------------------------------------------------
#' Lehmer Generator Simulation
#'
#' @description This function animates the processes of a basic Lehmer 
#' pseudo-random number generator. Also known in the literature as a 
#' multiplicative linear congruential generator (MLCG), the generator 
#' operates on the formula: 
#' 
#' \deqn{X_{k+1} \equiv a \cdot X_k \pmod{m}}{%
#'       X_{k+1} = a * X_k mod m}
#'
#' @param a             Multiplier in equation
#' @param m             Modulus component in equation
#' @param seed          Seed in equation; the initial X value x_0 
#' @param maxSteps      Maximum number of steps to display; infinite by default
#'                      Ignored if showOutput is false
#' @param showOutput    Should the visual output be displayed, if not, it will just return output
#' @param showTitles    Should the title be displayed in the main plot
#' @param plotDelay     Wait time between transitioning; -1 (default) for interactive
#'                      mode, where the user is queried for input to progress
#' @param sf_ratio      Multiplied for font scaling ratio relative to screen x/y ratio. 
#'                      Set to c(2, 1) by default with ideal screening at a 2:1 ratio
#'
#' @return The PRNG cycle. If multiple possible options, will do one of the following. The path returned 
#'    is the last path encountered by the generator in its visual generation loop. If showOutput is 
#'    FALSE, it will return the first found one.
#'
#' @concept  random variate generation
#' 
#' @importFrom shape plotellipse
#'
#' @examples
#'  # Default case (m, a = 31, 13); small full period
#'  lehmer(maxStep = 50)                   # Interactive mode (plotDelay = -1)
#'  lehmer(plotDelay = 0.1, maxStep = 50)  # Auto-progressing
#'  
#'  # Change behavior of fonts relative to x/y ratio of display device
#'  #Should be customized per device if desired
#'  lehmer(plotDelay = 0.1, maxStep = 50, sf_ratio=c(3, 1))  # Good for wider display
#'  lehmer(plotDelay = 0.1, maxStep = 50, sf_ratio=c(1, 3))  # Good for taller display
#' 
#'  # Special case with booleanic generator
#'  lehmer(a = 11, m = 20, maxStep = 50, plotDelay = 0.1)
#' 
#'  # Special edge case where the seed changes because 1x13 == (78x13) mod 1001 
#'  lehmer(a = 12, m = 20, maxStep = 50, sf_ratio=c(1, 3), plotDelay = 0.1)
#'  
#' @export
################################################################################
lehmer <- function(
                a = 13,
                m = 31,
                seed                  = 1,
                maxSteps              = Inf,
                showOutput            = TRUE,
                showTitles            = TRUE,
                plotDelay             = -1,
                sf_ratio              = c(3, 1)
) {
  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  {
    checkVal(seed, "i", min = 1)

    checkVal(m, "i", minex = 0)
    checkVal(a, "i", minex = 0, maxex = m)
    checkVal(maxSteps, "i", minex = 1)
    if (a >= m) 
      stop("'a' must be strictly less than 'm'")
    if (m %% a == 0) 
      stop("'a' cannot be a factor of 'm'")

    checkVal(showOutput,   "l")
    checkVal(showTitles,   "l")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
      stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
    
    if (any(is.na(sf_ratio)) || length(sf_ratio) < 2) {
      stop("sf_ratio must be a list of two values")
    }
  }

  #############################################################################

  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function() return(NA)
  
  sf <- function(n) ScaleFont(n, f = 2, r = sf_ratio)   # font-scaling function

  # Construct title for the plot
  titleText <- paste(sep="", "Multiplicative Lehmer Generator(a = ", a, ", m = ", m, ", x0 = ", seed, ")");
  #titleText <- bquote("Multiplicative Lehmer Generator(a = " ~.(a) ~", m = " ~.(m) ~", " ~ x[0] ~ " = " ~.(seed) ~ ")")
  titleText <- bquote(bold("Multiplicative Lehmer Generator(a = " ~.(a) ~", m = " ~.(m) ~", " ~ x[0] ~ " = " ~.(seed) ~ ")"))

  ####################################################################################
  ##  Define graphical components
  ####################################################################################
  # Ranges for Plots (Keep bounds in (10, 190))
  # -------------   xmin, xmax, ymin, ymax ------------
  cycleRange    <- c(110,  190,   50,  170)     # Cycle Plot range
  equationRange <- c( 10,   90,   50,  170)     # Equation Plot Range
  ####################################################################################


  ####################################################################################
  ### -----------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   -----------------  ###
  ####################################################################################


  ####################################################################################
  ## DrawCycle
  ## --------------------------------------------------------------------------------
  ## Initialized the Cycle plot depending on specifications
  ## @param step     The current tick of the system, with 1 being at "12:00"
  ## @param seed_idx The index of the seed
  ## @param Xs       The set of all Xs computed
  ####################################################################################
  DrawCycle <- function(step, seed_idx, Xs) {

    cbRange <- cycleRange + c(-5, 5, 0, 5) # Range displacement for background components

    ScalePlots(cycleRange)

    # Draw border around cycle region
    TogglePlot(cbRange)
    DrawBorder("grey", "white")

    # Initialize main plot
    TogglePlot(cycleRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")

    t <- length(Xs)

    c_rad  <- if (t < 11) 60 else 70    # Circle_radius
    f_size <- if (t < 11) 20 else 20    # Default Font size
    t_size <- if (t < 11) 30 else 20    # Default Tick font size

    # Plot main cycle ellipse (circle)
    plotellipse(rx = c_rad, ry = c_rad, mid = c(100, 100), from = -pi, to = pi, col = "lightgreen")

    # Basic function to compute x and y position at end of clock hand
    tick_xy_func <- function(index) {
      angle <- pi/2 - (2 * pi * (index - 1) / t)
      return(c(cos(angle), sin(angle)))
    }
    
    # Compute current and previous indices
    curr_idx <- ((step + seed_idx) %% t) + 1
    prev_idx <- ((step + seed_idx-1) %% t) + 1
    
    percent = (step + seed_idx - 1)/t
    mainxy = tick_xy_func(curr_idx)     ## Angle to point to current value
    seedxy = tick_xy_func(seed_idx + 1) ## Angle to point to seed
    
    ## Plot clock hands
    lines(
      x = c(100, 100 + 1.1 * c_rad * mainxy[1]),
      y = c(100, 100 + 1.1 * c_rad * mainxy[2])
    )

    # Basic function to help compute multiplier to scale fonts relative to content
    multf <- function(x) max(nchar(toString(x)), 2)
    
    # Interval between ticks in which a textbox is displayed
    if (length(Xs) < 50) {
      tick_interval <- ceiling(length(Xs)/20)
      ticks <- 1 + 0:(t/tick_interval - 1) * tick_interval
    } else {
      ticks <- round(seq(1, t+1, length.out = t/ceiling(t/20)))
      if (ticks[length(ticks)] > t) ticks <- ticks[1:(length(ticks)-1)]
    }
    
    # This stuff does not show if there are too many components in plot
    for(i in ticks) {
      # Compute appropriate clock handle angles
      xy <- tick_xy_func(i)

      # Display clock hands
      lines(
        x = c(100 + 0.9*c_rad*xy[1], 100 + c_rad*xy[1]),
        y = c(100 + 0.9*c_rad*xy[2], 100 + c_rad*xy[2])
      )

      # Background color for textboxes for X values on clocks
      bg <- NA

      # Textboxes surrounding circle
      TextBox(
        text = Xs[i],
        mw = 100 + 1.25*c_rad*xy[1], mh = 100 + 1.2*c_rad*xy[2],
        hw = 0.2 * sf(f_size) * multf(Xs[i]), hh = 10,
        bg = bg,
        size = sf(t_size)
      )
    }
    
    # Render original seed location box
    seed_m_idx <- ((seed_idx + 1) %% t) ## modification seed index for 1-based computation
    TextBox(
      text = Xs[seed_m_idx],
      mw  = 100 + 1.25*c_rad*seedxy[1], mh = 100 + 1.2*c_rad*seedxy[2],
      hw  = 0.2 * sf(f_size) * multf(Xs[seed_m_idx]), hh = 10,
      bg  = "red",
      col = "white",
      size = sf(t_size)
    )
    
    # Render current location box
    TextBox(
      text = Xs[curr_idx],
      mw = 100 + 1.25*c_rad*mainxy[1], mh = 100 + 1.2*c_rad*mainxy[2],
      hw = 0.2 * sf(f_size) * max(multf(Xs[i]), 2), hh = 10,
      bg = "yellow",
      size = sf(t_size)
    )

    # Center text box for uniform value
    TextBox(
      text = round(Xs[curr_idx]/m, 3),
      mw = 100, mh = 100,
      hw = 50, hh = 20,
      bg = "cadetblue1",
      size = sf(f_size*2)
    )
    
    title(paste(sep="", "Generation Circle (Period = ", t, ")"), cex.main = 1.4)
  }
  ####################################################################################

  ####################################################################################
  ## DrawEquation
  ## --------------------------------------------------------------------------------
  ## Draw the relationship equations based on current state
  ## @param x_idx   The index of the previous x (0-base)
  ## @param xc      The previous x that was generated
  ## @param xn      The latest x to be generated
  ####################################################################################
  DrawEquation <- function (x_idx, xc, xn) {

    ebRange  <- equationRange + c(-5, 5, 0, 5)

    ScalePlots(equationRange)
    
    # Toggle to subplot and draw border 
    TogglePlot(ebRange)
    DrawBorder("grey", "white")

    # Toggle to main plot and initiate coordinate-bound plot
    TogglePlot(equationRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
         xlab = "", ylab = "", bty = "n", las = 1, type = "s")
    # Special plot call to use 's' option

    TextBox( # Plots basic semi-static recurrence relation: X_{n} = a * X_{n-1} mod m
      text = bquote(X[.(x_idx+1)] == a %.% X[.(x_idx)] ~ "mod" ~ m),
      # text = Encoding(bquote(X[.(x_idx+1)] == a %.% X[.(x_idx)] ~ "mod" ~ m)),
      mw = 100, mh = 170,
      hw = 100, hh = 30,
      size = sf(30)
    )

    tbmh <- 120     # Mid height for textbox (for plug-in equation)
    tbhh <-  15     # Half height for textbox elements
    tbsz <- sf(20)  # Textbox font size for textbox elements
    
    { # Plots basic non-static recurrence relation: X_{n} = a * X_{n-1} mod m
      texts <- c(xn, paste("=", a, "\U2022"), xc, paste("mod", m))
      TextBox(text = texts[1], mw =  12, mh = tbmh, hw = 16, hh = tbhh, size = tbsz, bg = "yellow")
      TextBox(text = texts[2], mw =  61, mh = tbmh, hw = 29, hh = tbhh, size = tbsz)
      TextBox(text = texts[3], mw = 107, mh = tbmh, hw = 17, hh = tbhh, size = tbsz, bg = "lightgrey")
      TextBox(text = texts[4], mw = 162, mh = tbmh, hw = 38, hh = tbhh, size = tbsz)
    }

    segments(x0 = 20, y0 = 85, x1 = 180) # Breakage line

    tbmh <- 50     # Mid height for textbox (for plug-in equation)
    tbhh <- 15     # Half height for textbox elements
    tbsz <- sf(20) # Textbox font size for textbox elements
    
    { # Plots basic normalization equation: X_{n} / m = u
      segments(x0 = 65, y0 = 40, x1 = 75, y1 = 60)
      # segments(x0 = 20, y0 = 40, x1 = 180) # Breakage line
      texts <- c(xn, paste(m, " = ", sep = ""), round(xn/m, 3))
      TextBox(text = texts[1], mw =  35, mh = tbmh, hw = 16, hh = tbhh, size = tbsz, bg = "yellow")
      TextBox(text = texts[2], mw =  95, mh = tbmh, hw = 29, hh = tbhh, size = tbsz)
      TextBox(text = texts[3], mw = 155, mh = tbmh, hw = 24, hh = tbhh, size = tbsz, bg = "cadetblue1")
    }

  }

  ####################################################################################

  ####################################################################################
  ##  DrawLinePlot (and associated function-scope holders)
  ## --------------------------------------------------------------------------------
  ##  Initialized the generated value plot depending on specifications
  ##  @param xn      Point just generated; flashes as light blue
  ##  @param period  How long should the generated list be
  ####################################################################################
  plottedXs <- c()    # Function-scope repository for X computations
  showSeed  <- FALSE  # Should the seed be shown
  DrawLinePlot <- function(xn, period) {

    lineplotRange <- c(10, 190, 20, 50)        
    lpRange  <- lineplotRange + c(-5, 5, 0, 5)  # Coordinate system mutation for background

    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(lineplotRange)
    TogglePlot(lineplotRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,1), ylim = c(-1,1), yaxt = "n",
        xaxp = c(0,1,10), cex.axis = 1.4,  # bgl
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")
      
    # Draw all of the points as necessary
    points(x = plottedXs, y = rep(0, length(plottedXs)), bg = "grey", pch = 21, cex = 2)
    points(x = xn/m, y = 0, bg = "cadetblue1", col = "black", pch = 21, cex = 2)
    
    # If showSeet is set to true, flash the red seed and clear out repository for next run
    if (showSeed) {
      points(x = xn/m, y = 0, bg = "red", col = "black", pch = 21, cex = 2)
      plottedXs <<- c()
      showSeed  <<- FALSE
    }
    else {
      # If it's the last component, flash the seed in the next iteration
      if (length(plottedXs) >= period - 2) 
        showSeed  <<- TRUE
      
      # Populate repository
      plottedXs <<- c(plottedXs, xn/m) 
    }
  }

  ####################################################################################


  ####################################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------------
  ##  The equivalent of main() from a C program.  This function will be
  ##  invoked at the end of the encompassing ssq() function, causing the
  ##  discrete-event simulation to execute.
  ####################################################################################
  main <- function(seed)
  {
    if(is.null(dev.list())) dev.new(width=7, height=4.5)
    par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)
    dev.flush(dev.hold()-1L)

    pauseData <<- SetPausePlot(
      plotDelay = plotDelay, 
      prompt    = "Hit 'ENTER' to proceed and 'q' to end: "
    )

    PauseCurrPlot <<- function() {
      PausePlot(pauseData = pauseData)
    }
    
    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    i   <- 1                 # Appending index
    xn  <- 0                 # New X
    xc  <- seed              # Current X
    xm  <- seed              # Minimum X
    xmi <- 1                 # Minimum X index (for pivot)
    Xs  <- rep(-1, m-1)      # Set of all possible X's, starting at seed
    Xs[i] <- seed

    # Sample large value in event of edge case: inability to return to seed
    # Can happen when seed/m is large, i.e. with (a, m, seed) = (13, 1001, 1)
    xmax <- 0
    xmax_i <- 0
    xmax_check <- FALSE
    
    # Find Period
    while(i <= m) {
      i  <- i + 1
      xn <- (a * xc) %% m
      
      if (xn > xmax) {
        xmax <- xn
        xmax_i <- i
        xmax_check <- FALSE
      }

      # If the new x value is the same as the seed, the cycle is done
      # If we have a repeat element, then this combination is invalid
      if (xn == seed || xn == xc || (xmax_check && xmax == xn)) break 
      
      xmax_check <- TRUE

      # Transfer new x into X set and set it to be the current x
      Xs[i] <- xn
      xc <- xn

      # Keeping track of minimum value to pivot it all later
      if (xc < xm) {
        xm  <- xc
        xmi <- i
      }
    }
    
    # If the current X is equal to the previous X, the period has degenerated to 0
    # Otherwise, record period as normal
    if (xn == xc) {             # Edge case, period of 1
      period <- 1
      Xs <- c(xn)
      seed_idx = 1
    } else if (xn == xmax) {    # Edge case, period diminishes to not include seed
      period <- i - xmax_i
      Xs <- c(Xs[1:(i-xmax_i)])
      seed_idx <- 0 
    } else {                    # Default case, a constant period
      period <- i-1
      Xs <- c(Xs[xmi:period], Xs[1:xmi-1])   # Pivoting of Xs to make lowest value on top
      seed_idx <- (period - xmi + 1) %% period
    }
    message("Period found to be ", period)
    
    i   <- 0                 # Current clock tick
    xn  <- 0                 # New X
    xc  <- seed              # Current C 

    while(pauseData$plotDelay != 0 && showOutput && i < maxSteps) {
      
      ResetPlot() # Clear out the plot for the next flush
      if (showTitles)
        title(titleText, line = -0.6, cex.main = 1.2)
      
      # Actually calculate the next xn; computation left for pedagogical reasons
      xn <- (a * xc) %% m 
      
      if (Xs[(i + seed_idx + 1) %% period + 1] != xn)
        Xs[(i + seed_idx + 1) %% period + 1] <- xn
      
      # Draw all of the mandatory components
      DrawCycle(i+1, seed_idx, Xs)
      DrawEquation(i, xc, xn)
      DrawLinePlot(xn, length(Xs))
      
      # Lazily pause current plot
      pauseData <- PauseCurrPlot()

      # Assign new X as current x and increment counter
      xc <- xn
      i  <- i + 1
    }

    if (seed == 1) {
        return(Xs)
    } else {
        where = which(Xs == seed)
        Xs_reorg= c(Xs[where:length(Xs)], Xs[1:(where - 1)])
        return(Xs_reorg)
    }


  } # main
  ####################################################################################

  # ********************************************************************
  # * CALL THE MAIN FUNCTION, executing the simulation, return result. *
  # ********************************************************************
  return(main(seed))

}
