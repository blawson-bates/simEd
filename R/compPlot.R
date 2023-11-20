####################################################################################
##  NewPlot
## --------------------------------------------------------------------------------
##  Makes new blank plot of specifiable range (default from 0 to 200)
##  Comes preloaded with defaults optimized for non-graph plots;
##  Recommended to use own plot function when plotting graphs
####################################################################################
NewPlot <- function(xlim = c(0, 200), ylim = c(0, 200)) {
  plot(NA, NA, xlim = xlim, ylim = xlim, xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n", las = 1)
}
####################################################################################


####################################################################################
##  DrawBorder
## --------------------------------------------------------------------------------
##  Draws a rectangular border or rectangle of specified color that extends
##  across full range of default PlotCoordRange
####################################################################################
DrawBorder <- function(br, col = rgb(0,0,0,0)) {
  rect(0, 0, 200, 200, border = br, col = col, lwd = 2)
}
####################################################################################


####################################################################################
##  ResetPlot
## --------------------------------------------------------------------------------
##  Pad the current plot components to fill up the graph;
##  This keeps everything in animation consistently located
####################################################################################
ResetPlot <- function() {
  # Initialize a new plotting window overlaying the current one
  par(mfrow = c(1,1), new = FALSE)
  # dev.off(); dev.new()          # Alternative 
  NewPlot()
}
####################################################################################


####################################################################################
##  ScalePlot
## --------------------------------------------------------------------------------
##  Scales initial plot to a set dimension and specifies plot boxes to scale to
####################################################################################
ScalePlots <- function(dims, mfrow = c(2, 3)) {
  # Setting mfrow here scales all subplots as if to fit 3,2 plot
  par(fig = dims/200, mfrow = mfrow, mar = c(0,0,0,0), new = TRUE)
  NewPlot()
}
####################################################################################


####################################################################################
##  TogglePlot
## --------------------------------------------------------------------------------
##  Toggle to a new plot with new dimension specification
####################################################################################
TogglePlot <- function(dims, initPlot = TRUE, mar = c(0,0,0,0)) {
  par(fig = dims/200, mar = mar, new = TRUE)
  if (initPlot)  NewPlot()
}
####################################################################################



################################################################################
## ScaleFont  -  Font-Scaling Function
## -----------------------------------------------------------------------------
#' Font-Scaling Function
#'
#' @description Returns a scaling of the inputted number based on the device.
#'
#' @param n Size to scale to
#'
#' @return A numeric value of the scaled n
#'
#' @template signature
#' @keywords internal
################################################################################
ScaleFont <- function(n, f = 100, r = c(1,1)) return(n/f * min(dev.size() / r))
################################################################################


################################################################################
## TextBox  -  Plot Text Box
## -----------------------------------------------------------------------------
#' Plot Text Box
#'
#' @description This function plots a TextBox with a variety of options including
#'    a scaled raster image for the background
#'
#' @param text    Text to be displayed in the box
#' @param mw      Mid-width of the textbox
#' @param mh      Mid-height of the textbox
#' @param hw      Half-width of the textbox
#' @param hh      Half-height of the textbox
#' @param col     Color of text
#' @param bg      Color of background box (NA for no background)
#' @param img     Raster image to be desplayed as background
#' @param font    Font style of text
#'                (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic)
#' @param size    Scaled font-size of text
#' @param txd     Text x-displacement from center as a percentage of half-width
#' @param tyd     Text y-displacement from center as a percentage of half-height
#' @param textf   Function to be applied to text prior to displaying
#' @param xs      Scaling function to be applied to x
#' @param ys      Scaling function to be applied to y
#'
#' @template signature
#' @keywords internal
################################################################################
TextBox <- function(
  text = "", mw, mh, hw = NA, hh = hw,
  col = "black", bg = NA, border = "black", img = NA,
  font = 1, size = 15, txd = 0, tyd = 0,
  textf = 0, xs = 0, ys = 0
) {
  if (!is.function(xs))    xs    <- function(c) return(c)
  if (!is.function(ys))    ys    <- function(c) return(c)
  if (!is.function(textf)) textf <- function(t) return(t)

  tryCatch({
    if ((is.numeric(text) || is.character(text)) && is.na(text))  text <- "-"
  }, error = function(e) {
    stop(paste("Error in TextBox: invalid text", text, "of type", typeof(text)))
  })
  if (text == "i")  font <- 3  # To be phased out later
  size <- ScaleFont(size)

  # Shift midwidth and midheight to not go over edge
  mw <- max(min(mw, 200 - hw), hw)
  mh <- max(min(mh, 200 - hh), hh)
  hw1 <- hw
  hh1 <- hh

  if (is.na(img[1])) {

    if (!is.na(bg))
      rect(xs(mw - hw), ys(mh - hh), xs(mw + hw), ys(mh + hh), col = bg, border = border)

  } else {
    
    if (!requireNamespace("magick", quietly = TRUE))
      stop("Package \"magick\" needed for images to work. Please install it.", 
           call. = FALSE)

    picratio <- (dim(img)[1] / dim(img)[2])
    pltratio <- 1/diff(par("fig")[3:4])
    yratio   <- pltratio * picratio
    hhnew    <- hw * yratio

    if (hhnew > hh)
      hw1 <- hw  * hh1/hhnew
    else
      hh1 <- hhnew

    rasterImage(img,
      xs(mw - hw1), ys(mh - hh1),
      xs(mw + hw1), ys(mh + hh1))
  }

  text(
    xs(mw + hw * txd), ys(mh + hh * tyd),
    labels = textf(text), col = col, cex = size, font = font)
}
################################################################################



################################################################################
## DrawPoint  -  Plot Point with fill and outline
## -----------------------------------------------------------------------------
#' Plot Text Box
#'
#' @description This function plots a Point with outline
#'
#' @param x    X-coordinate
#' @param y    Y-coordinate
#' @param col     Color of point fill
#' @param cex     Size of the point
#' @param bg      Outline/Background of point
#'
#' @template signature
#' @keywords internal
################################################################################
DrawPoint <- function(x, y, col, cex = 1.5, bg = "black") {

  lx <- length(x)
  ly <- length(y)

  if (lx * ly > 1) {
    if (lx != ly && min(lx, ly) > 1)
      warning(paste("DrawPoint() called with |x| ==", lx, "and |y| ==", ly))
    else for (i in 1:max(lx, ly))
      DrawPoint(
        x   = x[min(lx, i)],
        y   = y[min(ly, i)],
        col = col,
        cex = cex,
        bg  = bg
      )
    return()
  }

  isInvalid <- suppressWarnings(is.na(as.numeric(x)) || is.na(as.numeric(y)))
  if (isInvalid)  return()

  points(x, y, pch = 20, cex = cex, col = col, xpd = NA)
  points(x, y, pch = 21, cex = cex, col = bg, xpd = NA)
}
################################################################################



################################################################################
## SetPausePlot  -  Pause Plot Value Setter
## -----------------------------------------------------------------------------
#' Pause Plot Value Setter
#'
#' @description Overrides currently set/default value of PausePlot calls.
#'
#' @details See PausePlot for more thorough description of parameters
#'
#' @param prompt       Default prompt for PausePlot calls
#' @param viewCommand  Default view command for PausePlot calls
#' @param viewInstruct Default view instruction for PausePlot calls
#' @param viewFunction Default view function for PausePlot calls
#'
#' @template signature
#' @keywords internal
################################################################################
SetPausePlot <- function(
  plotDelay     = -1,
  prompt        = NA,
  viewCommand   = c(),
  viewInstruct  = c(),
  viewFunction  = c(),
  recLimit      = 5
) {
  if (1 != length(unique(c(
    length(viewCommand), length(viewInstruct), length(viewFunction)
  )))) {
    warning(paste("view parameters (viewCommand, viewInstruct, and viewFunction)",
      "are not of the same length. Each element of one should correspond to ",
      "the same-indexed element of the others"
    ))
  }
  
  return(list(
    plotDelay    = plotDelay,
    prompt       = prompt,
    viewCommand  = viewCommand,
    viewInstruct = viewInstruct,
    viewFunction = viewFunction,
    passUntil    = -3,
    PlotRec      = rep(list(NA, 6)),
    recCount     = 0,
    recLimit     = recLimit
  ))
}
################################################################################



################################################################################
## PausePlot  -  Pause Plot For User Input
## -----------------------------------------------------------------------------
#' Pause Plot For User Input
#'
#' @description
#'    Handles pausing of plot at regular intervals depending on plotDelay.
#'    Also handles user input and processing for plotDelay == -1
#'
#' @param plotDelay    Current delay setting, in secs. If -1, prompt for input
#' @param prompt       Default prompt for PausePlot calls
#' @param viewCommand  A vector of strings a user can input to call custom
#'                       view functions
#' @param viewInstruct A vector of string instructions to display alongside
#'                       default view instructions. Should correspond with
#'                       viewCommand values.
#' @param viewFunction A list of functions to be called when a commmand is
#'                       issued via prompt. Should correspond with viewCommand.
#'                       Must have at least one parameter, which should expect
#'                       the value inputted by user after command string.
#' @param currStep     A numerical value representing a 'current step' count.
#'                       Specifying this will enable jump command. Should be positive
#'
#' @return An updated plotDelay
#' 
#' @importFrom grDevices dev.flush dev.hold recordPlot replayPlot
#'
#' @template signature
#' @keywords internal
################################################################################
PausePlot <- function(
  pauseData,
  prompt        = NA,
  viewCommand   = c(),
  viewInstruct  = c(),
  viewFunction  = c(),
  currStep      = 0
) {

  if (pauseData$passUntil == -3) {
    # message("In starting stage")
    # Special State 1: passUntil is -3 (Starting state)
    # Flush initial state without pausing; to be paused in next call
    pauseData$passUntil <- -1
    dev.flush(dev.hold())  # sets hold level to 0 
    dev.hold(2L)           # hold level now 2
    pauseData$PlotRec <- rep(list(recordPlot()), 6)
    pauseData$PlotRec[[1]]  <- recordPlot()
    
    if (Sys.getenv("RSTUDIO") != "1")
      return(pauseData)
  }
  else if (pauseData$passUntil == -2) {
    # message("Just finished jumping")
    # Special State 2: passUntil is -2 (Just finished jumping)
    # Flush current state for next view and proceed to pause
    pauseData$passUntil <- -1
    dev.flush(dev.hold())  # sets hold level to 0
    dev.hold()             # hold level now 1
    return(pauseData)
  }
  else if (currStep == pauseData$passUntil) {
    # message("Jump Finished")
    # Special State 3: passUntil == current state (jump finished)
    # Set passUntil to Special State 1 (above) and exit
    pauseData$passUntil <- -2
    pauseData$plotDelay <- -1
    return(pauseData)
  }

  if (pauseData$plotDelay == 0) {
    # message("PlotDelay = 0, so don't pause at all")
    # If plotDelay is zero, get outta here.
    return(pauseData)
  }

  if (is.na(pauseData$PlotRec[[1]][1])) {
    # message("Initializing PlotRecordings")
    # If PlotRec is empty, initialize it
    pauseData$PlotRec  <- rep(list(recordPlot()), 6)
    pauseData$recCount <- 0
  }
  
  if (pauseData$plotDelay == -1) {
    input <- " "

    # While the input is not just an enter, keep looping request for input
    while (pauseData$plotDelay == -1 && input != "") {
      # message("Interactive mode, waiting for input")
      
      input <- readline(pauseData$prompt)

      if (input != "") {

        incmp <- tolower(unlist(strsplit(input, split = " ", fixed = TRUE)))

        # Handle quitting immedately
        if (incmp[1] == "q" || incmp[1] == "quit") {
          pauseData$plotDelay <- 0
        } 
      
        # Handle implicit backing with plotRecord
        else if (incmp[1] == "b" || incmp[1] == "back") {
          curr <- recordPlot()
          pauseData$recCount <- pauseData$recCount + 1

          if (pauseData$recCount <= pauseData$recLimit) {

            replayPlot(pauseData$PlotRec[[pauseData$recCount + 1]])

            dev.flush(dev.hold())  # sets hold level to 0
            dev.hold()             # hold level now 1

            pauseData <- PausePlot(
              pauseData = pauseData,
              currStep  = currStep
            )
          }
          else  message("Only ", pauseData$recLimit, " plot history is stored")

          replayPlot(pauseData$PlotRec[[pauseData$recCount]])
          pauseData$recCount <- pauseData$recCount - 1
          dev.flush(dev.hold())   # sets hold level to 0
          dev.hold()              # hold level now 1
          replayPlot(curr)
        } 
        
        # Handle jump function (skips until job number reached)
        else if (incmp[1] == "j" || incmp[1] == "jump") {
          
          arg <- as.numeric(gsub('[a-zA-Z *]','', input))
          
          if (is.na(arg))
            message(" - 'jump' needs desired job number i.e. 'jump 5'")
  
          else if (arg < currStep)
            message(" - 'jump ", arg, "' kindly ignored as it has already passed")
          
          else {
            pauseData$passUntil  <- arg
            pauseData$plotDelay  <- 0
            return(pauseData)
          }
        }

        # Handle help function
        else if (incmp[1] == "h" || incmp[1] == "help") {
          
          for (msg in viewInstruct)  message(msg)
          
          message("'n'/'next'/ENTER  = proceed to next stage")
          if (!is.na(currStep))
          message("'j'/'jump'        = jump to the nth initialization")
          message("'b'/'back'        = step back by one plot snapshot")
          message("'q'/'quit'        = change plotDelay to 0")
        }

        # Shows job statistics if requested
        else for (opt in 1:length(pauseData$viewCommand)) {
          if (
            !is.na(input) 
            && nchar(input) >= nchar(pauseData$viewCommand[opt])
            && substr(input, 1, nchar(pauseData$viewCommand[opt])) == pauseData$viewCommand[opt]
          ) {
            arg <- as.numeric(gsub('[a-zA-Z *]','', input))
            pauseData$plotDelay <- pauseData$viewFunction[[opt]](arg)
          }
        }
      }
    }
  }
  if (pauseData$recCount == 0 && pauseData$plotDelay == -1) {
    # Throw the latest plot record into PlotRec
    for (i in rev(2:length(pauseData$PlotRec)))
      pauseData$PlotRec[[i]] <- pauseData$PlotRec[[i-1]]
    pauseData$PlotRec[[1]] <- recordPlot()
  }

  dev.flush(dev.hold())  # sets hold level to 0
  if (pauseData$plotDelay > 0)  Sys.sleep(pauseData$plotDelay)
  dev.hold()             # hold level now 1
  
  return(pauseData)
}



################################################################################
## PlotTimer  -  Time execution and plot results
## -----------------------------------------------------------------------------
#' Times Execution and Plots Results
#'
#' @description
#'    Helper function to help visualize component timing. Maintains running 
#'    times list which is returned and should be passed in. Also serves 
#'    as a TogglePlot use-case. 
#'
#' @param times        Times component. Generated on initial PlotTimer call. 
#'                     Last element is the most recent time record or change in 
#'                     time recorded; all previous entries are previous changes in 
#'                     time, in order of recording time. 
#' @param plotRange    Coordinate range of output. Consistent with TogglePlot specs.
#'                     4-element vector with x0, x1, y0, and y1, in that order. 
#' @param borderDisp   Displacement component used for moving background border. 
#'                     4-element vector with x0, x1, y0, and y1, in that order. 
#' @param start        Whether or not the timer should start with this call. 
#'                     If yes, last element will be set to System time (last recorded time) 
#'                     If no, last element will be set to 0 (latest change in time)
#'
#' @return Updated vector that should be inputted into subsequent PlotTimer/ToggleTimer calls.
#'    Last element is either last-recorded time or change in time. Prior elements are 
#'    the changes in time recorded thus far.
#'
#' @template signature
#' @keywords internal
################################################################################
PlotTimer <- function(
  times = c(), 
  plotRange = c(160, 200, 0, 30), 
  borderDisp = c(-15, 0, 0, 0), 
  start = TRUE, 
  print = FALSE, 
  showPlot = FALSE
){

  currtime <- Sys.time()

  if (!length(times)) {
    if (start) return(as.double(currtime))
    else       return(c(0))
  }

  # If the time is an actual time count, toggle tail element to be a change in time. 
  if (times[length(times)] > 1000000) {
    times <- ToggleTimer(times)
  }

  if (showPlot) {
    
    op <- par(no.readonly = TRUE)

    timerPlotBorderRange <- plotRange + borderDisp
    
    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(plotRange)
    TogglePlot(timerPlotBorderRange)
    DrawBorder("grey", "white")
    TogglePlot(plotRange, initPlot = FALSE, mar = c(1,1,1,1))
  
    plot(times, bty = "n", las = 1, type = "s")
    par(op)
    
  }

  if (print) cat("\r", "Time = ", times[length(times)])

  # Either set last element to current time (start timer) or 0 (record stopped timer)
  times <- c(times, if(start) Sys.time() else 0)
  
  return(times)
}


################################################################################
## ToggleTimer  -  Pauses or Resumes Timer
## -----------------------------------------------------------------------------
#' Toggles Timer State To Either Stop or Start Recording
#'
#' @description
#'    Helper function to help visualize component timing. Toggles tail element of 
#'    PlotTimer output vector betweeen change in time and actual time. In effect, 
#'    tells timer to pause or resume timing. 
#'
#' @param times        Times component. Generated on initial PlotTimer call. 
#'                     Last element is the most recent time record or change in 
#'                     time recorded; all previous entries are previous changes in 
#'                     time, in order of recording time. 
#'
#' @return Updated vector with updated state
#'
#' @template signature
#' @keywords internal
################################################################################
ToggleTimer <- function(times = c()){

  times[length(times)] <- as.double(Sys.time() - times[length(times)])

  return(times)
}
