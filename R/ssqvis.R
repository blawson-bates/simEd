## -------------------------------------------------------------------------
## This program animates the details of an event-drivent simulation of a 
## single-server queue with a FIFO (first-in-first-out) queue discipline and 
## default iid exponentially distributed interarrival times and iid 
## exponentially distributed service times (i.e., an M/M/1 queue).
##
## Assumptions:
##   * Initial state of the system:  empty queue and idle server
##   * Server takes no breaks
##   * No feedback (once a customer is serviced, the customer departs)
##   * Immediate service (no time delay between serving customers)
##   * Ending conditions:  the simulation is terminated by 1 of 3 conditions
##       - number of customers departed,
##       - simulated time exceeds a prescribed ending time,
##       - number of arrivals (this option allows the system to clear out,
##            unless one of the other stopping conditions overrides),
##     whichever occurs first
##
## Name            : ssqvis.R  (single server queue visualization)
## Authors         : Vadim Kudlay & Barry Lawson & Larry Leemis
## Language        : R (part of simEd package)
## Latest Revision : 13 Dec 2020
## -------------------------------------------------------------------------
#'
#' Single-Server Queue Simulation Visualization
#'
#' @description A modified ssq implementation that illustrates event-driven
#' details, including the event calendar, inversion for interarrival and service
#' time variate generation, the simulation clock, the status of the queueing 
#' system, and statistics collection, plotting step-by-step in either an
#' interactive mode or time-delayed automatic mode.
#'  
#' @param showSkylineInQueue  Should skyline plot track number in queue over time
#' @param showSkylineInSystem Should skyline plot track number in system over time
#' @param showSkylineInServer Should skyline plot track number in server over time
#' @param showSkyline         Shorthand for specifyigng showSkyline... parameters
#'      using chmod-like component specification: use 1, 2, 4 for system, queue, 
#'      and server in that order, sum to get combination (i.e. 7 for all).
#'
#' @details
#' Illustrates and animates the details of an event-driven implementation of a
#' single-server queue simulation.
#'
#' Animates the details of an event-driven implementation of a single-server 
#' queue simulation.
#'
#' The event calendar, inversion for interarrival and service time variates, 
#' and an abbreviated (current) timeline are animated in the top pane of the 
#' window.  In this pane, blue corresponds to the arrival process, orange 
#' corresponds to the service process, and purple corresponds to uniform 
#' variates used in inversion.  Yellow is used to highlight recent updates.
#'
#' The state of the queueing system is animated in the middle pane of the 
#' window.  In this pane, red indicates an idle server, orange indicates that
#' a new customer has just arrived to the server and a corresponding service 
#' time is being generated, and green indicates a busy server.  By default, 
#' customers are depicted as black rectangles and identified by increasing 
#' arrival number, but this depiction can be overridden by the \code{jobImage} 
#' parameter.
#'
#' Statistics are displayed in the bottom pane of the window.  Time-persistent 
#' statistics are shown as "skyline functions" in the left portion of this 
#' pane.  Both time-persistent and based-on-observation statistics are shown 
#' in respective tables in the right portion of this pane. In the tables, 
#' yellow is used to highlight recent updates.
#'
#' @importFrom graphics legend
#' @importFrom shape roundrect Arrows
#' @importFrom grDevices as.raster 
#' @importFrom stats sd
#'
#' @template queue
#' @keywords utilities
#' @concept  queueing
#'
#' @examples
#'
#'  # Visualizing ssq with a set seed, infinite queue capacity, 20 arrivals,
#'  # and showing skyline for all 3 attributes
#'  ssqvis(seed = 1234, maxArrivals = 20, showSkyline = 7)
#'
#'  # Perform simulation again with finite queue of low capacity. Note same
#'  # variable generation but different outcomes due to rejection pathway
#'  ssqvis(seed = 1234, maxArrivals = 25, showSkyline = 7, maxInSystem = 5)
#'
#'  # Perform simulation again with finite queue of low capacity. Note same
#'  # variable generation but different outcomes due to rejection pathway
#'  ssqvis(seed = 1234, maxTime = 25, showSkyline = 7, maxInSystem = 5)
#'
#'  # Using default distributions to make a default M/G/1 Queue
#'  ssqvis(seed = 1234, maxDepartures = 10, interarrivalType = "M", serviceType = "G")
#'
#' @export
################################################################################
ssqvis <- function(
                maxArrivals           = Inf,
                seed                  = NA,  # NA : use rng state; NULL : sys gen
#######
# custom arrival and service functions not (yet) supported for ssqvis
#                interarrivalFcn       = NA,  # defaultInterarrival,
#                serviceFcn            = NA,  # defaultService,
                interarrivalType      = "M",
                serviceType           = "M",
                maxTime               = Inf,
                maxDepartures         = Inf,
                maxInSystem           = Inf,
                maxEventsPerSkyline   = 15,
                saveAllStats          = FALSE,
                saveInterarrivalTimes = FALSE,
                saveServiceTimes      = FALSE,
                saveWaitTimes         = FALSE,
                saveSojournTimes      = FALSE,
                saveNumInQueue        = FALSE,
                saveNumInSystem       = FALSE,
                saveServerStatus      = FALSE,
                showOutput            = TRUE,
                showSkylineInQueue    = TRUE,
                showSkylineInSystem   = TRUE,
                showSkylineInServer   = TRUE,
                showSkyline           = NULL,
                showTitles            = TRUE,
                showProgress          = FALSE,
                jobImage              = NA,
                plotDelay             = -1
          )
{
  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  {
    interarrivalFcn <- NA
    serviceFcn <- NA

    checkVal(seed, "i", minex = 1, na = TRUE, null = TRUE)

    checkVal(maxTime,                  minex = 0)
    checkVal(maxArrivals,         "i", minex = 0)
    checkVal(maxDepartures,       "i", minex = 0)
    checkVal(maxEventsPerSkyline, "i", minex = 0)
    checkVal(maxInSystem,         "i", min   = 1)

    if (maxTime == Inf && maxArrivals == Inf && maxDepartures == Inf)
      stop("at least one of 'maxTime', 'maxArrivals', or 'maxDepartures' must be < Inf")

    if (!(interarrivalType %in% c("M","G")))
      stop("for animation purposes, interarrivalType must be either 'M' or 'G'")
    if (!(serviceType %in% c("M","G")))
      stop("for animation purposes, serviceType must be either 'M' or 'G'")

    checkVal(saveAllStats,          "l")
    checkVal(saveInterarrivalTimes, "l")
    checkVal(saveServiceTimes,      "l")
    checkVal(saveWaitTimes,         "l")
    checkVal(saveSojournTimes,      "l")
    checkVal(saveNumInQueue,        "l")
    checkVal(saveNumInSystem,       "l")
    checkVal(saveServerStatus,      "l")

    if (saveAllStats) {
      saveInterarrivalTimes <- TRUE
      saveServiceTimes      <- TRUE
      saveWaitTimes         <- TRUE
      saveSojournTimes      <- TRUE
      saveNumInQueue        <- TRUE
      saveNumInSystem       <- TRUE
      saveServerStatus      <- TRUE
    }

    checkVal(showOutput,   "l")
    checkVal(showTitles,   "l")
    checkVal(showProgress, "l")

    checkVal(showSkylineInSystem, "l")
    checkVal(showSkylineInQueue,  "l")
    checkVal(showSkylineInServer, "l")

    showSkylineResults  <- ParseShow(
      showBools   = c(showSkylineInSystem, showSkylineInQueue, showSkylineInServer),
      show        = showSkyline,
      ignoreBools = missing(showSkylineInSystem)
                 && missing(showSkylineInQueue)
                 && missing(showSkylineInServer)
    )
    showSkylineInSystem <- showSkylineResults[1]
    showSkylineInQueue  <- showSkylineResults[2]
    showSkylineInServer <- showSkylineResults[3]

    if (!is.na(jobImage) && !is.character(jobImage))
      stop("'jobImage' must be a local link or URL to an image (or a vector of such)")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
      stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")

    if (plotDelay == -1 && showProgress == TRUE) {
      warning("disabling text-based 'showProgress' with 'plotDelay' being -1 (interactive mode)",
              immediate. = TRUE)
      showProgress <- FALSE
    }

  }

  #############################################################################

  #############################################################################
  # Parses the show and associated parameters
  # Should handle all show, showQueue, showSkyline, and showECDF of parent function
  #############################################################################

  numServers  <-  1

  #############################################################################
  ## default interarrival and service functions...
  ## defaultService needs to come prior to serviceFcn error checking below
  #############################################################################

  if (typeof(interarrivalFcn) != "closure") {
    fcnInfo         <- GetDefaultDistroFcn(interarrivalType, isArrival = TRUE, asList = TRUE)
    interarrivalFcn <- fcnInfo$fcn
    arrivalNotation <- fcnInfo$notation
  } else {
    # for backward compatibility, we want to allow the user to provide a
    # function having no arguments, a la
    #       iaFcn <- function() { rexp(1, rate = 2, stream = 1) }
    # but Vadim's new code expects the function to allow for a variable number
    # of arguments, a la
    #       iaFcn <- function(...) { rexp(1, rate = 2, stream = 1) }
    # so parse the function to rebuild allowing for variable number of arguments
    # if necessary:
    interarrivalFcn <- ParseDistroFcn(interarrivalFcn, asList = TRUE)
    if (is.na(interarrivalFcn))
        stop("Error: custom distributions not supported for ssqvis")
    arrivalNotation <- GetDistroNotation(interarrivalFcn)
  }

  if (typeof(serviceFcn) != "closure") {
    fcnInfo         <- GetDefaultDistroFcn(serviceType, isArrival = FALSE, asList = TRUE)
    serviceFcn      <- fcnInfo$fcn
    serviceNotation <- fcnInfo$notation
  } else {
    # allow for variable # of args if necessary -- see comment above
    serviceFcn      <- ParseDistroFcn(serviceFcn, asList = TRUE)
    if (is.na(serviceFcn))
        stop("Error: custom distributions not supported for ssqvis")
    serviceNotation <- GetDistroNotation(serviceFcn)
  }
  

  #############################################################################

  # Function that returns picture to be used for elements. Returns NA by default.
  # If picture links are found, function is overridden.
  GetPic <- function(i) return(NA)

  if (!requireNamespace("magick", quietly = TRUE)) {
    message("Package \"magick\" needed for image inclusion to work. ",
            "Defaulting from using images.", call. = FALSE)
  }
  # Try to parse in pictures for jobs if any were specified
  else if (!is.na(jobImage[1])) 
  {
    # Initialize setup of picture-handling structures
    # Attempt to determine max number of jobs to use as size of random image matrix
    num.jobs <- if (maxArrivals < Inf)  maxArrivals+1  else  100

    # Parse the picture links, scale them to 80 width, and convert them to raster images
    pics <- lapply(jobImage, function(p) 
      as.raster(magick::image_scale(magick::image_read(p), "80")))

    # Override the GetPic function to either return
    #  - a randomly-set picture consistent with the inputted index
    #  - the picture to be used for all of the elements
    if (length(jobImage > 2)) {
      pictype <- sample(1:length(pics), num.jobs, replace = TRUE)
      GetPic  <- function(i) return(pics[[pictype[i]]])
    }  else  {
      GetPic  <- function(i) return(pics)
    }
  }

  #############################################################################

  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function(...) return(NA)
  pauseData <- NULL

  # Construct title for the plot
  if (maxInSystem < Inf || maxArrivals < Inf) {
      paramsText <- paste(" (",
                      if (maxInSystem  < Inf) paste("K = ", maxInSystem,  ", ", sep = ""),
                      if (maxArrivals < Inf)  paste("N = ", maxArrivals, ")", sep = ""),
                      sep = "")
  } else {
      paramsText <- ""
  }

  titleText <- bquote(
      .(arrivalNotation) ~ "/" ~ .(serviceNotation)
      ~ "/" ~ .(numServers) ~ .(paramsText))
  
  sf_ratio = c(1,2)  # in future update, allow user to specify this
  fs <- function(n) ScaleFont(n, f = 3, r = sf_ratio)

  # We maintain a calendar as two R lists (consistent with msq.R):
  #   - a list of length one corresponding to the next arrival
  #   - a list of length one (single sever) corresponding to the server
  # For arrivals, the event data structure entries correspond to:
  #   - type:  'a' for arrivals
  #   - time:  time of next arrival to occur
  #   - state: 0 <--> arrivals disallowed; 1 <--> arrivals allowed
  # For the server, the event data structure entries correspond to:
  #   - type:  's' for server
  #   - time:  time of next (state == 1) or last (state == 0) completion of svc
  #   - state: 0 <--> server currently idle; 1 <--> server currently busy
  arrivalsCal <- list(type = 'a', time = Inf, state = 0)
  serverCal   <- list(type = 's', time = 0,   state = 0)

  # progress bar to keep the user updated
  bar <- NULL
  if (interactive() && showProgress)
    bar <- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)


  ####################################################################################
  ##  Define graphical components
  ####################################################################################
  #TextBox    <- NULL  # defined in compPlot.R
  #DrawBorder <- NULL  # defined in compPlot.R
  #NewPlot    <- NULL  # defined in compPlot.R
  DrawCol    <- NULL  # to be defined below

  # Ranges for Plots (Keep bounds in (10, 190))
  # ----------   xmin, xmax, ymin, ymax ------------
  # These define the (x,y) areas for plotting into the window:
  #   1) calendarPlotRange: topmost plot, containing calendar & inversion plots
  #   2) queuePlotRange:    middle plot, containing the queue animation
  #   3) skylinePlotRange:  lower-left plot, containing skyline functions
  #   4) statsPlotRange:    lower-right plot, containing TPS & BOO stats (bank)
  #   5) upArrowPlotRange:  narrow plot b/w topmost and middle, w/ up arrow
  #                            pointing at current time on timeline in top plot
  calendarPlotRange <- c(  10,  190,  135,  190)
  queuePlotRange    <- c(  10,  190,   88,  130)
  skylinePlotRange  <- c(  18,  120,   18,   78)
  statsPlotRange    <- c( 130,  181,   18,   78)
  upArrowPlotRange  <- c(  65,   70,  126,  136)

  calendarPlotBGColor  <- rgb(0.95, 0.95, 0.95)
  ####################################################################################


  ####################################################################################
  ### -----------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   -----------------  ###
  ####################################################################################


  ####################################################################################
  ##  getNextEvent
  ## --------------------------------------------------------------------------------
  ##  Return a copy of the next event type
  ####################################################################################
  getNextEvent <- function() {

    minTime   <- Inf
    nextEvent <- NULL

    # first search the list-of-one arrivals calendar
    if (arrivalsCal$state == 1 && arrivalsCal$time < minTime) {
      minTime   <- arrivalsCal$time
      nextEvent <- arrivalsCal
    }

    # then search the list-of-one server event
    if (serverCal$state == 1 && serverCal$time < minTime) {
      minTime <- serverCal$time
      nextEvent <- serverCal
    }

    return (nextEvent)
  }
  ####################################################################################


  ####################################################################################
  ##  updateProgressBar
  ## --------------------------------------------------------------------------------
  ##  Updating the progress bar: defaults to using time if one provided; o/w
  ##  uses the min of maxArrivals and maxDepartures.  In the event that both
  ##  time and (one or more of) maxArrivals/maxDepartures are provided and
  ##  maxArrivals is hit well before maxTime, we update the bar after the
  ##  simulation loop so it always finishes at 100%.
  ####################################################################################
  updateProgressBar <- function(t, numArrivals, numDepartures)
  {
     if (maxTime < Inf)
        utils::setTxtProgressBar(bar, t / maxTime)
     else if (maxArrivals < maxDepartures)
        utils::setTxtProgressBar(bar, numArrivals / maxArrivals)
     else
        utils::setTxtProgressBar(bar, numDepartures / maxDepartures)

      utils::flush.console()
  }
  ####################################################################################

  # To be initialized to helper function
  DrawJob          <- NA
  GetJobHalfWidth  <- NA
  GetJobHalfHeight <- NA
  initDrawJob      <- FALSE

  # default/initial values used in DrawCurrQueue below
  SSQVals <- list(
    time             = 0,    # Time
    idsInSystem      = c(),  # numeric ids of customers currently in server + queue
    idJustRejected   = 0,    # numeric id of customer just reject and departing
    idNextToEnter    = 1,    # numeric id of customer next to enter the system
    idJustServed     = 0,    # numeric id of customer just served and departing
    currentProgress  = 0,    # current progress
    numRejects       = 0,    # number rejected
    serviceTime      = NA,   # service time
    interarrivalTime = NA,   # interarrival time
    arrivalTime      = NA,   # arrival time
    completionTime   = NA    # completion time
  )

  ####################################################################################
  ##  DrawCurrQueue
  ## --------------------------------------------------------------------------------
  #  Function to be initialized by setDrawCurrQueue
  #  Draws the current SSQ with default values modifiable by parameters
  ####################################################################################
  DrawCurrQueue <- function(time, 
                            idsInSystem, 
                            idJustRejected, 
                            idNextToEnter, 
                            idJustServed,
                            currentProgress, 
                            numRejects, 
                            interarrivalTime, 
                            arrivalTime, 
                            serviceTime, 
                            completionTime,
                            forceDraw = FALSE
                           )
  {
    if (plotDelay == 0 && !forceDraw) return()

    # set SSQVals to defaults if any are missing
    if (missing(time))             time             <- SSQVals[["time"]]
    if (missing(idsInSystem))      idsInSystem      <- SSQVals[["idsInSystem"]]
    if (missing(idJustRejected))   idJustRejected   <- SSQVals[["idJustRejected"]]
    if (missing(idNextToEnter))    idNextToEnter    <- SSQVals[["idNextToEnter"]]
    if (missing(idJustServed))     idJustServed     <- SSQVals[["idJustServed"]]
    if (missing(currentProgress))  currentProgress  <- SSQVals[["currentProgress"]]
    if (missing(numRejects))       numRejects       <- SSQVals[["numRejects"]]
    if (missing(interarrivalTime)) interarrivalTime <- SSQVals[["interarrivalTime"]]
    if (missing(arrivalTime))      arrivalTime      <- SSQVals[["arrivalTime"]]
    if (missing(serviceTime))      serviceTime      <- SSQVals[["serviceTime"]]
    if (missing(completionTime))   completionTime   <- SSQVals[["completionTime"]]

    # call DrawQueueComponents (below) to plot the queueing system in middle of plot
    DrawQueueComponents(time             = time,
                        idsInSystem      = idsInSystem, 
                        idJustRejected   = idJustRejected,
                        idNextToEnter    = idNextToEnter,
                        idJustServed     = idJustServed,
                        currentProgress  = currentProgress, 
                        numRejects       = numRejects, 
                        interarrivalTime = interarrivalTime, 
                        arrivalTime      = arrivalTime,
                        serviceTime      = serviceTime,
                        completionTime   = completionTime,
                        forceDraw        = forceDraw
                       )

    # switch to the up-arrow portion of the plot and draw time arrow there
    TogglePlot(upArrowPlotRange)
    Arrows(100, 0, 100, 150)

    ScalePlots(dims = c(0, 200, 0, 200))
  }

  ####################################################################################
  # Updates DrawCurrQueue with specified value defauls
  ####################################################################################
  setDrawCurrQueue <- function(time, 
                               idsInSystem, 
                               idJustRejected, 
                               idNextToEnter, 
                               idJustServed,
                               currentProgress, 
                               numRejects, 
                               interarrivalTime, 
                               arrivalTime,
                               serviceTime,
                               completionTime,
                               shiftInQueue   = FALSE,
                               #shiftInServer  = FALSE,   # this was set in a call, but never used here!
                               shiftOutServer = FALSE
                              ) 
  {
    drawPlot <- FALSE
    if (!missing(time))             { SSQVals[["time"]]             <<- time;             drawPlot <- TRUE }
    if (!missing(idsInSystem))      { SSQVals[["idsInSystem"]]      <<- idsInSystem;      drawPlot <- TRUE }
    if (!missing(idJustRejected))   { SSQVals[["idJustRejected"]]   <<- idJustRejected;   drawPlot <- TRUE }
    if (!missing(idNextToEnter))    { SSQVals[["idNextToEnter"]]    <<- idNextToEnter;    drawPlot <- TRUE }
    if (!missing(idJustServed))     { SSQVals[["idJustServed"]]     <<- idJustServed;     drawPlot <- TRUE }
    if (!missing(currentProgress))  { SSQVals[["currentProgress"]]  <<- currentProgress;  drawPlot <- TRUE }
    if (!missing(numRejects))       { SSQVals[["numRejects"]]       <<- numRejects;       drawPlot <- TRUE }
    if (!missing(interarrivalTime)) { SSQVals[["interarrivalTime"]] <<- interarrivalTime; drawPlot <- TRUE }
    if (!missing(arrivalTime))      { SSQVals[["arrivalTime"]]      <<- arrivalTime;      drawPlot <- TRUE }
    if (!missing(serviceTime))      { SSQVals[["serviceTime"]]      <<- serviceTime;      drawPlot <- TRUE }
    if (!missing(completionTime))   { SSQVals[["completionTime"]]   <<- completionTime;   drawPlot <- TRUE }

    if (shiftInQueue) {
      SSQVals[["idsInSystem"]]   <<- c(SSQVals[["idsInSystem"]], SSQVals[["idNextToEnter"]])
      SSQVals[["idNextToEnter"]] <<- SSQVals[["idNextToEnter"]] + 1
      drawPlot <- TRUE
    }
    if (shiftOutServer) {
      SSQVals[["idJustServed"]]  <<- SSQVals[["idsInSystem"]][1]
      SSQVals[["idsInSystem"]]   <<- SSQVals[["idsInSystem"]][-1]
      drawPlot <- TRUE
    }

    return(drawPlot)
  }
  ####################################################################################


  ####################################################################################
  ##  DrawQueueComponents
  ## --------------------------------------------------------------------------------
  ##  Function to generate SSQ plot given current status of simulation
  ####################################################################################
  DrawQueueComponents <- function(time, 
                        idsInSystem,          # vector of customer ids currently in system (server + queue)
                        idJustRejected = 0,   # customer # of just rejected (if any)
                        idNextToEnter = 0,    # customer # of next to enter
                        idJustServed = 0,     # customer # of just served (if any)
                        currentProgress,      # % of simulation completed -- for progress bar
                        numRejects = 0,
                        interarrivalTime,
                        arrivalTime,
                        serviceTime,
                        completionTime,
                        forceDraw = FALSE
                       )
  {
    if (!initDrawJob) 
    {
      initDrawJob <<- TRUE
      hasImg <- !is.na(jobImage[1])

      if (hasImg) {
        jtcol <- "red"
        GetJobHalfWidth <<- function(i) 6
        GetJobHalfHeight <<- function(i) 25
      } else {
        jtcol <- "white"
        GetJobHalfWidth <<- function(i) max(6, 2 * nchar(i, "width"))
        GetJobHalfHeight <<- function(i) ScaleFont(250)
      }

      # set up the DrawJob function used below
      DrawJob <<- function(i, midWidth, midHeight, 
                           textXDisplace = 0, textYDisplace = 0, bg) 
      {
        if (!hasImg)  
            textXDisplace <- textYDisplace <- 0
        else  
            bg <- NA
        # TextBox: can plot a textbook w/ text or scaled image -- see compPlot.R
        TextBox(i, midWidth, midHeight, GetJobHalfWidth(i), GetJobHalfHeight(i), 
                bg = bg, col = jtcol, img = GetPic(i), 
                txd = textXDisplace, tyd = textYDisplace)
      }
    }

    if (plotDelay == 0 && !forceDraw) return()
    # Initialize DrawJob function

    # use new == TRUE, the next high-level plotting command will not clean the
    # frame before drawing
    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)


    # Initialize plot and draw optional border (border to be phased out)
    {
      NewPlot()                     # create an initially empty plot
      if (showTitles)
        title(titleText, cex.main = 0.975, line = -1)
      TogglePlot(queuePlotRange)    # set drawing region to be queue (middle)
      DrawBorder("grey", "white")
    }

    # Draw main components of plot
    {
      TextBox(paste("System Clock: t =", pround(time)), 45, 170, 25, size = fs(16))

      # draw lines defining the queue boundaries
      {
        segments (30,  c(73, 127), x1 = 128, lwd = 2)
        segments (128, c(73, 127), y1 = c(80, 120), lwd = 2)
      }

      # draw server node with current time and processing node
      {
        f.svrColor <- simcolors$busySvr

        f.svcText  <-
          if (length(serviceTime) == 0 || is.na(serviceTime))
                bquote(s[.(idsInSystem[1])] == ...)
          else if (serviceTime <= 0)
                bquote(s[.(idsInSystem[1])] == .(sym$infinity))
          else  bquote(s[.(idsInSystem[1])] == .(pround(serviceTime)))

        f.cmpText  <-
          if (length(completionTime) == 0 || is.na(completionTime))
                bquote(c[.(idsInSystem[1])] == ...)
          else if (completionTime <= 0)
                bquote(c[.(idsInSystem[1])] == .(sym$infinity))
          else  bquote(c[.(idsInSystem[1])] == .(pround(completionTime)))

        if (length(serviceTime) == 0 || is.na(serviceTime)
          || length(completionTime) == 0 || is.na(completionTime))
            f.svrColor <- simcolors$pendSvr

        if (length(idsInSystem) == 0) {
          f.svrColor <- simcolors$idleSvr
          f.svcText  <- "idle"
          f.cmpText  <- ""
        }

        roundrect(c(150, 100), radx = 15, rady = 50, rx = 5, col = f.svrColor)
        TextBox(f.svcText, 150, 130, 20, size = fs(12))
        TextBox(f.cmpText, 150,  65, 20, size = fs(12))

        if (length(idsInSystem) > 0)
          DrawJob(idsInSystem[1], midWidth = 150, midHeight = 100, 
                  textXDisplace = 1.3, bg = simcolors$svgJob)
      }

      # Draw incoming and outgoing arrows and jobs next to them
      {
        TextBox("", 12, 100, 12, 50, bg = simcolors$arr)
        Arrows(  5, 100,  25, 100)
        Arrows(170, 100, 190, 100)

        if (idNextToEnter > 0) {
          DrawJob(idNextToEnter, midWidth = 10, midHeight = 100, 
                  textXDisplace = 1.3, bg = simcolors$inqJob)

          f.iarText <-
            if (is.na(interarrivalTime))
                bquote(r[.(idNextToEnter)] == ...)
            else  
                bquote(r[.(idNextToEnter)] == .(pround(interarrivalTime)))

          f.arrText <-
            if (is.na(arrivalTime))
                bquote(a[.(idNextToEnter)] == ...)
            else  
                bquote(a[.(idNextToEnter)] == .(pround(arrivalTime)))

          fsize <- if (is.na(arrivalTime))  fs(10)  else  fs(10 - log10(arrivalTime))

          # mw: mid-width  mh: mid-height  hw: half-width
          TextBox(f.iarText, mw = 12, mh = 131, hw = 12, size = fsize)
          TextBox(f.arrText, mw = 12, mh =  65, hw = 12, size = fsize)

        }

        if (idJustServed > 0) {
          DrawJob(idJustServed, midWidth = 178, midHeight = 100, 
                  textYDisplace = 1.6, bg = simcolors$svdJob)
        }
      }

      # Draw current progress bar
      {
        # Scaled half-height of progress bar
        f.phh <- ScaleFont(300)
        rect(0, 25 - f.phh, 200,                25 + f.phh, col = simcolors$progbar[1])
        rect(0, 25 - f.phh, 200 * currentProgress, 25 + f.phh, col = simcolors$progbar[2])

        # Output message associated with current progress
        f.ptext <- paste(sep = "", round(currentProgress * 100), "% Completed",
          if (numRejects > 0) paste(" (", numRejects, " Rejected)", sep = ""))
        # mw: mid-width  mh: mid-height  hw: half-width  hh: half-height
        TextBox (f.ptext, mw = 100, mh = 25, hw = 100, hh = f.phh, 
                 col = simcolors$progtxt, size = fs(15))
      }

      # Handle possible reject pathway
      if (maxInSystem < Inf && idJustRejected > 0)
        DrawJob(idJustRejected, midWidth = 10, midHeight = 30, 
                textXDisplace = 1.3, bg = "red")
    }

    # Try to print all of the nodes in queue
    if (length(idsInSystem) > 1)
    {
      f.lastjob  <- idsInSystem[length(idsInSystem)]           # Last job in system
      f.numslots <- max(3, 7 - floor(log10(f.lastjob)))        # Max num elements shown in queue
      es <- function(n) return(134 - 100 * (n - 1)/f.numslots) # X-Scale for nth element

      # Largest index to consider plotting; jobs after this (besides last) not plotted
      f.peakindex <- min(f.numslots, length(idsInSystem))

      for (i in 2:f.peakindex) {
        # If queue is too long, insert "..."
        if (i == f.numslots - 1 && length(idsInSystem) > f.numslots)
          points(
            x = es(f.numslots - 1) + 3 * c(-1,0,1),
            y = c(100,100,100),
            cex =  0.5, col = "black"
          )

        # If last job slot to fill, plot the last element in the queue
        else if (i == f.numslots)
          DrawJob(f.lastjob, midWidth = es(i), midHeight = 100, 
                  textYDisplace = -1.6, bg = simcolors$inqJob)

        # Otherwise, just plot the ith element with default x-scaling
        else  
          DrawJob(idsInSystem[i], midWidth = es(i), midHeight = 100, 
                  textYDisplace = -1.6, bg = simcolors$inqJob)
      }
    }
  }
  ####################################################################################

  ####################################################################################
  ##  PlotInitSkyline
  ## --------------------------------------------------------------------------------
  ##  Initialized the Skyline plot depending on specifications
  ####################################################################################
  PlotInitSkyline <- function (xRange = c(0,1), yRange = c(0,1))
  {
    skybgRange  <- skylinePlotRange + c(-5, 5, 0, 5) # Skyline background range
    canShow  <- c(showSkylineInSystem, showSkylineInQueue, showSkylineInServer)

    ScalePlots(skylinePlotRange)

    # set drawing region to be lower-left skyline functions area 
    TogglePlot(skybgRange)

    DrawBorder("grey", "white")
    TextBox("t", 194, 34, 5, 5)

    # set the drawing region to again be the lower-left, but now overlay
    TogglePlot(skylinePlotRange, initPlot = FALSE, mar = c(1,1,1,1))

    plot(NA, NA, xlim = xRange, ylim = yRange, xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")

    fontScale <- ScaleFont(15)
    legend("top", c("System", "Queue", "Server", "Avg")[canShow],
        lty = c(canShow[canShow > 0], 2),
        col = simcolors$sky[c(which(canShow > 0), 1)],
        cex = fontScale, x.intersp = fontScale, horiz = TRUE)

    if (showTitles)  title(paste("Number in System and In Queue"), cex.main = 0.975)
  }
  ####################################################################################


  ####################################################################################
  ##  PlotSkyline
  ## --------------------------------------------------------------------------------
  ##  Plots Skyline plot in appropriate spot if showSkyline is true
  ####################################################################################
  PlotSkyline <- function(times, 
                          numsInSystem, 
                          numsInQueue, 
                          numsInServer,
                          rangeForPlotting, 
                          entireRange,
                          forceDraw = FALSE)
  {
    if (plotDelay == 0 && !forceDraw) return()

    rangePlot <- rangeForPlotting
    rangeAll  <- entireRange

    yminratio <- (-0.4)/ScaleFont(15)   # Special ymin ratio to scale w/ resizings

    # If there's not enough data to plot, plot base graph and return
    if (length(rangeAll) < 2)  
    {
      #PlotInitSkyline(c(-0.02, 0.8), c(yminratio, 1.5))
      PlotInitSkyline(c(0, 1), c(yminratio, 1.5))
      axis(1, 0:1, line = -2, cex.axis = ScaleFont(15))
      axis(2, 0:1, line =  0, tck = -0.02, mgp = c(3,0.5,0), cex.axis = ScaleFont(15), las = 1)
      #points(0, 0, col = "black", cex = 0.5, pch = 19)
      return()
    }

    if (length(rangePlot) > 1 && length(rangeAll) > 1) {
      # Some function-side cleaning to ensure that no na values are let pass
      while (is.na(numsInSystem[rangePlot[2]])) rangePlot[2] <- rangePlot[2] - 1
      while (is.na(numsInSystem[rangeAll[2]]))  rangeAll[2]  <- rangeAll[2] - 1
      rangePlot <- rangePlot[1]:rangePlot[2]
      rangeAll  <- rangeAll [1]:rangeAll [2]
    }

    # Get subsets of input to match range
    timesSub <- times[rangePlot]
    numsSub  <- numsInSystem[rangePlot]
    numsQSub <- numsInQueue[rangePlot]
    numsSSub <- numsInServer[rangePlot]

    maxTime  <- timesSub[length(timesSub)]
    minTime  <- timesSub[1]

    # Specify the plotting range
    xRange   <- c(minTime, maxTime)
    yRange   <- c(yminratio, 1.5) * max(numsSub)

    PlotInitSkyline(xRange, yRange)

    # Plot lines and axes for the final graph
    if (showSkylineInSystem)
      lines(timesSub, numsSub,  type = "s", col = simcolors$sky[1], lwd = 1.25)
    if (showSkylineInQueue)
      lines(timesSub, numsQSub, type = "s", col = simcolors$sky[2])
    if (showSkylineInServer)
      lines(timesSub, numsSSub, type = "s", col = simcolors$sky[3], lwd = 0.75)

    xlabs <- pretty(c(minTime, maxTime))
    ylabs <- 0:max(numsSub)
    if (ylabs[length(ylabs)] > 5)  ylabs <- pretty(ylabs)

    axis(1, xlabs, line = -2, cex.axis = ScaleFont(15))
    axis(2, ylabs, line =  0, tck = -0.02, mgp = c(3,0.5,0), 
         cex.axis = ScaleFont(15), las = 1)
         # notes: tck used to reduce tick length; 
         #        mgp used to reduce distance b/w tick & label from 1 to 0.5

    # Plot average nums for the execution so far
    if (length(times) > 1) 
    {
      if (showSkylineInSystem)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInSystem[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[1])
      if (showSkylineInQueue)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInQueue[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[2])
      if (showSkylineInServer)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInServer[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[3])
    }
  }
  ####################################################################################

  calendar_xRange <- calendarPlotRange[2] - calendarPlotRange[1]
  calendar_yRange <- calendarPlotRange[4] - calendarPlotRange[3]
  stats_yRange    <- statsPlotRange[4]    - statsPlotRange[3]

  # Ranges for Plots (Keep bounds in (10, 190))
  #  - Order of vectors in Range : xmin, xmax, ymin, ymax
  #  - Add/Minus Multiple of xRange : Shifts as percentage of full plot

  # subRange <- baseRange + shift per dimension (allows for changes in baserange)
  titleRange <- calendarPlotRange + c(0, 0, calendar_yRange - 6, 0)
  box1Range  <- calendarPlotRange + c(0.01 * calendar_xRange, -0.75 * calendar_xRange, 0, -5)
  box2Range  <- calendarPlotRange + c(0.25 * calendar_xRange, -0.24 * calendar_xRange, 0, -5)
  box3Range  <- calendarPlotRange + c(0.73 * calendar_xRange, -0.04 * calendar_xRange, 0, -5)

  box2PlotRange    <- box2Range + c(10, -5, 20, -10)
  box2TimeRange    <- box2Range + c(10, -5, 10, 0)
  box2TimeRange[4] <- box2TimeRange[3] + 10

  statsRange    <- statsPlotRange + c(-5, 5, 0, 5)
  tpsStatsRange <- statsPlotRange + c(0, 0, 5 + stats_yRange * ( 0.48), 0)
  booStatsRange <- statsPlotRange + c(0, 0, 5,  stats_yRange * (-0.44))

  # Function to set title of the current step component
  SetTitle <- function(text) {
    TogglePlot(titleRange)
    TextBox(text, 100, 100, 100, 100,
      bg = "grey22", col = "white", font = 2, size = fs(18))
  }

  ####################################################################################
  ##  DrawEmptyIDF
  ## --------------------------------------------------------------------------------
  ##  Draw an empty "No Active Generation" box that will eventually be overlayed
  ##  with an interarrival or service time inversion animation.
  ####################################################################################
  DrawEmptyIDF <- function() 
  {
    # Plot the main background for component
    TogglePlot(calendarPlotRange + c(0, 0, 0, -4.5))
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    TogglePlot(box2PlotRange + c(-10, 10, -6, 6))
    DrawBorder("grey")
    TextBox("No Active \nGeneration", 100, 100, 100, 100, size = fs(20))
  }
  ####################################################################################


  ####################################################################################
  ##  DrawIDF
  ## --------------------------------------------------------------------------------
  ##  Draw IDF plot in steps plot box 2/3 in accordance with inputs
  ##  - u is the random variable from U(0,1) as generated in DrawInversionProcess
  ##  - x is the resulting mapped value from x
  ##  - xColor is the color of the x (should correlate with plt.colors)
  ##  - gxRange is the x range (0.01 - 0.99 expected) of the plot
  ##  - g{x/y}Exact are the x and y coordinates of the theorhetical CDF
  ##  - invline dictates whether or not to draw a line to continue potential idf above
  ####################################################################################
  DrawIDF <- function(u, x = NA, gxRange, gxExact, gyExact, isArrival = FALSE) 
  {

    timeMax <- gxRange[2]
    xWidth  <- diff(gxRange)
    gyRange <- if (u > 0.1)  c(-0.05,1)  else  c(-0.2,1)

    f.pcol <- if (isArrival)  simcolors$arr  else  simcolors$svc
    f.xlab <- if (isArrival)  "r"  else  "s"

    TogglePlot(box2PlotRange, initPlot = FALSE)

    plot(NA, NA, xlim = gxRange, ylim = gyRange,
        xaxt = "n", yaxt = "n", bty = "n", xlab = "")

    # wrap up so axis isn't drawn twice (heavy)
    # draw an empty-line bottom axis with ticks...
    if (is.na(x)) {
      axis(side = 1, at = pretty(c(0, timeMax)), lwd = 0, lwd.ticks = 1, labels = TRUE,
        padj = -0.5/ScaleFont(15), cex.axis = ScaleFont(15))
      axis(side = 1, at = pretty(c(0, timeMax)), labels = FALSE, tcl = 0,
        padj = -0.5, cex.axis = ScaleFont(15))
      text(timeMax*1.08, gyRange[1] - 0.05, f.xlab,
        cex = ScaleFont(15), xpd = NA)
      axis(side = 2, at = c(0, 0.25, 0.5, 0.75, 1), las = 1,
        cex.axis = ScaleFont(15))
    } else {
      # draw segments before points so points will cover segments
      segments(0, u, min(x, timeMax), u, lty = "dashed", lwd = 1, col = "red")
      if (x < timeMax) segments(x, 0, x, u, lty = "dashed", lwd = 1, col = "red")
    }

    DrawPoint(-xWidth/55, u, simcolors$u, cex = ScaleFont(25))
    if (!is.na(x) && x < timeMax)
      DrawPoint(x, if (gyRange[1] == -0.05) -0.02 else -0.15, f.pcol, cex = ScaleFont(25))

    # draw the curve at the end so it covers segments
    lines(gxExact, gyExact, col = f.pcol, lwd = 2)

  }
  ####################################################################################


  ####################################################################################
  ##  DrawTimelineComponents
  ## --------------------------------------------------------------------------------
  ##  Draw timeline in steps plot box 2/3 in accordance with inputs
  ##  - Time and maxTime are for plotting range, and should correlate with IDF
  ##  - Arrival/completion times specified by simcolors$arr/simcolors$svc
  ##  - Nodes designated as new/old will be marked with simcolors$new/grey, resp.
  ##  - invline dictates whether or not to draw a line to continue potential idf above
  ####################################################################################
  DrawTimelineComponents <- function(time, 
                                     maxTime,
                                     arrivalTime    = NA, 
                                     completionTime = NA, 
                                     oldTimes       = NA,   # may be one or more
                                     newTime        = NA,   # only one new time to plot
                                     inversionLine  = TRUE
                                    )
  {
    # Cover for clean slate before plotting
    TogglePlot(box2TimeRange + c(-5, 11, -6.5, -6.5))
    DrawBorder(NA, calendarPlotBGColor)

    maxX <- time + maxTime

    TogglePlot(box2TimeRange, initPlot = FALSE)

    plot(NA, NA, xlim = c(time, maxX), ylim = c(0, 1),
          bty = "n", xaxt = "n", yaxt = "n")
    axis(side = 1, at = pround(time + pretty(c(0, maxTime))), lwd = 0, lwd.ticks = 1,
          labels = TRUE, padj = -0.5/ScaleFont(15), cex.axis = ScaleFont(15))
    axis(side = 1, at = pround(time + pretty(c(0, maxTime + 0.25))),
          labels = FALSE, tcl = 0, padj = -0.5, cex.axis = ScaleFont(15))

    if (inversionLine) {
        segments(newTime, 0, newTime, 1, lty = "dashed", lwd = 1, col = "red")
    }

    showDots <- FALSE
    for (i in 1:length(oldTimes)) {
      if      (isValNum(oldTimes[i]) && oldTimes[i] == Inf)   oldTimes <- NA
      else if (isValNum(oldTimes[i]) && oldTimes[i] > maxX) { oldTimes[i] <- maxX; showDots <- TRUE }
    }
    for (i in 1:length(newTime)) {
      if      (isValNum(newTime[i]) && newTime[i] == Inf)   newTime[i] <- NA
      else if (isValNum(newTime[i]) && newTime[i] > maxX) { newTime[i] <- maxX; showDots <- TRUE }
    }
    if      (isValNum(arrivalTime)    && arrivalTime == Inf)    { arrivalTime <- NA }
    else if (isValNum(arrivalTime)    && arrivalTime > maxX)    { arrivalTime <- maxX; showDots <- TRUE }
    if      (isValNum(completionTime) && completionTime == Inf) { completionTime <- NA }
    else if (isValNum(completionTime) && completionTime > maxX) { completionTime <- maxX; showDots <- TRUE }

    if (showDots)  text(time + maxTime * 0.95, 0.1, "...")

    DrawPoint(oldTimes,       0.1, col = "grey",                cex = ScaleFont(25))
    DrawPoint(arrivalTime,    0.1, col = simcolors$arrivalTime, cex = ScaleFont(25))
    DrawPoint(completionTime, 0.1, col = simcolors$svc,         cex = ScaleFont(25))
    DrawPoint(newTime,        0.1, col = simcolors$newTime,     cex = ScaleFont(25))

    text(time + maxTime * 1.1, -0.05, "time", cex = ScaleFont(15), xpd = NA)
  }
  ####################################################################################


  ####################################################################################
  ##  DrawCalendarComponents
  ## --------------------------------------------------------------------------------
  #  Draws calendar with current values into topmost region on the left
  #  - calendarTimes are in the order: next arrival, next completion-of-service;
  #  - if either time is noted to be encircled, a blue text box will be drawn
  #       around that time
  #  - if either time is noted to be highlighted, its background will be yellow
  ####################################################################################
  DrawCalendarComponents <- function(calendarTimes  = c(0,0),
                                     encircleTimes  = c(FALSE, FALSE),
                                     highlightTimes = c(FALSE, FALSE)
                                    )
  {
    TogglePlot(box1Range)

    # determine whether to highligth either the arrival or CoS
    arrcol <- if (highlightTimes[1])  "yellow"  else  simcolors$arr
    svccol <- if (highlightTimes[2])  "yellow"  else  simcolors$svc

    TextBox("",         100, 110, 75, 75, bg  = "white")
    TextBox("Calendar", 100, 180, 75, 15, col = "white", bg = "lightsteelblue4", size = fs(20))

    # determine whether the arrival or CoS (or both) should have a notifying
    # blue rectangle draw around it
    if (encircleTimes[1])  TextBox("", 100, 120, 67.5, 20, bg = "blue")
    if (encircleTimes[2])  TextBox("", 100,  60, 67.5, 20, bg = "blue")

    TextBox("Next Arrival",    100, 150, 100, 10)
    TextBox("Next Completion", 100,  88, 100, 10)
    TextBox(paste("a =", pround(calendarTimes[1])),  100, 120,   60, 15, bg = arrcol)
    TextBox(paste("c =", pround(calendarTimes[2])),  100,  60,   60, 15, bg = svccol)
  }
  ####################################################################################


  ####################################################################################
  ##  DrawCurrCalendar
  ## --------------------------------------------------------------------------------
  #  Function to be initialized by setDrawCurrCalendar
  #  Draws the current calendar with default values overridable by parameters
  ####################################################################################
  CalendarVals <- list(arrivalTime    = 0, 
                       completionTime = 0, 
                       encircleTimes  = c(FALSE, FALSE),
                       highlightTimes = c(FALSE, FALSE))
  DrawCurrCalendar <- function(arrivalTime, 
                               completionTime,
                               encircleTimes,
                               highlightTimes,
                               forceDraw = FALSE) 
  {
    if (plotDelay == 0 && !forceDraw) return()

    if (missing(arrivalTime))     arrivalTime    <- CalendarVals[["arrivalTime"]]
    if (missing(completionTime))  completionTime <- CalendarVals[["completionTime"]]
    if (missing(encircleTimes))   encircleTimes  <- CalendarVals[["encircleTimes"]]
    if (missing(highlightTimes))  highlightTimes <- CalendarVals[["highlightTimes"]]

    DrawCalendarComponents(c(arrivalTime, completionTime), encircleTimes, highlightTimes)
  }

  ####################################################################################
  # Updates DrawCurrCalendar with specified value defauls
  ####################################################################################
  setDrawCurrCalendar <- function(arrivalTime, 
                                  completionTime, 
                                  encircleTimes  = c(FALSE, FALSE), 
                                  highlightTimes = c(FALSE, FALSE)
                                 )
  {
    plotCalendar <- FALSE
    if (!missing(arrivalTime))    { CalendarVals[["arrivalTime"]]    <<- arrivalTime;    plotCalendar <- TRUE }
    if (!missing(completionTime)) { CalendarVals[["completionTime"]] <<- completionTime; plotCalendar <- TRUE }
    if (!missing(encircleTimes))  { CalendarVals[["encircleTimes"]]  <<- encircleTimes;  plotCalendar <- TRUE }
    if (!missing(highlightTimes)) { CalendarVals[["highlightTimes"]] <<- highlightTimes; plotCalendar <- TRUE }
    return(plotCalendar)
  }
  ####################################################################################


  ####################################################################################
  ##  DrawCurrTimeline
  ## --------------------------------------------------------------------------------
  #  Function with default values in TimeLineVals
  #  Draws the current timeline with default values overridable by parameters
  ####################################################################################
  TimeLineVals <- list(time           = 0,
                       maxTime        = 4,
                       arrivalTime    = 0,
                       completionTime = 0,
                       oldTimes       = NA,
                       newTime        = NA)

  DrawCurrTimeline <- function(time, 
                               arrivalTime,
                               completionTime, 
                               oldTimes, 
                               newTime, 
                               inversionLine = TRUE,
                               forceDraw = FALSE) 
  {
    if (plotDelay == 0 && !forceDraw) return()

    if (missing(time))           time           <- TimeLineVals[["time"]]
    if (missing(arrivalTime))    arrivalTime    <- TimeLineVals[["arrivalTime"]]
    if (missing(completionTime)) completionTime <- TimeLineVals[["completionTime"]]
    if (missing(oldTimes))       oldTimes       <- TimeLineVals[["oldTimes"]]
    if (missing(newTime))        newTime        <- TimeLineVals[["newTime"]]
    DrawTimelineComponents(time, 
                           TimeLineVals[["maxTime"]],
                           arrivalTime, 
                           completionTime, 
                           oldTimes, 
                           newTime,
                           inversionLine)
  }
  ####################################################################################
  # Updates DrawCurrTimeline with specified value defauls
  ####################################################################################
  setDrawCurrTimeline <- function(time, 
                                  maxTime,
                                  arrivalTime,
                                  completionTime,
                                  oldTimes = NA,
                                  newTime  = NA
                                 )
  {
    plotTimeline <- FALSE
    if (!missing(time))           { TimeLineVals[["time"]]           <<- time;           plotTimeline <- TRUE; }
    if (!missing(maxTime))        { TimeLineVals[["maxTime"]]        <<- maxTime;        plotTimeline <- TRUE; }
    if (!missing(arrivalTime))    { TimeLineVals[["arrivalTime"]]    <<- arrivalTime;    plotTimeline <- TRUE; }
    if (!missing(completionTime)) { TimeLineVals[["completionTime"]] <<- completionTime; plotTimeline <- TRUE; }
    if (!missing(oldTimes))       { TimeLineVals[["oldTimes"]]       <<- oldTimes;       plotTimeline <- TRUE; }
    if (!missing(newTime))        { TimeLineVals[["newTime"]]        <<- newTime;        plotTimeline <- TRUE; }
    return(plotTimeline)
  }
  ####################################################################################


  ####################################################################################
  ##  SpecifyEventSteps
  ## --------------------------------------------------------------------------------
  ##  Plots Steps Required In Handling Current Event
  ##    The 'process' parameter determines the steps to be animated for each type
  ##    of event and is defined as follows:
  ##       0: initialize the calendar
  ##       1: generate service time for a customer arriving to empty system
  ##       2: generate next interarrival time for a customer arriving to empty 
  ##          system (and already entered/entering into service)
  ##       3: generate next interarrival time for a customer arriving to non-empty
  ##          system, and therefore entering the queue
  ##       4: generate next completion-of-service time when customer departs 
  ##          with other customers waiting in the queue
  ##       5: nothing in queue to be served, so put infinity for CoS in calendar
  ##       6: arrivals have been stopped, so put infinity for arrival in calendar
  ##       7: plot a customer rejection (arriving to full system)
  ##
  ##    Note that this function returns (almost) immediately if the plotDelay is
  ##    set to 0 (i.e., jump to final graphical display), returning only the 
  ##    interarrival or service time generated.
  ####################################################################################
  SpecifyEventSteps <- function(currentTime, 
                                arrivalTime, 
                                completionTime,
                                process,    # see defintions above
                                isArrival, 
                                advanceTime,
                                numInSystem,
                                forceDraw = FALSE
                               ) 
  {
    if (plotDelay == 0 && !forceDraw)
    {
      # if not drawing/plotting, just return the intarr or service time gen'd;
      # recall that Vadim's approach expects numInSystem as first argument
      # (perhaps revisit this idea later?)
      vfunc <- if (isArrival == TRUE)  interarrivalFcn  else  serviceFcn
      return(vfunc(num_in_sys = numInSystem, as_list = FALSE))
    }

    #   note: these appear in the "action" bar in pairs as msg[1]:msg[1+i]
    if (process == 0) 
        msg <- c(
            # this process describes generating the first arrival time
            # on calendar initialization
            "Initialize Calendar",                 # always present
            "Initially, Impossible Events",        # first action
            "Generate U(0, 1) For 1st Interarrival",  # second action ...
            "Generate 1st Interarrival Time By Inverting",
            "Compute 1st Arrival Time",
            "Place 1st Arrival Time On Calendar"
            )
    else if (process == 1)
        msg <- c(
            # this process describes generating the service time for
            # a customer that arrives to an empty system
            "Process Arrival (Server Idle)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Service Time",
            "Compute Next Service Time By Inverting",
            "Compute Next Completion Time",
            "Place Next Completion Time On Calendar"
            )
    else if (process == 2) 
        msg <- c(
            # this process describes generating the next arrival time
            # when a customer arrives to an empty system
            # (technically the customer immediately enters service
            #  so we do not describe as "Server Idle"
            "Process Arrival",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Interarrival Time",
            "Compute Next Interarrival Time By Inverting",
            "Compute Next Arrival Time",
            "Place Next Arrival Time On Calendar"
            )
    else if (process == 3) 
        msg <- c(
            # this process describes generating the next arrival time
            # when a customer arrives to an non-empty system and enters queue
            "Process Arrival (System Not Empty)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Interarrival Time",
            "Compute Next Interarrival Time By Inverting",
            "Compute Next Arrival Time",
            "Place Next Completion Time on Calendar"
            )
    else if (process == 4) 
        msg <- c(
            # this process describes generating the next CoS time
            # when a customer departs with other customers waiting in queue
            "Process Completion (Queue Not Empty)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Service Time",
            "Compute Next Service Time By Inverting",
            "Compute Next Completion Time",
            "Place Next Completion Time on Calendar"
            )

    # SPECIAL PROCESS: REMOVE SERVICE TIME
    else if (process == 5) {
      # this process is called when there is nothing to serve, 
      # puts inf in CoS slot in calendar
      DrawServiceAsImpossible(currentTime, arrivalTime, completionTime)
      return()
    }

    # SPECIAL PROCESS: REMOVE ARRIVAL TIME
    else if (process == 6) {
      # this process is called when stopping arrivals, puts inf 
      # in arrival slot in calendar
      DrawArrivalAsImpossible(currentTime, arrivalTime, completionTime)
      return()
    }

    else if (process == 7) {
      # this process describes animate a rejection when a customer
      # arrives to an already-at-capacity system
      DrawRejection(currentTime, arrivalTime, completionTime)
      pauseData <<- PauseCurrPlot(pauseData)
      return()
    }

    return(DrawInversionProcess(currentTime, arrivalTime, completionTime,
                                isArrival, process, msg, advanceTime, numInSystem))
  }
  ####################################################################################


  ####################################################################################
  ##  DrawServiceAsImpossible
  ## --------------------------------------------------------------------------------
  ##  Special progression function that visualizes placing inf on calendar for
  ##  completion of service
  ####################################################################################
  DrawServiceAsImpossible <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # set the plotting region to be the topmost, with calendar and inversion 
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetTitle("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrCalendar(encircleTimes = c(FALSE, TRUE))
    DrawCurrTimeline(time = cmp.time)

    pauseData <<- PauseCurrPlot(pauseData)
    if (plotDelay == 0) return()

    SetTitle("Process Completion (Queue Empty) : Remove Expired Completion From Calendar")
    DrawEmptyIDF()
    setDrawCurrCalendar(completionTime = Inf)
    DrawCurrCalendar(highlightTimes = c(FALSE, TRUE))
    setDrawCurrTimeline(time = cmp.time, completionTime = NA)
    DrawCurrTimeline(newTime = c(cmp.time), inversionLine = FALSE)

    pauseData <<- PauseCurrPlot(pauseData)
    if (plotDelay == 0) return()

    if (arr.time != Inf) {

      #SetTitle(paste("Determine Next Event Type : Find Min Time In Calendar"))
      SetTitle(paste("Determine Next Event Type : Find Minimum Time On Calendar"))

      DrawCurrTimeline(inversionLine = FALSE)
      DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    }

    # PauseCurrPlot NOT NEEDED AT END, AS IT WILL BE HANDLED IN MAIN
  }
  ####################################################################################


  ####################################################################################
  ##  DrawArrivalAsImpossible
  ## --------------------------------------------------------------------------------
  ##  Special progression function that visualizes placing inf on calendar for
  ##  next arrival (i.e., "closes the door on arrivals")
  ####################################################################################
  DrawArrivalAsImpossible <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # Plot the main background for component
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetTitle("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    DrawCurrTimeline(time = arr.time)

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0) return()
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return()
    }

    SetTitle("Finalizing Arrival : Cease Further Arrival Generations")
    DrawEmptyIDF()
    setDrawCurrCalendar(arrivalTime = Inf)
    DrawCurrCalendar(highlightTimes = c(TRUE, FALSE))
    setDrawCurrTimeline(time = arr.time, arrivalTime = NA)
    DrawCurrTimeline(newTime = c(arr.time), inversionLine = FALSE)

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0) return()
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return()
    }

    SetTitle("Progressing Simulation : Awaiting Next Service")
  }
  ####################################################################################


  ####################################################################################
  ##  DrawRejection
  ## --------------------------------------------------------------------------------
  ##  Special progression function that visualizes the ejection of completion time
  ####################################################################################
  DrawRejection <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # Plot the main background for component
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetTitle("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrQueue(idNextToEnter = SSQVals["idJustRejected"], idJustRejected = 0)  # Shows SSQ before rejection
    DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    DrawCurrTimeline(time = arr.time)

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0) return()
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return()
    }

    SetTitle("Rejecting Arrival : Ejecting Job and Removing Stale Arrival")
    DrawEmptyIDF()
    DrawCurrQueue()
    setDrawCurrCalendar(arrivalTime = Inf)
    DrawCurrCalendar(highlightTimes = c(TRUE, FALSE))
    setDrawCurrTimeline(time = arr.time, arrivalTime = Inf)
    DrawCurrTimeline(newTime = c(arr.time), inversionLine = FALSE)

  }
  ####################################################################################

  ####################################################################################
  ##  DrawInversionProcess
  ## --------------------------------------------------------------------------------
  ##  Plots calendar changes experienced during sim progression as popup window
  ####################################################################################
  DrawInversionProcess <- function(time, 
                             arrivalTime,
                             completionTime, 
                             isArrival,
                             process,      # defined b/w 0-7: see SpecifyEventSteps above
                             message,      # a vector of the event-steps defined in SpecifyEventSteps
                             advanceTime,  # boolean indicating whether time should advance here
                             numInSystem
                    ) 
  {

    # Determine whether the function being plotted is interarrival or service function;
    # recall that this function will be a list (see generators.R, with asList parameter)
    # containing the u and x values, quantile function, and text representation of the
    # distribution
    vfunc <- if (isArrival == TRUE)  interarrivalFcn  else  serviceFcn

    # Parse initial arrival and completion time to a value
    arr0  <- if (arrivalTime    == 0)  Inf  else  arrivalTime
    cmp0  <- if (completionTime == 0)  Inf  else  completionTime

    # Compute inversion and get results and function for plotting
    result   <- vfunc(numInSystem, TRUE)
    u        <- result[["u"]]          # Random variable generated from stream
    x        <- result[["x"]]          # Mapping of u onto distribution
    quantFnc <- result[["quantile"]]   # q* function parameterized to distribution
    plotText <- result[["text"]]       # Textual representation of distribution

    # Specify CDF plot ranges and ideal curve data
    gxRange  <- quantFnc(c(0.01, 0.99))
    gyExact  <- seq(0, 1, by = (diff(0:1))/1000)
    gxExact  <- quantFnc(gyExact)

    # Get variable set and color of graph based on type of graph;
    # These will be displayed either in the arriving customer rectangle
    # or in the server rounded rectangle, with corresponding times.
    displayVars  <- if (isArrival)  c("r", "a")  else  c("s", "c")

    ## SETUP STAGE: Set plot scale, draw border, and set title ##
    {
      # Plot the main plot for the component
      ScalePlots(calendarPlotRange)

      # set the drawing region to be the upper-portion w/ calendar & inversion
      TogglePlot(calendarPlotRange)
      DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

      # either "Initialize Calendar" or "Advance System Clock"
      ctitle <- if (time == 0) message[1] else "Advance System Clock"
      SetTitle(ctitle)

      DrawEmptyIDF()

      DrawCalendarComponents(calendarTimes = c(arr0,  cmp0), 
                             encircleTimes = c(FALSE, FALSE))

      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = NA)
    }


    # Skip this step unless the time is advancing
    if (advanceTime) {
      pauseData <<- PauseCurrPlot(pauseData)
      #if (pauseData$plotDelay == 0) return(x)
      if (pauseData$plotDelay == 0) {
          if (is.null(bar)) 
              bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
          showProgress <<- TRUE
          return(x)
      }
    }

    ## STAGE 1: Plot Initial calendar and that is all
    {
      # message("stage 1")

      # Plot initial calendar in slot 1
      SetTitle(paste(message[1], ":", message[2]))

      DrawCalendarComponents(
        calendarTimes = c(arr0, cmp0),
        encircleTimes = c(time == 0 || isArrival, 
                          time == 0 || !isArrival) )
    }

    if (time == 0) {
      pauseData <<- PauseCurrPlot(pauseData)
      #if (pauseData$plotDelay == 0) return(x)
      if (pauseData$plotDelay == 0) {
          if (is.null(bar)) 
              bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
          showProgress <<- TRUE
          return(x)
      }
    }

    ## STAGE 2: Show generation of U(0,1) and put point on graph
    {
      # message("stage 2")

      # Plot function inversion in slot 2
      SetTitle(paste(message[1], ":", message[3]))

      # If first generation, redraw calendar with arrival in replacement
      if (time == 0) {
        DrawCalendarComponents(calendarTimes = c(arr0, cmp0), 
                               encircleTimes = c(isArrival, !isArrival))
      }

      # Create plot of theorhetical CDF and plot u on y-axis
      TogglePlot(box2PlotRange + c(-16, 16, -8, 8))
      DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

      # Display title of graph
      TogglePlot(box2Range + c(-2, 2, 0, 0), initPlot = FALSE)
      typeText <- if (isArrival) "Interarrival" else "Service"
      TextBox(paste(typeText, "CDF:", plotText), 100, 180, 100, 10, font = 2, size = fs(18))

      # Switch to shifted subplot to plot actual ideal function
      DrawIDF(u, x = NA, gxRange, gxExact, gyExact, isArrival)

      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = NA)

      # Plot variable movement in slot 3
      TogglePlot(box3Range)

      TextBox("U(0,1)",  125, 175,   75, 20, bg = simcolors$u, tyd = 0.3)
      TextBox(pround(u), 125, 155, 75/2, 15, bg = "white")
      Arrows(50, 175,  15, 163)
    }

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0)  return(x)
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return(x)
    }

    ## STAGE 3: Invert uniform value across function and show resulting statistic
    {
      # message("stage 3")

      SetTitle(paste(message[1], ":", message[4]))

      # Update CDF plot by adding inversion visual
      DrawIDF(u, x = x, gxRange, gxExact, gyExact, isArrival)

      if (isArrival)
          setDrawCurrQueue(interarrivalTime = x)
      else
          #setDrawCurrQueue(shiftInServer = TRUE, serviceTime = x)
          setDrawCurrQueue(serviceTime = x)
      DrawCurrQueue()

      TogglePlot(box3Range)
      boxtitle <- paste("New", displayVars[1])
      TextBox(boxtitle,  125, 110,   75, 20, tyd = 0.3,
              bg = if (isArrival)  simcolors$arr  else  simcolors$svc)
      TextBox(pround(x), 125,  90, 75/2, 15, bg = "white")

      Arrows(0,  110,  35, 110)
    }

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0)  return(x)
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return(x)
    }

    ## STAGE 4: Add new value to timeline and compute resulting completion/arrival
    ## Tease at entrance of new value into calendar
    {
      # message("stage 4")
      SetTitle(paste(message[1], ":", message[5]))

      # Make box for new calendar time in box 3/3
      TogglePlot(box3Range)
      boxtitle <- paste("New", displayVars[2])
      boxvalue <- paste(pround(time), "+", displayVars[1])
      TextBox(boxtitle, 125, 45,    75, 20, bg = "yellow", tyd = 0.3)
      TextBox(boxvalue, 125, 25, 56.25, 15, bg = "white")
      Arrows(70, 90, 70, 60)

      # Update timeline to show inversion line
      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = time + x)

      if (isArrival)
          setDrawCurrQueue(arrivalTime = time + x)
      else
          setDrawCurrQueue(completionTime = time + x)
      DrawCurrQueue()

      # Add textbox underneath calendar to tease the new time's entrance into calendar
      TogglePlot(box1Range)
      TextBox(paste(displayVars[2], "=", pround(time + x)), 100, 20, 60, 15, bg = "yellow")
    }

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0) return(x)
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return(x)
    }

    ## STAGE 5: Add value to plot 1, updating visual calendar
    {
      # message("stage 5")
      SetTitle(paste(message[1], ":", message[6]))

      arr1 <- if ( isArrival)  pround(time + x)  else  arr0
      cmp1 <- if (!isArrival)  pround(time + x)  else  cmp0

      # Hide new value slot under calendar
      TogglePlot(box1Range)
      rect(38, 3, 162, 34.4, col = calendarPlotBGColor, border = NA)

      # reinitialize ssqvis-scope DrawCurrCalendar with latest arr and cmp
      setDrawCurrCalendar(arrivalTime = arr1, 
                          completionTime = cmp1)
      DrawCurrCalendar(encircleTimes  = c(isArrival, !isArrival),
                       highlightTimes = c(isArrival, !isArrival))

      DrawTimelineComponents(
        time           = time,
        maxTime        = gxRange[2],
        arrivalTime    = arr1,
        completionTime = cmp1,
        oldTimes       = c(arr0, cmp0),
        inversionLine  = FALSE
      )
    }

    pauseData <<- PauseCurrPlot(pauseData)
    #if (pauseData$plotDelay == 0) return(x)
    if (pauseData$plotDelay == 0) {
        if (is.null(bar)) 
            bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
        showProgress <<- TRUE
        return(x)
    }

    ## STAGE 6:
    {
      # message("stage 6")

      DrawEmptyIDF()
      # reinitialize ssqvis-scope DrawCurrTimeline with latest stats
      setDrawCurrTimeline(time           = time, 
                          maxTime        = gxRange[2], 
                          arrivalTime    = arr1,
                          completionTime = cmp1)
      DrawCurrTimeline()
      DrawCurrCalendar()
      SetTitle("Initialization Complete : Begin Event Loop")
    }

    # Plot this at all times, but stop only if it's the first one
    # Allows plot consistent setup into next visual
    if (process == 0) {
      pauseData <<- PauseCurrPlot(pauseData)
      #if (pauseData$plotDelay == 0) return(x)
      if (pauseData$plotDelay == 0) {
          if (is.null(bar)) 
              bar <<- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
          showProgress <<- TRUE
          return(x)
      }
    }

    ## STAGE 7:
    if (time == 0 || cmp1 < Inf) {

      # If completion time is set or it is the first step, find the minimum i
      # calendar time and tease entrance in next call
      # message("stage 7-1")

      # Plot initial calendar in slot 1
      SetTitle(paste("Determine Next Event Type : Find Mininum Time On Calendar"))

      DrawCurrTimeline(inversionLine = FALSE)
      minIsArrival <- (arr1 < cmp1) && arr1 != Inf
      DrawCurrCalendar(encircleTimes = c(minIsArrival, !minIsArrival))

    } else {

      # If the completion time is unset, tease generation of new completion time
      # message("stage 7-2")

      # Plot initial calendar in slot 1
      SetTitle(paste("Process Arrival : Generate New Service Time"))

      DrawCurrTimeline(inversionLine = FALSE)
      DrawCalendarComponents(
        calendarTimes  = c(arr1, cmp1),
        encircleTimes  = c(FALSE, TRUE),
        highlightTimes = c(FALSE, FALSE)
      )
    }

    # return the x value being generated
    return(x) 
  }
  ####################################################################################

  StatsValsFresh <- FALSE
  StatsVals <- list(nt = rep(0, 3),   
                    qt = rep(0, 3),   
                    xt = rep(0, 3),
                    w  = rep("-", 3),  
                    s  = rep("-", 3),  
                    o  = rep("-", 3))

  ####################################################################################
  ##  DrawStatsComponents
  ## --------------------------------------------------------------------------------
  ##  Plots time-persistent and based-on-observation statistics in a grid in 
  ##  the lower right of the window.
  ####################################################################################
  DrawStatsComponents <- function(n  = 0,
                                  nt = StatsVals[["nt"]],
                                  qt = StatsVals[["qt"]],
                                  xt = StatsVals[["xt"]],
                                  w  = StatsVals[["w"]],
                                  s  = StatsVals[["s"]],
                                  o  = StatsVals[["o"]],
                                  forceDraw = FALSE
                                 )
  {
    if (plotDelay == 0 && !forceDraw) return()

    # Plot only with fresh StatsVals unless plot forced.
    # If fresh values, update StatsValsFresh booleon
    if (identical(list(nt, qt, xt, w, s, o), StatsVals)) 
    {
      if (forceDraw == FALSE && StatsValsFresh == FALSE) {
        return()
      } else {
        StatsValsFresh <<- FALSE
      }
    } else {
      StatsValsFresh <<- TRUE
    }

    # Compute tracking of number in queue and pull out plotted subset
    ScalePlots(statsPlotRange)

    # set the plotting region to be the stats grid in lower right
    TogglePlot(statsRange)
    DrawBorder("grey")

    # Initialize DrawCol only once, push it to global instance
    if (is.null(DrawCol))
    {
      tw  <- c(35, 55, 55, 55)
      th  <- c(35, 40, 40, 40, 40)
      hbg <- "lightgrey"
      cbg <- "white"

      # Build table mid- and half-coords
      tmw <- tw[1]/2
      tmh <- th[1]/2
      if (length(tw) > 2) for (i in 2:length(tw))
        tmw <- c(tmw, sum(tw[1:(i-1)]) + tw[i]/2)
      if (length(th) > 2) for (i in 2:length(th))
        tmh <- c(tmh, sum(th[1:(i-1)]) + th[i]/2)
      tmh <- 200 - tmh

      # Define DrawCell to be used in DrawCol
      DrawCell <- function(text, c, r, bg = cbg, textf = NA) 
      {
        TextBox(text, tmw[c], tmh[r+1], tw[c]/2, th[r+1]/2,
          bg = bg, textf = pround)
      }

      # Define DrawCol
      DrawCol <<- function(col, texts, bg = cbg) 
      {
        for (i in 1:length(texts))
          DrawCell(texts[i], col, i, bg = if (col == 1 || i == 1) hbg else bg)
      }
    }

    col1 <- "white"
    col2 <- "yellow"

    TogglePlot(tpsStatsRange)

    TextBox("Time-Averaged Statistics", 100, 200, 100, 17, bg = "grey22", col = "white")
    DrawCol(1, c("", "@t", "avg", "sd"))
    DrawCol(2, c("n(t)", nt), if (all(nt == StatsVals[["nt"]])) col1 else col2)
    DrawCol(3, c("q(t)", qt), if (all(qt == StatsVals[["qt"]])) col1 else col2)
    DrawCol(4, c("x(t)", xt), if (all(xt == StatsVals[["xt"]])) col1 else col2)

    # Dash all values if the first element is NaN

    TogglePlot(booStatsRange)

    w <- if (!is.na(w[1])) w else rep("-", 3)
    s <- if (!is.na(s[1])) s else rep("-", 3)
    o <- if (!is.na(o[1])) o else rep("-", 3)

    TextBox(paste("Observed Statistics (n = ", n, ")", sep=""), 100, 200, 100, 17, bg = "grey22", col = "white")

    DrawCol(1, c("", "i", "avg", "sd"))
    DrawCol(2, c("wait",    w), if (all(w == StatsVals[["w"]])) col1 else col2)
    DrawCol(3, c("service", s), if (all(s == StatsVals[["s"]])) col1 else col2)
    DrawCol(4, c("sojourn", o), if (all(o == StatsVals[["o"]])) col1 else col2)

    StatsVals[1:6] <<- list(nt, qt, xt, w, s, o)
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

    numEntries <- 1000  ## initial size of storage vectors

    # list of vectors, one entry per customer, optionally returned on exit
    jobs <- list(
        arrTimes      = rep(NA, numEntries),  # arrival time of customer i
        intArrTimes   = rep(NA, numEntries),  # interarrival time of customer i
        waitTimes     = rep(NA, numEntries),  # wait time of customer i
        serviceTimes  = rep(NA, numEntries),  # service time of customer i
        sojournTimes  = rep(NA, numEntries),  # service time of customer i
        currState     = rep("pending",  numEntries))

    # a running list of the customer ids currently present in the system
    idsInSystem <- c()

    # for storing system-state changes: times and corresponding num in sys
    times       <- rep(NA, numEntries)    # times of changes to number in system
    numsInSys   <- rep(NA, numEntries)    # corresponding number in system
    numsInQue   <- rep(NA, numEntries)    # corresponding number in system
    numsInSvr   <- rep(NA, numEntries)    # corresponding number in system

    timesPos    <- 0                      # track where to place in times, nums
    svrPos      <- 0                      # track where to place in timesServer, numsServer

    timesServer <- rep(NA, numEntries)    # times of per-job idle/busy server changes
    numsServer  <- rep(NA, numEntries)    # corresponding number (0:idle, 1:busy)

    ## initialize system variables
    currTime      <- 0.0      # current time
    prevTime      <- 0.0      # time before the current time

    currSvcTime     <- 0.0      # holds current service time
    currIntArrTime  <- 0.0      # holds current interarrival time

    ## Running counters that only show current info
    numInSystem   <- 0      # number in the node
    numArrivals   <- 0      # used to count jobs arriving to the system
    numStarted    <- 0      # used to count jobs that have at least begun service
    numDepartures <- 0      # used to count processed jobs
    numRejects    <- 0      # used to count dropout count

    idJustRejected <- 0     # used to index newly-rejected customer (for plotting)
    idJustServed   <- 0     # used to index just-served customer, if one exists (for plotting)

    ##############################################################################

    ####################################################################
    ## Setter functions and generic statistic utility functions
    ## -----------------------------------------------------------------
    {
      # Returns mean of first i based-on-observation elements of sparse array
      GetBOOAvg <- function(d, i = length(d), getSd = FALSE) 
      {
        vals <- d[1:i]; vals <- vals[!is.na(vals)]
        if (length(vals) == 0) return(rep(NA, 1 + getSd))

        xbar <- mean(vals)
        if (getSd) {
          s <- sd(vals)
          s <- if (is.na(s)) 0 else s
          return(c(xbar, s))
        }
        return(xbar)
      }

      # mean of time-persistent statistics of first i elements of sparse array
      GetTPSAvg <- function(n, t = times, i = length(n), getSd = FALSE)
      {
        nVals <- n[1:i]; nVals <- nVals[!is.na(nVals)]
        tVals <- t[1:i]; tVals <- tVals[!is.na(tVals)]
        if (length(nVals) == 0)  return(rep(NA, 1 + getSd))
        nbar <- if (length(tVals) > 1) meanTPS(tVals, nVals)  else  mean(nVals)
        if (getSd) {
          s <- sdTPS(tVals, nVals)
          s <- if (is.na(s)) 0 else s
          return(c(nbar, s))
        }
        return(nbar)
      }

      # Sets the current time and number in system/queue/server
      SetSystemState   <- function(time, numInSystem)
      {
        timesPos <<- timesPos + 1
        if (timesPos > length(times)) {
           times     <<- resize(times)
           numsInSys <<- resize(numsInSys)
           numsInQue <<- resize(numsInQue)
           numsInSvr <<- resize(numsInSvr)
        }
        times    [timesPos] <<- time
        numsInSys[timesPos] <<- numInSystem
        numsInQue[timesPos] <<- max(0, numInSystem - numServers)
        numsInSvr[timesPos] <<- min(numServers, numInSystem)
      }

      # Sets current server state
      # (Note: Vadim has tried to design this to allow for future msq use,
      #  even though this animation is for ssq only...)
      SetServerState   <- function(time, numInService)
      {
        svrPos <<- svrPos + 1
        if (svrPos > length(timesServer)) {
            timesServer <<- resize(timesServer)
            numsServer  <<- resize(numsServer)
        }
        timesServer[svrPos] <<- time
        numsServer [svrPos] <<- numInService
      }

      # Sets arrival & interarrival times, updates job state, and ups counts
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      SetJobStateArr  <- function(arrivalTime, interarrivalTime, 
                                  state, i = numArrivals + 1)
      {
        while (i > length(jobs$arrTimes))
          jobs$arrTimes    <<- resize(jobs$arrTimes)
        while (i > length(jobs$intArrTimes))
          jobs$intArrTimes <<- resize(jobs$intArrTimes)
        while (i > length(jobs$currState))
          jobs$currState   <- resize(jobs$currState)

        # Double jobImage to facilitate job images
        if (!is.na(jobImage) && length(idsInSystem) > length(pictype))
          pictype <<- c(pictype, pictype)

        jobs$arrTimes   [i] <<- arrivalTime
        jobs$intArrTimes[i] <<- interarrivalTime
        jobs$currState  [i] <<- state
      }

      # Sets arrival & interarrival times, updates job state, and ups counts.
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      UpdateJobStateArr  <- function(arrivalTime, interarrivalTime, 
                                     state, i = numArrivals + 1)
      {
        jobs$arrTimes   [i] <<- arrivalTime
        jobs$intArrTimes[i] <<- interarrivalTime
        jobs$currState  [i] <<- state

        if (i == numArrivals + 1)  numArrivals <<- i
      }

      # Sets service & completion times, updates job state, and ups counts
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      SetJobStateSvc  <- function(waitTime, serviceTime, 
                                  state, i = numStarted + 1) 
      {
        while (i > length(jobs$waitTimes))
            jobs$waitTimes     <<- resize(jobs$waitTimes)
        while (i > length(jobs$serviceTimes))
            jobs$serviceTimes  <<- resize(jobs$serviceTimes)
        while (i > length(jobs$sojournTimes))
            jobs$sojournTimes  <<- resize(jobs$sojournTimes)

        jobs$waitTimes    [i]  <<- waitTime
        jobs$serviceTimes [i]  <<- serviceTime
        jobs$sojournTimes [i]  <<- waitTime + serviceTime
        jobs$currState    [i]  <<- state

        if (i == numStarted + 1)  numStarted <<- i
      }
    }
    ####################################################################

    SetSystemState(time = 0, numInSystem = 0)
    SetServerState(time = 0, numInService = 0)

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a
    # job-started/job-completed basis; while some of this extra may be
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis;
    # same for numsServer
    # numsQueue     <- sapply(numsInSys, function(n) { max(0, n - 1) })
    # avgNumInQueue <- meanTPS(times, numsQueue)
    ##############################################################################


    ##############################################################################

    drawQueueSkylineStats  <- function(forceDraw = FALSE) { return() } # To be declared
    
    viewJob <- function(job.num, data = jobs) 
    {
      if (is.na(job.num))
        message(" - 'job' must be followed by the job # you want to view,",
                " i.e. 'job 5'")
      else if (job.num > numArrivals)
        message(" - Job ", job.num, " has not been considered yet")
      else {
        message("Viewing Job ", job.num)
        message(" - Arrival Time      = ", data$arrTimes[job.num])
        message(" - Interarrival Time = ", data$intArrTimes[job.num])
        message(" - Wait Time         = ", data$waitTimes[job.num])
        message(" - Service Time      = ", data$serviceTimes[job.num])
        message(" - Departure Time    = ", data$arrTimes[job.num]
               + data$waitTimes[job.num] + data$serviceTimes[job.num])
      }
    }

    pauseData <<- SetPausePlot(
      plotDelay    = plotDelay,
      prompt       = "Hit 'ENTER' to proceed, 'q' to end, or 'help' for more: ",
      viewCommand  = c("job"),  # vector of strings for calling custom view functions
      viewInstruct = c("'job n'           = shows attributes of nth job"),
      viewFunction = list("1" = function(n) viewJob(n))
    )

    PauseCurrPlot <<- function(...) 
    {
      updatedPauseData <- PausePlot(
        pauseData = pauseData, 
        currStep  = numArrivals+1
      )
      if (updatedPauseData$passUntil == -2) {
        message("Finished jumping")
        drawQueueSkylineStats(forceDraw = TRUE)
        dev.flush(dev.hold())  # resets hold level to zero
        dev.hold()             # hold level now 1
      }
      
      if (pauseData$plotDelay != 0) 
      {
        DrawStatsComponents(n = numStarted)
      }
      
      return(updatedPauseData)
    }


    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    ## BEGIN PRIMARY PLOTTING/DRAWING CODE

    # Force layout to show only outputted plots if possible
    if(is.null(dev.list())) {
        dev.new(width = 5, height = 6) 
    }
    par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)

    # a bit about dev.hold() and dev.flush():
    #   - successive calls to dev.hold() will increase the hold level (default
    #     increase is 1, but value for level increase can be provided)
    #   - successive calls to dev.flush() will decrease the hold level (same
    #     as dev.hold() for default and optional value)
    #   - only when hold level is 0 will plots be displayed
    #   - dev.flush(dev.hold()) will _always_ decrease hold level to 0
    dev.hold()   # hold level now 1

    ResetPlot()  # just starts a new empty plot on the device

    # draw the default queue in the middle, with values drawn from the 
    # initial SSQVals list above
    DrawCurrQueue(forceDraw = TRUE)

    # draw the initial skyline plot
    PlotSkyline(times = 0, 
                numsInSystem = 0, numsInQueue = 0, numsInServer = 0,
                rangeForPlotting = 0, entireRange = 0, forceDraw = TRUE)

    # draw the initial stats grid (bank), with values drawn from the
    # initial StatsVals list above
    DrawStatsComponents(n = 0, forceDraw = TRUE)  # no jobs seen so far
    ##############################################################################

    # Used for timing functions
    start.t   <- rep(NA, numEntries)
    end.t     <- rep(NA, numEntries)
    run.count <- 0

    # generate initial arrival time, and save the first interarrival to
    # be added below to the list of saved arrivals;
    # as appropriate, plot (in topmost region) the steps associated with
    # initializing the calendar (i.e., process == 0)
    currIntArrTime <- SpecifyEventSteps(currentTime    = 0, 
                                        arrivalTime    = 0, 
                                        completionTime = 0, 
                                        process        = 0,  # 0: init calendar
                                        isArrival      = TRUE,
                                        advanceTime    = TRUE,
                                        numInSystem    = 0,
                                        forceDraw      = TRUE)

    arrivalsCal$time  <<- currIntArrTime
    arrivalsCal$state <<- 1   # indicate that arrivals are permitted

    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)

    DrawQueueComponents(time = 0,
                        idsInSystem      = NULL,
                        idJustRejected   = 0,
                        idNextToEnter    = 1,
                        idJustServed     = 0,
                        currentProgress  = 0,
                        numRejects       = 0,
                        interarrivalTime = currIntArrTime,
                        arrivalTime      = currIntArrTime,
                        serviceTime      = NA,
                        completionTime   = NA,
                        forceDraw        = TRUE
                        ) 
    
    TogglePlot(upArrowPlotRange)
    Arrows(100, 0, 100, 150)

    pauseData <<- PauseCurrPlot()

    ####################################################################
    ## current statistic retrieval/plotting functions
    ## -----------------------------------------------------------------
    {
      # Function for calculating current progress
      getCurrProgress <- function()
        return(max(c(numDepartures/maxDepartures, numArrivals/maxArrivals, currTime/maxTime)))

      # Plots current SSQ graphic representation
      drawCurrQueue <- function(forceDraw = FALSE) 
      {
        #if (plotDelay == 0 && !forceDraw) return()
        # par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)

        iar <- jobs$intArrTimes[numArrivals + 1]
        svc <- jobs$serviceTimes[idsInSystem[1]]
        arr <- if (is.na(iar)) iar else arrivalsCal$time
        cmp <- if (is.na(svc)) svc else serverCal$time

        if (setDrawCurrQueue(time             = currTime,
                             idsInSystem      = idsInSystem,
                             idJustRejected   = idJustRejected,
                             idNextToEnter    = numArrivals + 1,
                             idJustServed     = idJustServed,
                             currentProgress  = getCurrProgress(),
                             numRejects       = numRejects,
                             serviceTime      = svc,
                             interarrivalTime = iar,
                             arrivalTime      = arr,
                             completionTime   = cmp)) {
            DrawCurrQueue(forceDraw = forceDraw)
        }

        # Draws finishing line to connect SSQ plot with mapping plot
        TogglePlot(upArrowPlotRange)
        Arrows(100, 0, 100, 150)
      }

      # Plots current variable bank
      drawCurrStats <- function(forceDraw = FALSE) 
      {
        DrawStatsComponents(
          n = numStarted,
          #n  = numDepartures,
          nt = c(numsInSys[timesPos],
            GetTPSAvg(numsInSys,        i = timesPos,   getSd = TRUE)),
          qt = c(numsInQue[timesPos],
            GetTPSAvg(numsInQue,        i = timesPos,   getSd = TRUE)),
          xt = c(numsInSvr[timesPos],
            GetTPSAvg(numsInSvr,        i = timesPos,   getSd = TRUE)),
          w  = c(jobs$waitTimes[numStarted],
            GetBOOAvg(jobs$waitTimes,    i = numStarted, getSd = TRUE)),
          s  = c(jobs$serviceTimes[numStarted],
            GetBOOAvg(jobs$serviceTimes, i = numStarted, getSd = TRUE)),
          o  = c(jobs$sojournTimes[numStarted],
            GetBOOAvg(jobs$sojournTimes, i = numStarted, getSd = TRUE)),
          #w  = c(jobs$waitTimes[numDepartures],
          #  GetBOOAvg(jobs$waitTimes,    i = numDepartures, getSd = TRUE)),
          #s  = c(jobs$serviceTimes[numDepartures],
          #  GetBOOAvg(jobs$serviceTimes, i = numDepartures, getSd = TRUE)),
          #o  = c(jobs$sojournTimes[numDepartures],
          #  GetBOOAvg(jobs$sojournTimes, i = numDepartures, getSd = TRUE)),
          forceDraw = forceDraw
        )
      }

      # Plots current syline function
      plotCurrSkyline <- function(forceDraw = FALSE) 
      {
        if (plotDelay == 0 && !forceDraw) return()
        rangePlot <-
          if (canRunSim())  
            c(max(timesPos - maxEventsPerSkyline, 1), timesPos)
          else  
            c(1, timesPos)
        PlotSkyline(times, numsInSys, numsInQue, numsInSvr, rangePlot, c(1, timesPos), forceDraw)
      }

      # Plots current generation mapping
      specifyCurrentEventSteps <- function(process, 
                                           isArrival, 
                                           advanceTime = TRUE,
                                           forceDraw = FALSE
                                          )
      {
        drawQueueSkylineStats(forceDraw = forceDraw)
        generatedTime <- 
            SpecifyEventSteps(currentTime    = currTime,
                              arrivalTime    = if (currIntArrTime == Inf) Inf else arrivalsCal$time,
                              completionTime = if (currSvcTime == Inf) Inf else serverCal$time,
                              process        = process, # integer 0-7: see SpecifyEventSteps
                              isArrival      = isArrival,
                              advanceTime    = advanceTime,
                              numInSystem    = numInSystem,
                              forceDraw      = forceDraw
                             )
        return(generatedTime)
      }

      drawQueueSkylineStats  <- function(forceDraw = FALSE) 
      {
        if (pauseData$plotDelay != 0 || forceDraw == TRUE)
        {
          drawCurrQueue(forceDraw = TRUE)
          plotCurrSkyline(forceDraw = TRUE)
          drawCurrStats(forceDraw = TRUE)
        }
      }

      # Function to check if the simulation should be progressed
      canRunSim <- function() 
      {
        return(currTime < maxTime
           && (numDepartures < maxDepartures)
           && (arrivalsCal$state == 1 || numInSystem > 0))
      }
    }
    ####################################################################

    ## BEGIN MAIN SIMULATION LOOP
    while (canRunSim())
    {
      ##############################################################################
      # enter the simulation event loop:
      # Continue so long as:
      #    - the maximum simulated time has not been reached, and
      #    - the maximum number of departures has not been reached, and
      #    - there are jobs in the system still to be flushed out
      # The net effect of the last piece of the compound condition is to
      # allow the user to set maxArrivals and allow them all to be cleared
      # (unless the user specifies a maxTime that doesn't allow them to all
      # be cleared, or unless the user specifies a maxDepartures that is
      # less than maxArrivals)

      # run.count <- run.count + 1
      # start.t[run.count] <- Sys.time()

      idJustServed <- idJustRejected <- 0  # Initializes newly-changed jobs
      nextEvent    <- getNextEvent()       # Retrieves next event for consideration
      prevTime     <- currTime             # Stores previous clock tine
      currTime     <- nextEvent$time       # Advances the clock

      if (prevTime != 0) drawQueueSkylineStats()

      # Handle end of simulation
      if (currTime > maxTime || numDepartures >= maxDepartures) 
      {
          if (currTime > maxTime) currTime <- maxTime

          # ensure max time vals are added to end of times/num structures
          SetSystemState(time = currTime, numInSystem = numsInSys[timesPos])

          # Update server states to reflect end of simulation state
          SetServerState(time = currTime, numInService = numsServer[svrPos])

          break
      }

      # event type 'a': process an arrival
      if (nextEvent$type == 'a') 
      {
        # If queue is infinite OR finite w/ room, process arrival
        #if (numInSystem[length(numInSystem)] <= maxInSystem)
        if (numInSystem < maxInSystem) 
        {

          # Add new arrival statistics into jobs list
          UpdateJobStateArr(arrivalTime = currTime, 
                            interarrivalTime = currIntArrTime, 
                            state = "queued")

          numInSystem <- numInSystem + 1

          # numArrivals is the latest customer id number
          idsInSystem <- append(idsInSystem, numArrivals)
          setDrawCurrQueue(shiftInQueue = TRUE)

          # Double jobImage to facilitate job images
          if (!is.na(jobImage) && length(idsInSystem) > length(pictype))
            pictype <<- c(pictype, pictype)

          # Add new time interval and system population size to times and nums
          SetSystemState(time = currTime, numInSystem = numInSystem)

          # If the current queue had only one element, get new service time immediately
          if (numInSystem == 1)
          {
            # Plots generation w/ inversion plot
            # process 1: arrival with server idle
            currSvcTime <- specifyCurrentEventSteps(process = 1, isArrival = FALSE)
            serverCal$time  <<- currTime + currSvcTime
            serverCal$state <<- 1 # INDICATE SERVER IS BUSY
        
            # Add service time/wait time statistics for current element in queue
            SetJobStateSvc(waitTime = 0, 
                           serviceTime = currSvcTime, 
                           state = "in service")
        
            # Update timesServer and numsServer to reflect current time state
            SetServerState(time = currTime, numInService = 1)
          }
        }
        else # Otherwise, process rejection
        {
          # Add new rejection statistics into jobs list
          UpdateJobStateArr(arrivalTime = currTime,
                            interarrivalTime = currIntArrTime, 
                            state = "rejected")

          numRejects <- numRejects + 1

          idJustRejected <- numArrivals

          # Special mapping routine for job rejection
          # process 7: rejection of customr
          specifyCurrentEventSteps(process = 7, isArrival = FALSE)

          currArr <- Inf
          currIntArrTime <- Inf

          # Add new time interval and system population size to times and nums
          SetSystemState(time = currTime, numInSystem = numInSystem)
        }

        ## Add new time interval and system population size to times and nums
        #SetSystemState(time = currTime, numInSystem = numInSystem)

        # handle end-of-simulation scenarios
        if (arrivalsCal$time >= maxTime || numArrivals == maxArrivals)
          arrivalsCal$state <<- 0   # NO MORE ARRIVALS PERMITTED

        if (numArrivals < maxArrivals) 
        {
          if (numInSystem == 1) {
            # process 2: generate the next arrival after customer
            #  arrived to an empty system & entered service immediately
            currIntArrTime <- specifyCurrentEventSteps(process = 2, 
                        isArrival = TRUE, advanceTime = FALSE)
          } else {
            # process 3: generate the next arrival when customer
            #  arrived to a non-empty system & entered queue
            currIntArrTime <- specifyCurrentEventSteps(process = 3, isArrival = TRUE)
          }
          arrivalsCal$time <<- arrivalsCal$time + currIntArrTime
          SetJobStateArr(arrivalTime = arrivalsCal$time, 
                         interarrivalTime = currIntArrTime, 
                         state = "not arrived")

        } else {
          # process 6: no more arrivals allows, so put Inf in 
          #  arrival slot in calendar
          specifyCurrentEventSteps(process = 6, isArrival = TRUE)
          currIntArrTime <- Inf
          currArr <- Inf

        }
      }
      else # event type 's': process a departure
      {
        numDepartures <- numDepartures + 1
        numInSystem   <- numInSystem   - 1

        jobs$currState[idsInSystem[1]] <- "served"
        idJustServed <- idsInSystem[1]
        idsInSystem <- idsInSystem[-1]

        # Update time intervals and nums in system to current state
        SetSystemState(time = currTime, numInSystem = numInSystem)

        # customers waiting, so begin serving the next
        # immediately put first in queue into service
        if (numInSystem > 0) {

          # Generate the current service time for queue
          # process 4: customer departs with other customers
          #  waiting in the queue
          currSvcTime <- specifyCurrentEventSteps(process = 4, isArrival = FALSE)

          serverCal$time <<- currTime + currSvcTime   # Update server calendar

          # Update wait times and service times for next element to be serviced
          SetJobStateSvc(waitTime = currTime - jobs$arrTimes[idsInSystem[1]],
                         serviceTime = currSvcTime, 
                         state ="in service")

          # Update server values to reflect departure
          SetServerState(time = currTime, numInService = 1)

        }
        else 
        {
          # nothing waiting in queue -- server goes idle
          serverCal$state <<- 0  # INDICATE SERVER IS NOW IDLE

          SetServerState(time = currTime, numInService = 0)

          if (pauseData$plotDelay != 0) {
            # process 5: customer departed and queue is empty
            specifyCurrentEventSteps(process = 5, isArrival = FALSE)
          }

          currSvcTime <- Inf
          currCmp <- Inf
        }
      }
      ########################################################################

      ########################################################################
      ## Segment to animate all output at end of cycle
      ########################################################################

      # Handles outdated calendar values if not overridden
      currArr <- if (currTime >= arrivalsCal$time) Inf else arrivalsCal$time
      currCmp <- if (currTime >=   serverCal$time) Inf else serverCal$time

      setDrawCurrTimeline(
            time           = currTime,
            arrivalTime    = currArr,
            completionTime = currCmp)
      setDrawCurrCalendar(
            arrivalTime   = currArr,
            completionTime = currCmp)

      if (canRunSim()) {
        DrawCurrTimeline()
        drawQueueSkylineStats()
        pauseData <<- PauseCurrPlot()
        ResetPlot()
      }

      # Update console progress bar
      if (interactive() && showProgress)
        updateProgressBar(currTime, numArrivals, numDepartures)

      ########################################################################

    } # while (...)

    pauseData <<- PauseCurrPlot()
    ResetPlot()

    drawQueueSkylineStats(forceDraw = TRUE)
    DrawEmptyIDF()
    SetTitle("Execution Finished")
    DrawCurrCalendar(forceDraw = TRUE)
    setDrawCurrTimeline(time           = currTime, 
                        arrivalTime    = NA, 
                        completionTime = NA)
    DrawCurrTimeline(forceDraw = TRUE)
    dev.flush(dev.hold())  # this will automatically flush all, regardless of hold level

    ##############################################################################
    ## Conduct final saves, formats, and returns
    ##############################################################################

    # ensure bar runs through end (e.g., if maxTime too big for maxArrivals)
    if (interactive() && showProgress) {
       utils::setTxtProgressBar(bar, 1)
       close(bar)
    }

    # "growing" per-customer vectors may leave NA values at ends -- remove them
    jobs$arrTimes     <- jobs$arrTimes     [!is.na(jobs$arrTimes)]
    jobs$intArrTimes  <- jobs$intArrTimes  [!is.na(jobs$intArrTimes)]
    jobs$waitTimes    <- jobs$waitTimes    [!is.na(jobs$waitTimes)]
    jobs$serviceTimes <- jobs$serviceTimes [!is.na(jobs$serviceTimes)]

    times       <- times       [!is.na(times)]
    numsInSys   <- numsInSys   [!is.na(numsInSys)]
    numsInQue   <- numsInQue   [!is.na(numsInQue)]
    numsInSvr   <- numsInSvr   [!is.na(numsInSvr)]
    timesServer <- timesServer [!is.na(timesServer)]
    numsServer  <- numsServer  [!is.na(numsServer)]

    # if there is a job still in service at the end of the simulation, it will
    # have received a wait time and a service time, but we should not include
    # its (automatically computed, via the sum below) sojourn time, as the job
    # has not actually completed service
    jobs$sojournTimes <- (jobs$waitTimes + jobs$serviceTimes)[1:numDepartures]

    avgWait     <- mean(jobs$waitTimes)
    avgSojourn  <- mean(jobs$sojournTimes)
    avgNumInSys <- meanTPS(times, numsInSys)
    util        <- meanTPS(timesServer, numsServer)
    avgNumInQue <- meanTPS(times, numsInQue)

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a
    # job-started/job-completed basis; while some of this extra may be
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis;
    # same for numsServer

    # make sure the default output is printed
    {
      printed <- ""
      printed <- paste(printed, "$customerArrivals\n[1]", sep = "")
      printed <- paste(printed, numArrivals)
      printed <- paste(printed, "\n\n$customerDepartures\n[1]", sep = "")
      printed <- paste(printed, numDepartures)
      printed <- paste(printed, "\n\n$simulationEndTime\n[1]", sep = "")
      printed <- paste(printed, round(min(currTime, maxTime), digits = 5))
      printed <- paste(printed, "\n\n$avgWait\n[1]", sep = "")
      printed <- paste(printed, signif(avgWait, digits = 5))
      printed <- paste(printed, "\n\n$avgSojourn\n[1]", sep = "")
      printed <- paste(printed, signif(avgSojourn, digits = 5))
      printed <- paste(printed, "\n\n$avgNumInSystem\n[1]", sep = "")
      printed <- paste(printed, signif(avgNumInSys, digits = 5))
      printed <- paste(printed, "\n\n$avgNumInQueue\n[1]", sep = "")
      printed <- paste(printed, signif(avgNumInQue, digits = 5))
      printed <- paste(printed, "\n\n$utilization\n[1]", sep = "")
      printed <- paste(printed, signif(util, digits = 5))
      printed <- paste(printed, "\n\n", sep = "")
      if (showOutput) on.exit(cat(printed))
    }

    # create a list of the output, to be returned to the user
    ssq <- list(customerArrivals   = numArrivals,
                customerDepartures = numDepartures,
                simulationEndTime  = min(currTime, maxTime),
                avgWait            = avgWait,
                avgSojourn         = avgSojourn,
                avgNumInSystem     = avgNumInSys,
                avgNumInQueue      = avgNumInQue,
                utilization        = util)

    # note that computing interarrivals as diff(c(0, jobs$arrTimes)) gives
    # _slightly_ different times than storing interarrivals when generated;
    # similar for having computed sojurns as waits + services above
    {
      if (saveInterarrivalTimes) ssq$interarrivalTimes <- jobs$intArrTimes
      if (saveServiceTimes)      ssq$serviceTimes      <- jobs$serviceTimes
      if (saveWaitTimes)         ssq$waitTimes         <- jobs$waitTimes
      if (saveSojournTimes)      ssq$sojournTimes      <- jobs$sojournTimes

      if (saveNumInSystem) {
         ssq$numInSystemT  <- times
         ssq$numInSystemN  <- numsInSys
      }
      if (saveNumInQueue) {
         ssq$numInQueueT   <- times
         ssq$numInQueueN   <- numsInQue
      }
      if (saveServerStatus) {
         ssq$serverStatusT <- timesServer
         ssq$serverStatusN <- numsServer
      }
    }
    ##############################################################################

    return(invisible(ssq)) # invisible() makes sure big list of times aren't printed!

  } # main

  ####################################################################################

  # *********************************************************
  # * CALL THE MAIN ssq FUNCTION, executing the simulation. *
  # * This passes a list back to the R user.                *
  # *********************************************************
  return(main(seed))

}
