#' @param maxArrivals
#'    maximum number of customer arrivals allowed to enter the system
#' @param seed
#'    initial seed to the random number generator (NA uses current state of
#'    random number generator; NULL seeds using system clock)
#' @param interarrivalType
#'    string representation of desired interarrival process 
#'    (default is "M": exponential with rate 1).
#' @param serviceType
#'    string representation of desired service process 
#'    (default is "M": exponential with rate 10/9).
#' @param maxTime
#'    maximum time to simulate
#' @param maxDepartures
#'    maximum number of customer departures to process
#' @param maxInSystem
#'    maximum number of customers that the system can hold (server(s) plus queue). 
#'    Infinite by default.
#' @param maxEventsPerSkyline
#'    maximum number of events viewable at a time in the skyline plot.
#'    A large value for this parameter may result in plotting delays.
#'    This parameter does not impact the final plotting, which will show all 
#'    end-of-simulation results.
#' @param saveAllStats
#'    if TRUE, returns all vectors of statistics (see below)
#'    collected by the simulation
#' @param saveInterarrivalTimes
#'    if TRUE, returns a vector of all interarrival times generated
#' @param saveServiceTimes
#'    if TRUE, returns a vector of all service times generated
#' @param saveWaitTimes
#'    if TRUE, returns a vector of all wait times (in the queue) generated
#' @param saveSojournTimes
#'    if TRUE, returns a vector of all sojourn (time in the system)
#'    times generated
#' @param saveNumInQueue
#'    if TRUE, returns a vector of times and a vector of counts for whenever
#'    the number in the queue changes
#' @param saveNumInSystem
#'    if TRUE, returns a vector of times and a vector of counts for whenever
#'    the number in the system changes
#' @param saveServerStatus
#'    if TRUE, returns a vector of times and a vector of
#'    server status (0:idle, 1:busy) for whenever the status changes
#' @param showOutput
#'    if TRUE, displays summary statistics upon completion
#' @param showSkyline 
#'    Shorthand for specifying showSkyline... parameters using chmod-like 
#'    octal component specification: use 1, 2, 4 for system, queue, and server 
#'    respectively, summing to indicate desired combination (e.g., 7 for all).
#' @param showSkylineInSystem
#'    if TRUE, the skyline will track the number of elements in the system.
#' @param showSkylineInQueue
#'    if TRUE, the skyline will track the number of elements in the queue.
#' @param showSkylineInServer
#'    if TRUE, the skyline will track the number of elements in the server.
#' @param showTitles
#'    if TRUE, titles will be shown on plots
#' @param showOutput
#'    if TRUE, displays summary statistics upon completion
#' @param showProgress
#'    if TRUE, displays a progress bar on screen during execution
#' @param jobImage
#'    a vector of URLs/local addresses of images to use as jobs. Requires
#'    package 'Magick'.
#' @param plotDelay
#'    a positive numeric value indicating seconds between state plottings. 
#'    A value of -1 enters 'interactive' mode, where the state will pause 
#'    for user input at each step.  A value of 0 will display only the final
#'    end-of-simulation plot.
#'
#' @details
#'   The \code{seed} parameter can take one of three valid
#'    argument types:
#'    \itemize{
#'      \item \code{NA} (default), which will use the current state of the random
#'           number generator without explicitly setting a new seed (see examples);
#'      \item a positive integer, which will be used as the initial seed passed in
#'           an explicit call to \code{\link{set.seed}}; or
#'      \item \code{NULL}, which will be passed in an explicit call to to
#'          \code{\link{set.seed}}, thereby setting the initial seed using the
#'          system clock.
#'   }
#'
#' @return
#'  The function returns a list containing:
#'    \itemize{
#'      \item the number of arrivals to the system (\code{customerArrivals}),
#'      \item the number of customers processed (\code{customerDepartures}),
#'      \item the ending time of the simulation (\code{simulationEndTime}),
#'      \item average wait time in the queue (\code{avgWait}),
#'      \item average time in the system (\code{avgSojourn}),
#'      \item average number in the system (\code{avgNumInSystem}),
#'      \item average number in the queue (\code{avgNumInQueue}), and
#'      \item server utilization (\code{utilization}).
#'    }
#'    of the queue as computed by the simulation.
#'    When requested via the ``save...'' parameters, the list may also contain:
#'    \itemize{
#'      \item a vector of interarrival times (\code{interarrivalTimes}),
#'      \item a vector of wait times (\code{waitTimes}),
#'      \item a vector of service times (\code{serviceTimes}),
#'      \item a vector of sojourn times (\code{sojournTimes}),
#'      \item two vectors (time and count) noting changes to number in the system
#'           (\code{numInSystemT}, \code{numInSystemN}),
#'      \item two vectors (time and count) noting changes to number in the queue
#'           (\code{numInQueueT}, \code{numInQueueN}), and
#'      \item two vectors (time and status) noting changes to server status
#'           (\code{serverStatusT}, \code{serverStatusN}).
#'    }
#'
#' @keywords utilities
#' @concept  queueing
#'
#' @author
#'    Barry Lawson (\email{blawson@@richmond.edu}), \cr
#'    Larry Leemis (\email{leemis@@math.wm.edu}), \cr
#'    Vadim Kudlay (\email{vadim.kudlay@@richmond.edu})
#'
#' @seealso \code{\link{rstream}}, \code{\link{set.seed}},
#'          \code{\link[=runif]{stats::runif}}
