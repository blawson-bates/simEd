#' @details
#'   The algorithm for generating random variates from the <%=distrolc%> distribution is
#'   synchronized (one random variate for each random number) and monotone in u.
#'   This means that the variates generated here might be useful in some variance
#'   reduction techniques used in Monte Carlo and discrete-event simulation.
#'
#'   Values from the u vector are plotted in the cdf plot along the vertical axis
#'   as colored dots.  A horizontal, dashed, colored line extends from the dot to
#'   the population cdf.  At the intersection, a vertical, dashed colored line
#'   extends downward to the horizontal axis, where a second colored dot, denoting
#'   the associated <%=distrolc%> random variate is plotted.
#'
#'   This is not a particularly fast variate generation algorithm because it uses
#'   the base R \code{q<%= funct %>} function to invert the values contained in \code{u}.
#'
#'   All of the elements of the \code{u} vector must be between 0 and 1.
#'   Alternatively, \code{u} can be \code{NULL} in which case plot(s) of the
#'   theoretical <%=PXF%> and cdf are displayed according to plotting parameter
#'   values (defaulting to display of both the <%=PXF%> and cdf).
#'
#'   The \code{show} parameter can be used as a shortcut way to denote plots to
#'   display.  The argument to \code{show} can be either:
#'   \itemize{
#'     \item a binary vector of length three, where the entries from left to right
#'         correspond to \code{showCDF}, \code{show<%=PXF%>}, and \code{showECDF},
#'         respectively.  For each entry, a 1 indicates the plot should be
#'         displayed, and a 0 indicates the plot should be suppressed.
#'     \item an integer in [0,7] interpreted similar to the Unix chmod command.  That
#'         is, the integer's binary representation can be transformed into a
#'         length-three vector discussed above (e.g., 6 corresponds to c(1,1,0)).
#'         See examples.
#'   }
#'   Any valid value for \code{show} takes precedence over existing individual
#'   values for \code{showCDF}, \code{show<%=PXF%>}, and \code{showECDF}.
#'
#'   If \code{respectLayout} is \code{TRUE}, the function respects existing
#'   settings for device layout.  Note, however, that if the number of plots
#'   requested (either via \code{show} or via \code{showCDF}, \code{showPMF}, and
#'   \code{showECDF}) exceeds the number of plots available in the current layout
#'   (as determined by \code{prod(par("mfrow"))}), the function will display all
#'   requested plots but will also display a warning message indicating that the
#'   current layout does not permit simultaneous viewing of all requested plots.
#'   The most recent plot with this attribute can be further annotated after the call.
#'
#'   If \code{respectLayout} is \code{FALSE}, any existing user settings for device
#'   layout are ignored.  That is, the function uses \code{par} to explicitly set
#'   \code{mfrow} sufficient to show all requested plots stacked vertically to
#'   align their horizontal axes, and then resets row, column, and margin settings
#'   to their prior state on exit.
#'
#'   The \code{minPlotQuantile} and \code{maxPlotQuantile} arguments are present in
#'   order to compress the plots horizontally.   The random variates generated are
#'   not impacted by these two arguments.  Vertical, dotted, black lines are
#'   plotted at the associated quantiles on the plots.
#'
#'   \code{plotDelay} can be used to slow down or halt the variate generation for
#'   classroom explanation.
#'
#'   In the plot associated with the <%=PXF%>, the maximum plotting height is
#'   associated with 125\% of the maximum height of <%=PXF%>. Any histogram cell
#'   that extends above this limit will have three dots appearing above it.
#'
#' @keywords hplot dynamic distribution
#'
#' @concept random variate generation
#'
#' @author
#'    Barry Lawson (\email{blawson@@bates.edu}), \cr
#'    Larry Leemis (\email{leemis@@math.wm.edu}), \cr
#'    Vadim Kudlay (\email{vkudlay@@nvidia.com})
#'
#' @seealso \code{\link[=runif]{stats::runif}}, 
#'          \code{\link[=vunif]{simEd::vunif}}
