#' @param location Location parameter (default 0)
#' @param scale Scale parameter (default 1)
#'
#' @details
#'     The Cauchy distribution has density
#'     \deqn{f(x) = \frac{1}{\pi s} \ \left(1 + \left( \frac{x - l}{s} \right)^2
#'               \right)^{-1}}{%
#'           f(x) = f(x) = 1 / (π s (1 + ((x-l)/s)^2))}
#'   for all \eqn{x}.
#'
#'   The mean is \eqn{a/(a+b)} and the variance is \eqn{ab/((a+b)^2 (a+b+1))}.
#'
#' @seealso \code{\link[=rcauchy]{stats::rcauchy}}
