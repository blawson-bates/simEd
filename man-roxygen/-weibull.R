#' @param shape Shape parameter
#' @param scale Scale parameter (default 1)
#'
#' @details
#'  The Weibull distribution with parameters \code{shape} = \eqn{a} and
#'  \code{scale} = \eqn{b} has density
#'
#'      \deqn{f(x) = \frac{a}{b} \left(\frac{x}{b}\right)^{a-1} e^{-(x/b)^a}}{%
#'            f(x) = (a/b) (x/b)^(a-1) exp(-(x/b)^a)}
#'
#'   for \eqn{x \ge 0}, \eqn{a > 0}, and \eqn{b > 0}.
#'
#' @seealso \code{\link[=rweibull]{stats::rweibull}}
