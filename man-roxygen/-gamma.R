#' @param shape Shape of Distrobution
#' @param scale Scale of Distrobution
#' @param rate Alternate parameterization for scale
#'
#' @details
#'     The gamma distribution with parameters \code{shape} = \eqn{a} and
#'     \code{scale} = \eqn{s} has density
#'
#'          \deqn{f(x) = \frac{1}{s^a\, \Gamma(a)} x^{a-1} e^{-x/s}}{%
#'                f(x) = 1/(s^a Gamma(a)) x^(a-1) e^(-x/s)}
#'
#'     for \eqn{x \ge 0}, \eqn{a > 0}, and \eqn{s > 0}.
#'     (Here \eqn{\Gamma(a)}{Gamma(a)} is the function implemented by
#'     R's \code{\link[base:Special]{gamma}()} and defined in its help.)
#'
#'     The population mean and variance are \eqn{E(X) = as}
#'     and \eqn{Var(X) = as^2}.
#'
#' @seealso \code{\link[=rgamma]{stats::rgamma}}

