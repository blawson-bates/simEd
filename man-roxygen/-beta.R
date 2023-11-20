#' @param shape1 Shape parameter 1 (alpha)
#' @param shape2 Shape parameter 2 (beta)
#' @param ncp Non-centrality parameter (default 0)
#'
#' @details
#'    The beta distribution has density
#'
#'     \deqn{f(x) = \frac{\Gamma(a+b)}{\Gamma(a) \ \Gamma(b)} x^{a-1}(1-x)^{b-1}}{%
#'           f(x) = Gamma(a+b)/(Gamma(a)Gamma(b)) x^(a-1)(1-x)^(b-1)}
#'
#'    for \eqn{a > 0}, \eqn{b > 0} and \eqn{0 \leq x \leq 1}{0 <= x <= 1} where the
#'    boundary values at \eqn{x=0} or \eqn{x=1} are defined as by continuity (as limits).
#'
#'    The mean is \eqn{\frac{a}{a+b}}{a/(a+b)} and the variance is
#'    \eqn{{ab}{(a+b)^2 (a+b+1)}}{ab/((a+b)^2 (a+b+1))}
#'
#' @seealso \code{\link[=rbeta]{stats::rbeta}}
