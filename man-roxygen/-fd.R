#' @param df1 Degrees of freedom > 0
#' @param df2 Degrees of freedom > 0
#' @param ncp Non-centrality parameter >= 0
#'
#' @details
#'    The F distribution with \code{df1} \eqn{= n_1}{= n1} and \code{df2}
#'    \eqn{= n_2}{= n2} degrees of freedom has density
#'
#'    \deqn{f(x) = \frac {\Gamma(n_1/2 + n_2/2)} {\Gamma(n_1/2) \ \Gamma(n_2/2)}
#'                 \left( \frac{n_1}{n_2} \right)^{n_1/2} x^{n_1/2 - 1}
#'                 \left( 1 + \frac{n_1x}{n_2} \right) ^ {-(n_1 + n_2)/2}}{%
#'          f(x) = Gamma((n1 + n2)/2) / (Gamma(n1/2) Gamma(n2/2)) (n1/n2)^(n1/2)
#'                 x^(n1/2 - 1) (1 + (n1/n2) x)^-(n1 + n2)/2}
#'
#'    for \eqn{x > 0}.
#'
#' @seealso \code{\link[=rf]{stats::rf}}
