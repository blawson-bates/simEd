#' @param location Location parameter
#' @param scale Scale parameter (default 1)
#'
#' @details
#'  The logistic distribution with \code{location} \eqn{= \mu}{= \mu} and
#'  \code{scale} \eqn{= \sigma}{= \sigma} has distribution function
#'
#'  \deqn{F(x) = \frac{1}{1 + e^{-(x - \mu) / \sigma}}}{%
#'        F(x) = 1 / (1 + exp(-(x-\mu)/\sigma))}
#'
#'  and density
#'
#'  \deqn{f(x) = \frac{1}{\sigma} \frac{e^{(x-\mu)/\sigma}}
#'                      {(1 + e^{(x-\mu)/\sigma})^2}}{%
#'       f(x) = 1/\sigma exp((x-\mu)/\sigma) (1 + exp((x-\mu)/\sigma))^-2}
#'
#'  It is a long-tailed distribution with mean \eqn{\mu}{\mu} and
#'  variance \eqn{\pi^2 / 3 \sigma^2}{\pi^2 /3 \sigma^2}.
#'
#' @seealso \code{\link[=rlogis]{stats::rlogis}}
