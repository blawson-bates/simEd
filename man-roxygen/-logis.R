#' @param location Location parameter
#' @param scale Scale parameter (default 1)
#'
#' @details
#'  The logistic distribution with \code{location} \eqn{= \mu}{= μ} and
#'  \code{scale} \eqn{= \sigma}{= σ} has distribution function
#'
#'  \deqn{F(x) = \frac{1}{1 + e^{-(x - \mu) / \sigma}}}{%
#'        F(x) = 1 / (1 + exp(-(x-μ)/σ))}
#'
#'  and density
#'
#'  \deqn{f(x) = \frac{1}{\sigma} \frac{e^{(x-\mu)/\sigma}}
#'                      {(1 + e^{(x-\mu)/\sigma})^2}}{%
#'       f(x) = 1/σ exp((x-μ)/σ) (1 + exp((x-μ)/σ))^-2}
#'
#'  It is a long-tailed distribution with mean \eqn{\mu}{μ} and
#'  variance \eqn{\pi^2 / 3 \sigma^2}{π^2 /3 σ^2}.
#'
#' @seealso \code{\link[=rlogis]{stats::rlogis}}
