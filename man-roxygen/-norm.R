#' @param mean Mean of distribution (default 0)
#' @param sd Standard deviation of distribution (default 1)
#'
#' @details
#'  The normal distribution has density
#'
#'      \deqn{f(x) = \frac{1}{\sqrt{2\pi}\sigma} e^{-(x - \mu)^2/(2 \sigma^2)}}{%
#'            f(x) = 1/(\sqrt(2\pi)\sigma) e^(-(x - \mu)^2/(2 \sigma^2))}
#'
#'  for \eqn{-\infty < x < \infty} and \eqn{\sigma > 0}, where \eqn{\mu} is the
#'  mean of the distribution and \eqn{\sigma} the standard deviation.
#'
#' @seealso \code{\link[=rnorm]{stats::rnorm}}
