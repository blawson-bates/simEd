#' @param meanlog Mean of distribution on log scale (default 0)
#' @param sdlog Standard deviation of distribution on log scale (default 1)
#'
#' @details
#'  The log-normal distribution has density
#'
#'     \deqn{f(x) = \frac{1}{\sqrt{2 \pi} \sigma x}
#'                  e^{-(\log{x} - \mu)^2 / (2 \sigma^2)} }{%
#'           f(x) = 1/(\sqrt(2 \pi) \sigma x) e^-((log x - \mu)^2 / (2 \sigma^2))}
#'
#'  where \eqn{\mu}{\mu} and \eqn{\sigma}{\sigma} are the mean and standard deviation
#'  of the logarithm.
#'
#'  The mean is
#'    \eqn{E(X) = \exp(\mu + 1/2 \sigma^2)}{E(X) = exp(\mu + 1/2 \sigma^2)},
#'  the median is
#'    \eqn{med(X) = \exp(\mu)}{med(X) = exp(\mu)},
#'  and the variance is
#'    \eqn{Var(X) = \exp(2\times \mu +\sigma^2)\times (\exp(\sigma^2)-1)}{%
#'    Var(X) = exp(2*\mu + \sigma^2)*(exp(\sigma^2) - 1)}
#'  and hence the coefficient of variation is
#'    \eqn{sqrt(\exp(\sigma^2)-1)}{sqrt(exp(\sigma^2) - 1)}
#'  which is approximately \eqn{\sigma}{\sigma} when small
#'  (e.g., \eqn{\sigma < 1/2}{\sigma < 1/2}).
#'
#' @seealso \code{\link[=rlnorm]{stats::rlnorm}}
