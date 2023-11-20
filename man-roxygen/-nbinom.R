#' @param size target for number of successful trials, or dispersion
#'        parameter (the shape parameter of the gamma mixing
#'        distribution).  Must be strictly positive, need not be
#'        integer.
#' @param prob Probability of success in each trial;  '0 < prob <= 1'
#' @param mu alternative parameterization via mean
#'
#' @details
#'  The negative binomial distribution with \code{size} \eqn{= n} and
#'  \code{prob} \eqn{= p} has density
#'
#'           \deqn{p(x) = \frac{\Gamma(x+n)}{\Gamma(n) \ x!} p^n (1-p)^x}{%
#'                 p(x) = Gamma(x+n)/(Gamma(n) x!) p^n (1-p)^x}
#'
#'  for \eqn{x = 0, 1, 2, \ldots, n > 0} and \eqn{0 < p \leq 1}. This represents the
#'  number of failures which occur in a sequence of Bernoulli trials before a
#'  target number of successes is reached.
#'
#'  The mean is \eqn{\mu}{μ}\eqn{ = n(1 - p)/p} and variance \eqn{n(1 - p)/p^2}
#'
#' @seealso \code{\link[=rnbinom]{stats::rnbinom}}
