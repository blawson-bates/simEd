#' @param lambda Rate of change of distribution
#'
#' @details
#'  The Poisson distribution has density
#'      \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}}{%
#'            p(x) = λ^x exp(-λ)/x!}
#'  for \eqn{x = 0, 1, 2, \ldots}.
#'  The mean and variance are \eqn{E(X) = Var(X) = \lambda}{E(X) = Var(X) = λ}
#'
#' @seealso \code{\link[=rpois]{stats::rpois}}
