#' @param df Degrees of freedom (non-negative, but can be non-integer)
#' @param ncp Non-centrality parameter (non-negative)
#'
#' @details
#' The chi-squared distribution with \code{df} = \eqn{n \geq 0}{n >= 0} degrees
#' of freedom has density
#'
#'     \deqn{f_n(x) = \frac{1}{2^{n/2} \ \Gamma(n/2)} x^{n/2-1} e^{-x/2}}{%
#'           f_n(x) = 1 / (2^(n/2) Γ(n/2)) x^(n/2-1) e^(-x/2)}
#'
#' for \eqn{x > 0}. The mean and variance are \eqn{n} and \eqn{2n}.
#'
#'
#' The non-central chi-squared distribution with \code{df} = n degrees of
#' freedom and non-centrality parameter \code{ncp} \eqn{= \lambda}{= λ} has density
#'
#'     \deqn{f(x) = e^{-\lambda/2} \sum_{r=0}^\infty \frac{(\lambda/2)^r}{r!} f_{n + 2r}(x)}{%
#'           f(x) = exp(-λ/2) SUM_{r=0}^∞ ((λ/2)^r / r!) dchisq(x, df + 2r)}
#'
#' for \eqn{x \geq 0}{x >= 0}.
#'
#' @seealso \code{\link[=rchisq]{stats::rchisq}}
