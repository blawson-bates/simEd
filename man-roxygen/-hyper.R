#' @param m Number of positive-attribute elements in set
#' @param n Number of negative-attribute elements in set
#' @param k Number of samples taken
#'
#' @details
#'   The hypergeometric distribution is used for sampling without replacement.
#'   The density of this distribution with parameters \code{m}, \code{n} and
#'   \code{k} (named \eqn{Np}, \eqn{N-Np}, and \eqn{n}, respectively) is given by
#'         \deqn{p(x) = {m \choose x} {n \choose k-x} \bigg/ {m+n \choose k}}{%
#'         p(x) = choose(m, x) choose(n, k-x) / choose(m+n, k)}
#'   for \deqn{x = 0, \ldots, k}{x = 0, ..., k}.
#'
#' @seealso \code{\link[=rhyper]{stats::rhyper}}
