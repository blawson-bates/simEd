#' @param size number of trials (zero or more)
#' @param prob probability of success on each trial (0 \eqn{<} \code{prob} \eqn{\le} 1)
#'
#' @details
#'   The binomial distribution with parameters \code{size} = \eqn{n} and
#'   \code{prob} = \eqn{p} has pmf
#'            \deqn{p(x) = {n \choose x} p^x (1-p)^{(n-x)}}{%
#'                  p(x) = choose(n, x) p^x (1-p)^(n-x)}
#'   for \eqn{x = 0, \ldots, n}.
#'
#' @seealso \code{\link[=rbinom]{stats::rbinom}}
