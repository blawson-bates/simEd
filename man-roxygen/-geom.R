#' @param prob Probability of success in each trial (0 \eqn{<} \code{prob} \eqn{\le} 1)
#'
#' @details
#'  The geometric distribution with parameter \code{prob} = \eqn{p} has density
#'      \deqn{p(x) = p (1-p)^x}
#'  for \eqn{x = 0, 1, 2, \ldots}, where \eqn{0 < p \le 1}.
#'
#' @seealso  \code{\link[=rgeom]{stats::rgeom}}
