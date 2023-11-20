#' @param df Degrees of freedom > 0
#' @param ncp Non-centrality parameter delta (default NULL)
#'
#' @details
#'  The t-distribution with \code{df} \eqn{= v} degrees of freedom has density
#'
#'  \deqn{f(x) = \frac{\Gamma((v+1)/2)}{\sqrt{v\pi} \
#'              \Gamma(v/2)} \ (1 + x^2/v)^{-(v+1)/2}}{%
#'        f(x) = Γ((v+1)/2) / (√(v π) Γ(v/2)) (1 + x^2/v)^-((v+1)/2)}
#'
#'  for all real \eqn{x}. It has mean 0 (for \eqn{v > 1}) and variance
#'  \eqn{v/(v-2)} (for \eqn{v > 2}).
#'
#'  The general non-central t with parameters \eqn{(\nu, \delta)}{(v, δ)} =
#'  \code{(df, ncp)} is defined as the distribution of
#'    \eqn{T_{\nu}(\delta) := (U + \delta) \ / \ \sqrt{(V/\nu)}}{%
#'          T(df, Del) := (U + Del) / √(V/df)}
#'  where \eqn{U} and \eqn{V} are
#'  independent random variables, \eqn{U \sim \mathcal{N}(0,1)}{U ~ N(0,1)}
#'  and \eqn{V \sim \chi^2(\nu)}{V ~ χ^2(df)}.
#'
#' @seealso \code{\link[=rt]{stats::rt}}
