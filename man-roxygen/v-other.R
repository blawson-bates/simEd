#' @param stream if \code{NULL} (default), uses \code{\link[=runif]{stats::runif}}
#'     to generate uniform variates to invert via 
#'     \code{\link[=<%= quant %>]{stats::<%= quant %>}};
#'     otherwise, an integer in 1:25 indicates the \code{\link{rstream}} stream
#'     from which to generate uniform variates to invert via
#'     \code{\link[=<%= quant %>]{stats::<%= quant %>}};
