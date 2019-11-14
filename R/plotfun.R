#' Plot qmix object
#'
#' General plot function for \code{qmix} objects, which dispatches the chosen
#' type of plotting to the corresponding function.
#'
#' @param x A \code{qmix} object to be plotted.
#' @param type Character string giving the type of plotting. The options are
#'   \code{"trace"} for trace plots, \code{"coef"} for coefficient plots.
#' @param ... Additional arguments to be passed to subsequent plot functions.
#'
#' @export
#'
#'
#'
plot.qmix <- function(x, type = "trace", ...) {
  printFunName <- paste0("plot_", type, ".qmix")
  do.call(printFunName, args = c(list(object = x), list(...)))
}



#' Make traceplots for qmix
#'
#' Plot traceplots from a \code{qmix} object.
#'
#' @param object A \code{qmix} object.
#' @param ... Additional parameters to be passed.
#'
#' @export
#'
#'
plot_trace.qmix <- function(object, ...) {
  rstan::traceplot(object$stanfit,...)
}

#' Make coefficient plots for qmix
#'
#' Plot traceplots from a \code{qmix} object.
#'
#' @param object A \code{qmix} object.
#' @param ... Additional parameters to be passed.
#'
#' @export
#'
#'
plot_coef.qmix <- function(object, ...) {
  rstan::plot(object$stanfit,...)
}
