#' Plot qmix object
#'
#' General plot function for \code{qmix} objects, which dispatches the chosen
#' type of plotting to the corresponding function.
#'
#' @param x A \code{qmix} object to be plotted.
#' @param type Character string giving the type of plotting. The options are
#'   \code{"trace"} for trace plots, \code{"coef"} for coefficient plots. The default is "coef".
#' @param ... Additional arguments to be passed to subsequent plot functions.
#'
#' @export
#'
#'
#'
plot.qmix <- function(x, type = "coef", ...) {
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
  if (object$binarylogic == TRUE & object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(paste0(
        rep(paste0("C", 1:object$nmix, ": "), object$nmix),
        rep(object$xnames, each = object$nmix)
      ),
      paste0("C", 1:object$nmix, ": proportion"))
  } else if (object$binarylogic == FALSE &
             object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma")
      )
  } else if (object$binarylogic == TRUE &
             object$design == "random") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  } else {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  }
  rstan::traceplot(object$stanfit, ...)
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
  if (object$binarylogic == TRUE & object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(paste0(
        rep(paste0("C", 1:object$nmix, ": "), object$nmix),
        rep(object$xnames, each = object$nmix)
      ),
      paste0("C", 1:object$nmix, ": proportion"))
  } else if (object$binarylogic == FALSE &
             object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma")
      )
  } else if (object$binarylogic == TRUE &
             object$design == "random") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  } else {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$nmix),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  }
  rstan::plot(object$stanfit, ...)
}
