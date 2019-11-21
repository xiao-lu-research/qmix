#' Extract qmix Coefficients
#'
#' Create a table of coefficient results from a \code{qmix} object.
#'
#' @param object A \code{qmix} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A table of coefficients with their corresponding lower and upper bounds.
#' @export
#'
#' @method coef qmix
#'
#'
coef.qmix <- coefficients.qmix <- function(object, ...) {
  coefmat <-
    cbind(matrix(object$means[1:(object$npars * object$nmix)], nrow = object$npars * object$nmix),
          t(object$ulbs[, 1:(object$npars * object$nmix)]))
  row.names(coefmat) <-
    paste0(rep(paste0("C", 1:object$nmix, ": "), object$nmix),
           rep(object$xnames, each = object$nmix))
  coef_theta <-
    cbind(matrix(object$means[(object$npars * object$nmix + 1):(object$npars * object$nmix + object$nmix)], nrow = object$nmix), t(object$ulbs[, (object$npars * object$nmix + 1):(object$npars * object$nmix + object$nmix)]))
  row.names(coef_theta) <-
    paste0("C", 1:object$nmix, ": proportion")
  coefmat <- rbind(coefmat, coef_theta)
  if (object$binarylogic == FALSE) {
    coef_sigma <-
      cbind(matrix(object$means[(object$npars * object$nmix + object$nmix + 1):(object$npars * object$nmix + 2 *
                                                                                  object$nmix)], nrow = object$nmix), t(object$ulbs[, (object$npars * object$nmix + object$nmix + 1):(object$npars * object$nmix + 2 *
                                                                                                                                                                                        object$nmix)]))
    row.names(coef_sigma) <- paste0("C", 1:object$nmix, ": sigma")
    coefmat <- rbind(coefmat, coef_sigma)
  }

  if (object$binarylogic == FALSE & object$design == "random") {
    coef_p <-
      cbind(matrix(object$means[(object$npars * object$nmix + 2 * object$nmix + 1):(object$npars * object$nmix + 3 *
                                                                                      object$nmix)], nrow = object$nmix), t(object$ulbs[, (object$npars * object$nmix + 2 *
                                                                                                                                             object$nmix + 1):(object$npars * object$nmix + 3 * object$nmix)]))
    row.names(coef_p) <- paste0("C", 1:object$nmix, ": quantile")
    coefmat <- rbind(coefmat, coef_p)
  }

  if (object$binarylogic == TRUE & object$design == "random") {
    coef_p <-
      cbind(matrix(object$means[(object$npars * object$nmix + object$nmix + 1):(object$npars * object$nmix + 2 *
                                                                                  object$nmix)], nrow = object$nmix), t(object$ulbs[, (object$npars * object$nmix + object$nmix + 1):(object$npars * object$nmix + 2 *
                                                                                                                                                                                        object$nmix)]))
    row.names(coef_p) <- paste0("C", 1:object$nmix, ": quantile")
    coefmat <- rbind(coefmat, coef_p)
  }

  colnames(coefmat) <- c("Estimate", "LB", "UB")
  return(coefmat)
}


#' Print qmix object
#'
#' General print function for \code{qmix} objects, which dispatches the chosen type
#' of printing to the corresponding function.
#'
#' @param x A \code{qmix} object to be printed.
#' @param type Character string giving the type of printing, such as
#'   \code{"text"}, \code{"mcmc"}, \code{"coef"}.
#' @param ... Additional arguments to be passed to print functions.
#'
#' @export
#'
#'
#'
print.qmix <- function(x, type = "text", ...) {
  printFunName <- paste0("print_", type, ".qmix")
  do.call(printFunName, args = c(list(object = x), list(...)))
}


#' Print the main results from a \code{qmix} object.
#'
#' @param object A \code{qmix} object.
#' @param digits Number of digits to display.
#'
#' @export
#'
#'
#'
print_text.qmix <- function(object, digits = 3) {
  cat("Finite Quantile Mixture with",
      object$design,
      "quantile specification \n")
  cat("\nCall:\n",
      paste(deparse(object$Call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  cat(
    "MCMC run for",
    object$nsim,
    "iterations, with",
    object$stanfit@sim$warmup2,
    "used. \n\n"
  )
  cat("Coefficients:\n")
  print(round(coef(object), digits))
  cat("\n")
  names(object$thetas) = paste0("C", 1:object$nmix)
  cat("Estimated proportions of each mixture component: ",
      object$thetas)
  cat("\n")
}


#' Print the mcmc results from a qmix object
#'
#' This prints a number of diagnostics about the results of a \code{qmix} objects
#'
#'
#' @param object A \code{qmix} object.
#' @param ... Additional arguments to be passed.
#'
#' @export
#'
#'
#'
print_mcmc.qmix <- function(object, ...) {
  print(object$stanfit, ...)
}



#' Print qmix coefficients
#'
#' @param object A \code{qmix} object.
#' @param digits Number of digits to display.
#'
#' @export
#'
#'
#'
print_coef.qmix <- function(object, digits = 3) {
  print(round(coef(object), digits))
}
