#' Plot qmix object
#'
#' General plot function for \code{qmix} objects, which dispatches the chosen
#' type of plotting to the corresponding function.
#'
#' @param x A \code{qmix} object to be plotted.
#' @param type Character string giving the type of plotting. The options are
#'   \code{"trace"} for trace plots, \code{"coef"} for coefficient plots, \code{"effect"} for plots of substantive effects. The default is "coef".
#' @param ... Additional arguments to be passed to subsequent plot functions (check the See Also section).
#'
#' @import ggplot2
#'
#' @return None.
#'
#' @seealso \code{\link{plot_trace.qmix}} and \code{\link{plot_coef.qmix}}.
#' @export
#'
#'
#'
plot.qmix <- function(x, type = "coef", ...) {
  printFunName <- paste0("plot_", type, ".qmix")
  do.call(printFunName, args = c(list(object = x), list(...)))
}

#' Plots of substantive effects for qmix
#'
#' \code{plot_effect.qmix} is used to produce plots of substantive effects from a \code{qmix} object from the main function \code{\link{qmix}}.
#'
#' @param object A \code{qmix} object from running the main function \code{\link{qmix}}.
#' @param var Variable whose effect is to be ploted.
#' @param new_data New simulated data to be ploted.
#' @param sim_var A numeric vector of the simulated variable.
#' @param xlab Label of x-axis.
#' @param ylab Label of y-axis.
#' @param ... Additional parameters to be passed to \code{\link[rstan]{traceplot}}.
#'
#' @return None.
#' @export
#'
#'
plot_effect.qmix <- function(object, var = NULL, new_data = NULL, sim_var = NULL,
                             xlab = "Variable of interest", ylab = "Substantive effects on outcomes", ...) {
  if (is.null(var)){
    stop("No variable name supplied!")
  }
  if (!is.null(new_data) & is.null(sim_var)){
    stop("The range of the simulated variable must be specified in sim_var!")
  }
  nvar <- object$npars
  nmix <- object$nmix
  nobs <- dim(object$x)[1]
  beta <- list()
  for (i in 1:nmix){
    beta[[i]] <- t(as.matrix(object$sampledf[,seq(i , nvar * nmix, by = nmix)]))
  }
  thetas <- round(object$summaryout[(nvar * nmix + 1) : (nvar * nmix + nmix),1],2)
  # sigmas <- object$sampledf[ , (nvar * nmix + nmix + 1) : (nvar * nmix + 2 * nmix)]
  if (object$design == "fixed") {
    taus <- object$q
  } else {
    taus <- round(object$summaryout[(nvar * nmix + 2 * nmix + 1) : (nvar * nmix + 3 * nmix),1],2)
  }
  xb <- list()
  if (!is.null(new_data)){
    for (i in 1:nmix){
        xb[[i]] <- new_data %*% (beta[[i]])
    }
  } else {
    new_data <- matrix(0, nrow = dim(object$x)[1], ncol = dim(object$x)[2])
    varid <- which(object$xnames %in% var)
    if (length(varid) == 0){
      stop("No variable found!")
    }
    sim_var <- seq(min(object$x[,varid]), max(object$x[,varid]), length.out = nobs)
    new_data[,varid] <- sim_var
    for (i in 1:nmix){
      xb[[i]] <- new_data %*% (beta[[i]])
    }
  }

  quantile_apply <- function(x,probs){
    return(apply(x,1,quantile, probs = probs))
  }

  mean_apply <- function(x){
    return(apply(x,1,mean))
  }

  probs <- c((1-object$CIsize)/2, 1 - (1-object$CIsize)/2)

  xb_me <- lapply(xb, mean_apply)
  xb_lu <- lapply(xb, quantile_apply, probs)

  dfplot <- as.data.frame(matrix(NA,nrow = dim(new_data)[1] * nmix, ncol = 6))
  names(dfplot) <- c("xs",
                     "mean",
                     "lower",
                     "upper",
                     "quantile",
                     "prop")

  quantilename <- paste("Quantile:",taus,sep = "")
  propname <- paste("Proportion:", thetas * 100, "%", sep = "")

  dfplot[,1] <- rep(sim_var, nmix)
  nnobs <- dim(new_data)[1]
  for (i in 1:nmix){
      dfplot[((i-1)*nnobs + 1) : (i*nnobs),2] <- xb_me[[i]]
      dfplot[((i-1)*nnobs + 1) : (i*nnobs),3] <- xb_lu[[i]][1,]
      dfplot[((i-1)*nnobs + 1) : (i*nnobs),4] <- xb_lu[[i]][2,]
  }
  dfplot$quantile = rep(quantilename, each = nnobs)
  dfplot$prop = rep(propname, each = nnobs)

  dfplot$quantile = factor(dfplot$quantile, levels = quantilename)
  dfplot$prop = factor(dfplot$prop, levels = propname)

  xs <- lower <- upper <- NULL
  ggplot2::ggplot(dfplot, aes(x = xs, y = mean)) +
    geom_line(color = "gray10")+
    geom_ribbon(aes(min = lower, max = upper),color = "gray",alpha = 0.5)+
    xlab(xlab)+
    ylab(ylab)+
    theme_bw()+
    facet_wrap(prop ~ quantile, ncol = nmix, scales = "free_y")

}
#' Trace plots for qmix
#'
#' \code{plot_trace.qmix} is used to produce trace plots from a \code{qmix} object from the main function \code{\link{qmix}}.
#'
#' @param object A \code{qmix} object from running the main function \code{\link{qmix}}.
#' @param ... Additional parameters to be passed to \code{\link[rstan]{traceplot}}.
#'
#' @return None.
#' @export
#'
#'
plot_trace.qmix <- function(object, ...) {
  if (object$binarylogic == TRUE & object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(paste0(
        rep(paste0("C", 1:object$nmix, ": "), object$npars),
        rep(object$xnames, each = object$nmix)
      ),
      paste0("C", 1:object$nmix, ": proportion"))
  } else if (object$binarylogic == FALSE &
             object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
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
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  } else {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  }
  rstan::traceplot(object$stanfit, ...)
}

#' Make coefficient plots for a \code{qmix} object
#'
#' \code{plot_coef.qmix} is used to produce coefficient plots from a \code{qmix} object.
#'
#' @param object A \code{qmix} object from running the main function \code{\link{qmix}}.
#' @param ... Additional parameters to be passed to \code{\link[rstan]{stan_plot}}.
#'
#' @return None.
#' @export
#'
#'
plot_coef.qmix <- function(object, ...) {
  if (object$binarylogic == TRUE & object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(paste0(
        rep(paste0("C", 1:object$nmix, ": "), object$npars),
        rep(object$xnames, each = object$nmix)
      ),
      paste0("C", 1:object$nmix, ": proportion"))
  } else if (object$binarylogic == FALSE &
             object$design == "fixed") {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
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
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  } else {
    names(object$stanfit)[-length(names(object$stanfit))] <-
      c(
        paste0(
          rep(paste0("C", 1:object$nmix, ": "), object$npars),
          rep(object$xnames, each = object$nmix)
        ),
        paste0("C", 1:object$nmix, ": proportion"),
        paste0("C", 1:object$nmix, ": sigma"),
        paste0("C", 1:object$nmix, ": quantile")
      )
  }
  rstan::plot(object$stanfit, ...)
}
