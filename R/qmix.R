#' Check if a predictor is dichotomous, adopted from package \code{circGLM}
#'
#' @param x A character or numerical vector to be tested.
#'
#' @return A logical, \code{TRUE} if the \code{x} has dummy coding (0, 1),
#'   \code{FALSE} otherwise.
#'
is.dichotomous <- function(x) {
  n_unique_x <- length(unique(x))
  if (n_unique_x == 2) {
    if (all(x == 0 | x == 1)) {
      return(TRUE)
    } else {
      warning("A predictor might be dichotomous but not 0|1.")
    }
  } else if (n_unique_x > 2 & n_unique_x < 8) {
    warning(
      paste(
        "A predictor has between 3 and 7 unique values.",
        "It might be categorical with multiple categories",
        "but without dummy coding."
      )
    )
  } else if (n_unique_x == 1) {
    stop("A predictor had only a single unique value.")
  }
  FALSE
}

#' Fitting conditional binary quantile models
#'
#' The main function for running the finite quantile mixture model. The function returns a \code{qmix} object that can be further investigated using standard functions such as \code{plot}, \code{print}, and \code{coef}. The model can be passed using a \code{formula} as in \code{lm()}. Convergence diagnotics can be performed using either \code{print(object, "mcmc")} or \code{plot(object, "mcmc")}.
#'
#' @param formula An object of class "formula" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param nmix The number of mixture components.
#' @param design Quantile specification. Options include "fixed" and "random". The default choice is "fixed" which requires quantile inputs from the user.
#' @param q The quantile value.
#' @param nsim The number of iterations.
#' @param burnin The number of burnin iterations.
#' @param thin Thinning parameter.
#' @param CIsize The size of posterior confidence interval.
#' @param nchain The number of parallel chains.
#' @param seeds Random seeds to replicate the results.
#' @param offset Offset values to enhance sampling stability. The default value is 1e-20.
#' @param inverse_distr If FALSE, the ALD will not be reversed. The default is FALSE.
#'
#' @return A \code{qmix} object. An object of class \code{qmix} contains the following elements
#'
#'   \describe{
#'
#'   \item{\code{Call}}{The matched call.}
#'   \item{\code{formula}}{Symbolic representation of the model.}
#'
#' }
#'
#' @importFrom stats coef model.frame model.matrix quantile
#'
#' @export
#'
#'
#'
#'
qmix <- function(formula,
                 data,
                 nmix = 3,
                 design = "fixed",
                 q = NULL,
                 # quantile
                 nsim = 1000,
                 burnin = NULL,
                 thin = 1,
                 CIsize = .95,
                 nchain = 1,
                 seeds = 12345,
                 offset = 1e-20,
                 inverse_distr = FALSE) {
  if (is.null(burnin))
    burnin = floor(nsim / 2)
  if (burnin < 0)
    stop("Burn-in must be non-negative.")
  if (thin < 1)
    stop("Thinning factor must be positive.")
  if (CIsize <= 0)
    stop("Confidence interval size 'CIsize' must be positive.")
  if (CIsize > 1)
    stop(paste0("Confidence interval size 'CIsize' ",
                "can not be larger than 1."))
  if (missing(formula) | missing(data)) {
    stop(paste0("Formula and data should be given."))
  }
  if (missing(nmix) &
      design == "random")
    stop(
      "The number of mixture components nmix needs to be specified in the random-quantile design."
    )
  if (nmix != length(q))
    stop("The number of mixture components in q does not match the number of specified quantiles nmix.")

  # if (design == "fixed" & is.null(q)) {
  #   q = seq(0.1,0.9,length.out = nmix)
  #   warning(paste0("Quantiles are needed in the fixed-quantile specification. Quantiles are set to the default: ",q))
  # } else if (design == "fixed") {
  #   nmix = length(q)
  # } else {
  #   nmix = nmix
  # }
  if (nmix < 2)
    stop("The number of mixture components must be larger than one.")
  if (nmix > 3) {
    warning("The number of specified quantiles nmix is larger than 3. Check convergence carefully using posterior samples from multiple chains!")
  }
  if (!all(q > 0 &
           q < 1))
    stop("The specified quantiles are out of range. The values must be in (0,1).")

  f = Formula::Formula(formula)
  data = model.frame(f, data)
  y = c(as.matrix(model.frame(f, data)[, 1]))

  x = model.matrix(f, data)

  n_covariate = dim(x)[2]
  N <- length(y)

  if (length(unique(y)) == 2 &
      !is.dichotomous(y))
    stop("The binary dependent variable must be coded with values in {0,1}.")
  binarylogic = is.dichotomous(y)
  if (binarylogic == FALSE) {
    if (design == "fixed") {
      stanmodel = stanmodels$qmixcf
      datlist = list(
        N = N,
        Y = c(y),
        D = n_covariate,
        X = x,
        k = nmix,
        p = q
      )
    } else {
      stanmodel = stanmodels$qmixcr
      datlist = list(
        N = N,
        Y = c(y),
        D = n_covariate,
        X = x,
        k = nmix
      )
    }
  } else {
    if (design == "fixed") {
      if (inverse_distr == FALSE) {
        stanmodel = stanmodels$qmixbfv
        datlist = list(
          N = N,
          Y = c(y),
          D = n_covariate,
          X = x,
          k = nmix,
          p = q,
          offset = offset
        )
      } else {
        stanmodel = stanmodels$qmixbf
        datlist = list(
          N = N,
          Y = c(y),
          D = n_covariate,
          X = x,
          k = nmix,
          p = q,
          offset = offset
        )
      }
    } else {
      if (inverse_distr == FALSE) {
        stanmodel = stanmodels$qmixbrv
        datlist = list(
          N = N,
          Y = c(y),
          D = n_covariate,
          X = x,
          k = nmix,
          offset = offset
        )
      } else {
        stanmodel = stanmodels$qmixbr
        datlist = list(
          N = N,
          Y = c(y),
          D = n_covariate,
          X = x,
          k = nmix,
          offset = offset
        )
      }
    }
  }

  if (binarylogic == FALSE) {
    pars = c("beta", "theta")
  } else {
    pars = c("beta", "theta", "sigma")
  }

  stanout = sampling(
    stanmodel,
    data = datlist,
    pars = pars,
    seed = seeds,
    iter = nsim,
    thin = thin,
    warmup = burnin,
    chains = nchain
  )

  summaryout = rstan::summary(stanout)$summary
  sampledf = as.data.frame(stanout)[, 1:n_covariate]

  out = list()
  class(out) <- c("qmix", class(out))
  out$Call <- match.call()
  out$formula <- formula
  out$nmix  <- nmix
  out$design <- design
  out$q <- q
  out$nsim <- nsim
  out$burnin <- burnin
  out$thin <- thin
  out$seeds <- seeds
  out$CIsize  <- CIsize
  out$inverse_distr <- inverse_distr
  out$offset <- offset
  out$data   <- data
  out$x    <- x
  out$y <- y
  out$xnames = colnames(x)
  out$stanfit = stanout
  out$sampledf = sampledf
  out$summaryout = summaryout
  out$npars = n_covariate
  out$ulbs =  apply(sampledf, 2, quantile, probs = c((1 - CIsize) / 2, 1 - (1 - CIsize) / 2))
  out$means = summaryout[1:n_covariate, 1]

  return(out)


}
