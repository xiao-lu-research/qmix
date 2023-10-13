#' Probability density function of asymmetric Laplace distributions
#'
#' \code{dald} calculates probability densities of asymmetric Laplace distributions.
#'
#' @param x Random variable.
#' @param mu Position parameter.
#' @param p Quantile.
#' @param sigma Scale parameter.
#'
#' @return probability density of \code{x}.
#' @export
#'
dald <- function(x,mu,p,sigma){
  out <- ifelse(x<=mu,p*(1-p)/sigma*exp(-(x-mu)/sigma*(p-1)),p*(1-p)/sigma*exp(-(x-mu)/sigma*p) )
  return(out)
}

#' Cumulative density function of asymmetric Laplace distributions
#'
#' \code{pald} calculates cumulative densities of asymmetric Laplace distributions.
#'
#' @param x Random variable.
#' @param mu Position parameter.
#' @param p Quantile.
#' @param sigma Scale parameter.
#'
#' @return cumulative probability density of \code{x}.
#' @export
#'
pald <- function(x,mu,p,sigma){
  out <- ifelse(x<=mu,p*exp((x-mu)*(1-p)/sigma), 1-(1-p)*exp(-(x-mu)*(p)/sigma))
  return(out)
}

#' Inverse function
#'
#' \code{inverse} generates inverse function of any given function.
#'
#' @param f pald function
#' @param mu Position parameter.
#' @param p Quantile.
#' @param sigma Scale parameter.
#' @param lower Lower bound.
#' @param upper Upper bound.
#'
#'
#' @return inversed pald
#'
#' @importFrom stats runif uniroot
#'
inverse <- function (f, mu, p, sigma, lower = -10000, upper = 10000) {
  function (y,mu,p,sigma) uniroot((function (x) f(x, mu,p,sigma) - y), lower = lower, upper = upper)[1]
}

#' Quantile function of asymmetric Laplace distributions
#'
#' \code{qald} calculates quantiles values of asymmetric Laplace distributions.
#'
#' @param y quantile value.
#' @param mu Position parameter.
#' @param p Quantile.
#' @param sigma Scale parameter.
#'
#' @return quantile value.
#' @export
#'
qald <- inverse(pald)

#' Random number generator of asymmetric Laplace distributions
#'
#' \code{rald} generates random numbers from asymmetric Laplace distributions.
#'
#' @param n Number of random numbers to be generated.
#' @param mu Position parameter.
#' @param p Quantile.
#' @param sigma Scale parameter.
#'
#' @return random numbers.
#' @export
#'
rald <- function(n,mu,p,sigma){
  tmp <- runif(n)
  out <-  NA
  for (i in 1:n){
    out[i] <- qald(tmp[i],mu,p,sigma)
  }
  return(unlist(out))
}


#' Generate synthetic data using finite mixture of quantiles
#'
#' \code{gendata_qmix} generates synthetic data using finite mixture of quantiles based on asymmetric Laplace distributions.
#'
#' @param k Number of mixture components. The default is 2.
#' @param n Number of observations for each component. The default is 50.
#' @param beta Coefficients.
#' @param sigma Scale parameters. The default is 1.
#' @param quantile Quantiles for the respective mixture components. The default values are 0.1 and 0.9 for the two-component mixture.
#' @param seed Seed for the random number generator.
#'
#' @return A data frame with outcomes Y and a covariate X.
#' @importFrom stats rnorm
#' @export
#'
gendata_qmix <- function(k = 2, n = 50, beta = c(-10, 10), sigma = c(1,1), quantile = c(0.1, 0.9), seed = NULL){

  if (length(beta)!=k){
    stop("The number of coefficients beta must be equal to the number of mixture components!")
  }

  if (length(sigma)!=k){
    stop("The number of scale parameter sigma must be equal to the number of mixture components!")
  }

  if (length(quantile)!=k){
    stop("The number of quantiles must be equal to the number of mixture components!")
  }

  if (!is.null(seed)){
    set.seed(seed)
  }
  X <- stats::rnorm(k*n, 0, 1)
  Y <- rep(NA, k*n)
  for (i in 1:k){
    tmp_xb <- rep(NA, n)
    tmp_xb <- X[((i-1)*n+1):(i*n)] * beta[i]
    tmp_y <- rep(NA, n)
    for (j in 1:n){
      tmp_y[j] <- rald(1, mu = tmp_xb[j], p = quantile[i], sigma = sigma[i])
    }
    Y[((i-1)*n+1):(i*n)] <- tmp_y
  }
  return(as.data.frame(cbind(Y,X)))
}
