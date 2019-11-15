#' 'qmix': A R Package for Estimating Finite Quantile Mixture Models
#'
#' @description This package provides easy-to-use functionalities to estimate finite quantile mixture models with either fixed- or random-quantile specification. The estimation is implemented using MCMC methods available in \code{rstan}.
#'
#' @docType package
#' @name qmix-package
#' @useDynLib qmix, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#' Lu, Xiao (2019). Beyond the Average: Conditional Hypothesis Testing with Quantile Mixture.
#'
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. https://mc-stan.org
#'
NULL
