# qmix: An R Package for Finite Quantile Mixture Models


The package `qmix` provides functions to estimate finite quantile mixture models using MCMC methods. Both fixed- and random quantile specifications are allowed as pre-specified inputs to the estiamtion fucntion.  

Caution: The package is still under initial development and there is no guarantee about the functionality of the package.

## Installation

```r
# Make sure that the following packages have been installed in your local R environment
if(!require(rstan)) install.packages("rstan")

# Install cirque from github
if(!require(devtools)) install.packages("devtools")
devtools::install_github("xiao-lu-research/qmix")
```


## Usage

```r

# Load the package
library(qmix)

# Get help
?qmix

# Generate a random dataset for circular estimation

# Estimate the model


# Access model outputs
# View the estimated coefficients
print(model)
coef(mod)

# MCMC information
print(model, type = "mcmc")
# trace plot and density plot
plot(mod)

```

## References

Lu, Xiao (2019). Beyond the Average: Conditional Hypothesis Testing Using Quantile Mixture. Working Paper. 
