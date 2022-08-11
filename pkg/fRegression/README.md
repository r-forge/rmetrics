
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fRegression

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/fRegression?branch=main&svg=true)](https://ci.appveyor.com/project/paulnorthrop/fRegression)
[![R-CMD-check](https://github.com/paulnorthrop/fRegression/workflows/R-CMD-check/badge.svg)](https://github.com/paulnorthrop/fRegression/actions)
[![Coverage
Status](https://codecov.io/github/paulnorthrop/fRegression/coverage.svg?branch=main)](https://codecov.io/github/paulnorthrop/fRegression?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fRegression)](https://cran.r-project.org/package=fRegression)
[![Downloads
(monthly)](https://cranlogs.r-pkg.org/badges/fRegression?color=brightgreen)](https://cran.r-project.org/package=fRegression)
[![Downloads
(total)](https://cranlogs.r-pkg.org/badges/grand-total/fRegression?color=brightgreen)](https://cran.r-project.org/package=fRegression)

## Rmetrics - Modelling Extreme Events in Finance

The **fRegression** package is a collection of functions for linear and
non-linear regression modelling. It implements a wrapper for several
regression models available in the base and contributed packages of R.

### An example

The following code simulates some regression data and fits various
models to these data.

``` r
library(fRegression)
# Simulate data: the response is linearly related to 3 explanatory variables 
x <- regSim(model = "LM3", n = 100)
  
# Linear modelling       
regFit(Y ~ X1 + X2 + X3, data = x, use = "lm") 
#> 
#> Title:
#>  Linear Regression Modeling 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#> (Intercept)           X1           X2           X3  
#>     0.01578      0.73967      0.25128     -0.50611

# Robust linear modelling    
regFit(Y ~ X1 + X2 + X3, data = x, use = "rlm") 
#> 
#> Title:
#>  Robust Linear Regression Modeling 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#> (Intercept)           X1           X2           X3  
#>     0.01968      0.74264      0.24736     -0.50123

# Generalised additive modelling       
regFit(Y ~ X1 + X2 + X3, data = x, use = "gam")  
#> 
#> Title:
#>  Generalized Additive Modeling 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#> (Intercept)           X1           X2           X3  
#>     0.01578      0.73967      0.25128     -0.50611

# Projection pursuit modelling
regFit(Y ~ X1 + X2 + X3, data = x, use = "ppr") 
#> 
#> Title:
#>  Projection Pursuit Regression 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#> -- Projection Direction Vectors --
#>        term 1     term 2
#> X1  0.7950116 -0.4422500
#> X2  0.2733278 -0.4863312
#> X3 -0.5415242 -0.7535894
#> -- Coefficients of Ridge Terms --
#>    term 1    term 2 
#> 0.9163087 0.0439332

# Feed-forward neural network modelling   
regFit(Y ~ X1 + X2 + X3, data = x, use = "nnet") 
#> 
#> Title:
#>  Feedforward Neural Network Modeling 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#>    a 3-2-1 network with 11 weights
#>    options were - linear output units 
#>  [1]  3.3664690  0.5597762  0.2646774 -0.5300914  0.8276914 -0.4493467
#>  [7] -0.1400424  0.2787105 -0.5420174  5.4429808 -6.7838054

# Polychotonous Multivariate Adaptive Regression Splines
regFit(Y ~ X1 + X2 + X3, data = x, use = "polymars")
#>          1          2          3          4          5          6 
#>  0.9145273  1.1607611  1.0482997 -0.5673597 -0.4692621 -1.3336450 
#>           X1          X2          X3
#> 1  1.8197351 -0.39077723  0.24075985
#> 2  1.3704395  0.39665330 -0.02049151
#> 3  1.1963182  0.78156956  0.29685497
#> 4 -0.4068792 -0.01912605  0.55061347
#> 5 -0.6109788 -1.94431293 -0.71396821
#> 6 -1.5089120 -0.24550669  0.38003407
#> 
#> Title:
#>  Polytochomous MARS Modeling 
#> 
#> Formula:
#>  Y ~ X1 + X2 + X3
#> 
#> Family:
#>  gaussian identity 
#> 
#> Model Parameters:
#>   pred1 knot1 pred2 knot2       coefs          SE
#> 1     0    NA     0    NA  0.01577838 0.009803798
#> 2     1    NA     0    NA  0.73967249 0.009930477
#> 3     3    NA     0    NA -0.50611270 0.010729997
#> 4     2    NA     0    NA  0.25127670 0.010419817
```

### Installation

To get the current released version from CRAN:

``` r
install.packages("fRegression")
```
