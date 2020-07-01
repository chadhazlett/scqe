
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scqe

<!-- badges: start -->

<!-- badges: end -->

The scqe package allows users to implement the stability controlled
quasi-experiment (SCQE) (Hazlett, 2019) approach to study the effects of
newly adopted treatments that were not assigned at random. This package
contains tools to help users avoid making statistical assumptions that
rely on infeasible assumptions.

## Motivation

Typical covariate-adjustment techniques used in statistical analysis
impose the often too strict “no-unobserved confounding” assumption.
Ignoring relevant cofounding biases can lead to overconfidence or
inaccuracy of experimental results. SCQE instead imposes an assumption
about the “baseline trend” for the change in average non-treatment
outcome between successive cohorts in observational studies. More
information about this method can be found in Hazlett, 2019.

## Installation

You can install the development version of scqe from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chadhazlett/scqe")
```

## Example

This is a basic example which shows you how to use the scqe function in
order to obtain the scqe estimates and the se/CI for each delta. The
function allows for either a single value for delta or a vector of
several values.

Say you have the following data. You have already determined your
delta=.5

``` r
#library(scqe)
set.seed(100)
my.data = data.frame(post=sample(c(0,1), replace=T,size=10), treatment=sample(c(0,1), replace=T,size=10), outcome=runif(10))
my.data
#>    post treatment   outcome
#> 1     1         1 0.5358112
#> 2     0         1 0.7108038
#> 3     1         1 0.5383487
#> 4     1         1 0.7489722
#> 5     0         0 0.4201015
#> 6     0         1 0.1714202
#> 7     1         1 0.7703016
#> 8     1         0 0.8819536
#> 9     1         0 0.5490967
#> 10    0         0 0.2777238
```

NOTE: this should not be here once the package is up and running

``` r
scqe = function(post, treatment, outcome, delta, ...){
    y2 = outcome - post %*% t(delta)
    r <- data.frame(term=numeric(length(delta)), estimate=numeric(length(delta)), conf.low=numeric(length(delta)),conf.high=numeric(length(delta)))
    for (i in 1:length(delta)){
      iv.out = summary(AER::ivreg(y2[,i] ~ treatment | post))
      est = iv.out$coef["treatment",1]
      se = iv.out$coef["treatment",2]
      conf.low = est - 1.96*se
      conf.high = est + 1.96*se
      r[i,] = c(delta[i], est, conf.low, conf.high)
    }
    class(r) <- c("scqe", "data.frame")
    return(r)
  }
```

You wish to calculate the scqe estimates: single value of delta

``` r
d = .5
scqe(post=my.data$post, treatment=my.data$treatment, outcome=my.data$outcome, delta=d)
#>   term estimate conf.low conf.high
#> 1  0.5 -1.34559  -7.0434   4.35222
#"term" column signifies the delta corresponding to estimates in that line of the df
```

Let’s say instead you want to get the scqe estimate for a range of
values of delta: .5, .75, and 1

``` r
d2 <- c(.5, .75, 1)
scqe(post=my.data$post, treatment=my.data$treatment, outcome=my.data$outcome, delta=d2)
#>   term estimate  conf.low conf.high
#> 1 0.50 -1.34559  -7.04340  4.352220
#> 2 0.75 -2.84559 -14.60075  8.909575
#> 3 1.00 -4.34559 -22.22111 13.529932
```

Since you fed the function a vector of deltas, it outputs a data frame
with each row corresponding to a different delta or “term.”
