
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scqe

<!-- badges: start -->

<!-- badges: end -->

The scqe package allows users to implement the stability controlled
quasi-experiment (SCQE) (Hazlett, 2019) approach to study the effects of
newly adopted treatments that were not assigned at random. This package
created tools to help users avoid making statistical assumptions that
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

This is a basic example which shows you how to use the getSCQE function
in order to obtain the scqe estimates and the se/CI for each delta. The
function allows for either a single value for delta or a vector of
several values.

Say you have the following data. You have already determined your
delta=.5

``` r
#library(scqe)

#your data:
#we will use different data later, this is a sample for now
#the real data should eventually be stored in /data
my.data = data.frame(post=sample(c(0,1), replace=T,size=10), tmt=sample(c(0,1), replace=T,size=10), out=runif(10))
my.data
#>    post tmt       out
#> 1     1   1 0.7852252
#> 2     0   0 0.7327932
#> 3     0   0 0.3064098
#> 4     0   0 0.7060504
#> 5     1   1 0.3853947
#> 6     0   0 0.4593918
#> 7     0   0 0.2356888
#> 8     0   1 0.5715070
#> 9     1   0 0.3581574
#> 10    0   0 0.9139753
```

NOTE: this should not be here once the package is up and running

``` r
library(AER)
#> Loading required package: car
#> Warning: package 'car' was built under R version 3.6.2
#> Loading required package: carData
#> Warning: package 'carData' was built under R version 3.6.2
#> Loading required package: lmtest
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> Loading required package: survival
getSCQE = function(post, treatment, outcome, delta){
  if(length(c(delta))==1){
    y2 = outcome - post*c(delta)
    iv.out = summary(ivreg(y2 ~ treatment | post))
    est = iv.out$coef["treatment",1]
    se = iv.out$coef["treatment",2]
    conf.low = est - 1.96*se
    conf.high = est + 1.96*se
    r = c(delta, est, conf.low, conf.high)
    names(r) = c("term", "estimate","conf.low","conf.high")
    return(r)

  }else{
    y2 = outcome - post %*% t(delta)
    r <- data.frame(term=numeric(length(delta)), estimate=numeric(length(delta)), conf.low=numeric(length(delta)),conf.high=numeric(length(delta)))
    for (i in 1:length(delta)){
      iv.out = summary(ivreg(y2[,i] ~ treatment | post))
      est = iv.out$coef["treatment",1]
      se = iv.out$coef["treatment",2]
      conf.low = est - 1.96*se
      conf.high = est + 1.96*se
      r[i,] = c(delta[i], est, conf.low, conf.high)
    }
    return(r)
  }
}
```

You wish to calculate the scqe estimates: sigle value of delta

``` r
d = .5
getSCQE(my.data$post, my.data$tmt, my.data$out, d)
#>       term   estimate   conf.low  conf.high 
#>  0.5000000 -1.0523643 -2.4956079  0.3908792
#term signifies the delta used=
```

Let’s say instead you want to get the scqe estimate for a range of
values of delta: .5, .75, and 1

``` r
d2 <- c(.5, .75, 1)
getSCQE(my.data$post, my.data$tmt, my.data$out, d2)
#>   term  estimate  conf.low conf.high
#> 1 0.50 -1.052364 -2.495608 0.3908792
#> 2 0.75 -1.529637 -3.473062 0.4137875
#> 3 1.00 -2.006910 -4.466464 0.4526447
```

Since you fed the function a vector of deltas, it outputs a data frame
with each row corresponding to a different delta or “term.”
