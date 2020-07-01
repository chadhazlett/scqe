#' scqe: Stability Controlled Quasi-Experimentation
#'
#' The scqe package contains several function for statistical analysis that factor in
#' confounding variables and their impact on estimates (Hazlett, 2019).
#'
#' The main function in the package is \code{\link{scqe}}, which...
#'
#' @references Hazlett, C. (2019), "Estimating causal effects of new treatments despite self-selection: The case of experimental medical treatments. Journal of Causal Inference.
#'
#'@section package dependencies:
#'AER
#'ggplot2
#'
#' @name scqe-package
NULL

#' Stability controlled quasi-experiment (scqe)
#'
#' @description
#' This function returns the scqe estimates, standard deviations,  confidence
#' intervals,...
#'
#' @param post Binary vector corresponding to T=0,1 for each observation.
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Can take either a single value or vector of possible values for
#'   delta.
#'
#' @references  Hazlett, C. (2019), "Estimating causal effects of new treatments despite self-selection: The case of experimental medical treatments. Journal of Causal Inference.
#'
#' @examples
#' # Put examples here!
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' scqe.out = scqe(post=post, treatment=tx, outcome=y, delta=c(-0.1,0,.1))
#'
#' plot(scqe.out)
#'
#'@export
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


#' Plot method for \code{scqe}
#' @rdname plot.scqe
#' @description
#' The \code{print} method provides...
#'
#' @param scqe.obj an object of class \code{\link{scqe}}
#'
#' @examples
#' # give example here
#'
#' @export
#'
plot.scqe = function(scqe.obj){
  ggplot2::ggplot() +
  ggplot2::geom_pointrange(data=scqe.obj, mapping=aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high),  size=.5,shape=16) +ylab("Average treatment effect on treated") +xlab("Delta") + coord_flip() + theme_bw()
  }


