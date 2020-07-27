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

#' Stability controlled quasi-experiment (scqe) for 2 cohort case FULL DATA
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
  if(any(delta > 1 | delta < -1)){
    warning("One or more delta(s) are not in range")
  }
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


#' Stability controlled quasi-experiment (scqe) for 1 cohort case with FULL DATA
#'
#' @description
#' This function returns the scqe estimates, standard deviations,  confidence
#' intervals for the one cohort case. This function is similar to the main scqe function
#' but does not have 2 cohort (ie there is only one measurement time (no "post" input)).
#'
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Can take either a single value or vector of possible values for
#'   delta.
#'
#' @references  Hazlett, C. (?)
#'
#' @examples
#' # Put examples here!
#' set.seed(1234)
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .25, size=1)
#'
#'
#' scqe_1cohort.out = scqe_1cohort(treatment=tx, outcome=y, delta=c(-0.1,0,.1))
#'
#' plot(scqe_1cohort.out)
#'
#'@export
scqe_1cohort = function(treatment, outcome, delta){
  if(any(delta > 1 | delta < -1)){
    warning("One or more delta(s) are not in range")
  }
  N <- length(treatment) #number of obs
  pi1 <- sum(tx)/N #number of treated ind/N
  Ybar_T1 <- sum(outcome)/N #the sum of outcomes for treated and untreated/N

  r <- data.frame(term=numeric(length(delta)), estimate=numeric(length(delta)), conf.low=numeric(length(delta)),conf.high=numeric(length(delta)))
  for(i in 1:length(delta)){
    Beta_SCQE_outcome <- (Ybar_T1 - delta[i])/pi1 #code adapted from shiby app for calculations here
    SE_B_SCQE_outcome <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                             ((Ybar_T1-delta[i])^2*(pi1*(1-pi1)))/(pi1^4) ) )

    Beta_SCQE_1C <- c(Beta_SCQE_outcome)
    SE_B_SCQE_1C <- c(SE_B_SCQE_outcome)


    r[i,] <- c(delta[i], Beta_SCQE_1C, Beta_SCQE_1C - 1.96*SE_B_SCQE_1C, Beta_SCQE_1C + 1.96*SE_B_SCQE_1C)

  }
  class(r) <- c("scqe", "data.frame")
  return(r)
}






#' Stability controlled quasi-experiment (scqe) for 1 cohort case SUMMARY STATISTICS AS INPUT
#'
#' @description
#' This function returns the scqe estimates, standard deviations,  confidence
#' intervals for the one cohort case. This function is similar to the main scqe function
#' but does not have 2 cohorts (ie there is only one measurement time (no "post" input)).
#' It takes in summary statistics about the data instead of the full data.
#'
#' @param untr_1C Number of unreated individuals.
#' @param Y_untr_1C Outcome for untreated individuals.
#' @param tr_1C Number of treated individuals.
#' @param Y_tr_1C Outcome for treated individuals.
#' @param min_outcome Minimum possible delta.
#' @param max_outcome Maximum possible delta.
#'
#' @references  Hazlett, C. (?)
#'
#' @examples
#' # Put examples here!
#' set.seed(1234)
#' one_cohort_scqe.out <- one_cohort_scqe(200, 5, 50, 25, .1, 1)
#' plot(one_cohort_scqe.out)
#'
#'
#'@export
one_cohort_scqe <- function(untr_1C, Y_untr_1C, tr_1C, Y_tr_1C, min_outcome, max_outcome){
  N <- tr_1C + untr_1C
  pi1 <- tr_1C/N
  Ybar_T1 <- (Y_tr_1C + Y_untr_1C)/N

  if(min_outcome == max_outcome){
    #spread out the outcome range to +/- 0.2 from the single entered
    #outcome rate, ensuring the range doesn't go beyond the possible bounds
    min_outcome <- max(0, min_outcome - 0.20)
    max_outcome <- min(0.99, min_outcome + 0.40)
    min_outcome <- max_outcome - 0.40
  }

  outcome_list <- seq(from = max_outcome, to = min_outcome, length.out = 11)

  Beta_SCQE_1C <- NULL
  SE_B_SCQE_1C <- NULL

  for(Y_T0 in outcome_list){

    Beta_SCQE_outcome <- (Ybar_T1 - Y_T0)/pi1

    SE_B_SCQE_outcome <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                             ((Ybar_T1-Y_T0)^2*(pi1*(1-pi1)))/(pi1^4) ) )

    Beta_SCQE_1C <- c(Beta_SCQE_1C, Beta_SCQE_outcome)
    SE_B_SCQE_1C <- c(SE_B_SCQE_1C, SE_B_SCQE_outcome)
  }

  SCQE_1C_df <- data.frame(assumed_nontreat_outcome = outcome_list, SCQE_estimate = Beta_SCQE_1C, SCQE_stderr = SE_B_SCQE_1C,
                           term = outcome_list, estimate = Beta_SCQE_1C,
                           conf.low = Beta_SCQE_1C - 1.96*SE_B_SCQE_1C, conf.high = Beta_SCQE_1C + 1.96*SE_B_SCQE_1C)

  class(SCQE_1C_df) <- c("scqe", "data.frame")
  return(SCQE_1C_df)

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
  return(ggplot2::ggplot(scqe.obj, ggplot2::aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
           ggplot2::geom_pointrange(size=.5, shape=16) +
           ggplot2::ylab("Average treatment effect on treated") +
           ggplot2::xlab("Delta") + ggplot2::coord_flip() + ggplot2::theme_bw() +
           ggplot2::geom_hline(yintercept = 0, color="gray50"))
}




#the below function is for the summary statistics 1c case

#' Delta optimization method for \code{scqe}
#' @rdname delta.optim.scqe
#' @description
#' The \code{print} method provides...
#'
#' @param scqe.obj an object of class \code{\link{scqe}}
#'
#' @examples
#' # give example here
#'
#' @export
delta.optim.scqe <- function(Y_T0, untreated, Y_untreated, treated, Y_treated, obj, specified = NULL){

  N <- treated + untreated
  pi1 <- treated/N
  Ybar_T1 <- (Y_untreated + Y_untreated)/N

  Beta_SCQE_1C <- (Ybar_T1 - Y_T0)/pi1

  SE_B_SCQE_1C <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                      ((Ybar_T1-Y_T0)^2*(pi1*(1-pi1)))/(pi1^4) ) )

  if(obj == "zero"){return(Beta_SCQE_1C^2)}
  if(obj == "less"){return((Beta_SCQE_1C + 1.96*SE_B_SCQE_1C)^2)}
  if(obj == "harm"){return((Beta_SCQE_1C - 1.96*SE_B_SCQE_1C)^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_1C)^2)}
}




#DELTA OPTIM FOR 1C FULL DATA CASE:

delta.optim.scqe.1cfull <- function(treatment, outcome, delta, obj, specified = NULL){

  N <- length(treatment)
  pi1 <- sum(treatment)/N
  Ybar_T1 <- sum(outcome)/N

  Beta_SCQE_1C <- (Ybar_T1 - delta)/pi1

  SE_B_SCQE_1C <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                      ((Ybar_T1-delta)^2*(pi1*(1-pi1)))/(pi1^4) ) )

  if(obj == "zero"){return(Beta_SCQE_1C^2)}
  if(obj == "less"){return((Beta_SCQE_1C + 1.96*SE_B_SCQE_1C)^2)}
  if(obj == "harm"){return((Beta_SCQE_1C - 1.96*SE_B_SCQE_1C)^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_1C)^2)}
}







#Right now this is only working for 1C full data case example/I am having trouble making it work more generally:

#' Summary method for \code{scqe}
#' @rdname summary.scqe
#' @description
#' The \code{summary} method provides...
#'
#' @param scqe.obj an object of class \code{\link{scqe}}
#'
#' @examples
#' set.seed(1234)
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .25, size=1)
#'
#' scqe_1cohort.out = scqe_1cohort(treatment=tx, outcome=y, delta=c(-0.1,0,.1))
#'
#' plot(scqe_1cohort.out)
#' summary(scqe_1cohort.out)
#'
#' @export
summary.scqe = function(scqe.obj){

  # optimize for the "less likely case"
  opt_less_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
             treatment=tx, outcome=y, obj = "less", tol = 0.0001)[1]), 3)

  # claim: treatment makes outcome less likely
  cat("To claim the treatment made the outcome significantly less likely,\n one must claim the shift in outcomes under no treatment change was",
      opt_less_1C_full)

  #optimize for "more likely case"
  opt_harm_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
                                                treatment=tx, outcome=y, obj = "harm", tol = 0.0001)[1]), 3)
  # claim: treatment makes outcome more likely
  cat("\n To claim the treatment made the outcome significantly more likely,\n one must claim the shift in outcomes under no treatment change was",
      opt_harm_1C_full)

  # optimize for the "no effect case"
  opt_zero_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
                                                treatment=tx, outcome=y, obj = "zero", tol = 0.0001)[1]), 3)

  #claim: treatment had 0 effect
  cat("\n To claim the treatment had exactly 0 effect on the outcome,\n one must claim the shift in outcomes under no treatment change was exactly",
      opt_zero_1C_full)

}
