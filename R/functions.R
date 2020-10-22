#' scqe: Stability Controlled Quasi-Experimentation
#'
#' The scqe package contains several function for statistical analysis that
#' factor in confounding variables and their impact on estimates
#' (Hazlett, 2019).
#'
#' The main function in the package is \code{\link{scqe}}, which computes scqe
#' estimates and confidence intervals for one or two cohorts with summary or
#' full data given.
#'
#' @references Hazlett, C. (2019), "Estimating causal effects of new treatments
#' despite self-selection: The case of experimental medical treatments."
#' Journal of Causal Inference.
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
#' Main scqe function. Computes scqe estimates and corresponding confidence
#' intervals.
#'
#' @param post Binary vector corresponding to T=0,1 for each observation.
#' @param treatment Binary or continuous vector corresponding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param cohort Numeric, 1 or 2 depending on cohort membership.
#' @param untr_pre Integer number of untreated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param untr_post Integer number of untreated patients in the second cohort
#' if applicable (summary statistics input) (T=1).
#' @param tr_post Integer number of treated patients in the second cohort if
#' applicable (summary statistics input) (T=1).
#' @param tr_pre Integer number of treated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param Y_tr_post Outcome for patients who received treatment at time T=1
#' (summary statistics input).
#' @param Y_untr_post Outcome for patients who did not receive treatment at
#' time T=1 (summary statistics input).
#' @param Y_tr_pre Outcome for patients who did receive treatment at time T=0
#' (summary statistics input).
#' @param Y_untr_pre Outcome for patients who did not receive treatment at time
#'  T=0 (summary statistics input).
#' @param untr Integer number of untreated patients (summary statistics input).
#' @param tr Integer number of treated patients (summary statistics input).
#' @param Y_tr Outcome for treated patients (summary statistics input).
#' @param Y_untr Outcome for untreated patients (summary statistics input).
#' @param min_delta Minimum delta.
#' @param max_delta Maximum delta.
#' @param alpha Numeric alpha for confidence interval (default is alpha=.05).
#' @param ... Extra optional arguments.
#'
#' @references  Hazlett, C. (2019), "Estimating causal effects of new treatments
#' despite self-selection: The case of experimental medical treatments."
#' Journal of Causal Inference.
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # Two cohorts, full data
#' scqe.2cohort.full = scqe(post=post, treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' plot(scqe.2cohort.full)
#' summary(scqe.2cohort.full)
#'
#' # One cohort, full data
#' scqe.1cohort.full = scqe(treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' plot(scqe.1cohort.full)
#' summary(scqe.1cohort.full)
#'
#' # Two cohorts, summary data only
#' scqe.2cohort.sum = scqe(untr_pre=200,untr_post=150,tr_post=50,tr_pre=0,
#' Y_tr_post=20,Y_untr_post=1,
#' Y_tr_pre=0, Y_untr_pre=5,min_delta=.1, max_delta=1)
#' plot(scqe.2cohort.sum)
#' summary(scqe.2cohort.sum)
#'
#' # One cohort, summary data only
#' scqe.1cohort.sum = scqe(untr=100,tr=200,Y_untr=5,Y_tr=50,
#'  min_delta=.1,max_delta=1)
#' plot(scqe.1cohort.sum)
#' summary(scqe.1cohort.sum)
#'
#' @export
scqe = function(post=NA, treatment=NA, outcome=NA, delta=NA, cohort=NA, untr_pre=NA,untr_post=NA,tr_post=NA,tr_pre=NA,Y_tr_post=NA, Y_untr_post=NA,Y_tr_pre=NA,Y_untr_pre=NA, untr=NA,tr=NA,Y_tr=NA,Y_untr=NA, min_delta=NA, max_delta=NA,alpha=NA,...){
  scqe.obj <- NA

  # first define class of object
  # is.numeric(post) = TRUE indicates 2 cohort case
  # is.numeric(untr) indicates summary case
  if(!is.numeric(post) & !is.numeric(untr)  & !is.numeric(untr_post)) {class(scqe.obj) = "1cfull"}
  else if(is.numeric(post) & !is.numeric(untr) & !is.numeric(untr_post)) {class(scqe.obj) = "2cfull"}
  else if(!is.numeric(post) & is.numeric(untr) & !is.numeric(untr_post)) {class(scqe.obj) = "1csumm"}
  else if(is.numeric(untr_post)) {class(scqe.obj) = "2csumm"}


  # call the main scqe function
  return(scqemethod(scqe.obj,post=post, treatment=treatment, outcome=outcome,
                    delta=delta, cohort=cohort, untr_pre=untr_pre,untr_post=untr_post,tr_post=tr_post,
                    tr_pre=tr_pre,Y_tr_post=Y_tr_post,Y_untr_post=Y_untr_post,Y_tr_pre=Y_tr_pre,
                    Y_untr_pre=Y_untr_pre, untr=untr,tr=tr,Y_tr=Y_tr,Y_untr=Y_untr,
                    min_delta=min_delta, max_delta=max_delta,alpha=alpha))
}


#' Stability controlled quasi-experiment (scqe)
#'
#'@description Discpactches to correct scqe function
#'
#' @param ...  Arguments from scqe
#'
scqemethod <- function(...){
  UseMethod('scqe')
}

## 2 COHORT CASE FULL DATA FXN

#' Stability controlled quasi-experiment (scqe) for 2 cohort case, full data
#'
#' @description
#' This function returns the scqe estimates and confidence intervals for the
#' 2 cohort case when the user inputs full data.
#'
#' @param post Binary vector corresponding to T=0,1 for each observation.
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param alpha Numeric alpha for confidence interval (default is alpha=.05).
#' @param max_delta Max delta (see delta description), optional.
#' @param min_delta Min delta (see delta description), optional.
#' @param ... Extra optional arguments.
#'
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # Two cohorts, full data
#' scqe.2cohort.full = scqe(post=post, treatment=tx, outcome=y,
#' delta=seq(from=-.1,to=.1, by=0.05))
#' plot(scqe.2cohort.full)
#' summary(scqe.2cohort.full)
#'
#'@export
scqe.2cfull = function(post, treatment, outcome, delta, alpha,min_delta,max_delta,...){

  if(!is.numeric(alpha)){
    alpha <- .05
  }

  value <- stats::qnorm(1-(alpha/2))

  if(all(post==TRUE|post==FALSE)){
    post <- as.numeric(post)
  }

  if(!is.numeric(delta) & !is.numeric(min_delta) & !is.numeric(max_delta)){
    delta <- seq(from=-.1, to=.1, by=.05)
  }

  if(!is.numeric(delta) & is.numeric(min_delta) & is.numeric(max_delta)){

    if(max_delta == min_delta){
      #spread out the delta range to +/- 0.2 from the single entered
      #delta, ensuring the range doesn't go beyond the possible bounds
      min_delta <- max(-0.99, min_delta - 0.20)
      max_delta <- min(0.99, min_delta + 0.40)
      min_delta <- max_delta - 0.40
    }

    delta <- seq(from = max_delta, to = min_delta, length.out = 11)
  }


  if(any( class(post)=="character" | class(treatment)=="character" | class(outcome)=="character" | class(delta)=="character")){
    stop("One or more inputs to function are of invalid class")
  }
  #check if delta is in range for binary case
  if(all(outcome == 1 | outcome == 0)){
    if(any(delta > 1 | delta < -1)){
      warning("One or more deltas out of expected range (-1,1)")
    }
  }else{
    #checks if delta is in range for non-binary case
    quant <- stats::quantile(outcome, probs=c(.25,.75))
    diff <- quant[[2]] - quant[[1]]
    if(any(delta > diff | delta < -diff)){
      warning("One or more deltas out of expected range")
    }

  }
  y2 = outcome - post %*% t(delta)
  r <- data.frame(term=numeric(length(delta)), estimate=numeric(length(delta)), conf.low=numeric(length(delta)),conf.high=numeric(length(delta)), se=numeric(length(delta)))
  for (i in 1:length(delta)){
    iv.out = summary(AER::ivreg(y2[,i] ~ treatment | post))
    est = iv.out$coef["treatment",1]
    se = iv.out$coef["treatment",2]
    conf.low = est - value*se
    conf.high = est + value*se
    r[i,] = c(delta[i], est, conf.low, conf.high,se)
  }
  out <- list()
  out$result <- r
  out$cohort <- 2
  out$post <- post
  out$treatment <- treatment
  out$outcome <- outcome
  out$delta <- delta
  out$alpha <- alpha
  class(out) <- c("scqe")

  # print only result data frame but retain all attributes
  cat("-- SCQE Method Result Table -- \n\n")
  print(out$result)
  cat("\nFor more information, see full summary.")
  invisible(out)
}

##ONE COHORT CASE FULL DATA FXN
#' Stability controlled quasi-experiment (scqe) for 1 cohort case, full data
#'
#'@description
#' This function returns the scqe estimates and confidence intervals for the
#' 1 cohort case (ie there is not 'post' input) when the user inputs full data.
#'
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param alpha Numeric alpha for confidence interval (default is alpha=.05).
#' @param max_delta Max delta (see delta description), optional.
#' @param min_delta Min delta (see delta description), optional.
#' @param ... Extra optional arguments.
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # One cohort, full data
#' scqe.1cohort.full = scqe(treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' plot(scqe.1cohort.full)
#' summary(scqe.1cohort.full)
#'
#'
#'
#'@export
scqe.1cfull = function(treatment, outcome, delta, alpha, min_delta, max_delta, ...){

  if(!is.numeric(alpha)){
    alpha <- .05
  }

  value <- stats::qnorm(1-(alpha/2))

  if(!is.numeric(delta) & !is.numeric(min_delta) & !is.numeric(max_delta)){
    delta <- seq(from=-.1, to=.1, by=.05)
  }

  if(!is.numeric(delta) & is.numeric(min_delta) & is.numeric(max_delta)){

    if(max_delta == min_delta){
      #spread out the delta range to +/- 0.2 from the single entered
      #delta, ensuring the range doesn't go beyond the possible bounds
      min_delta <- max(-0.99, min_delta - 0.20)
      max_delta <- min(0.99, min_delta + 0.40)
      min_delta <- max_delta - 0.40
    }

    delta <- seq(from = max_delta, to = min_delta, length.out = 11)
  }

  if(any( (treatment)=="character" | class(outcome)=="character" | class(delta)=="character")){
    stop("One or more inputs to function are of invalid class")
  }
  #check if delta is in range for binary case
  if(all(outcome == 1 | outcome == 0)){
    if(any(delta > 1 | delta < -1)){
      warning("One or more deltas out of expected range (-1,1)")
    }
  }


  N <- length(treatment) #number of obs
  pi1 <- sum(treatment)/N #number of treated ind/N
  Ybar_T1 <- sum(outcome)/N #the sum of outcomes for treated and untreated/N

  r <- data.frame(term=numeric(length(delta)), estimate=numeric(length(delta)), conf.low=numeric(length(delta)),conf.high=numeric(length(delta)),se=numeric(length(delta)))
  for(i in 1:length(delta)){
    Beta_SCQE_outcome <- (Ybar_T1 - delta[i])/pi1 #code adapted from shiby app for calculations here
    SE_B_SCQE_outcome <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                             ((Ybar_T1-delta[i])^2*(pi1*(1-pi1)))/(pi1^4) ) )

    Beta_SCQE_1C <- c(Beta_SCQE_outcome)
    SE_B_SCQE_1C <- c(SE_B_SCQE_outcome)


    r[i,] <- c(delta[i], Beta_SCQE_1C, Beta_SCQE_1C - value*SE_B_SCQE_1C, Beta_SCQE_1C + value*SE_B_SCQE_1C,SE_B_SCQE_1C)

  }
  out <- list()
  out$result <- r
  out$cohort <- 1
  out$treatment <- treatment
  out$outcome <- outcome
  out$delta <- delta
  out$alpha <- alpha
  class(out) <- c("scqe")

  # print only result data frame but retain all attributes
  cat("-- SCQE Method Result Table -- \n\n")
  print(out$result)
  cat("\nFor more information, see full summary.")
  invisible(out)

}

## 2 COHORT CASE SUM STATS FXN
#' Stability controlled quasi-experiment (scqe) for 1 cohort case,
#' summary statistics
#'
#'@description
#' This function returns the scqe estimates and confidence intervals for the
#' 2 cohort case when the user inputs only summary statistics.
#'
#' @param untr_pre Integer number of untreated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param tr_pre Integer number of treated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param untr_post Integer number of untreated patients in the second cohort
#' if applicable (summary statistics input) (T=1).
#' @param tr_post Integer number of treated patients in the second cohort if
#' applicable (summary statistics input) (T=1).
#' @param Y_tr_post Outcome for patients who received treatment at time T=1
#' (summary statistics input).
#' @param Y_untr_post Outcome for patients who did not receive treatment at
#' time T=1 (summary statistics input).
#' @param Y_tr_pre Outcome for patients who did receive treatment at time T=0
#' (summary statistics input).
#' @param Y_untr_pre Outcome for patients who did not receive treatment at time
#'  T=0 (summary statistics input).
#' @param min_delta Minimum delta.
#' @param max_delta Maximum delta.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param alpha Numeric alpha for confidence interval (default is alpha=.05).
#' @param ... Extra optional arguments.
#'
#' @examples
#' # Two cohorts, summary data only
#' scqe.2cohort.sum = scqe(untr_pre=200,untr_post=150,tr_post=50,tr_pre=0,
#' Y_tr_post=20,Y_untr_post=1,Y_tr_pre=0,
#' Y_untr_pre=5,min_delta=.1, max_delta=1)
#' plot(scqe.2cohort.sum)
#' summary(scqe.2cohort.sum)
#'
#'
#'@export
scqe.2csumm <- function(untr_pre,untr_post,tr_post,tr_pre,Y_tr_post,
                        Y_untr_post,Y_tr_pre,Y_untr_pre,
                        min_delta, max_delta, delta, alpha, ...){

  if(!is.numeric(alpha)){
    alpha <- .05
  }

  value <- stats::qnorm(1-(alpha/2))


  if(!is.numeric(min_delta) & !is.numeric(max_delta) & !is.numeric(delta)){
    min_delta <- -.1
    max_delta <- .1
    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
  }

  if(!is.numeric(min_delta) & !is.numeric(max_delta) & is.numeric(delta)){
    delta_list <- delta
  }



  if(any( class(untr_pre)=="character" | class(untr_post)=="character" | class(tr_post)=="character" | class(tr_pre)=="character"| class(Y_tr_post)=="character" | class(Y_untr_post)=="character" | class(Y_tr_pre)=="character" | class(Y_untr_pre)=="character" )){
    stop("One or more inputs to function are of invalid class")
  }

  if(!is.numeric(delta) & is.numeric(min_delta) & is.numeric(max_delta) ){
    if(max_delta == min_delta){
      #spread out the delta range to +/- 0.2 from the single entered
      #delta, ensuring the range doesn't go beyond the possible bounds
      min_delta <- max(-0.99, min_delta - 0.20)
      max_delta <- min(0.99, min_delta + 0.40)
      min_delta <- max_delta - 0.40
    }


    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
  }


  N_pre <- untr_pre + tr_pre
  N_post <- untr_post + tr_post
  N <- N_pre + N_post
  Y_pre <- Y_tr_pre + Y_untr_pre
  Y_post <- Y_tr_post + Y_untr_post

  P_T1 <- N_post/N
  P_D1 <- (tr_post + tr_pre)/N
  P_D1_T1 <- tr_post/N
  P_D1_T0 <- tr_pre/N
  P_D0_T1 <- (N_post - tr_post)/N
  P_D0_T0 <- (N_pre - tr_pre)/N

  P_T1_D1_Y1 <- Y_tr_post/N
  P_T0_D1_Y1 <- Y_tr_pre/N
  P_T1_D0_Y1 <- Y_untr_post/N
  P_T0_D0_Y1 <- Y_untr_pre/N
  P_T1_D1_Y0 <- (tr_post - Y_tr_post)/N
  P_T0_D1_Y0 <- (tr_pre - Y_tr_pre)/N
  P_T1_D0_Y0 <- (N_post - tr_post - Y_untr_post)/N
  P_T0_D0_Y0 <- (N_pre - tr_pre - Y_untr_pre)/N

  Beta_SCQE <- NULL
  SE_B_SCQE <- NULL

  for(d in delta_list){

    #mean(delta_Y[post==1])
    #mean(outcome[post==1])-delta
    #mean(outcome&post)/mean(post) - delta
    tildeY_in_post <- Y_post/N_post - d ###

    #mean(delta_Y[post==0])
    #mean(outcome[post==0])
    #mean(outcome&!post)/mean(!post)
    tildeY_in_pre <- Y_pre/N_pre

    #mean(tr[post==1])
    #mean(tr==1&post==1)/mean(post==1)
    tr_in_post <- tr_post/N_post

    #mean(tr[post==0])
    #mean(tr==1&post==0)/mean(post==0)
    tr_in_pre <- tr_pre/N_pre

    Beta_SCQE_delta <- (tildeY_in_post - tildeY_in_pre) / (tr_in_post - tr_in_pre) ###

    #mean(delta_Y)
    #mean(outcome) - delta*mean(post)
    tildeY_all <- (Y_pre + Y_post - d*N_post)/N ###

    Beta_0 <- tildeY_all - Beta_SCQE_delta*P_D1 ###


    SE_B_SCQE_delta <- sqrt((P_T1*(1-P_T1))/(N-2)
                            *(P_T1_D1_Y1*(1-d-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y1*(1-d-Beta_0)^2 +
                                P_T1_D1_Y0*(-d-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y0*(-d-Beta_0)^2 +
                                P_T0_D1_Y1*(1-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y1*(1-Beta_0)^2 +
                                P_T0_D1_Y0*(-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y0*(-Beta_0)^2)
    )/
      (P_D1_T1*(1-P_D1)*(1-P_T1) + P_D1_T0*(1-P_D1)*(-P_T1) +
         P_D0_T1*(-P_D1)*(1-P_T1) + P_D0_T0*(-P_D1)*(-P_T1))      ###

    Beta_SCQE <- c(Beta_SCQE, Beta_SCQE_delta)
    SE_B_SCQE <- c(SE_B_SCQE, SE_B_SCQE_delta)
  }
  SCQE_2C_df <- data.frame(term = delta_list, estimate = Beta_SCQE,
                           conf.low = Beta_SCQE - value*SE_B_SCQE, conf.high = Beta_SCQE + value*SE_B_SCQE,se = SE_B_SCQE)

  out <- list()
  out$result <- SCQE_2C_df
  out$cohort <- 2
  out$treatment <-  c(rep(0, untr_pre + untr_post), rep(1, tr_pre+tr_post))
  out$post <- c(rep(0, untr_pre), rep(1,untr_post), rep(0,tr_pre),rep(1,tr_post))
  out$outcome <- c(rep(1,Y_untr_pre),rep(0, ifelse(untr_pre-Y_untr_pre < 0, 0,untr_pre-Y_untr_pre) ),
                   rep(1,Y_untr_post),rep(0, ifelse(untr_post-Y_untr_post<0,0,untr_post-Y_untr_post)),
                   rep(1,Y_tr_pre),rep(0, ifelse(tr_pre-Y_tr_pre<0,0,tr_pre-Y_tr_pre)),
                   rep(1,Y_tr_post),rep(0, ifelse(tr_post-Y_tr_post<0,0,tr_post-Y_tr_post)))
  out$alpha <- alpha
  class(out) <- c("scqe")

  # print only result data frame but retain all attributes
  cat("-- SCQE Method Result Table -- \n\n")
  print(out$result)
  cat("\nFor more information, see full summary.")
  invisible(out)

}




##ONE COHORT CASE SUM STATS FXN

#' Stability controlled quasi-experiment (scqe) for 1 cohort case, summary
#' statistics
#'
#' @description
#' This function returns the scqe estimates and confidence intervals for the
#' 1 cohort case when the user inputs only summary statistics.
#'
#' @param untr_1C Number of untreated individuals.
#' @param Y_untr_1C Outcome for untreated individuals.
#' @param tr_1C Number of treated individuals.
#' @param Y_tr_1C Outcome for treated individuals.
#' @param min_delta Minimum possible delta.
#' @param max_delta Maximum possible delta.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param alpha Numeric alpha for confidence interval (default is alpha=.05).
#' @param ... Extra optional arguments.
#'
#' @examples
#' # One cohort, summary data only
#' scqe.1cohort.sum = scqe(untr=100,tr=200,Y_untr=5,Y_tr=50,
#' min_delta=.1,max_delta=1)
#' plot(scqe.1cohort.sum)
#' summary(scqe.1cohort.sum)
#'
#'
#'@export
scqe.1csumm <- function(untr_1C, Y_untr_1C, tr_1C, Y_tr_1C, min_delta, max_delta,delta, alpha, ...){

  if(!is.numeric(alpha)){
    alpha <- .05
  }

  value <- stats::qnorm(1-(alpha/2))

  if(!is.numeric(min_delta) & !is.numeric(max_delta) & !is.numeric(delta)){
    min_delta <- -.1
    max_delta <- .1
    outcome_list <- seq(from = max_delta, to = min_delta, length.out = 11)
  }

  if(!is.numeric(min_delta) & !is.numeric(max_delta) & is.numeric(delta)){
    outcome_list <- delta
  }

  if(!is.numeric(delta) & is.numeric(min_delta) & is.numeric(max_delta) ){
    if(max_delta == min_delta){
      #spread out the delta range to +/- 0.2 from the single entered
      #delta, ensuring the range doesn't go beyond the possible bounds
      min_delta <- max(-0.99, min_delta - 0.20)
      max_delta <- min(0.99, min_delta + 0.40)
      min_delta <- max_delta - 0.40
    }


    outcome_list <- seq(from = max_delta, to = min_delta, length.out = 11)
  }

  if(any( class(untr_1C)=="character" | class(Y_untr_1C)=="character" | class(tr_1C)=="character" | class(Y_tr_1C)=="character")){
    stop("One or more inputs to function are of invalid class")
  }
  N <- tr_1C + untr_1C
  pi1 <- tr_1C/N
  Ybar_T1 <- (Y_tr_1C + Y_untr_1C)/N


  Beta_SCQE_1C <- NULL
  SE_B_SCQE_1C <- NULL

  for(Y_T0 in outcome_list){

    Beta_SCQE_outcome <- (Ybar_T1 - Y_T0)/pi1

    SE_B_SCQE_outcome <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                             ((Ybar_T1-Y_T0)^2*(pi1*(1-pi1)))/(pi1^4) ) )

    Beta_SCQE_1C <- c(Beta_SCQE_1C, Beta_SCQE_outcome)
    SE_B_SCQE_1C <- c(SE_B_SCQE_1C, SE_B_SCQE_outcome)
  }

  SCQE_1C_df <- data.frame(term = outcome_list, estimate = Beta_SCQE_1C,
                           conf.low = Beta_SCQE_1C - value*SE_B_SCQE_1C, conf.high = Beta_SCQE_1C + value*SE_B_SCQE_1C,se = SE_B_SCQE_1C)

  out <- list()
  out$result <- SCQE_1C_df
  out$cohort <- 1
  out$treatment <-  c(rep(0,untr_1C),rep(1,tr_1C))
  out$outcome <- c(rep(1,Y_untr_1C),rep(0, ifelse(untr_1C - Y_untr_1C <0,0,untr_1C - Y_untr_1C )), rep(1,Y_tr_1C),rep(0, ifelse(tr_1C - Y_untr_1C <0,0,tr_1C - Y_untr_1C)))
  out$alpha <- alpha
  class(out) <- c("scqe")

  # print only result data frame but retain all attributes
  cat("-- SCQE Method Result Table -- \n\n")
  print(out$result)
  cat("\nFor more information, see full summary.")
  invisible(out)
}






#DELTA OPTIM: ONE COHORT SUMMARY STATS

#' Delta optimization method for \code{scqe} 1 cohort, summary statistics
#' @rdname delta.optim.scqe
#' @description
#' The \code{print} method provides the critical values presented in the summary
#' method for scqe objects.
#'
#'@param Y_T0 Y
#'@param untreated Number of untreated individuals.
#'@param Y_untreated Outcome for untreated individuals.
#'@param treated Number of treated individuals.
#'@param Y_treated Outcome for treated individuals.
#'@param obj scqe object.
#'@param specified Specified optional arguments.
#'@param alpha Numeric alpha for confidence intervals (default alpha=.05).
#'@param ... Extra optional arguments.
#'
#' @export
delta.optim.scqe <- function(Y_T0, untreated, Y_untreated, treated, Y_treated, obj, specified = NULL,alpha,...){

  value <- stats::qnorm(1-(alpha/2))


  N <- treated + untreated
  pi1 <- treated/N
  Ybar_T1 <- (Y_untreated + Y_untreated)/N

  Beta_SCQE_1C <- (Ybar_T1 - Y_T0)/pi1

  SE_B_SCQE_1C <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                      ((Ybar_T1-Y_T0)^2*(pi1*(1-pi1)))/(pi1^4) ) )

  if(obj == "zero"){return(Beta_SCQE_1C^2)}
  if(obj == "less"){return((Beta_SCQE_1C + value*SE_B_SCQE_1C)^2)}
  if(obj == "harm"){return((Beta_SCQE_1C - value*SE_B_SCQE_1C)^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_1C)^2)}
}


#DELTA OPTIM: TWO COHORT SUMMARY STATS

#' Delta optimization method for \code{scqe} 2 cohort, summary statistics
#' @rdname delta_optim_SCQE_2C
#' @description
#' The \code{print} method provides the critical values presented in the summary
#' method for scqe objects.
#'
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param untr_pre Integer number of untreated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param untr_post Integer number of untreated patients in the second cohort
#' if applicable (summary statistics input) (T=1).
#' @param tr_post Integer number of treated patients in the second cohort if
#' applicable (summary statistics input) (T=1).
#' @param tr_pre Integer number of treated patients in the first cohort if
#' applicable (summary statistics input) (T=0).
#' @param Y_tr_post Outcome for patients who received treatment at time T=1
#' (summary statistics input).
#' @param Y_untr_pre Outcome for patients who did not receive treatment at time
#'  T=0 (summary statistics input).
#' @param Y_tr_pre Outcome for patients who did receive treatment at time T=0
#' (summary statistics input).
#' @param Y_untr_post Outcome for patients who did not receive treatment at
#' time T=1 (summary statistics input).
#' @param obj scqe object.
#' @param specified Specified optional arguments.
#' @param alpha Numeric alpha for confidence intervals (default alpha=.05).
#' @param ... Extra optional arguments.
#'
#'
#' @export
delta_optim_SCQE_2C <- function(delta,untr_pre,untr_post,tr_post,tr_pre,
                                Y_tr_post, Y_untr_post,Y_tr_pre,Y_untr_pre,
                                obj, specified = NULL,alpha,...){

  value <- stats::qnorm(1-(alpha/2))

  N_pre <- untr_pre + tr_pre
  N_post <- untr_post + tr_post
  N <- N_pre + N_post
  Y_pre <- Y_tr_pre + Y_untr_pre
  Y_post <- Y_tr_post + Y_untr_post

  P_T1 <- N_post/N
  P_D1 <- (tr_post + tr_pre)/N
  P_D1_T1 <- tr_post/N
  P_D1_T0 <- tr_pre/N
  P_D0_T1 <- (N_post - tr_post)/N
  P_D0_T0 <- (N_pre - tr_pre)/N

  P_T1_D1_Y1 <- Y_tr_post/N
  P_T0_D1_Y1 <- Y_tr_pre/N
  P_T1_D0_Y1 <- Y_untr_post/N
  P_T0_D0_Y1 <- Y_untr_pre/N
  P_T1_D1_Y0 <- (tr_post - Y_tr_post)/N
  P_T0_D1_Y0 <- (tr_pre - Y_tr_pre)/N
  P_T1_D0_Y0 <- (N_post - tr_post - Y_untr_post)/N
  P_T0_D0_Y0 <- (N_pre - tr_pre - Y_untr_pre)/N

  tildeY_in_post <- Y_post/N_post - delta

  tildeY_in_pre <- Y_pre/N_pre

  tr_in_post <- tr_post/N_post

  tr_in_pre <- tr_pre/N_pre

  Beta_SCQE_delta <- (tildeY_in_post - tildeY_in_pre) / (tr_in_post - tr_in_pre)

  tildeY_all <- (Y_pre + Y_post - delta*N_post)/N

  Beta_0 <- tildeY_all - Beta_SCQE_delta*P_D1

  SE_B_SCQE_delta <- sqrt((P_T1*(1-P_T1))/(N-2)
                          *(P_T1_D1_Y1*(1-delta-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y1*(1-delta-Beta_0)^2 +
                              P_T1_D1_Y0*(-delta-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y0*(-delta-Beta_0)^2 +
                              P_T0_D1_Y1*(1-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y1*(1-Beta_0)^2 +
                              P_T0_D1_Y0*(-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y0*(-Beta_0)^2)
  )/
    (P_D1_T1*(1-P_D1)*(1-P_T1) + P_D1_T0*(1-P_D1)*(-P_T1) +
       P_D0_T1*(-P_D1)*(1-P_T1) + P_D0_T0*(-P_D1)*(-P_T1))

  if(obj == "zero"){return(Beta_SCQE_delta^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_delta)^2)}
  #If treatment use is increasing, the threshold delta for a significant
  #negative effect is a minimum, and the threshold delta for a significant
  #positive effect is a maximum
  if(P_D1_T1 > P_D1_T0){
    if(obj == "neg"){return((Beta_SCQE_delta + value*SE_B_SCQE_delta)^2)}
    if(obj == "pos"){return((Beta_SCQE_delta - value*SE_B_SCQE_delta)^2)}
  }
  #If treatment use is decreasing, these minimums and maximums flip
  if(obj == "pos"){return((Beta_SCQE_delta + value*SE_B_SCQE_delta)^2)}
  if(obj == "neg"){return((Beta_SCQE_delta - value*SE_B_SCQE_delta)^2)}
}





#DELTA OPTIM: TWO COHORT FULL DATA

#' Delta optimization method for \code{scqe} 2 cohort, full data
#' @rdname delta.optim.scqe2
#' @description
#' The \code{print} method provides the critical values presented in the summary
#' method for scqe objects.
#'
#' @param post Binary vector corresponding to T=0,1 for each observation.
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1.
#' @param outcome Continueous vector representing the outcome for each
#' observation
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param obj scqe object.
#' @param alpha Numeric alpha for confidence intervals (default alpha=.05).
#' @param specified Specified optional arguments.
#' @param ... Extra optional arguments.
#'
#' @export
delta.optim.scqe2 <- function(post, treatment, outcome, delta, obj, alpha, specified = NULL,...){

  value <- stats::qnorm(1-(alpha/2))

  untr_pre <- length(intersect(which(treatment == 0), which(post == 0)))
  untr_post <- length(intersect(which(treatment == 0), which(post == 1)))
  tr_post <- length(intersect(which(treatment == 1), which(post == 1)))
  tr_pre <- length(intersect(which(treatment == 1), which(post == 0)))
  Y_tr_post <- sum(outcome[intersect(which(treatment == 1), which(post == 1))])
  Y_untr_post <- sum(outcome[intersect(which(treatment == 0), which(post == 1))])
  Y_tr_pre <- sum(outcome[intersect(which(treatment == 1), which(post == 0))])
  Y_untr_pre <- sum(outcome[intersect(which(treatment == 0), which(post == 0))])


  N_pre <- untr_pre + tr_pre
  N_post <- untr_post + tr_post
  N <- N_pre + N_post
  Y_pre <- Y_tr_pre + Y_untr_pre
  Y_post <- Y_tr_post + Y_untr_post

  P_T1 <- N_post/N
  P_D1 <- (tr_post + tr_pre)/N
  P_D1_T1 <- tr_post/N
  P_D1_T0 <- tr_pre/N
  P_D0_T1 <- (N_post - tr_post)/N
  P_D0_T0 <- (N_pre - tr_pre)/N

  P_T1_D1_Y1 <- Y_tr_post/N
  P_T0_D1_Y1 <- Y_tr_pre/N
  P_T1_D0_Y1 <- Y_untr_post/N
  P_T0_D0_Y1 <- Y_untr_pre/N
  P_T1_D1_Y0 <- (tr_post - Y_tr_post)/N
  P_T0_D1_Y0 <- (tr_pre - Y_tr_pre)/N
  P_T1_D0_Y0 <- (N_post - tr_post - Y_untr_post)/N
  P_T0_D0_Y0 <- (N_pre - tr_pre - Y_untr_pre)/N

  tildeY_in_post <- Y_post/N_post - delta

  tildeY_in_pre <- Y_pre/N_pre

  tr_in_post <- tr_post/N_post

  tr_in_pre <- tr_pre/N_pre

  Beta_SCQE_delta <- (tildeY_in_post - tildeY_in_pre) / (tr_in_post - tr_in_pre)

  tildeY_all <- (Y_pre + Y_post - delta*N_post)/N

  Beta_0 <- tildeY_all - Beta_SCQE_delta*P_D1

  SE_B_SCQE_delta <- sqrt((P_T1*(1-P_T1))/(N-2)
                          *(P_T1_D1_Y1*(1-delta-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y1*(1-delta-Beta_0)^2 +
                              P_T1_D1_Y0*(-delta-Beta_0-Beta_SCQE_delta)^2 + P_T1_D0_Y0*(-delta-Beta_0)^2 +
                              P_T0_D1_Y1*(1-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y1*(1-Beta_0)^2 +
                              P_T0_D1_Y0*(-Beta_0-Beta_SCQE_delta)^2 + P_T0_D0_Y0*(-Beta_0)^2)
  )/
    (P_D1_T1*(1-P_D1)*(1-P_T1) + P_D1_T0*(1-P_D1)*(-P_T1) +
       P_D0_T1*(-P_D1)*(1-P_T1) + P_D0_T0*(-P_D1)*(-P_T1))

  if(obj == "zero"){return(Beta_SCQE_delta^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_delta)^2)}
  #If treatment use is increasing, the threshold delta for a significant
  #negative effect is a minimum, and the threshold delta for a significant
  #positive effect is a maximum
  if(P_D1_T1 > P_D1_T0){
    if(obj == "less"){return((Beta_SCQE_delta + value*SE_B_SCQE_delta)^2)}
    if(obj == "harm"){return((Beta_SCQE_delta - value*SE_B_SCQE_delta)^2)}
  }
  #If treatment use is decreasing, these minimums and maximums flip
  if(obj == "less"){return((Beta_SCQE_delta + value*SE_B_SCQE_delta)^2)}
  if(obj == "harm"){return((Beta_SCQE_delta - value*SE_B_SCQE_delta)^2)}
}












#DELTA OPTIM: ONE COHORT FULL DATA

#' Delta optimization method for \code{scqe} 1 cohort, full data
#' @rdname delta.optim.scqe.1cfull
#' @description
#' The \code{print} method provides the critical values presented in the summary
#' method for scqe objects.
#'
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#' (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#' observation.
#' @param delta Single value or vector of possible values for change in
#' average non-treatment outcome between cohorts (if applicable).
#' @param obj scqe object.
#' @param specified Specified optional arguments.
#' @param alpha Numeric alpha for confidence intervals (default alpha=.05).
#' @param ... Extra optional arguments.
#'
#' @export
delta.optim.scqe.1cfull <- function(treatment, outcome, delta, obj, specified = NULL,alpha,...){

  value <- stats::qnorm(1-(alpha/2))

  N <- length(treatment)
  pi1 <- sum(treatment)/N
  Ybar_T1 <- sum(outcome)/N

  Beta_SCQE_1C <- (Ybar_T1 - delta)/pi1

  SE_B_SCQE_1C <- sqrt( (1/(N-1))*( ((Ybar_T1*(1-Ybar_T1))/(pi1^2)) +
                                      ((Ybar_T1-delta)^2*(pi1*(1-pi1)))/(pi1^4) ) )

  if(obj == "zero"){return(Beta_SCQE_1C^2)}
  if(obj == "less"){return((Beta_SCQE_1C + value*SE_B_SCQE_1C)^2)}
  if(obj == "harm"){return((Beta_SCQE_1C - value*SE_B_SCQE_1C)^2)}
  if(obj == "spec"){return((specified - Beta_SCQE_1C)^2)}
}



##SCQE CLASS PLOT METHOD

#' Plot method for \code{scqe}
#' @rdname plot.scqe
#' @description
#' The \code{print} method provides a plot of the estimates
#' and confidence intervals for the scqe estimates for the range of
#' values of delta provided by the user.
#'
#' @param scqe.obj an object of class \code{\link{scqe}}
#' @param xlab Optional character label for x axis.
#' @param ylab Optional character label for y axis.
#' @param ... Extra optional arguments
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # Two cohorts, full data
#' scqe.2cohort.full = scqe(post=post, treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' plot(scqe.2cohort.full)
#'
#' @export
#'
plot.scqe = function(scqe.obj,xlab,ylab,...){
  if(missing(xlab)){
    xlab <- c("Delta")
  }
  if(missing(ylab)){
    ylab <- c("Average treatment effected on treated")
  }
  scqe.obj <- as.data.frame(scqe.obj$result)
  term <- NULL
  estimate <- NULL
  conf.high <- NULL
  conf.low <- NULL
  return(ggplot2::ggplot(scqe.obj, ggplot2::aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
           ggplot2::geom_pointrange(size=.5, shape=16) +
           ggplot2::ylab(ylab) +
           ggplot2::xlab(xlab) + ggplot2::coord_flip() + ggplot2::theme_bw() +
           ggplot2::geom_hline(yintercept = 0, color="gray50"))
}


##SCQE SUMMARY METHOD

#' Summary method for \code{scqe}
#' @rdname summary.scqe
#' @description
#' The \code{summary} method provides several statements that summarize
#' important values of delta requires to make different conclusions
#' about the treatment's effect on patient outcome.
#'
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # Two cohorts, full data
#' scqe.2cohort.full = scqe(post=post, treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' summary(scqe.2cohort.full)
#'
#' @importFrom stats optimize
#'
#' @export
#'
summary.scqe = function(scqe.obj,...) {

  treatment <- scqe.obj$treatment
  post <- scqe.obj$post
  outcome <- scqe.obj$outcome
  cohort <- scqe.obj$cohort
  alpha <- scqe.obj$alpha
  scqe.obj <- as.data.frame(scqe.obj$result)
  value <- stats::qnorm(1-(alpha/2))

  if(cohort==2){
    # optimize for the "less likely case"
    opt_less_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe2, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome,post=post,alpha=alpha,obj = "less", tol = 0.0001)[1]), 3)

    # claim: treatment makes outcome less likely
    one <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was",opt_less_1C_full, "or above."))

    #optimize for "more likely case"
    opt_harm_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe2, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome, post=post,alpha=alpha, obj = "harm", tol = 0.0001)[1]), 3)
    # claim: treatment makes outcome more likely
    two <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was",opt_harm_1C_full, "or below."))

    # optimize for the "no effect case"
    opt_zero_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe2, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome,post=post,alpha=alpha, obj = "zero", tol = 0.0001)[1]), 3)

    #claim: treatment had 0 effect
    three <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was exactly",paste0(opt_zero_1C_full,".")))

    critical_point <- data.frame("less likely"=opt_less_1C_full,"more likely"=opt_harm_1C_full,"zero effect"=opt_zero_1C_full)

    rlist <-
      list("less.likely" = utils::capture.output(cat("To claim the treatment made the outcome significantly less likely: \n", one)),
           "more.likely" = utils::capture.output(cat("To claim the treatment made the outcome significantly more likely: \n", two)),
           "no.effect" =  utils::capture.output(cat("To claim the treatment had 0 effect on the outcome:\n", three)),
           "critical.points" = critical_point,
           "full.results"= scqe.obj
      )


  } else {
    # optimize for the "less likely case"
    opt_less_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome,alpha=alpha,obj = "less", tol = 0.0001)[1]), 3)

    # claim: treatment makes outcome less likely
    one <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was",opt_less_1C_full, "or above."))

    #optimize for "more likely case"
    opt_harm_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome, alpha=alpha, obj = "harm", tol = 0.0001)[1]), 3)
    # claim: treatment makes outcome more likely
    two <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was",opt_harm_1C_full, "or below."))

    # optimize for the "no effect case"
    opt_zero_1C_full <- round(as.numeric(optimize(f = delta.optim.scqe.1cfull, interval = c(-1,1),
                                                  treatment=treatment, outcome=outcome,alpha=alpha, obj = "zero", tol = 0.0001)[1]), 3)

    #claim: treatment had 0 effect
    three <- utils::capture.output(cat("One must claim the shift in outcomes under no treatment change was exactly",paste0(opt_zero_1C_full,".")))

    critical_point <- data.frame("less likely"=opt_less_1C_full,"more likely"=opt_harm_1C_full,"zero effect"=opt_zero_1C_full)


    rlist <-
      list("less.likely" = utils::capture.output(cat("To claim the treatment made the outcome significantly less likely: \n", one)),
           "more.likely" = utils::capture.output(cat("To claim the treatment made the outcome significantly more likely: \n", two)),
           "no.effect" =  utils::capture.output(cat("To claim the treatment had 0 effect on the outcome:\n", three)),
           "critical.points" = critical_point,
           "full.results"= scqe.obj
      )

  }

  cat("-- SCQE Method Results -- \n\n")

  cat("- Claims About Treatment Effects -\n")

  cat(" 1.", "To claim the treatment made the outcome significantly less likely:\n")
  cat("   ", one,"\n\n")

  cat(" 2.","To claim the treatment made the outcome significantly more likely:\n")
  cat("   ",two,"\n\n")

  cat(" 3.","To claim the treatment had 0 effect on the outcome:\n")
  cat("   ",three,"\n\n\n")

  cat("- Full Results Table: - \n")
  print(scqe.obj)

  invisible(rlist)
}

##SCQE PRINT METHOD

#' Print method for \code{scqe}
#' @rdname summary.scqe
#' @description
#' The \code{ptin} method provides the result table that
#' includes the given delta values and their conclusions
#' about the treatment's effect on patient outcome.
#'
#' @param scqe.obj an object of class \code{\link{scqe}}
#' @param ... Extra optional arguments
#'
#' @examples
#' set.seed(1234)
#' post = c(rep(0,100), rep(1,100))
#' tx = c(rep(0, 100), rbinom(n = 100, prob=.27, size=1))
#' y = rbinom(n=200, prob = .1 + .02*post - .05*tx, size=1)
#'
#' # Two cohorts, full data
#' scqe.2cohort.full = scqe(post=post, treatment=tx, outcome=y,
#' delta=seq(from=-.1, to=.1, by=0.05))
#' scqe.2cohort.full
#' print(scqe.2cohort.full)
#'
#' @export
#'

print.scqe = function(scqe.obj,...) {
  treatment <- scqe.obj$treatment
  post <- scqe.obj$post
  outcome <- scqe.obj$outcome
  cohort <- scqe.obj$cohort
  result <- as.data.frame(scqe.obj$result)


  cat("-- SCQE Method Result Table -- \n\n")

  print(result)

  cat("\nFor more information, see full summary.")

}
