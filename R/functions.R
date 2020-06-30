#' SCQE.
#'
#' \code{scqe} returns the scqe estimates, standard deviations, and relevant confidence
#' intervals.
#'
#' @param post Binary vector corresponding to T=0,1 for each observation.
#' @param treatment Binary or continuous vector correspoding (usually) to 0,1
#'   (no treatment or treatment) for each observation.
#' @param outcome Continuous vector representing the outcome for each
#'   observation.
#' @param delta Can take either a single value or vector of possible values for
#'   delta.
#'
#'@export
scqe = function(post, treatment, outcome, delta){
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
    class(r) <- "scqe"
    return(r)
  }


scqe.plot = ggplot() +
  geom_pointrange(data=r, mapping=aes(x=estimate, y=term, ymin=conf.low, ymax=conf.high),  size=.5, color="blue", fill="white", shape=22) + ggtitle("Treatment effect estimation SCQE") +xlab("scqe estimate") +ylab("delta")

