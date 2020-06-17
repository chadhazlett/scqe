#build functions here

getSCQE = function(post, treatment, outcome, delta){
  y2 = outcome - post*c(delta)
  iv.out = summary(ivreg(y2 ~ treatment | post))
  est = iv.out$coef["treatment",1]
  se = iv.out$coef["treatment",2]
  conf.low = est - 1.96*se
  conf.high = est + 1.96*se
  r = c(delta, est, conf.low, conf.high)
  names(r) = c("term", "estimate","conf.low","conf.high")
  return(r)
}
