#build functions here

#the ivreg requires the AER package!!!
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

