library(rjd3sts)
library(rjd3toolkit)
# create the model
bsm_td_periodic<-function(s, tdgroups, period, noisyperiod, contrast = FALSE, initialization="Augmented_Robust"){
  bsm<-model()
  
  # create the components and add them to the model
  add(bsm, locallineartrend("ll"))
  add(bsm, seasonal("s", 12, type="Crude"))
  add(bsm, regtd("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = 0, fixed=TRUE))
  add(bsm, noise("n", 1, TRUE))
  if (! is.null(noisyperiod)){
    for (i in 1:length(noisyperiod)){
      add(bsm, noise(paste("pn", i, sep=""), variance = .01, fixed=FALSE))
    }
  }
  # create the equation (fix the variance to 1)
  eq<-equation("eq", 0, TRUE)
  add.equation(eq, "ll")
  add.equation(eq, "s")
  add.equation(eq, "td")
  add.equation(eq, "n")
  if (! is.null(noisyperiod)){
    for (i in 1:length(noisyperiod)){
      add.equation(eq,paste("pn", i, sep=""), 1, TRUE, loading_periodic(period, noisyperiod[i]))
    }
  }
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, marginal=F, initialization=initialization, concentrated=TRUE)
  return(rslt)

}

q<-bsm_td_periodic(log(retail$BookStores), c(1,1,1,1,2,3,0),12,c(1,7,8))
ss<-result(q, "ssf.smoothing.states")

plot(ss[,17]+ss[,18]+ss[,19]+ss[,20], type="l")
z<-ss[,18]+ss[,19]+ss[,20]
z<-sapply(z, function(x){if (abs(x)==0)return(NA)else return(x)})
points(z, col="red", pch=16)





