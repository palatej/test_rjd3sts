library(rjd3toolkit)
library(rjd3sts)


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

bsm_td<-function(s, tdgroups, contrast = FALSE){
  # create the model
  bsm<-model()
  # create the components and add them to the model
  add(bsm, locallineartrend("ll"))
  add(bsm, seasonal("s", 12, type="HarrisonStevens"))
  add(bsm, regtd("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = .0001, fixed=FALSE))
  # create the equation (fix the variance to 1)
  eq<-equation("eq", 1, TRUE)
  add.equation(eq, "ll")
  add.equation(eq, "s")
  add.equation(eq, "td")
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, marginal=F, initialization="Augmented_Robust", concentrated=TRUE, precision = 1e-20)
  return(rslt)
}

bsm_std<-function(s){
  # create the model
  bsm<-model()
  # create the components and add them to the model
  add(bsm, locallineartrend("ll"))
  add(bsm, seasonal("s", 12, type="HarrisonStevens"))
  add(bsm, periodic("td", 2.871795, 1))
  # create the equation (fix the variance to 1)
  eq<-equation("eq", 1, T)
  add.equation(eq, "ll")
  add.equation(eq, "s")
  add.equation(eq, "td")
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, marginal=F, initialization="Augmented_Robust", concentrated=TRUE, precision = 1e-20)
  return(rslt)
}

bsm_td_fixed<-function(s, tdgroups, contrast = FALSE){
  # create the model
  bsm<-model()
  # create the components and add them to the model
  add(bsm, locallineartrend("ll"))
  add(bsm, seasonal("s", 12, type="Crude"))
  add(bsm, td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = 0, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq<-equation("eq", 1, TRUE)
  add.equation(eq, "ll")
  add.equation(eq, "s")
  add.equation(eq, "td")
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, marginal=T, concentrated=TRUE, precision=1e-12)
  return(rslt)
}

bsm_td_all<-function(s, contrast = FALSE){
  rslt=bsm_td(s, c(1,1,1,1,1,0,0), contrast)
  print(result(rslt, "likelihood.ll"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,1,1,1,1,2,0), contrast)
  print(result(rslt, "likelihood.ll"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,1,1,1,2,3,0), contrast)
  print(result(rslt, "likelihood.ll"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,2,3,4,5,6,0), contrast)
  print(result(rslt, "likelihood.ll"))
  print(result(rslt, "parameters"))
  rslt=bsm_std(s)
  print(result(rslt, "likelihood.ll"))
  print(result(rslt, "parameters"))
}


bsm_td_all(log(s), T) 