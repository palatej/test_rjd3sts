library(rjd3sts)
library(rjd3toolkit)
load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-log(retail$BookStores)

# create the model
bsm<-model()
# create the components and add them to the model
add(bsm, locallineartrend("ll"))
add(bsm, seasonal("s", 12, type="HarrisonStevens"))
add(bsm, noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq<-equation("eq", 0, TRUE)
add_equation(eq, "ll")
add_equation(eq, "s")
add_equation(eq, "n")
add(bsm, eq)
rslt<-estimate(bsm, log(s), marginal=FALSE)
print(result(rslt, "likelihood.ll"))
print(result(rslt, "parameters"))

# create the model
bsm2<-model()
# create the components and add them to the model
add(bsm2, locallineartrend("ll", 
     levelVariance = 1, fixedLevelVariance = TRUE) )
add(bsm2, seasonal("s", 12, type="HarrisonStevens", 
     variance = 1, fixed = TRUE))
add(bsm2, noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq2<-equation("eq", 0, TRUE)
add_equation(eq2, "ll", .01, FALSE)
add_equation(eq2, "s", .01, FALSE)
add_equation(eq2, "n")
add(bsm2, eq2)
rslt2<-estimate(bsm2, log(s), marginal=TRUE)
print(result(rslt2, "likelihood.ll"))
print(result(rslt2, "parameters"))

# create the model
bsm3<-model()
# create the components and add them to the model
add(bsm3, locallevel("l", initial = 0) )
add(bsm3, locallineartrend("lt", levelVariance = 0, 
                                   fixedLevelVariance = TRUE) )
add(bsm3, seasonal("s", 12, type="HarrisonStevens"))
add(bsm3, noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq3<-equation("eq", 0, TRUE)
add_equation(eq3, "l")
add_equation(eq3, "lt")
add_equation(eq3, "s")
add_equation(eq3, "n")
add(bsm3, eq3)
rslt3<-estimate(bsm3, log(s), marginal=TRUE)
print(result(rslt3, "likelihood.ll"))
print(result(rslt3, "parameters"))


#result(rslt, "ssf.P0")
#result(rslt, "ssf.T")

fs<-result(rslt, "ssf.filtering.states")
ss<-result(rslt, "ssf.smoothing.states")
plot(fs[,1], type='l')
lines(ss[,1], col="red")

ll<-function(s, l){
  # create the model
  bsm2<-jd3_ssf_model()
  # create the components and add them to the model
  add(bsm2, locallineartrend("ll", 
                                     levelVariance = 1, fixedLevelVariance = TRUE,
                                     slopevariance = 0, fixedSlopeVariance = TRUE) )
  add(bsm2, seasonal("s", 12, type="Trigonometric", 
                             variance = 1, fixed = TRUE))
  add(bsm2, noise("n", 1, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq2<-equation("eq", 0, TRUE)
  add_equation(eq2, "ll", sqrt(.000019), TRUE)
  add_equation(eq2, "s", l, TRUE)
  add_equation(eq2, "n", sqrt(0.000143), TRUE)
  add_equation(bsm2, eq2)
  rslt2<-estimate(bsm2, log(s), marginal=TRUE)
  return( result(rslt2, "likelihood.ll"))
}

ll2<-function(s, l){
  # create the model
  bsm2<-jd3_ssf_model()
  # create the components and add them to the model
  add(bsm2, locallineartrend("ll", 
                                     levelVariance = .000019, fixedLevelVariance = TRUE,
                                     slopevariance = 0, fixedSlopeVariance = TRUE) )
  add(bsm2, seasonal("s", 12, type="Trigonometric", 
                             variance = abs(l*l*l), fixed = TRUE))
  add(bsm2, noise("n", 0.000143, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq2<-equation("eq", 0, TRUE)
  add_equation(eq2, "ll")
  add_equation(eq2, "s")
  add_equation(eq2, "n")
  add(bsm2, eq2)
  rslt2<-estimate(bsm2, log(s), marginal=FALSE)
  return( result(rslt2, "likelihood.ll"))
}

