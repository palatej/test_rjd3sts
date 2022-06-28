library(rjd3sts)
library(rjd3toolkit)

load("./Data/ABS.rda")

s<-rjd3toolkit::ABS$X0.2.20.10.M

# create the model
bsm<-model() 
# create the components and add them to the model
add(bsm, locallineartrend("ll"))
add(bsm, seasonal("s", 12, type="HarrisonStevens"))
add(bsm, td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation 
eq<-equation("eq", .01, F)
add.equation(eq, "ll")
add.equation(eq, "s")
add.equation(eq, "td")
add(bsm, eq)
#estimate the model
rslt<-estimate(bsm, log(s), marginal=T, concentrated=T)
ss<-rjd3toolkit::result(rslt, "ssf.smoothing.states")
plot(-(4*ss[,14]+ss[,15]+ss[,16]), type="l", ylim=c(-0.035,0))
print(rjd3toolkit::result(rslt, "likelihood.ll"))
print(rjd3toolkit::result(rslt, "parameters"))
# create the model
bsm2<-model()
# create the components and add them to the model
add(bsm2, locallineartrend("ll"))
add(bsm2, seasonal("s", 12, type="HarrisonStevens"))
add(bsm2, td("td", frequency(s), start(s), length(s), c(1,2,3,4,5,6,0)))
# create the equation 
eq2<-equation("eq", .01, F)
add.equation(eq2, "ll")
add.equation(eq2, "s")
add.equation(eq2, "td")
add(bsm2, eq2)
#estimate the model
rslt2<-estimate(bsm2, log(s), marginal=T, concentrated=T)
ss2<-result(rslt2, "ssf.smoothing.states")
lines(-(ss2[,14]+ss2[,15]+ss2[,16]+ss2[,17]+ss2[,18]+ss2[,19]), col="red")
print(result(rslt2, "likelihood.ll"))
print(result(rslt2, "parameters"))

# create the model
airline<-model()
# create the components and add them to the model
add(airline, sarima("air", 12, c(0,1,1), c(0,1,1), fixedvariance = FALSE) )
add(airline, td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation (fix the variance to 0)
eq<-equation("eq", 0, TRUE)
add.equation(eq, "air")
add.equation(eq, "td")
add(airline, eq)
#estimate the model
arslt<-estimate(airline, log(s), marginal=T, concentrated=T)
print(result(arslt, "parameters"))
ass<-result(arslt, "ssf.smoothing.states")
plot(-(4*ass[,15]+ass[,16]+ass[,17]), type="l", ylim=c(-0.035,0))
# create the model
airline<-model()
# create the components and add them to the model
add(airline, sarima("air", 12, c(0,1,1), c(0,1,1), fixedvariance = FALSE) )
add(airline, td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation (fix the variance to 0)
eq<-equation("eq", 0, TRUE)
add.equation(eq, "air")
add.equation(eq, "td")
add(airline, eq)
arslt<-estimate(airline, log(s), marginal=T, concentrated=F)
print(result(arslt, "parameters"))
afs<-result(arslt, "ssf.smoothing.states")
lines(-(4*afs[,15]+afs[,16]+afs[,17]), type="l", ylim=c(-0.035,0), col="red")



