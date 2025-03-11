s<-log(rjd3toolkit::retail$RetailAndFoodServicesSalesTotal)

# create the model
sm<-rjd3sts::model()
# create the components and add them to the model
rjd3sts::add(sm, rjd3sts::sarima("airline", frequency(s), c(0,1,1), c(0,1,1), var = 1, fixedvariance = TRUE))
rjd3sts::add(sm, rjd3sts::reg_td("cal", frequency(s), start(s), length(s), groups = c(1,1,1,1,1,2,0)))
#estimate the model
air_rslt<-rjd3sts::estimate(sm, s, marginal=TRUE)

# create the tdmodel
smtd<-rjd3sts::model()
# create the components and add them to the model
rjd3sts::add(smtd, rjd3sts::ltd_airline("tdairline", length(s), frequency(s), variance = 1, fixedvariance = TRUE))
rjd3sts::add(smtd, rjd3sts::reg_td("cal", frequency(s), start(s), length(s), groups = c(1,1,1,1,1,2,0)))
#estimate the model
tdair_rslt<-rjd3sts::estimate(smtd, s, marginal=TRUE)

print('airline')
print(rjd3toolkit::result(air_rslt, "likelihood.ll"))
print('tdairline')
print(rjd3toolkit::result(tdair_rslt, "likelihood.ll"))

print('airline')
print(rjd3toolkit::result(air_rslt, "parameters"))
print('tdairline')
print(rjd3toolkit::result(tdair_rslt, "parameters"))
