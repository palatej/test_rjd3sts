library(rjd3sts)
library(KFAS)

bsm0<-function(s, seasonal="Dummy"){
  # create the model
  bsm<-rjd3sts::model()
  # create the components and add them to the model
  rjd3sts::add(bsm, rjd3sts::locallineartrend("ll"))
  rjd3sts::add(bsm, rjd3sts::seasonal("s", frequency(s), type=seasonal))
  #estimate the model
  rslt<-rjd3sts::estimate(bsm, s, concentrated=T)
  return (rslt)
}

update_model<-function(pars, model){
  for (i in 1:3){
  model["Q"][i,i,1]<-pars[i]*pars[i]
  }
#  for (i in 3:13){
#    model["Q"][i,i,1]<-pars[3]*pars[3]
#  }
  model["H"]<-pars[4]*pars[4]
  model
}

check_model<-function(pars, model){
return (T)}

bsm1<-function(s, method="L-BFGS-B"){
  model<-SSModel(s~-1+SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA)))+SSMseasonal(period=frequency(s), Q=matrix(NA)), H=matrix(NA))
  fmodel<-fitSSM(model, inits = c(0.1,0.1,0.1,0.1), updatefn = update_model, checkfn=check_model, method=method)
  return (fmodel)
  
}

bsm2<-function(s, method="L-BFGS-B"){
  model<-SSModel(s~-1+SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA)))+SSMseasonal(period=frequency(s), Q=matrix(NA)), H=matrix(NA))
  fmodel<-fitSSM(model, inits = c(0.1,0.1,0.1,0.1), method=method)
  return (fmodel)
  
}

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:50){
  jd3a<-bsm0(log(rjd3toolkit::ABS$X0.2.06.10.M))
}

# Stop the clock
message("JD3")
print(proc.time() - ptm)


# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:50){
  kfasa<-bsm1(log(rjd3toolkit::ABS$X0.2.06.10.M))
}

# Stop the clock
message("KFAS-1")
print(proc.time() - ptm)

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:50){
  kfas2<-bsm2(log(rjd3toolkit::ABS$X0.2.06.10.M))
}

# Stop the clock
message("KFAS-2")
print(proc.time() - ptm)



    