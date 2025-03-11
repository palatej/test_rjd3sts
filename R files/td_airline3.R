s<-log(rjd3toolkit::retail$RetailAndFoodServicesSalesTotal)

# creates an airline model
airline<-function(period, th, bth){
  sarima<-rjd3toolkit::sarima_model("m", period, NULL, 1, th, NULL, 1, bth)
  return (sarima)
}

# time varying decomposition at point idx (we apply canonical decomposition and Kalman filter estimates
#  (WK estimates could be used)
.tvdecomp<-function(s, idx, period=12, th, bth){
  m<-airline(period, th, bth)
  ucm<-rjd3tramoseats::seats_decompose(m)
  q<-rjd3toolkit::ucarima_estimate(s, ucm, stdev = FALSE)
  
  return (q[idx,])
}

# time varying decomposition (non-optimized)
tvdecomp<-function(s, period=12, ths, bths){
  sapply(1:length(s), function(idx){.tvdecomp(s,idx,period,ths[idx], bths[idx])})  
}

est<-rjd3toolkitx::tdairline_estimation(s)

w<-t(tvdecomp(s, 12, est$ltd_sarima$th, est$ltd_sarima$bth))
wkf<-rjd3toolkitx::tdairline_decomposition(s, est$ltd_sarima$th, est$ltd_sarima$bth)

idx<-3
plot(w[,idx]-wkf[,idx], type='l')
plot(w[,idx], type='l')
lines(wkf[,idx], col="red")