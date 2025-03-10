s<-log(rjd3toolkit::retail$RetailAndFoodServicesSalesTotal)

q<-rjd3toolkitx::tdairline_estimation(s)

print('airline')
print(q$sarima$likelihood)
print('tdairline')
print(q$ltd_sarima$likelihood)

tdss<-rjd3toolkitx::tdairline_decomposition(s, q$ltd_sarima$th, q$ltd_sarima$bth)

air<-rjd3toolkit::sarima_model(period=12, d=1, theta=q$sarima$parameters[2],bd=1, btheta=q$sarima$parameters[3] )
ucm<-rjd3toolkit::sarima_decompose(air)

ss<-rjd3toolkit::ucarima_estimate(s, ucm)
plot(tdss[,2], type='l', col='red')
lines(ss[,2], col='gray')

plot(tdss[,2]-ss[,2], type='l')
