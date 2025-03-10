y<-log(rjd3toolkit::ABS$X0.2.20.10.M)

days<-c(1,1,1,1,2,3,0)

model1<-rjd3sts::model()
sarima<-rjd3sts::sarima('arima', 12, orders=c(0,1,1), seasonal=c(0,1,1))
td<-rjd3sts::reg_td('td', 12, start(y), length(y), groups=days, variance=1, fixed=FALSE)

rjd3sts::add(model1, sarima)
rjd3sts::add(model1, td)

rslt1<-rjd3sts::estimate(model1, y, marginal = TRUE, initialization = "Augmented_Robust")

print(rjd3toolkit::result(rslt1, 'likelihood.ll'))

model2<-rjd3sts::model()

xtd<-rjd3toolkit::td(12, start(y), length(y), groups=days)
td<-rjd3sts::reg('td', xtd)

rjd3sts::add(model2, sarima)
rjd3sts::add(model2, td)

rslt2<-rjd3sts::estimate(model2, y, marginal = TRUE)

print(rjd3toolkit::result(rslt2, 'likelihood.ll'))

cmp1<-rjd3sts::smoothed_components(rslt1)
cmp2<-rjd3sts::smoothed_components(rslt2)
plot(cmp1[,2], type='l', ylim=c(-0.03, 0.02), col='red')
#lines(cmp2[,2], col='gray')