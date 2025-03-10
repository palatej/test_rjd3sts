m<-read.csv2('./Data/Marees.csv')
y<-m$Hauteur

period<-12.42

model<-rjd3sts::model()

ll<-rjd3sts::locallineartrend('l')
seas<-rjd3sts::splines_regular('s', period, 5)
n<-rjd3sts::noise('n')
rjd3sts::add(model, ll)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)
sdecomp<-rjd3sts::smoothed_components(rslt)

adecomp<-rjd3highfreq::fractionalAirlineDecomposition(y, period)
adecomp<-adecomp$decomposition

plot(sdecomp[100000:100500, 2], type='l')
lines(adecomp$s[100000:100500], col='red')

plot(sdecomp[100000:100500, 3], type='l')
lines(adecomp$i[100000:100500], col='red')

plot(sdecomp[100000:100500, 1], type='l')
lines(adecomp$t[100000:100500], col='red')