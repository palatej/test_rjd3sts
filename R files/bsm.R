s<-rjd3toolkit::ABS$X0.2.09.10.M
y<-log(s)
model<-rjd3sts::model()

llt<-rjd3sts::locallineartrend('l')
seas<-rjd3sts::seasonal("s", 12, "HarrisonStevens")
n<-rjd3sts::noise('n')
rjd3sts::add(model, llt)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)

Tr<-rjd3toolkit::result(rslt, "ssf.T(0)")
print(Tr)

ss<-rjd3sts::smoothed_states(rslt)
fs<-rjd3sts::filtering_states(rslt)

plot(ss[,1], type='l')
lines(fs[,1], col='red')

model2<-rjd3sts::model()

vtd<-rjd3sts::reg_td("td", 12, start(s), length(s))
rjd3sts::add(model2, llt)
rjd3sts::add(model2, seas)
rjd3sts::add(model2, n)
rjd3sts::add(model2, vtd)

rslt2<-rjd3sts::estimate(model2, y)
ss2<-rjd3sts::smoothed_states(rslt2)
fs2<-rjd3sts::filtering_states(rslt2)
plot(- rowSums(fs2[,15:20]), type='l' , ylim=c(-0.05, 0.01))
lines(- rowSums(ss2[,15:20]), col='red')
lines(ss2[,17], col='blue')

