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

sa1<-rjd3toolkit::result(rslt, "ssf.smoothing.components")

model<-rjd3sts::model()
seas<-rjd3sts::splines_regular("s", 12, nodes=c(0:11))
n<-rjd3sts::noise('n')
rjd3sts::add(model, llt)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)

sa2<-rjd3toolkit::result(rslt, "ssf.smoothing.components")

print(summary(sa1[,2]-sa2[,2]))

model<-rjd3sts::model()
seas<-rjd3sts::splines_regular("s", 12, nodes=c(1,2,6,7,8,9,10,11))
n<-rjd3sts::noise('n')
rjd3sts::add(model, llt)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)

sa3<-rjd3toolkit::result(rslt, "ssf.smoothing.components")


matplot(cbind(sa1[301:336,2],sa2[301:336,2],sa3[301:336,2]), type='l')