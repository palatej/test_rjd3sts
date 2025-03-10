usclaims<-read.table("./Data/usclaims.txt")
y=usclaims[,1]
n<-length(y)
idx<-(n-52*3):n

period=365.25/7

model<-rjd3sts::model()
ll<-rjd3sts::locallevel('l')
seas<-rjd3sts::splines_regular('s', period, nnodes=52)
n<-rjd3sts::noise('n')
rjd3sts::add(model, ll)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)
m1<-rjd3sts::smoothed_components(rslt)


plot(idx, m1[idx, 2], "l")

model<-rjd3sts::model()
seas2<-rjd3sts::splines_regular('s', period, nnodes=35)
rjd3sts::add(model, ll)
rjd3sts::add(model, seas2)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)
m2<-rjd3sts::smoothed_components(rslt)
lines(idx, m2[idx,2], col="red")

