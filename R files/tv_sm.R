library(rjd3sts)
library(rjd3toolkit)

data<-read.csv("./Data/FR_Births.csv")
y<-data$births

# build the calendar

# Create holidays. See also testholidays.R for other examples and for weekly variables.
FR <- national_calendar(list(
  fixed_day(7,14),
  fixed_day(5,8),
  special_day('NEWYEAR'),
  special_day('CHRISTMAS'),
  special_day('MAYDAY'),
  special_day('EASTERMONDAY'),
  special_day('ASCENSION'),
  special_day('WHITMONDAY'),
  special_day('ASSUMPTION'),
  special_day('ALLSAINTSDAY'),
  special_day('ARMISTICE')
))


hol<-holidays(FR, "1968-01-01", 
                             length = length(y), type = "Skip")

# create the model
sm<-model()
eq<-equation("eq")
# create the components and add them to the model
add(sm, reg("cal", hol, 0.1))
add(sm, locallineartrend("ll"))
add(sm, splines_regular('y', 365.25, nnodes=6))
add(sm, seasonal("s", 7, type='HarrisonStevens'))
add(sm, noise("n"))
#estimate the model
rslt<-estimate(sm, log(y))
print(result(rslt, "likelihood.ll"))

ss<-result(rslt, "ssf.smoothing.states")

colfunc<-colorRampPalette(c("red","blue","green","#196F3D"))
colors <- (colfunc(12))
plot(ss[,1], type='l', col=colors[1], ylim=c(-0.4, 0.05))
lines(ss[,2], col=colors[2])
lines(ss[,3], col=colors[3])
lines(ss[,4], col=colors[4])
lines(ss[,5], col=colors[5])
lines(ss[,6], col=colors[6])
lines(ss[,7], col=colors[7])
lines(ss[,8], col=colors[8])
lines(ss[,9], col=colors[9])
lines(ss[,10], col=colors[10])
lines(ss[,11], col=colors[11])

cmps<-smoothed_components(rslt)
plot(ylim=c(-0.45, 0.1),cmps[,4], type='l', col='gray')
lines(cmps[,1], col='blue')
lines(cmps[,3], col='red')

start=32*365+8
range<-start:(start+367)
cmps<-smoothed_components(rslt)
plot(ylim=c(-0.45, 0.1),cmps[range,4], type='l', col='gray')
lines(cmps[range,1], col='blue')
lines(cmps[range,3], col='red')

