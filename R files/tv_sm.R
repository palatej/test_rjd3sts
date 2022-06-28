library(rjd3sts)
library(rjd3toolkit)
library(rjd3modelling)

data<-read.csv("./Data/FR_Births.csv")
y<-data$births
#data<-read.csv("./Data/edf.csv")
#y<-data$Electricity


# build the calendar

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-calendar.new()
calendar.holiday(jhol, "NEWYEAR")
calendar.holiday(jhol, "EASTERMONDAY")
calendar.holiday(jhol, "MAYDAY")
calendar.holiday(jhol, "ASCENSION")
calendar.holiday(jhol, "WHITMONDAY")
calendar.fixedday(jhol, month=5, day=8)
calendar.fixedday(jhol, month=7, day=14)
calendar.holiday(jhol, "ASSUMPTION")
calendar.holiday(jhol, "ALLSAINTSDAY")
calendar.holiday(jhol, "ARMISTICE")
calendar.holiday(jhol, "CHRISTMAS")


hol<-rjd3modelling::holidays(jhol, "1968-01-01", 
                             length = length(y), type = "Skip")
#hol<-rjd3modelling::holidays(jhol, "1996-01-01", 
#                             length = length(y), type = "Skip")

# create the model
sm<-model()
eq<-equation("eq")
# create the components and add them to the model
add(sm, noise("n"))
add(sm, seasonal("s", 7, type='HarrisonStevens'))
add(sm, locallineartrend("ll"))
add(sm, reg("cal", hol, 0.1))
add.equation(eq, "n")
add.equation(eq, "ll")
add.equation(eq, "s")
add.equation(eq, "cal")
add(sm, eq)
#estimate the model
rslt<-estimate(sm, log(y), marginal=F, initialization="SqrtDiffuse",
               optimizer="LevenbergMarquardt", 
               concentrated=TRUE, precision = 1e-10)

fs<-result(rslt, "ssf.filtered.states")
ss<-result(rslt, "ssf.smoothing.states")

colfunc<-colorRampPalette(c("red","blue","green","#196F3D"))
colors <- (colfunc(12))
plot(ss[,10], type='l', col=colors[1], ylim=c(-0.4, 0.05))
lines(ss[,11], col=colors[2])
lines(ss[,12], col=colors[3])
lines(ss[,13], col=colors[4])
lines(ss[,14], col=colors[5])
lines(ss[,15], col=colors[6])
lines(ss[,16], col=colors[7])
lines(ss[,17], col=colors[8])
lines(ss[,18], col=colors[9])
lines(ss[,19], col=colors[10])
lines(ss[,20], col=colors[11])

