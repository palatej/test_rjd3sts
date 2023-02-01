library(rjd3sts)
library(rjd3toolkit)

data<-read.csv("./Data/FR_Births.csv")
y<-data$births
#data<-read.csv("./Data/edf.csv")
#y<-data$Electricity

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
#hol<-rjd3modelling::holidays(jhol, "1996-01-01", 
#                             length = length(y), type = "Skip")

# create the model
sm<-model()
eq<-equation("eq")
# create the components and add them to the model
add(sm, sarima("airline", 7, c(0,1,1), c(0,1,1)))
add(sm, reg("cal", hol, 0.1))
add.equation(eq, "airline")
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

