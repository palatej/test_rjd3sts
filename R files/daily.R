edf<-read.csv("./Data/edf.csv")
y<-log(edf$Electricity)

# Create holidays. See also testholidays.R for other examples and for weekly variables.
FR <- rjd3toolkit::national_calendar(list(
  rjd3toolkit::fixed_day(7,14),
  rjd3toolkit::fixed_day(5,8),
  rjd3toolkit::special_day('NEWYEAR'),
  rjd3toolkit::special_day('CHRISTMAS'),
  rjd3toolkit::special_day('MAYDAY'),
  rjd3toolkit::special_day('EASTERMONDAY'),
  rjd3toolkit::special_day('ASCENSION'),
  rjd3toolkit::special_day('WHITMONDAY'),
  rjd3toolkit::special_day('ASSUMPTION'),
  rjd3toolkit::special_day('ALLSAINTSDAY'),
  rjd3toolkit::special_day('ARMISTICE')
))


hol<-rjd3toolkit::holidays(FR, "1996-01-01", 
              length = length(y), type = "Skip")



model<-rjd3sts::model()
ll<-rjd3sts::locallineartrend('l')
seas1<-rjd3sts::seasonal('w', 7, "HarrisonStevens", variance = 0.1, fixed=TRUE )
#seas2<-rjd3sts::splines_regular('y', 365.25, nnodes=45)
seas2<-rjd3sts::splines_daily('y', startYear=1996, nodes=seq(1, 365, 9), 
                              variance = 0.01, fixed = FALSE)
n<-rjd3sts::noise('n')
rjd3sts::add(model, ll)
rjd3sts::add(model, seas1)
rjd3sts::add(model, seas2)
rjd3sts::add(model, rjd3sts::reg("cal", hol, 0.1))
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)
m1<-rjd3sts::smoothed_components(rslt)

n<-length(y)
idx<-(n-366*2):n

plot(idx, m1[idx, 3], "l")
lines(idx, m1[idx,2], col='red')
lines(idx, m1[idx,4], col='blue')


