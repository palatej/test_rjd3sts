test<-function(z){
  q<-rjd3sts::tdairline_estimation(z)
  return (q$ltd_sarima$likelihood- q$sarima$likelihood)
}

all<-sapply(rjd3toolkit::Retail, function(z) test(z))

hist(all, breaks=10)
print(all)
