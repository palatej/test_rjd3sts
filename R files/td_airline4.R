test<-function(z){
  q<-rjd3toolkitx::tdairline_estimation(z)
  return (q$ltd_sarima$likelihood- q$sarima$likelihood)
}

all<-sapply(rjd3toolkit::retail, function(z) test(z))

hist(all)
print(all)
