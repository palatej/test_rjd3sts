example_1<-function(data,lvar=0.1, svar=0.1){
  ll<-rjd3sts::.local_linear_trend(lvar)
  s<-rjd3sts::.seasonal(12, type="HarrisonStevens", var=svar)
  m<-rjd3sts::.composite(list(ll, s))
  ssf<-rjd3sts::.ssf(m, rjd3sts::.loading(c(0,2)), 1)
  ss<-rjd3sts::.ssf_smooth(ssf, data, qtype = "NORMAL")
}

lvar<-seq(0.01, 0.3, 0.01)
q<-sapply(lvar, function(v){example_1(data = log(rjd3toolkit::ABS$X0.2.04.10.M), lvar=v)[,1]})

matplot(q, type='l')