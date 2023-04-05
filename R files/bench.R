library(rjd3sts)

Y<-rjd3toolkit::aggregate(rjd3toolkit::retail$RetailSalesTotal, 1)
Yc<-matrix(nrow=12*length(Y), ncol=1)
Yc[12*(1:length(Y)),1]<-Y

stoch<-locallevel("rw")
#stoch<-ar("ar", 0, fixedar = T )
reg<-reg("x", rjd3toolkit::retail$RetailAndFoodServicesSalesTotal, 1)
#all<-aggregation("m", list(reg, stoch))
c<-cumul("c", reg, 12)

model<-model()
add(model, c)

eq<-equation("eq")
add_equation(eq, "c")

add(model, eq)

rslt<-estimate(model, Yc)

ratio<-rjd3toolkit::result(rslt, "ssf.smoothing.array(1)")
a<-rjd3toolkit::retail$RetailAndFoodServicesSalesTotal*ratio

ts.plot(ts.union(a, rjd3toolkit::retail$RetailSalesTotal), col=c("red", "blue"))

print(summary(rjd3toolkit::aggregate(a-rjd3toolkit::retail$RetailSalesTotal)))

plot(a, type='l')
n<-length(rjd3toolkit::retail$RetailAndFoodServicesSalesTotal)
M=matrix(rep(1,n), nrow = n, ncol = 1)
t2<-msignal(rslt, M, 2)
et2<-msignal(rslt, M, 2, T)
plot(t2, type='l')
plot(et2, type='l')

x<-rep(1,n)
x[c(18,205)]<-2.5

vreg<-var_reg("x", rjd3toolkit::retail$RetailAndFoodServicesSalesTotal, x, scale=1, fixed=T)
#all<-aggregation("m", list(reg, stoch))
vc<-cumul("c", vreg, 12)

vmodel<-model()
add(vmodel, vc)

veq<-equation("eq")
add_equation(veq, "c")

add(vmodel, veq)

vrslt<-estimate(vmodel, Yc)

vratio<-rjd3toolkit::result(vrslt, "ssf.smoothing.array(1)")
va<-rjd3toolkit::retail$RetailAndFoodServicesSalesTotal*vratio

ts.plot(ts.union(va, rjd3toolkit::retail$RetailSalesTotal), col=c("red", "blue"))

print(summary(rjd3toolkit::aggregate(va-rjd3toolkit::retail$RetailSalesTotal)))

vt2<-msignal(vrslt, M, 2)
vet2<-msignal(vrslt, M, 2, T)
plot(vt2, type='l')
lines(t2, col='red')

plot(vet2, type='l')
lines(et2, col='red')
