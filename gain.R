###########################################
# X11 COMPOSITE LINEAR FILTERS
###########################################

# D2x12 is the trend-filter of 12-terms (2x12) centred MA (iteracion B tabla B2)
n<-6
L<-13
D2x12<-rep(0,L)
varpass<-0
pesos<-0
for(j in -n:n)
{
  if (j==-n | j==n) {D2x12[n+j+1]<-1/24}
  if (j>-n & j<n){D2x12[n+j+1]<-1/12}
  
  varpass <- varpass+(D2x12[n+j+1])^2
  print(varpass)
  
}

varpass
reduction<-1-varpass
reduction
D2x12
indice<-seq(1:L)-n-1
coef<-data.frame(indice,D2x12)
plot(indice,D2x12,type='l')
abline(v=indice)
title("D2x12")




freqResp <- function(filter)
{
  #	frf for asymmetric filter
  #		filter is a sequence of coefficients
  #		format: Y[t] = filter[1]*X[t+start] + filter[2]*X[t+start-1] + ... + filter[q]*X[t+start-q+1]
  #		so set start = 0 for a causal filter
  #		set start = (q-1)/2 for a symmetric filter
  lam <- seq(0,1000)*pi/1000
  q <- length(filter)
  start <- (q-1)/2
  frf <- rep(0,1001)
  for(j in 1:q)
  {
    frf <- frf + exp(1i*(start-j+1)*lam)*filter[j]	
  }
  gain <- Mod(frf)
  pdelay <- -1*Arg(frf)/lam
  return(cbind(frf,gain,pdelay))
}

D12<-freqResp(D2x12)
D12<-as.data.frame(D12)
Gain<-D12$gain
length(Gain)

w <- seq(0,1000)/2000

harmonics<-c(0, 1/12,2/12,3/12,4/12,5/12,6/12)
plot(w, Gain,type='l')
abline(v=harmonics)