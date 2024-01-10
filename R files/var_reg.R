# test

## Data
B1G_MANUF<-c(
  3.4, 2.5, 3.0,2.3, 0, -1.2, -3.3, -2.1, 0.7, 2.9, 1.4, 1.7, -0.9, -0.7, 1.3, 1.1, 2.8, 3.4, 
  5.3, 4.7, 2.8, -11.7, -4.1, -2.3, -3.5, 9.9, -2.5, -2.8, -0.1, 1, 2.2, -0.3
)
B1G_MANUF<-ts(B1G_MANUF, start=c(2015,1),frequency = 4)

BS_MANUF<-c(
  -9.3, -6.2, -7.3, -5.2, -8.5, -4.1, -3.6, -3.4, -3.2, -3.4, -5.5, -0.8, -1.4, -2.5, -1.9, -3.7, 
  -3.6, -7.1, -9.6, -7.5, -7.2, -33.1, -14.4, -7.4, -3.5, 6.1, 6.7, 2.6, -0.5, -0.5, -7.9, -19
)
BS_MANUF<-ts(BS_MANUF, start=c(2015,1),frequency = 4)

## Modeling
noise<-rjd3sts::noise("noise", variance = 1, fixed = TRUE)
#vreg<-var_reg("x", BS_MANUF, rep(1,length(BS_MANUF)), scale=1, fixed=T)
#vreg<-rjd3sts::reg("x", BS_MANUF, 1, F)
vreg<-reg("x", BS_MANUF)
model<-rjd3sts::model()
rjd3sts::add(model, vreg)
rjd3sts::add(model, noise)
eq<-rjd3sts::equation("eq")
rjd3sts::add_equation(eq, "x")
rjd3sts::add_equation(eq, "noise")
rjd3sts::add(model, eq)
rslt4<-rjd3sts::estimate(model, B1G_MANUF, marginal = T, initialization = "Augmented_Robust")
rjd3toolkit::result(rslt4,"ssf.smoothing.states")
rjd3toolkit::result(rslt4,"parameters")
rjd3toolkit::result(rslt4,"scalingfactor")

ss<-rjd3sts::smoothed_states(rslt4)

ss[,1]*BS_MANUF+ss[,2]
# !!! smoothed_states[,1] renvoie déjà beta_t * x_t et pas juste beta_t ?

#smoothed_states(rslt4)[,1]/BS_MANUF
rjd3toolkit::result(rslt4,"ssf.T(*)")
rjd3toolkit::result(rslt4,"ssf.Z(*)")
rjd3toolkit::result(rslt4,"ssf.V(*)")
