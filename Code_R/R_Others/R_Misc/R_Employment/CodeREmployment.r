rm(list=ls())
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Others/R_Misc/R_Employment")

library(car)
library(tseries)
library(forecast)

data1 = read.csv("DataEmployment1.csv",header=TRUE)
caemp = ts(data1[,"CAEMP"],start=c(1961,1),frequency=4)
caempsamp = ts(caemp[5:132],start=c(1962,1),frequency=4)

# dev.new()
par(mfrow=c(1,1))

par(xaxs="i",yaxs="i")
ts.plot(caemp, main="Figure 7.9 Canadian Employment Index",ylab = "Canadian Employment Index", xlim=c(1962,1993.75),ylim=c(80,115))
# dev.copy2pdf(file="fig_canadian_employment_index.pdf")

plot(acf(caempsamp,plot=F),xlim=c(0,3),ylim=c(-1,1),main="Autocorrelation")
plot(acf(caempsamp,plot=F,type="p"),xlim=c(0,3),ylim=c(-1,1), main="Partial Autocorrelation")

# dev.new()
par(mfrow=c(2,1), xaxs="i",yaxs="i")
plot(acf(caempsamp,plot=F),xlim=c(0,3),ylim=c(-1,1),main="Autocorrelation")
plot(acf(caempsamp,plot=F,type="p"),xlim=c(0,3),ylim=c(-1,1), main="Partial Autocorrelation")
# dev.copy2pdf(file="fig_canadian_employment_corr.pdf")


data2 = read.csv("DataEmployment2.csv",header=TRUE)
caemp = ts(data2[,"CAEMP"],start=c(1961,1),frequency=4)


# Fig 8.16 Employment ARMA(3,1) Model Residual Plot-------------
caemp.arma.model <- arma(caemp, order=c(3,1))
fitted.arma <- caemp.arma.model$fitted.values
resid.arma <- caemp.arma.model$residuals

par(mfrow=c(1,1))
plot(caemp,type="l", yaxt='n', ylim=c(50,120), xlab="",ylab="",col="red", main="Fig 8.16 Employment ARMA(3,1) Model Residual Plot")
lines(fitted.arma,col="green")
axis(side=4,at=c(80,90,100,110,120),lab=c(80,90,100,110,120),cex.axis=0.8)
par(new=T)
plot(resid.arma,col="blue", axes=FALSE, ylim=c(-4,20), xlab="",ylab="")
abline(h=0,col="black")
abline(h=1.4,col="black",lty="dashed")
abline(h=-1.4,col="black",lty="dashed")
axis(side=2,at=c(-4,-2,0,2,4,6,8),lab=c(-4,-2,0,2,4,6,8) ,cex.axis=0.6)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.5)


# Fig 8.12 Employment MA(4) Model Residual Plot 
caemp.ma.model <- arma(caemp,order=c(0,4))
fitted.ma <- caemp.ma.model$fitted.values
resid.ma <- caemp.ma.model$residuals

plot(caemp,type="l", yaxt='n', ylim=c(50,120), xlab="",ylab="",col="red", main="Fig 8.16 Employment MA(4) Model Residual Plot")
lines(fitted.ma,col="green")
axis(side=4,at=c(80,90,100,110,120),lab=c(80,90,100,110,120),cex.axis=0.8)
par(new=T)
plot(resid.ma,col="blue", axes=FALSE, ylim=c(-10,20), xlab="",ylab="")
abline(h=0,col="black")
abline(h=1.4,col="black",lty="dashed")
abline(h=-1.4,col="black",lty="dashed")
axis(side=2,at=c(-10,-5,0,5,10),lab=c(-10,-5,0,5,10) ,cex.axis=0.6)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.5)


# Fig 8.14 Employment AR(2) Model Residual Plot-------------
caemp.ar.model <- arma(caemp, order=c(2,0))
fitted.ar <- caemp.ar.model$fitted.values
resid.ar <- caemp.ar.model$residuals

par(mfrow=c(1,1))
plot(caemp,type="l", yaxt='n', ylim=c(50,120), xlab="",ylab="",col="red", main="Fig 8.16 Employment AR(2) Model Residual Plot")
lines(fitted.ar,col="green")
axis(side=4,at=c(80,90,100,110,120),lab=c(80,90,100,110,120),cex.axis=0.8)
par(new=T)
plot(resid.ar,col="blue", axes=FALSE, ylim=c(-4,20), xlab="",ylab="")
abline(h=0,col="black")
abline(h=1.4,col="black",lty="dashed")
abline(h=-1.4,col="black",lty="dashed")
axis(side=2,at=c(-4,-2,0,2,4,6,8),lab=c(-4,-2,0,2,4,6,8) ,cex.axis=0.6)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.5)


# Fig 9.1 Employment History and Forecast MA(4) Model--------
caemp.ma.pred <- predict(arima(caemp[1:132], order=c(0,0,4)), n.ahead=4)
caemp.ma.pred.forecast <- caemp.ma.pred$pred

caemp.ma.pred.se.lower <- NULL
caemp.ma.pred.se.upper <- NULL
for (i in seq_along(caemp.ma.pred.forecast)) {
  caemp.ma.pred.se.lower[16+i] <- caemp.ma.pred.forecast[i]-(caemp.ma.pred$se[i]*1.96)
  caemp.ma.pred.se.upper[16+i] <- caemp.ma.pred.forecast[i]+(caemp.ma.pred$se[i]*1.96)
}


caemp.history <- caemp[116:132]
caemp.history <- as.ts(caemp.history, frequency=4)

caemp.forecast <- NULL
for (j in seq_along(caemp.ma.pred.forecast)) {
  caemp.forecast[16+j] <- caemp.ma.pred.forecast[j]
}
caemp.forecast <- as.ts(caemp.forecast, frequency=4)

plot(caemp.history, type="l",xlim=c(1,20),ylim=c(80,120), ylab="History and Forecast",xlab="Time",axes=F, main="Fig 9.1 Employment History and Forecast MA(4) Model")
lines(caemp.forecast, col="red")
lines(caemp.ma.pred.se.lower,col="blue")
lines(caemp.ma.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")


# Fig 9.2 Employment History and Long Horizon Forecast MA(4) Model
caemp.ma.pred.lh <- predict(arima(caemp[1:132], order=c(0,0,4)), n.ahead=12)
caemp.ma.pred.forecast.lh <- caemp.ma.pred.lh$pred

caemp.ma.pred.lh.se.lower <- NULL
caemp.ma.pred.lh.se.upper <- NULL
for (i in seq_along(caemp.ma.pred.forecast.lh)) {
  caemp.ma.pred.lh.se.lower[16+i] <- caemp.ma.pred.forecast.lh[i]-(caemp.ma.pred.lh$se[i]*1.96)
  caemp.ma.pred.lh.se.upper[16+i] <- caemp.ma.pred.forecast.lh[i]+(caemp.ma.pred.lh$se[i]*1.96)
}

caemp.history.lh <- caemp[116:132]
caemp.history.lh <- as.ts(caemp.history.lh, frequency=4)

caemp.forecast.lh <- NULL
for (j in seq_along(caemp.ma.pred.forecast.lh)) {
  caemp.forecast.lh[16+j] <- caemp.ma.pred.forecast.lh[j]
}
caemp.forecast.lh <- as.ts(caemp.forecast.lh, frequency=4)

plot(caemp.history.lh, xlim=c(1,28), ylim=c(80,120), ylab="History and Forecast",xlab="Time",axes=F, main="Fig 9.1 Employment History and Long Horizon Forecast MA(4) Model")
lines(caemp.forecast.lh, col="red")
lines(caemp.ma.pred.lh.se.lower,col="blue")
lines(caemp.ma.pred.lh.se.upper,col="blue")
box()
axis(side=1, at=seq(1,28), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4,95.1,95.2,95.3,95.4,96.1,96.2,96.3,96.4))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")


# Fig 9.3 Employment History, Forecast and Realization MA(4) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,120), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Fig 9.3 Employment History, Forecast and Realization MA(4) Model")
lines(caemp.forecast, col="red")
lines(caemp.ma.pred.se.lower,col="blue")
lines(caemp.ma.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")


# Fig 9.4 Employment History and Forecast AR(2) Model
caemp.ar.pred <- predict(arima(caemp[1:132], order=c(2,0,0)), n.ahead=4)
caemp.ar.pred.forecast <- caemp.ar.pred$pred

caemp.ar.pred.se.lower <- NULL
caemp.ar.pred.se.upper <- NULL
for (i in seq_along(caemp.ar.pred.forecast)) {
  caemp.ar.pred.se.lower[16+i] <- caemp.ar.pred.forecast[i]-(caemp.ar.pred$se[i]*1.96)
  caemp.ar.pred.se.upper[16+i] <- caemp.ar.pred.forecast[i]+(caemp.ar.pred$se[i]*1.96)
}


caemp.history <- caemp[116:132]
caemp.history <- as.ts(caemp.history, frequency=4)

caemp.forecast <- NULL
for (j in seq_along(caemp.ar.pred.forecast)) {
  caemp.forecast[16+j] <- caemp.ar.pred.forecast[j]
}
caemp.forecast <- as.ts(caemp.forecast, frequency=4)

plot(caemp.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Fig 9.4 Employment History and Forecast AR(2) Model")
lines(caemp.forecast, col="red")
lines(caemp.ar.pred.se.lower,col="blue")
lines(caemp.ar.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")


# Fig 9.5 Employment History and Long Run Forecast AR(2) Model
caemp.ar.pred.lh <- predict(arima(caemp[1:132], order=c(2,0,0)), n.ahead=12)
caemp.ar.pred.forecast.lh <- caemp.ar.pred.lh$pred

caemp.ar.pred.lh.se.lower <- NULL
caemp.ar.pred.lh.se.upper <- NULL
for (i in seq_along(caemp.ar.pred.forecast.lh)) {
  caemp.ar.pred.lh.se.lower[16+i] <- caemp.ar.pred.forecast.lh[i]-(caemp.ar.pred.lh$se[i]*1.96)
  caemp.ar.pred.lh.se.upper[16+i] <- caemp.ar.pred.forecast.lh[i]+(caemp.ar.pred.lh$se[i]*1.96)
}

caemp.history.lh <- caemp[116:132]
caemp.history.lh <- as.ts(caemp.history.lh, frequency=4)

caemp.forecast.lh <- NULL
for (j in seq_along(caemp.ar.pred.forecast.lh)) {
  caemp.forecast.lh[16+j] <- caemp.ar.pred.forecast.lh[j]
}
caemp.forecast.lh <- as.ts(caemp.forecast.lh, frequency=4)

plot(caemp.history.lh, xlim=c(1,28), ylim=c(75,110), ylab="History and Forecast",xlab="Time",axes=F, main="Fig 9.5 Employment History and Long Horizon Forecast AR(2) Model")
lines(caemp.forecast.lh, col="red")
lines(caemp.ar.pred.lh.se.lower,col="blue")
lines(caemp.ar.pred.lh.se.upper,col="blue")
box()
axis(side=1, at=seq(1,28), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4,95.1,95.2,95.3,95.4,96.1,96.2,96.3,96.4))
axis(side=2, at=seq(80,110,by=5))
abline(v=17,lty="dashed")


# 9.7 Employment History, Forecast, and Realization AR(2) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,120), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Fig 9.7 Employment History, Forecast and Realization AR(2) Model")
lines(caemp.forecast, col="red")
lines(caemp.ar.pred.se.lower,col="blue")
lines(caemp.ar.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")


# 9.6 Employment History and Very Long-Horizon Forecast AR(2) Model
caemp.ar.pred.vlh <- predict(arima(caemp[1:132], order=c(2,0,0)), n.ahead=64)
caemp.ar.pred.forecast.vlh <- caemp.ar.pred.vlh$pred

caemp.ar.pred.vlh.se.lower <- NULL
caemp.ar.pred.vlh.se.upper <- NULL
for (i in seq_along(caemp.ar.pred.forecast.vlh)) {
  caemp.ar.pred.vlh.se.lower[16+i] <- caemp.ar.pred.forecast.vlh[i]-(caemp.ar.pred.vlh$se[i]*1.96)
  caemp.ar.pred.vlh.se.upper[16+i] <- caemp.ar.pred.forecast.vlh[i]+(caemp.ar.pred.vlh$se[i]*1.96)
}

caemp.history.vlh <- caemp[116:132]
caemp.history.vlh <- as.ts(caemp.history.vlh, frequency=4)

caemp.forecast.vlh <- NULL
for (j in seq_along(caemp.ar.pred.forecast.vlh)) {
  caemp.forecast.vlh[16+j] <- caemp.ar.pred.forecast.vlh[j]
}
caemp.forecast.vlh <- as.ts(caemp.forecast.vlh, frequency=4)

plot(caemp.history.vlh, xlim=c(1,84), ylim=c(75,120), ylab="History and Forecast",xlab="Time",axes=F, main="Fig 9.6 Employment History and Very Long-Horizon Forecast AR(2) Model")
lines(caemp.forecast.vlh, col="red")
lines(caemp.ar.pred.vlh.se.lower,col="blue")
lines(caemp.ar.pred.vlh.se.upper,col="blue")
box()
axis(side=1, at=seq(1,84, by=4), lab=c(90,91,92,93,94,95,96,97,98,99,00,01,02,03,04,05,06,07,08,09,10))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")