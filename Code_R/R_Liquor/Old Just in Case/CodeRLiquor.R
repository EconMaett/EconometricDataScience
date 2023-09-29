##House Keeping
rm(list=ls())
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Liquor")

##Packages
library(forecast)
library(plotrix)

##Fig 5.5 Various Linear Trends
#pdf(file="Fig 5.5 Various Linear Trends.pdf",height=6,width=7.5)
plot(x=NULL,y=NULL,xlim=c(0,100),ylim=c(-60,40),xlab="Time",ylab="Trend")
abline(a=10,b=-0.25)
abline(a=-50,b=0.8,lty="dashed")
text(20,15,"Trend=10-0.25*Time")
text(32,-42,"Trend=-50+.8*Time")
#dev.off()

##Fig 5.6 Liquor Sales
liquor=read.csv("DataLiquor.csv",header=TRUE)
liquor=na.omit(liquor)
names(liquor)[1] <- "liquor"
lsts=ts(data=liquor,start=c(1987,1),frequency=12)

#pdf(file="Fig 5.6 Liquor Sales.pdf",height=6,width=7.5)
plot(lsts,axes=FALSE,xlim=c(1988-1/12,2014),ylim=c(0,3000),xlab="Time",ylab="Liquor Sales",col="blue")
box()
axis(side=1,at=1987:2014,lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(0,500,1000,1500,2000,2500,3000),lab=c(0,500,1000,1500,2000,2500,3000),cex.axis=0.75)
#dev.off()

##Fig 5.7 Log Liquor Sales
logliquor=log(liquor)
names(logliquor)[1] <- "logliquor"
loglsts=ts(data=logliquor,start=c(1987,1),frequency=12)

#pdf(file="Fig 5.7 Log Liquor Sales.pdf",height=6,width=7.5)
plot(loglsts,axes=FALSE,xlim=c(1988-1/12,2014),ylim=c(6.0,8.0),xlab="Time",ylab="Log Liquor Sales",col="blue")
box()
axis(side=1,at=1987:2014,lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(6.0,6.4,6.8,7.2,7.6,8.0),lab=c(6.0,6.4,6.8,7.2,7.6,8.0),cex.axis=0.9)
#dev.off()

##Fig 5.8 Linear Trend Estimation
time=data.frame(c(1:336))
names(time)[1] <- "time"
logls=data.frame(time,logliquor)
linear.mod=lm(logliquor~time,data=logls)

print(summary(linear.mod))

##Fig 5.9 Residual Plot, Linear Trend Estimation
logls.act=logls$logliquor
logls.pred=predict(linear.mod,logls)
logls.resid=logls.act-logls.pred
logls.resid.plot=1.5*logls.resid+5.8

#pdf(file="Fig 5.9 Residual Plot, Linear Trend Estimation.pdf",height=6,width=7.5)
plot(logls.act,type="l",axes=FALSE,ylim=c(5.2,8.0),xlab="",ylab="",col="red")
lines(logls.pred,col="green")
lines(logls.resid.plot,col="blue")
abline(h=5.8,col="black")
abline(h=6.0,col="black",lty="dashed")
abline(h=5.6,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(5.2,5.5,5.8,6.1,6.4,6.7),lab=c(-0.4,-0.2,0,0.2,0.4,0.6),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.91)

legend(1,7.99,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.75)
#dev.off()

##Fig 5.10 Linear Trend Estimation with Seasonal Dummies
mond=seasonaldummy(loglsts)
Dec=matrix(data=rep(c(rep(0,11),1),28),nrow=336,ncol=1)
logls.seas=data.frame(cbind(logliquor,time,mond,Dec))
linear.seas.mod=lm(logliquor~.-1,data=logls.seas)

print(summary(linear.seas.mod))

##Fig 5.11 Residual Plot, Linear Trend Estimation with Seasonal Dummies
logls.seas.act=logls.seas$logliquor
logls.seas.pred=predict(linear.seas.mod,logls.seas)
logls.seas.resid=logls.seas.act-logls.seas.pred
logls.seas.resid.plot=6.0*logls.seas.resid+5.8

#pdf(file="Fig 5.11 Residual Plot, Linear Trend Estimation with Seasonal Dummies.pdf",height=6,width=7.5)
plot(logls.seas.act,type="l",axes=FALSE,ylim=c(4.9,8.0),xlab="",ylab="",col="red")
lines(logls.seas.pred,col="green")
lines(logls.seas.resid.plot,col="blue")
abline(h=5.8,col="black")
abline(h=6.45,col="black",lty="dashed")
abline(h=5.15,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.9,5.2,5.5,5.8,6.1,6.4,6.7),lab=c(-0.15,-0.10,-0.05,0,0.05,0.10,0.15),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.91)

legend(1,7.99,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.75)
#dev.off()

##Fig 5.12 Seasonal Pattern
moncoef=data.frame(coef(linear.seas.mod))
moncoef <- moncoef[-1,]

#pdf(file="Fig 5.12 Seasonal Pattern.pdf",height=6,width=7.5)
plot(moncoef,type="l",axes=FALSE,xlab="Estimated Seasonal Factors",ylab="Factor",col="blue")
box()
axis(side=1,at=c(1:12),lab=c(paste("M",1:12)),cex.axis=0.75)
axis(side=2,at=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9),lab=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9),cex.axis=1)
#dev.off()

##Fig 8.7 Log-Quadratic Trend Estimation
quad.mod=lm(logliquor~time+I(time^2),data=logls)

print(summary(quad.mod))

##Fig 8.8 Residual Plot, Log-Quadratic Trend Estimation
logqls.act=logls$logliquor
logqls.pred=predict(quad.mod,logls)
logqls.resid=logqls.act-logqls.pred
logqls.resid.plot=1.5*logqls.resid+5.5

#pdf(file="Fig 8.8 Residual Plot, Log-Quadratic Trend Estimation.pdf",height=6,width=7.5)
plot(logqls.act,type="l",axes=FALSE,ylim=c(4.9,8.0),xlab="",ylab="",col="red")
lines(logqls.pred,col="green")
lines(logqls.resid.plot,col="blue")
abline(h=5.5,col="black")
abline(h=5.7,col="black",lty="dashed")
abline(h=5.3,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.9,5.2,5.5,5.8,6.1,6.4),lab=c(-0.4,-0.2,0,0.2,0.4,0.6),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.85)

legend(1,7.99,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.75)
#dev.off()

##Fig 8.9 Log-Quadratic Trend Estimation with Seasonal Dummies
logqls.seas=data.frame(cbind(logliquor,time,I(time^2),mond,Dec))
quad.seas.mod=lm(logliquor~.-1,data=logqls.seas)

print(summary(quad.seas.mod))

##Fig 8.10 Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dummies
logqls.seas.act=logls.seas$logliquor
logqls.seas.pred=predict(quad.seas.mod,logqls.seas)
logqls.seas.resid=logqls.seas.act-logqls.seas.pred
logqls.seas.resid.plot=7*logqls.seas.resid+5.55

#pdf(file="Fig 8.10 Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dummies.pdf",height=6,width=7.5)
plot(logqls.seas.act,type="l",axes=FALSE,ylim=c(4.5,8.0),xlab="",ylab="",col="red")
lines(logqls.seas.pred,col="green")
lines(logqls.seas.resid.plot,col="blue")
abline(h=5.55,col="black")
abline(h=5.9,col="black",lty="dashed")
abline(h=5.2,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.5,4.85,5.2,5.55,5.9,6.25,6.6),lab=c(-0.15,-0.10,-0.05,0,0.05,0.10,0.15),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.8)

legend(1,7.99,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.75)
#dev.off()

##Fig 10.2.1 Scatterplot of e_t against e_t-1
logqls.seas.resid.l1 <- as.vector(NULL)
logqls.seas.resid.l1[1] <- 0
obs <- length(logqls.seas.resid)
i=0
for (i in 2:obs) {
  logqls.seas.resid.l1[i] <- logqls.seas.resid[i-1]
}

#pdf(file="Fig 10.2.1 Scatterplot of e_t against e_t-1.pdf")
plot(logqls.seas.resid.l1,logqls.seas.resid,axes=FALSE,cex=0.8,xlim=c(-0.12,0.16),ylim=c(-0.12,0.16),col="blue",xlab="RESIDL1",ylab="RESID")
box()
axis(side=1,at=c(seq(-0.12,0.16,by=0.04)),lab=c(seq(-0.12,0.16,by=0.04)))
axis(side=2,at=c(seq(-0.12,0.16,by=0.04)),lab=c(seq(-0.12,0.16,by=0.04)))
#dev.off()

##Fig 10.2.1 The Durbin-Watson Test - AR(1) Disturbance
logqls.seas.ar1=data.frame(cbind(logqls.seas[,-1],logqls.seas.resid.l1))
names(logqls.seas.ar1) <- c("TIME","TIME2",names(logqls.seas.ar1)[3:14],"RESID(-1)")
quad.seas.ar1.mod=lm(logqls.seas.resid~.-1,data=logqls.seas.ar1)

print(summary(quad.seas.ar1.mod))

##Fig 10.2.1 The Breusch-Godfrey Test - AR(4) Disturbance
logqls.seas.resid.l <- matrix(NA,nrow=obs,ncol=8)
l=0
for (l in 1:8) {
  for (i in 1:l) {
    logqls.seas.resid.l[i,l] <- 0
  }
}
l=0
for (l in 1:8) {
  for (i in (1+l):obs) {
    logqls.seas.resid.l[i,l] <- logqls.seas.resid[i-l]
  }
}

names.resid <- as.vector(NULL)
i=0
for (i in 1:8) {
  names.resid[i] <- paste("RESID(-",toString(i),")",sep="")
}

logqls.seas.ar4=data.frame(cbind(logqls.seas[,-1],logqls.seas.resid.l[,1:4]))
names(logqls.seas.ar4) <- c("TIME","TIME2",names(logqls.seas.ar4)[3:14],names.resid[1:4])
quad.seas.ar4.mod=lm(logqls.seas.resid~.-1,data=logqls.seas.ar4)

print(summary(quad.seas.ar4.mod))

##Fig 10.2.1 The Breusch-Godfrey Test - AR(8) Disturbance
logqls.seas.ar8=data.frame(cbind(logqls.seas[,-1],logqls.seas.resid.l))
names(logqls.seas.ar8) <- c("TIME","TIME2",names(logqls.seas.ar8)[3:14],names.resid)
quad.seas.ar8.mod=lm(logqls.seas.resid~.-1,data=logqls.seas.ar8)

print(summary(quad.seas.ar8.mod))

##Fig 10.2.3 Regression with Serially-Correlated Disturbances
logqls.seas.resid.acf <- acf(logqls.seas.resid,type="correlation",plot=FALSE)[1:obs]
logqls.seas.resid.pacf <- pacf(logqls.seas.resid,plot=FALSE)

#pdf(file="Fig 10.2.3 Correlogram of Residuals_Autocorrelation.pdf",height=6,width=7.5)
plot(logqls.seas.resid.acf,col="blue",xlab="Lag",ylab="Autocorrelation",main="")
#dev.off()

#pdf(file="Fig 10.2.3 Correlogram of Residuals_Partial Autocorrelation.pdf",height=6,width=7.5)
plot(logqls.seas.resid.pacf,col="blue",xlab="Lag",ylab="Partial autocorrelation",main="")
#dev.off()

##Fig 10.2.4 Serially-Correlated Disturbances vs. Lagged Dependent Variables
logqls.seas.l <- matrix(NA,nrow=obs-4,ncol=4)
l=0
for (l in 1:4) {
  for (i in 1:(obs-4)) {
    logqls.seas.l[i,l] <- logliquor[i-l+4,1]
  }
}

names.l <- as.vector(NULL)
i=0
for (i in 1:4) {
  names.l[i] <- paste("LSALES(-",toString(i),")",sep="")
}

logqls.seas.l4=data.frame(logqls.seas[5:obs,],logqls.seas.l)
names(logqls.seas.l4) <- c(names(logqls.seas.l4)[1],"TIME","TIME2",names(logqls.seas.l4)[4:15],names.l)
quad.seas.l4.mod=lm(logliquor~.-1,data=logqls.seas.l4)

print(summary(quad.seas.l4.mod))

##Fig 10.2.4 Serially-Correlated Disturbances vs. Lagged Dependent Variables (Figure 10.1)
coef.l <- quad.seas.l4.mod$coefficient[15:18]
v.seas <- matrix(NA,nrow=obs-4,ncol=ncol(logqls.seas)-1)
for (i in 2:ncol(logqls.seas)) {
  for (j in 5:obs) {
    prep.l <- 0
    for (l in 1:4) {
      prep.l <- prep.l+logqls.seas[j-l,i]*coef.l[l]
    }
    v.seas[j-4,i-1] <- logqls.seas[j,i]-prep.l
  }
}

names.ll <- as.vector(NULL)
i=0
for (i in 1:4) {
  names.ll[i] <- paste("AR(",toString(i),")",sep="")
}

logqls.seas.l4.ar=data.frame(logliquor[5:obs,1],v.seas,logqls.seas.l)
names(logqls.seas.l4.ar) <- c("logliquor","TIME","TIME2",names(logqls.seas.l4)[4:15],names.ll)
quad.seas.l4.ar.mod=lm(logliquor~.-1,data=logqls.seas.l4.ar)

print(summary(quad.seas.l4.ar.mod))

##Fig 10.2 Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dummies and AR(4) Disturbance
logqls.seas.l4.act=logls.seas$logliquor[5:obs]
logqls.seas.l4.pred=predict(quad.seas.l4.ar.mod,logqls.seas.l4.ar)
logqls.seas.l4.resid=logqls.seas.l4.act-logqls.seas.l4.pred
logqls.seas.resid.l4.plot=8.5*logqls.seas.l4.resid+5.4

#pdf(file="Fig 10.2 Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dummies and AR(4) Disturbance.pdf",height=6,width=7.5)
plot(logqls.seas.l4.act,type="l",axes=FALSE,ylim=c(4.5,8.0),xlab="",ylab="",col="red")
lines(logqls.seas.l4.pred,col="green")
lines(logqls.seas.resid.l4.plot,col="blue")
abline(h=5.4,col="black")
abline(h=5.7,col="black",lty="dashed")
abline(h=5.1,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.38,4.72,5.06,5.4,5.74,6.08),lab=c(-0.12,-0.08,-0.04,0,0.04,0.08),cex.axis=0.6)
axis(side=4,at=c(6.0,6.4,6.8,7.2,7.6,8.0),lab=c(6.0,6.4,6.8,7.2,7.6,8.0),cex.axis=0.8)
axis.break(axis=4,breakpos=5.8)

legend(1,7.99,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.75)
#dev.off()

##Fig 10.3 Correlogram of Residuals
logqls.seas.resid.l4.acf <- acf(logqls.seas.l4.resid,type="correlation",plot=FALSE)[1:length(logqls.seas.l4.resid)]
logqls.seas.resid.l4.pacf <- pacf(logqls.seas.l4.resid,plot=FALSE)

#pdf(file="Fig 10.3 Correlogram of Residuals_Autocorrelation.pdf",height=6,width=7.5)
plot(logqls.seas.resid.l4.acf,col="blue",xlab="Lag",ylab="Autocorrelation",main="")
#dev.off()

#pdf(file="Fig 10.3 Correlogram of Residuals_Partial Autocorrelation.pdf",height=6,width=7.5)
plot(logqls.seas.resid.l4.pacf,col="blue",xlab="Lag",ylab="Partial autocorrelation",main="")
#dev.off()
