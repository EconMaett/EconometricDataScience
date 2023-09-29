# House Keeping -----------------------------------------------------------

rm(list=ls())
workpath <- "C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Others/R_Housing"
setwd(workpath)


# Packages ----------------------------------------------------------------

library(forecast)
library(plotrix)
library(MSBVAR)
library(stats)

par(mfrow=c(1,1))

# Foundation --------------------------------------------------------------

housing <- read.csv("DataHousing.csv", header=TRUE)
starts <- as.ts(housing[,2], start=c(1968,1), frequency=12)
completions <- as.ts(housing[,3], start=c(1968,1), frequency=12)
row <- nrow(housing)


# Fig 16. US Housing Starts and Completions, 1968.01-1996.06 --------------

# pdf(file="Fig 16. U.S. Housing Starts and Completions, 1968.01-1996.06.pdf",height=6,width=7.5)
plot(starts, main="U.S. Housing Starts and Completions, 1968.01-1996.06", type="l", axes=FALSE, ylim=c(.5,3.5), ylab="Starts",xlab="Time")
lines(1.15*(completions+0.5), type="l", col="red")

box()
axis(side=1,at=c(seq(1,325,by=12)),lab=c(1968:1995),cex.axis=0.7)
axis(side=2,at=c(seq(0.5,3.0,by=0.5)),lab=c(0.5,1.0,1.5,2.0,2.5,3.0))
axis(side=4,at=c(seq(1.15,3.45,by=0.575)),lab=c(seq(0.5,2.5,by=0.5)))
mtext("Completions", side=4, line=0)
# dev.off()


# Fig 16. Starts Sample Autocorrelations and Partial Autocorrelati --------

starts.base <- as.ts(housing[1:288,2], frequency=12)

starts.acf <- acf(starts.base,type="correlation",plot=FALSE,lag.max=26)[1:24]
starts.pacf <- pacf(starts.base,plot=FALSE,lag.max=24)

# pdf(file="Fig 16. Starts Sample Autocorrelations.pdf",height=6,width=7.5)
plot(starts.acf,col="blue",xlab="Displacement",ylab="Starts Autocorrelation",main="Starts Sample Autocorrelations")
# dev.off()

# pdf(file="Fig 16. Starts Sample Partial Autocorrelations.pdf",height=6,width=7.5)
plot(starts.pacf,col="blue",xlab="Displacement",ylab="Starts Partial Autocorrelation",main="Starts Sample Partial Autocorrelations")
# dev.off()


# Fig 16. Completions Sample Autocorrelations and Partial Autocorr --------

completions.base <- as.ts(housing[1:288,3], frequency=12)

completions.acf <- acf(completions.base,type="correlation",plot=FALSE,lag.max=28)[1:24]
completions.pacf <- pacf(completions.base,plot=FALSE,lag.max=24)

# pdf(file="Fig 16. Completions Sample Autocorrelations.pdf",height=6,width=7.5)
plot(completions.acf,col="blue",xlab="Displacement",ylab="Completions Autocorrelation",main="Completions Sample Autocorrelations")
# dev.off()

# pdf(file="Fig 16. Completions Sample Partial Autocorrelations.pdf",height=6,width=7.5)
plot(completions.pacf,col="blue",xlab="Displacement",ylab="Completions Partial Autocorrelation",main="Completions Sample Partial Autocorrelations")
# dev.off()


# Fig 16. Starts and Completions Sample Cross Correlations ----------------

house.ccf <- ccf(starts.base,completions.base,type="correlation",plot=FALSE,lag.max=20)[-11:11]

# pdf(file="Fig 16. Starts and Completions Sample Cross Correlations.pdf",height=6,width=7.5)
plot(house.ccf,col="blue",xlab="Displacement",ylab="Starts and Completions Cross Correlations",main="Starts and Completions Sample Cross Correlations")
# dev.off()


# Fig 16. VAR Starts Equation ---------------------------------------------

starts.var <- as.vector(NULL)
comps.var <- as.vector(NULL)
i=1
while (housing$OBS[i] != "1992M01") {
  starts.var[i] <- housing$STARTS[i]
  comps.var[i] <- housing$COMPS[i]
  i=i+1
}

len=length(starts.var)
starts.var.reg <- matrix(NA,nrow=len-4,ncol=4)
comps.var.reg <- matrix(NA,nrow=len-4,ncol=4)
i=0
for (i in 1:(len-4)) {
  for (j in 1:4) {
    starts.var.reg[i,j] <- starts.var[i+4-j]
    comps.var.reg[i,j] <- comps.var[i+4-j]
  }
}

var.starts.l4 <- data.frame(starts.var[5:len],starts.var.reg,comps.var.reg)

names.starts <- as.vector(NULL)
names.comps <- as.vector(NULL)
i=0
for (i in 1:4) {
  names.starts[i] <- paste("STARTS(-",toString(i),")")
  names.comps[i] <- paste("COMPS(-",toString(i),")")
}
names(var.starts.l4) <- c("STARTS",names.starts,names.comps)

var.starts.l4.mod <- lm(STARTS~.,data=var.starts.l4)
print(summary(var.starts.l4.mod))


# Fig 16. VAR Starts Equation Residual Plot -------------------------------

var.starts.act <- starts.var[5:len]
var.starts.pred <- predict(var.starts.l4.mod,var.starts.l4)
var.starts.resid <- var.starts.act-var.starts.pred
var.starts.resid.plot=2*var.starts.resid+0.2

# pdf(file="Fig 16. VAR Starts Equation Residual Plot.pdf",height=6,width=7.5)
plot(var.starts.act,type="l",axes=FALSE,ylim=c(-1,3),xlab="",ylab="",main="VAR Starts Equation Residual Plot",col="red")
lines(var.starts.pred,col="green")
lines(var.starts.resid.plot,col="blue")
abline(h=0.2,col="black")
abline(h=0.45,col="black",lty="dashed")
abline(h=-0.05,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(8,284,by=12)),lab=c(1968:1991),cex.axis=0.7)
axis(side=2,at=c(-1,-0.6,-0.2,0.2,0.6,1,1.4),lab=c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6),cex.axis=0.7)
axis(side=4,at=c(seq(0.5,3.0,by=0.5)),lab=c(seq(0.5,3.0,by=0.5)),cex.axis=0.7)
axis.break(axis=4,breakpos=0)

legend(len-60,3,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.7)
# dev.off()


# Fig 16. VAR Starts Equation Residual Sample Autocorrelations and --------

var.starts.acf <- acf(var.starts.resid,type="correlation",plot=FALSE)[1:len]
var.starts.pacf <- pacf(var.starts.resid,plot=FALSE)

# pdf(file="Fig 16. VAR Starts Equation Residual Sample Autocorrelations.pdf",height=6,width=7.5)
plot(var.starts.acf,col="blue",xlab="Displacement",ylab="Starts Residual Autocorrelation",main="VAR Starts Equation Residual Sample Autocorrelations")
# dev.off()

# pdf(file="Fig 16. VAR Starts Equation Residual Sample Partial Autocorrelation.pdf",height=6,width=7.5)
plot(var.starts.pacf,col="blue",xlab="Displacement",ylab="Starts Residual Partial Autocorrelation",main="VAR Starts Equation Residual Sample Partial Autocorrelation")
# dev.off()


# Fig 16. VAR Completions Equation ----------------------------------------

var.comps.l4 <- data.frame(comps.var[5:len],starts.var.reg,comps.var.reg)
names(var.comps.l4) <- c("COMPS",names.starts,names.comps)
var.comps.l4.mod <- lm(COMPS~.,data=var.comps.l4)
print(summary(var.comps.l4.mod))


# Fig 16. VAR Completions Equation Residual Plot --------------------------

var.comps.act <- comps.var[5:len]
var.comps.pred <- predict(var.comps.l4.mod,var.comps.l4)
var.comps.resid <- var.comps.act-var.comps.pred
var.comps.resid.plot=4*var.comps.resid+0.2

# pdf(file="Fig 16. VAR Completions Equation Residual Plot.pdf",height=6,width=7.5)
plot(var.comps.act,type="l",axes=FALSE,ylim=c(-0.5,2.5),xlab="",ylab="",main="VAR Completions Equation Residual Plot",col="red")
lines(var.comps.pred,col="green")
lines(var.comps.resid.plot,col="blue")
abline(h=0.2,col="black")
abline(h=0.5,col="black",lty="dashed")
abline(h=-0.1,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(8,284,by=12)),lab=c(1968:1991),cex.axis=0.7)
axis(side=2,at=c(-0.6,-0.2,0.2,0.6,1,1.4,1.8),lab=c(-0.2,-0.1,0,0.1,0.2,0.3,0.4),cex.axis=0.7)
axis(side=4,at=c(seq(0.5,2.5,by=0.5)),lab=c(seq(0.5,2.5,by=0.5)),cex.axis=0.7)
axis.break(axis=4,breakpos=0)

legend(len-55,2.5,c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"),cex=0.65)
# dev.off()


# Fig 16. VAR Completions Equation Residual Sample Autocorrelation --------

var.comps.acf <- acf(var.comps.resid,type="correlation",plot=FALSE)[1:len]
var.comps.pacf <- pacf(var.comps.resid,plot=FALSE)

# pdf(file="Fig 16. VAR Completions Equation Residual Sample Autocorrelations.pdf",height=6,width=7.5)
plot(var.comps.acf,col="blue",xlab="Displacement",ylab="Completions Residual Autocorrelation",main="VAR Completions Equation Residual Sample Autocorrelations")
# dev.off()

# pdf(file="Fig 16. VAR Completions Equation Residual Sample Partial Autocorrelation.pdf",height=6,width=7.5)
plot(var.comps.pacf,col="blue",xlab="Displacement",ylab="Completions Residual Partial Autocorrelation",main="VAR Completions Equation Residual Sample Partial Autocorrelation")
# dev.off()


# Fig 16. Housing Starts and Completions Causality Tests ------------------

colnames <- list(rep(NULL,len),c("COMPS","STARTS"))
full <- matrix(cbind(comps.var,starts.var),nrow=len,ncol=2,byrow=FALSE,dimnames=colnames)
full.test <- granger.test(full,p=4)
print(full.test)


# Fig 16. Housing Starts and Completions VAR Impulse-Response Func --------

#im.res.fun <- VAR(cbind(comps.var,starts.var))
#plot(irf(im.res.fun))


# Fig 16. Starts History and Forecast -------------------------------------

reg.ar <- ar(starts.base)
pred.ar <- predict(reg.ar, n.ahead=54)

start.forecastandrealization <- NULL
start.forecastandrealization[1:288] <- starts.base
start.forecastandrealization[289:342] <- pred.ar$pred
start.forecastandrealization <- as.ts(start.forecastandrealization, frequency=12)

# pdf(file="Fig 16. Starts History and Forecast.pdf",height=6,width=7.5)
plot(start.forecastandrealization) + abline(v=288)
# dev.off()

# Fig 16. Forecasts and Realizations for Start ----------------------------

# pdf(file="Fig 16. Forecasts and Realizations for Start.pdf",height=6,width=7.5)
plot(start.forecastandrealization) + abline(v=288) + points(starts, type="l")
# dev.off()


# Fig 16. Completions History and Forecast --------------------------------

reg.ar <- ar(completions.base)
pred.ar <- predict(reg.ar, n.ahead=54)


completion.forecastandrealization <- NULL
completion.forecastandrealization[1:288] <- completions.base
completion.forecastandrealization[289:342] <- pred.ar$pred
completion.forecastandrealization <- as.ts(completion.forecastandrealization, frequency=12)

# pdf(file="Fig 16. Completions History and Forecast.pdf",height=6,width=7.5)
plot(completion.forecastandrealization) + abline(v=288)
# dev.off()


# Fig 16. Forecasts and Realization for Completions -----------------------

# pdf(file="Fig 16. Forecasts and Realizations for Completions.pdf",height=6,width=7.5)
plot(completion.forecastandrealization) + abline(v=288) + points(completions, type="l")
# dev.off()
