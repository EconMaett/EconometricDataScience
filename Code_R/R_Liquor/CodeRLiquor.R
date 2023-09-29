#####################################################################
# This R file is to produce textbook figures in undergraduate Econometrics textbook of Professor Diebold. 
#
# Author: Theodore Caputi, Donato Onorato, JoonYup Park, Sarah Winton
# Date: 6/14/2017
# Project: Undergraduate Econometrics Textbook (Liquor Sales from Jan. 1987)
# Contact: joonyup@wharton.upenn.edu
#
# Instructions / Notes:
#
#   1. Before running the script, please change your working directory in the prelim.
#
#   2. Before loading the packages, please make sure you have installed all the relevant packages in your R
#     Studio. You may use the following snippet of the code to install packages:
#
#         install.packages("forecast")
#
#     Above code is a command to install the "forecast" package. 
#
#   3. To have the output graph saved, please un-comment the lines with pdf command and dev.off command. Before
#     producing the graph, you may want to create a folder named "tabfig" in your directory. 
#
# Updates: 
#   Finalized 8/15/2017
#
#####################################################################


# House Keeping -----------------------------------------------------------

rm(list=ls())
setwd("") # <- input your working directory here


# Load Data ---------------------------------------------------------------

liquor <- read.csv("DataLiquor.csv", header=T)
liquor <- na.omit(liquor) #remove any rows with missing observations
names(liquor)[1] <- "liquor" #rename column
lsts <- ts(data=liquor, start=c(1987,1), frequency=12) #declare the data as time-series


# Load Packages -----------------------------------------------------------

library(forecast)
library(plotrix)
library(lmtest)
library(strucchange) #for Chow test



# Various Linear Trends ---------------------------------------------------

#pdf(file="./tabfig/plot_linear_trends.pdf", height=6, width=7.5)
plot(x=NULL, y=NULL, xlim=c(0,100), ylim=c(-60,40), xlab="Time", ylab="Trend")
abline(a=10, b=-0.25)
abline(a=-50, b=0.8, lty="dashed")
text(20, 15, "Trend=10-0.25*Time")
text(32 ,-42, "Trend=-50+.8*Time")
#dev.off()


# Liquor Sales ------------------------------------------------------------

#pdf(file="./tabfig/plot_liquor_sales.pdf", height=6, width=7.5)
plot(lsts,axes=FALSE,xlim=c(1988-1/12,2014),ylim=c(0,3000),xlab="Time",ylab="Liquor Sales",col="blue")
box()
axis(side=1,at=1987:2014,lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(0,500,1000,1500,2000,2500,3000),lab=c(0,500,1000,1500,2000,2500,3000),cex.axis=0.75)

plot.ts(lsts, ylab="Liquor Sales", col="blue")

#dev.off()


# Log Liquor Sales --------------------------------------------------------

ln.liquor <- log(liquor)
names(ln.liquor)[1] <- "ln.liquor"
ln.lsts <- ts(data=ln.liquor,start=c(1987,1),frequency=12)

#pdf(file="./tabfig/plot_logliquor_sales.pdf", height=6, width=7.5)
plot(ln.lsts, axes=FALSE, xlim=c(1988-1/12,2014), ylim=c(6.0,8.0), xlab="Time", ylab="Log Liquor Sales", col="blue")
box()
axis(side=1, at=1987:2014,lab=c(1987:2014), cex.axis=0.7)
axis(side=2, at=c(6.0,6.4,6.8,7.2,7.6,8.0), lab=c(6.0,6.4,6.8,7.2,7.6,8.0), cex.axis=0.9)

plot.ts(ln.lsts, ylab="Log Liquor Sales", col="blue")

#dev.off()


# Linear Trend Estimation -------------------------------------------------

n <- nrow(liquor)

time <- 1:n
ln.ls <- data.frame(time, ln.liquor)
reg.lin.trend <- lm(ln.liquor~time, data=ln.ls)

summary(reg.lin.trend)


# Residual Plot and Linear Trend Estimation -------------------------------

ln.ls.act <- ln.ls$ln.liquor #actual
ln.ls.pred <- predict(reg.lin.trend, ln.ls) #predicted
ln.ls.resid <- ln.ls.act-ln.ls.pred #residuals

#pdf(file="./tabfig/plot_resid_linear_trend", height=6, width=7.5)
##Plot first set of graph (actual vs. predicted)
plot(time, ln.ls.act, type="l", axes=F, ylim=c(min(ln.ls.act)-1,max(ln.ls.act)+0.5), xlab="", ylab="", col="red")
lines(time, ln.ls.pred, col="green")
axis(2, ylim=c(min(ln.ls.act)-1,max(ln.ls.act)+0.5), col="black", las=1, cex.axis=0.7)
box()

##Allow a second plot for residuals
par(new=T)
plot(time, ln.ls.resid, type="l", axes=F, xlab="", ylab="", ylim=c(min(ln.ls.resid), max(ln.ls.resid)+2), col="blue")
axis(4, ylim=c(min(ln.ls.resid), max(ln.ls.resid)+2), cex.axis=0.7)
abline(h=sd(ln.ls.resid), col="black", lty="dashed")
abline(h=0, col="black")
abline(h=-sd(ln.ls.resid), col="black", lty="dashed")
axis(side=1, at=c(seq(1,336, by=12)), lab=c(1987:2014), cex.axis=0.7)
legend("topleft",c("Residual","Actual","Fitted"), lty=c(1,1), col=c("blue","red","green"), cex=0.75)
#dev.off()


# Linear Trend Estimation with Seasonal Dummies ---------------------------

##Create seasonal dummies (this function only creates dummies from Jan-Nov)
month <- seasonaldummy(ln.lsts)

##Since we are going to exclude the intercept in the regression, create December dummy manually
Dec <- matrix(data=rep(c(rep(0,11),1),28), nrow=336, ncol=1)
ln.ls.seas <- data.frame(cbind(ln.liquor,time,month,Dec))

##Regress log liquor sales on trend and seasonal dummies
reg.lin.trend.seas <- lm(ln.liquor ~ . -1, data=ln.ls.seas)
summary(reg.lin.trend.seas)


# Residual Plot, Linear Trend Estimation with Seasonal Dummies ------------

ln.ls.seas.act <- ln.ls.seas$ln.liquor
ln.ls.seas.pred <- predict(reg.lin.trend.seas,ln.ls.seas)
ln.ls.seas.resid <- ln.ls.seas.act-ln.ls.seas.pred

##Plot first set of graph (actual vs. predicted)
#pdf(file="./tabfig/plot_resid_linear_trend_season.pdf", height=6, width=7.5)
plot(time, ln.ls.seas.act, type="l", axes=F, ylim=c(min(ln.ls.seas.act)-1,max(ln.ls.seas.act)+0.5), xlab="", ylab="", col="red")
lines(time, ln.ls.seas.pred, col="green")
axis(2, ylim=c(min(ln.ls.seas.act)-1,max(ln.ls.seas.act)+0.5), col="black", las=1, cex.axis=0.7)
box()

##Allow a second plot for residuals
par(new=T)
plot(time, ln.ls.seas.resid, type="l", axes=F, xlab="", ylab="", ylim=c(min(ln.ls.seas.resid), max(ln.ls.seas.resid)+1), col="blue")
axis(4, ylim=c(min(ln.ls.seas.resid), max(ln.ls.seas.resid)+1), cex.axis=0.7)
abline(h=sd(ln.ls.seas.resid), col="black", lty="dashed")
abline(h=0, col="black")
abline(h=-sd(ln.ls.seas.resid), col="black", lty="dashed")
axis(side=1, at=c(seq(1,336, by=12)), lab=c(1987:2014), cex.axis=0.7)
legend("topleft",c("Residual","Actual","Fitted"), lty=c(1,1), col=c("blue","red","green"), cex=0.75)
#dev.off()


# Seasonal Pattern --------------------------------------------------------

##Extract out coefficients of seasonal dummies from the regression
seas.coef <- data.frame(coef(reg.lin.trend.seas))
seas.coef <- seas.coef[-1,]

#pdf(file="./tabfig/plot_season_pattern.pdf",height=6,width=7.5)
plot(seas.coef, type="l", axes=FALSE, xlab="Estimated Seasonal Factors", ylab="Factor", col="blue")
box()
axis(side=1, at=c(1:12), lab=c(paste("M",1:12)), cex.axis=0.75)
axis(side=2, at=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9), lab=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9), cex.axis=1)
#dev.off()


# Log-Quadratic Trend Estimation ------------------------------------------

reg.quad.trend <- lm(ln.liquor ~ time + I(time^2), data=ln.ls)
summary(reg.quad.trend)


# Residual Plot, Log-Quadratic Trend Estimation ---------------------------

ln.quad.ls.act <- ln.ls$ln.liquor
ln.quad.ls.pred <- predict(reg.quad.trend,ln.ls)
ln.quad.ls.resid <- ln.quad.ls.act-ln.quad.ls.pred

#pdf(file="./tabfig/plot_resid_logquad_trend_estimation.pdf",height=6,width=7.5)
##Plot first set of graph (actual vs. predicted)
plot(time, ln.quad.ls.act, type="l", axes=F, ylim=c(min(ln.quad.ls.act)-1,max(ln.quad.ls.act)+0.5), xlab="", ylab="", col="red")
lines(time, ln.quad.ls.pred, col="green")
axis(2, ylim=c(min(ln.quad.ls.act)-1,max(ln.quad.ls.act)+0.5), col="black", las=1, cex.axis=0.7)
box()

##Allow a second plot for residuals
par(new=T)
plot(time, ln.quad.ls.resid, type="l", axes=F, xlab="", ylab="", ylim=c(min(ln.quad.ls.resid), max(ln.quad.ls.resid)+1), col="blue")
axis(4, ylim=c(min(ln.quad.ls.resid), max(ln.quad.ls.resid)+1), cex.axis=0.7)
abline(h=sd(ln.quad.ls.resid), col="black", lty="dashed")
abline(h=0, col="black")
abline(h=-sd(ln.ls.seas.resid), col="black", lty="dashed")
axis(side=1, at=c(seq(1,336, by=12)), lab=c(1987:2014), cex.axis=0.7)
legend("topleft",c("Residual","Actual","Fitted"), lty=c(1,1), col=c("blue","red","green"), cex=0.75)
#dev.off()


# Log-Quadratic Trend Estimation with Seasonal Dummies --------------------

ln.quad.ls.seas <- data.frame(cbind(ln.liquor,time,I(time^2),month,Dec))
reg.quad.trend.seas <- lm(ln.liquor~ . -1, data=ln.quad.ls.seas)
summary(reg.quad.trend.seas)


# Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dumm --------

ln.quad.ls.seas.act <- ln.ls.seas$ln.liquor
ln.quad.ls.seas.pred <- predict(reg.quad.trend.seas,ln.quad.ls.seas)
ln.quad.ls.seas.resid <- ln.quad.ls.seas.act-ln.quad.ls.seas.pred

#pdf(file="./tabfig/plot_resid_logquad_trend_estimation_season.pdf",height=6,width=7.5)
##Plot first set of graph (actual vs. predicted)
plot(time, ln.quad.ls.seas.act, type="l", axes=F, ylim=c(min(ln.quad.ls.seas.act)-1,max(ln.quad.ls.seas.act)+0.5), xlab="", ylab="", col="red")
lines(time, ln.quad.ls.seas.pred, col="green")
axis(2, ylim=c(min(ln.quad.ls.seas.act)-1,max(ln.quad.ls.seas.act)+0.5), col="black", las=1, cex.axis=0.7)
box()

##Allow a second plot for residuals
par(new=T)
plot(time, ln.quad.ls.seas.resid, type="l", axes=F, xlab="", ylab="", ylim=c(min(ln.quad.ls.seas.resid), max(ln.quad.ls.seas.resid)+1), col="blue")
axis(4, ylim=c(min(ln.quad.ls.seas.resid), max(ln.quad.ls.seas.resid)+1), cex.axis=0.7)
abline(h=sd(ln.quad.ls.seas.resid), col="black", lty="dashed")
abline(h=0, col="black")
abline(h=-sd(ln.ls.seas.resid), col="black", lty="dashed")
axis(side=1, at=c(seq(1,336, by=12)), lab=c(1987:2014), cex.axis=0.7)
legend("topleft",c("Residual","Actual","Fitted"), lty=c(1,1), col=c("blue","red","green"), cex=0.75)
#dev.off()


# Expanding Window Regression ---------------------------------------------

##Define parameters
n <- nrow(lsts)
k <- 121 #We will start from Jan. 1997. 121th row of the data refers to Jan. 1997

##Initialize empty vector to store coefficients as we expand our window
coef.expand <- vector(mode="numeric", length=n-k+1)

for (i in k:n) {
  
  #Cut our sample from beginning to expansion window
  ln.ls.seas.expand <- ln.ls.seas[1:i,]
  
  #Regress log liquor on trend and seasonal dummies
  reg.expand <- lm(ln.liquor ~ . -1, data=ln.ls.seas.expand)
  
  #Extract out coefficient for trend and store it
  index <- i-(k-1) 
  coef.expand[index] <- coef(reg.expand)["time"]
}

coef.expand <- ts(coef.expand, start=c(1997,1), frequency=12)

#pdf(file="./tabfig/plot_expanding_window.pdf",height=6,width=7.5)
plot.ts(coef.expand, xlab="Window Cut-off Time", ylab="Coefficient of Trend", main="Expanding Window", col="blue")
#dev.off()


# Rolling Window Regression -----------------------------------------------

##Define parameters
n <- nrow(lsts)
w <- 120 #We fix the window length to be 10 years

##Initialize empty vector to store coefficients as we roll through our window
coef.rolling <- vector(mode="numeric", length=n-w+1)

for (i in 1:(n-w+1)) {
 
  #Define cut-off
  w.start <- i
  w.end <- i+w-1 
  
  #Cut our sample from beginning to expansion window
  ln.ls.seas.rolling <- ln.ls.seas[w.start:w.end,]
  
  #Regress log liquor on trend and seasonal dummies
  reg.rolling <- lm(ln.liquor ~ . -1, data=ln.ls.seas.rolling)
  
  #Extract out coefficient for trend and store it
  index <- i
  coef.rolling[index] <- coef(reg.rolling)["time"]
}

coef.rolling <- ts(coef.rolling, start=c(1987,1), frequency=12)

#pdf(file="./tabfig/plot_rolling_window.pdf",height=6,width=7.5)
plot.ts(coef.rolling, xlab="Window Cut-off Start Time", ylab="Coefficient of Trend",main="Rolling Window Regression", sub=paste("Window Length:",w,"months",sep=" "), col="blue")
#dev.off()


# Chow Test ---------------------------------------------------------------

break.loc.min <- 0.15
break.loc.max <- 0.85

ts.ln.ls.seas <- ts(ln.ls.seas, start=c(1987,1), frequency=12)
ts.reg.lin.trend.seas <- tslm(ln.liquor ~ trend + season -1, data=ts.ln.ls.seas)

##Perform MaxChow Test
chow <- Fstats(ts.reg.lin.trend.seas, from=break.loc.min, to=break.loc.max, data=ts.ln.ls.seas)

##Check where the break point is
breakpoints(chow) #It's at September 2001

##Check the F-stat value at the breakpoint
sctest(chow)

##Plot it out
#pdf(file="./tabfig/plot_chow_test.pdf",height=6,width=7.5)
plot(chow, main="Max Chow Test", boundary=F)
lines(breakpoints(chow))
#dev.off()


# Scatter Plot of e_t against e_t-1 ---------------------------------------

##Let's review regression result of log(liquor sales) ~ trend + trend^2 + seasons
summary(reg.quad.trend.seas)

##Create a lag of our residuals from the above regression
ts.ln.quad.ls.seas.resid <- ts(ln.quad.ls.seas.resid, start=c(1987,1), frequency=12)
ln.quad.resid.lag <- ts.union(ts.ln.quad.ls.seas.resid, ts.ln.quad.ls.seas.resid.l1=lag(ts.ln.quad.ls.seas.resid, k=-1))
ln.quad.resid.lag <- as.data.frame(ln.quad.resid.lag)
names(ln.quad.resid.lag) <- c("RESID", "RESID.L1")

#pdf(file="./tabfig/plot_resid_residL1.pdf",height=6,width=7.5)
plot(ln.quad.resid.lag$RESID.L1, ln.quad.resid.lag$RESID, cex=0.8, xlim=c(-0.12,0.16), ylim=c(-0.12,0.16), col="blue", xlab="RESIDL1", ylab="RESID")
#dev.off()


# Durbin-Watson Test for AR(1) Distrubance --------------------------------

dwtest(reg.quad.trend.seas)


# Breusch-Godfrey Test for AR(p) Disturbance ------------------------------

p <- 4 #select the order of serial correlation you want to check for
bgtest(formula=reg.quad.trend.seas, order=p, type="Chisq")


# Regress on trend + trend^2 + season + p-lags ----------------------------

P <- 4 #<- change this value to show different results for different p
n <- nrow(ln.quad.ls.seas)

ln.quad.ls.seas.lag <- ts.union(ln.lsts)
var.list <- vector(mode="character")

for (p in 1:P) {
  
  lag.varname <- paste("ln.liquor.l",p,sep="")
  assign(lag.varname,lag(ln.lsts,k=-p))
  ln.quad.ls.seas.lag <- ts.union(ln.quad.ls.seas.lag, get(lag.varname))
  var.list <- c(var.list,lag.varname)
}

ln.quad.ls.seas.lag <- as.data.frame(ln.quad.ls.seas.lag)[1:n, 2:(P+1)]
colnames(ln.quad.ls.seas.lag) <- var.list
ln.quad.ls.seas.lag <- cbind(ln.quad.ls.seas, ln.quad.ls.seas.lag)
ln.quad.ls.seas.lag <- ln.quad.ls.seas.lag[complete.cases(ln.quad.ls.seas.lag),]

reg.quad.trend.seas.lag <- lm(ln.liquor ~ . -1, data=ln.quad.ls.seas.lag)
summary(reg.quad.trend.seas.lag)

##Check AIC(=Akaiake's Information Criterion) and BIC(=Schwarz's Bayesian Criterion)
AIC(reg.quad.trend.seas.lag)
BIC(reg.quad.trend.seas.lag)


ln.quad.ls.seas.lag.act <- ln.quad.ls.seas.lag$ln.liquor
ln.quad.ls.seas.lag.pred <- predict(reg.quad.trend.seas.lag)
ln.quad.ls.seas.lag.resid <- ln.quad.ls.seas.lag.act-ln.quad.ls.seas.lag.pred

time <- 1:nrow(ln.quad.ls.seas.lag)

#pdf(file="./tabfig/plot_resid_logquad_trend_estimation_season_lag.pdf",height=6,width=7.5)
##Plot first set of graph (actual vs. predicted)
plot(time, ln.quad.ls.seas.lag.act, type="l", axes=F, ylim=c(min(ln.quad.ls.seas.lag.act)-1,max(ln.quad.ls.seas.lag.act)+0.5), xlab="", ylab="", col="red")
lines(time, ln.quad.ls.seas.lag.pred, col="green")
axis(2, ylim=c(min(ln.quad.ls.seas.lag.act)-1,max(ln.quad.ls.seas.lag.act)+0.5), col="black", las=1, cex.axis=0.7)
box()

##Allow a second plot for residuals
par(new=T)
plot(time, ln.quad.ls.seas.lag.resid, type="l", axes=F, xlab="", ylab="", ylim=c(min(ln.quad.ls.seas.lag.resid), max(ln.quad.ls.seas.lag.resid)+0.3), col="blue")
axis(4, ylim=c(min(ln.quad.ls.seas.lag.resid), max(ln.quad.ls.seas.lag.resid)+0.3), cex.axis=0.7)
abline(h=sd(ln.quad.ls.seas.lag.resid), col="black", lty="dashed")
abline(h=0, col="black")
abline(h=-sd(ln.quad.ls.seas.lag.resid), col="black", lty="dashed")
axis(side=1, at=c(seq(1,336, by=12)), lab=c(1987:2014), cex.axis=0.7)
legend("topleft",c("Residual","Actual","Fitted"), lty=c(1,1), col=c("blue","red","green"), cex=0.75)
#dev.off()


# Regression with Serially-Correlated Disturbances ------------------------

##Correlogram of residuals (Auto-correlation)
ln.ls.seas.resid.acf <- acf(na.omit(ln.quad.resid.lag$RESID), type="correlation", plot=F)

#pdf(file="./tabfig/plot_resid_correl_autocorrelation.pdf",height=6,width=7.5)
plot(ln.ls.seas.resid.acf, col="blue", xlab="Lag", ylab="Autocorrelation", main="")
#dev.off()

##Correlogram of residuals (Partial Auto-correlation)
ln.ls.seas.resid.pacf <- pacf(na.omit(ln.quad.resid.lag$RESID), plot=F)

#pdf(file="./tabfig/plot_resid_correl_partial_autocorrelation.pdf",height=6,width=7.5)
plot(ln.ls.seas.resid.pacf, col="blue", xlab="Lag", ylab="Partial autocorrelation", main="")
#dev.off()
