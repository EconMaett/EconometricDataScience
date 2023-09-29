# Housekeeping--------------------------------------
rm(list=ls());
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_others/R_NYSE");

# Packages------------------------------------------
library(tseries)
library(fGarch)
library(moments)

# Read input data-------------------------------------------------------------------
input <- read.csv("DataNYSE.csv", head=TRUE);
data <- input[,1]

par(mfrow=c(1,1))

# Fig 11.1 Time Series of Daily NYSE Returns----------------------------------------
ts.plot(data,main="Figure 11.1: Time Series of Daily NYSE Returns", xlab="Time", ylab="Return",ylim=c(-.08,.06));
# dev.copy2pdf(file="Fig 11.1 Time Series of Daily NYSE Returns.pdf");

# Fig 11.2 Correlogram of Daily Stock Market Returns--------------------------------
acf(data, type="correlation", plot=T, ylim=c(-.06,1), main="Figure 11.2: Correlogram of Daily Stock Market Returns", xlab="Displacement", ylab="Autocorrelation");
# dev.copy2pdf(file="Fig 11.2 Correlogram of Daily Stock Market Returns.pdf")


# Fig 11.3 Histogram and Statistics for Daily NYSE Returns--------------------------
# hist01<-qplot(data, binwidth=.005,main="Figure 11.3: Histogram and Statistics for Daily NYSE Returns", xlab="Return", ylab="Count", ylim=c(0,1200)) 
hist01<-hist(data, main="Figure 11.3: Histogram and Statistics for Daily NYSE Returns", xlab="Return", ylab="Count", ylim=c(0,1200), breaks=seq(-.065,.065,by=.005))
# generate statistics
stats <- c(mean(data),median(data),max(data),min(data),sd(data),skewness(data),kurtosis(data),jarque.test(data)$statistic,jarque.test(data)$p.value);
statsName <- c("Mean","Median","Maximum","Minimum","Std. Dev.", "Skewness", "Kurtosis", "Jarque-Bera","Probability");
statsdf <- data.frame(statsName,stats);
# dev.copy2pdf(file="Fig 11.3 Histogram and Statistics for Daily NYSE Returns.pdf")

# Fig 11.4 Time Series of Daily Squared NYSE Returns-------------------------------
data2 <- data*data;
ts.plot(data2,main="Figure 11.4: Time Series of Daily Squared NYSE Returns", xlab="Time", ylab="Squared Return",ylim=c(.000,.005));
# dev.copy2pdf(file="Fig 11.4 Time Series of Daily Squared NYSE Returns.pdf");

# Fig 11.5 Correlogram of Daily Squred NYSE Returns--------------------------------
acf(data2, type="correlation", plot=T,  ylim=c(-.04,.16), main="Figure 11.5: Correlogram of Daily Squared Stock Market Returns", xlab="Displacement", ylab="Autocorrelation");
# dev.copy2pdf(file="Fig 11.5 Correlogram of Daily Squred NYSE Returns.pdf")

# Fig 11.7 Estimated Conditional Standard Deviation, Daily NYSE Returns------------
ts.nyse <- ts(data)
fit.nyse <- garch(ts.nyse, order= c(1,1))
cond_sd <- predict(fit.nyse, genuine=FALSE)
plot(cond_sd[,1], main="Figure 11.7: Estimated Conditional Standard Deviation, Daily NYSE Returns", xlab="Time", ylab="GARCH(1,1) Standard Deviation")
# dev.copy2pdf(file="Fig 11.7 Estimated Conditional Standard Deviation, Daily NYSE Returns.pdf")

# Fig 11.8 Conditional Standard Deviation, History and Forecast, Daily NYSE Returns--
reg.ar <- ar(cond_sd[3300:3459,1])
pred.ar <- predict(reg.ar, n.ahead=54)

nyse.forecastandrealization <- NULL
nyse.forecastandrealization[1:160] <- cond_sd[3300:3459,1]
nyse.forecastandrealization[161:214] <- pred.ar$pred
nyse.forecastandrealization <- as.ts(nyse.forecastandrealization, frequency=12)

plot(nyse.forecastandrealization, main="Figure 11.8: Conditional Standard Deviation, History and Forecast, Daily NYSE Returns", ylab="History and Forecast", xlab="Time", xaxt="n") + abline(v=160)
axis(1, at=seq(1,214, by=5), labels=seq(3300,3513, by=5))
# dev.copy2pdf(file="Fig 11.8 Conditional Standard Deviation, History and Forecast, Daily NYSE Returns.pdf")

# Fig 11.9 Correlogram of Squared standardized GARCH(1,1) Residuals, Daily NYSE Returns
fit <- garchFit(~garch(1,1),data)
res <- residuals(fit, standardize=TRUE)
res2 <- res*res
acf.res <- acf(res2, type="correlation", plot=FALSE)
acf.res <- acf.res$acf[2:30]
plot(acf.res, type="h", ylab="Autocorrelation", xlab="Displacement", ylim=c(-.04,.04))
abline(h=0, par(xpd = FALSE))
# dev.copy2pdf(file="Fig 11.9 Correlogram of Squared standardized GARCH(1,1) Residuals, Daily NYSE Returns.pdf")

