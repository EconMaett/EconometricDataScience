#####################################################################
# This R file is to produce textbook figures in undergraduate Econometrics
# textbook of Professor Diebold. 
#
# Author: Theodore Caputi, Donato Onorato, JoonYup Park, Sarah Winton
# Date: 4/12/2017
# Project: Undergraduate Econometrics Textbook (Wages: CPS Wage Data, 1995)
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
#Note: When setting up the directory, make sure you use "/" instead of the default "\" for folder separators.


# Load Data ---------------------------------------------------------------

wage95 <- read.csv("DataWages.csv", header=TRUE)
attach(wage95)


head(wage95)

# Packages ----------------------------------------------------------------

##Uncomment this part if you need to install the following packages:
#install.packages("lmtest") 
#install.packages("sandwich")
#install.packages("knitr")
#install.packages("quantreg")

library(lmtest)
library(sandwich)
library(knitr)
library(quantreg) #for quantile regression

#pdf(file="./tabfig/Fig 2.5.pdf")

# Histograms for WAGE and LNWAGE ------------------------------------------

#pdf(file="./tabfig/hist_WAGE.pdf")
hist(WAGE, breaks=28, main="")
#dev.off()

#pdf(file="./tabfig/hist_LNWAGE.pdf")
hist(LNWAGE, breaks=40, main="")
#dev.off()


# Kernel Density Plot for WAGE and LNWAGE ---------------------------------

#WAGE
wage.density <- density(WAGE) #returns the density data for WAGE
wage.normal <- density(rnorm(1000000, mean=mean(WAGE), sd=sd(WAGE))) #returns the density data for normal distribution from the WAGE data

#pdf(file="./tabfig/kden_WAGE.pdf")
plot(wage.density, col="blue", main="", xlab="Wage", ylab="Density") + points(wage.normal, type="l", col="red")
#dev.off()

#LNWAGE
lnwage.density <- density(LNWAGE) #returns the density data for LNWAGE
lnwage.normal <- density(rnorm(1000000, mean=mean(LNWAGE), sd=sd(LNWAGE))) #returns the density data for normal distribution from the LNWAGE data

#pdf(file="./tabfig/kden_LNWAGE.pdf")
plot(lnwage.density, col="blue", main="", xlab="Log Wage", ylab="Density", ylim=c(0,.8)) + points(lnwage.normal, type="l", col="red")
#dev.off()

#Comparing the two plots above illustrates how taking logarithms can help "smooth" the data. 
#The QQ plot below also illustrates this dynamic.


# QQ-Plot for WAGE and LNWAGE ---------------------------------------------

#pdf(file="./tabfig/qqplot_WAGE.pdf")
qqnorm(WAGE, pch=20, col="red", main=""); qqline(WAGE)
#dev.off()

#pdf(file="./tabfig/qqplot_LNWAGE.pdf")
qqnorm(LNWAGE, pch=20, col="red", main=""); qqline(LNWAGE)
#dev.off()


#par(mfrow=c(1,1))
#Notes on the function "par": 
# This function allows us to combine graphs. The "mfrow" command takes a number of rows and columns and fills them with graphs.
# This function will prove to be useful if you wish to combine graphs for your assignments. 


# Histograms for LNWAGE, EDUC, and EXPER ----------------------------------

#pdf(file="./tabfig/hist_LNWAGE_EDUC_EXPER.pdf",height=6,width=7.5)
par(mfrow=c(3,1))

#First Figure for LNWAGE Histogram
hist(LNWAGE, breaks=40, main="")

#Second Figure for EDUC Histogram
hist(EDUC, breaks=40, main="")

#Third Figure for EXPER Histogram
hist(EXPER, breaks=25, main="")

#dev.off()

par(mfrow=c(1,1))


# Scatterplot of EDUC vs. LNWAGE ------------------------------------------

#pdf(file="./tabfig/scatter_EDUC_v_LNWAGE.pdf",height=6,width=7.5)
plot(EDUC, LNWAGE, col="blue")
#dev.off()


# Fitted line through EDUC vs. LNWAGE -------------------------------------

#pdf(file="./tabfig/scatter_fit_EDUC_v_LNWAGE.pdf",height=6,width=7.5)
plot(EDUC, LNWAGE, col="blue") + abline(reg=lm(LNWAGE~EDUC), col="red")
#dev.off()


# Regress LNWAGE ~ EDUC, EXPER --------------------------------------------

reg.lnwage.by.educ.exper <- lm(LNWAGE~EDUC + EXPER)
summary(reg.lnwage.by.educ.exper)


#pdf(file="./tabfig/plot_fitLNWAGE_v_LNWAGE.pdf")
fit.lnwage.by.educ.exper <- fitted.values(reg.lnwage.by.educ.exper)
plot(fit.lnwage.by.educ.exper, LNWAGE, col="blue", xlab="FIT", xlim=c(0,5), ylim=c(0,5))
#dev.off()

#Creating residuals from Regression 1 predicted values
lnwage.actual <- LNWAGE
lnwage.pred <- predict(reg.lnwage.by.educ.exper, wage95)
lnwage.resid <- lnwage.actual - lnwage.pred

#Plot of residuals and Fitted values superimposed on #residuals

#pdf(file="./tabfig/plot_resid_fitted_LNWAGE_EDUC_EXPER.pdf")
plot(lnwage.actual, type="l", axes=FALSE, xlim=c(0,1300), ylim=c(-2,5), xlab="", ylab="", col="red") +
  lines(lnwage.pred, col="darkgreen") +
  lines(lnwage.resid, col="blue")
box()
axis(side=1, at=c(0,250,500,750,1000,1250))
axis(side=2, at=c(-2,-1,0,1,2))
axis(side=4, at=c(0,1,2,3,4,5))
par(xpd=TRUE)
legend(150, 5,inset=c(-0.3,0), legend=c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"), cex=0.7, horiz=TRUE)
#dev.off()


# Histograms of EDUC, EXPER, NONWHITE, UNION ------------------------------

#pdf(file="./tabfig/hist_EDUC_EXPER_NONWHITE_UNION.pdf")
par(mfrow=c(2,2))

#Histogram for EDUC
hist(EDUC, breaks=40)

#Histogram for EXPER
hist(EXPER, breaks=30)

#Histogram for NONWHITE
hist(NONWHITE)

#Histogram for UNION
hist(UNION)
#dev.off()

par(mfrow=c(1,1))


# Regress LNWAGE ~ FEMALE + NONWHITE + UNION + EDUC + EXPER ---------------

reg.lnwage.by.educ.exper.female.nonwhite.union <- lm(LNWAGE~.-AGE-WAGE, data=wage95)
summary(reg.lnwage.by.educ.exper.female.nonwhite.union)

#Get the fitted values of the regression above
fit.lnwage.by.educ.exper.female.nonwhite.union <- fitted.values(reg.lnwage.by.educ.exper.female.nonwhite.union)

#Plot the fitted Values of the regression above

#pdf(file="./tabfig/plot_fitLNWAGE_v_LNWAGE_2.pdf")
plot(fit.lnwage.by.educ.exper.female.nonwhite.union, LNWAGE, col="blue",xlab="FIT", xlim=c(0,5), ylim=c(0,5))
#dev.off()


# Various Linear Trends ---------------------------------------------------

time.sample <- seq(from=0, to=100, by=0.5)
trend.sample <- 10-0.25*time.sample
BB <- seq(from=-50, to=30, by=0.4)

#pdf(file="./tabfig/plot_linear_trends.pdf", height=6, width=7.5)
plot(time.sample, trend.sample, type="l", col="red", xlim=c(0,100), ylim=c(-60,40), xlab="Time", ylab="Trend")# + abline(a=-50, b=.8, col="blue")
lines(time.sample, BB, col="blue")
#dev.off()


# Regress LNWAGE on Quad Terms and Interaction Terms ----------------------

EDUC2 <- EDUC^2 #create a vector of EDUC^2
EXPER2 <- EXPER^2 #create a vector of EXPER^2

#Regression: LNWAGE ~ EDUC, EDUC^2, EXP, EXP^2, EDUC:EXPER, FEMALE, UNION, NONWHITE
reg.lnwage.quadratic <- lm(LNWAGE~EDUC+EDUC2+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE)
summary(reg.lnwage.quadratic)

#Regression with dummy variable interactions
reg.lnwage.educ.exper.dummy.interact <- lm(LNWAGE~EDUC+EXPER+FEMALE+UNION+NONWHITE+FEMALE:UNION+FEMALE:NONWHITE+UNION:NONWHITE)
summary(reg.lnwage.educ.exper.dummy.interact)


# Regress LNWAGE on Combining Several of the Previous Regressions ---------

reg.lnwage.quadratic.continter.discreteinter <- lm(LNWAGE~EDUC+EDUC2+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE+FEMALE:UNION+FEMALE:NONWHITE+UNION:NONWHITE)
summary(reg.lnwage.quadratic.continter.discreteinter)


# Plotting Different Time Trends ------------------------------------------

fig76.a <- 10+0.3*time.sample+0.3*time.sample^2
plot(time.sample, fig76.a, type="l", xlab="", ylab="", main="TREND = 10 + .3TIME + .3TIME2")

fig76.b <- 10+30*time.sample-0.3*time.sample^2
plot(time.sample, fig76.b, type="l", xlab="", ylab="", main="TREND = 10 + 30TIME + .3TIME2")

fig76.c <- 10-.4*time.sample-.4*time.sample^2
plot(time.sample, fig76.c, type="l", xlab="", ylab="", main="TREND = 10 - .4TIME - .4TIME2")

fig76.d <- 10-.25*time.sample+.3*time.sample^2
plot(time.sample, fig76.d, type="l", xlab="", ylab="", main="TREND = 10 - .25TIME + .3TIME2")



# Regress LNWAGE on Quad Terms and Interaction Terms 2 --------------------

#Regression: LNWAGE~EDUC, EXP, EXP^2, EDUC:EXP, FEMALE, UNION, NONWHITE
reg.lnwage.quadratic2 <- lm(LNWAGE~EDUC+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE)
summary(reg.lnwage.quadratic2)

#Get residuals and squared residuals from the regression above
resid.lnwage.quadratic2 <- resid(reg.lnwage.quadratic2)
resid.lnwage.quadratic2.squared <- resid.lnwage.quadratic2^2

#Plot residuals v. EDUC
plot(EDUC, resid.lnwage.quadratic2.squared, col="blue", xlab="EDUC", ylab="RESID2")


# Various Loss Functions --------------------------------------------------

x <- seq(-1,1,0.001)
y <- abs(x)
abs_err <- cbind(x,y)

y <- x^2
qu_err <- cbind(x,y)

y <- c(abs(x[which(x<=0)])*0.5,abs(x[which(x>0)])*1.5)
linLinLoss <- cbind(x,y)

#pdf("./tabfig/VariousLossFunctionFig.pdf",width=5,height=5)
grow <- 1.3
plot(linLinLoss,type="l",col=3,lwd=3,xlab="Error",ylab="Loss",main="Various Loss Functions",
     cex.lab=grow,cex.axis=grow,cex.main=grow,cex.sub=grow)
lines(abs_err,col=2,lwd=3)
lines(qu_err,col=1,lwd=3)
legend(-1,1.5,c("Quadratic Loss","Absolute Error Loss","LinLin Loss"),lwd=2.5,col=1:3)
#dev.off()


# LAD Regression ----------------------------------------------------------

reg.lnwage.educ.LAD <- rq(LNWAGE ~ EDUC, tau=0.5)
summary(reg.lnwage.educ.LAD)

#Round coefficient outputs to put on the graph as a text
cf.LAD <- round(coef(reg.lnwage.educ.LAD),3)
eq.text.LAD <- paste0("LNWAGE = ", cf.LAD[1], " + ", cf.LAD[2], " EDUC")

plot(EDUC, LNWAGE, col="blue", main="LNWAGE ~ c, EDUC (LAD)") 
abline(reg.lnwage.educ.LAD)
mtext(eq.text.LAD, side = 3, line=0)


# 10-percentile Regression ------------------------------------------------

reg.lnwage.educ.10pct <- rq(LNWAGE ~ EDUC, tau=0.1)
summary(reg.lnwage.educ.10pct)

#Round coefficient outputs to put on the graph as a text
cf.10pct <- round(coef(reg.lnwage.educ.10pct),3)
eq.text.10pct <- paste0("LNWAGE = ", cf.10pct[1], " + ", cf.10pct[2], " EDUC")

plot(EDUC, LNWAGE, col="blue", main="LNWAGE ~ c, EDUC (10-percentile)") 
abline(reg.lnwage.educ.10pct)
mtext(eq.text.10pct, side = 3, line=0)


# 90-percentile Regression ------------------------------------------------

reg.lnwage.educ.90pct <- rq(LNWAGE ~ EDUC, tau=0.9)
summary(reg.lnwage.educ.90pct)

#Round coefficient outputs to put on the graph as a text
cf.90pct <- round(coef(reg.lnwage.educ.90pct),3)
eq.text.90pct <- paste0("LNWAGE = ", cf.90pct[1], " + ", cf.90pct[2], " EDUC")

plot(EDUC, LNWAGE, col="blue", main="LNWAGE ~ c, EDUC (90-percentile)") 
abline(reg.lnwage.educ.90pct)
mtext(eq.text.90pct, side = 3, line=0)


# Comparison of regressions with different loss functions --------------------

dev.off()

plot(EDUC, LNWAGE, col="blue", main="LNWAGE ~ c, EDUC") 
abline(lm(LNWAGE ~ EDUC),col=1)
abline(reg.lnwage.educ.LAD,col=2)
abline(reg.lnwage.educ.10pct,col=3)
abline(reg.lnwage.educ.90pct,col=4)
legend(1,4.1,c("LS","LAD","10pct QR","90pct QR"),lwd=2.5,col=1:4)


# Leave-one-out Plot of Coefficients --------------------------------------

reg.lnwage.by.educ <- lm(LNWAGE ~ EDUC + EXPER 
                         # + FEMALE + NONWHITE + UNION <- extra variables
)
b.orig <- summary(reg.lnwage.by.educ)$coef[2] #get full-sample coefficient of LNWAGE ~ EDUC + EXPER

n <- nrow(wage95)
b.loo.coef <- vector(mode="numeric", length=n)
t <- 1:n

for (i in 1:n) {
  
  #Remove one observation from the data
  wage95.loo <- wage95[-i,]
  
  #Regression of LNWAGE on EDUC + EXPER leaving one row of data out 
  reg.loo <- lm(LNWAGE ~ EDUC + EXPER 
                # + FEMALE + NONWHITE + UNION <- extra variables
                , data=wage95.loo)
  
  #Get EDUC coefficient
  b.loo <- summary(reg.loo)$coef[2] 
  
  #Store new EDUC coefficient
  b.loo.coef[i] <- b.loo # - b.orig
}

#pdf("./tabfig/plot_loo_coef.pdf", width=5, height=4)
plot(t, b.loo.coef, col="blue", cex=0.5, ylab="Coefficient (Education)", xlab="Leave t out", main="Leave-One-Out Plot", ylim=c(b.orig-0.004,b.orig+0.004)) +
    abline(h=b.orig, col="red")
#dev.off()


# Leverage Plot -----------------------------------------------------------

reg.lnwage.by.educ <- lm(LNWAGE ~ EDUC)
lev <- hat(model.matrix(reg.lnwage.by.educ))

#pdf("./tabfig/plot_leverage.pdf", width=5, height=4)
plot(lev, col="blue", cex=0.5, ylab="Leverage", xlab="t", main="Leverage Plot")
#dev.off()


# Simple Plot of Index X EDUC ---------------------------------------------

#pdf("./tabfig/plot_index_educ.pdf", width=5, height=4)
plot(t,EDUC, col="blue", cex=0.5, ylab="EDUC", xlab="t", main="EDUC by Index")
#dev.off()
