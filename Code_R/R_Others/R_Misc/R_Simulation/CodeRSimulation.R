#Simulation Experiments for R 

##House Keeping--------------
rm(list=ls())
workpath <- "C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Others/R_Misc/R_Simulation"
setwd(workpath)


##Packages-------------------


##Perfect Multicollinearity Simulation------------
#for section 6.2.2

set.seed(1)
n <- 1000
x1 <- rnorm(n, mean=0, sd=1)
x2 <- 1+2*x1 #generate data that is perfectly dependent on x1

plot(x1,x2, col="blue", main="Perfect Multicollinearity") #notice the points are in a straight line

beta1 <- 1
beta2 <- 1
beta3 <- 1
eps <- rnorm(n, mean=0, sd=1)
y <- beta1 + beta2*x1 + beta3*x2 + eps #create environment with perfect multicollinearity

reg.perfect.multico <- lm(y~x1+x2) #OLS of y by x1 and x2
summary(reg.perfect.multico) #notice we get "NA" in our result for x2 and R automatically estimates for y=b1+b2*x1+eps


##Strong Multicollinearity Simulation------------
#for section 6.2.2

set.seed(1)
n <- 1000
x1 <- rnorm(n, mean=0, sd=1)
x2 <- 1+2*x1 + rnorm(n, mean=0, sd=0.01) #notice we added some noise to avoid PERFECT multicollinearity but maintain strong multicollinearity

plot(x1,x2,col="blue", main="Strong Collinearity") #notice we have stong, but not perfect, multicollinearity

beta1 <- 1
beta2 <- 1
beta3 <- 1
eps <- rnorm(n, mean=0, sd=1)
y <- beta1 + beta2*x1 + beta2*x2 + eps

reg.strong.multico <- lm(y~x1+x2)
summary(reg.strong.multico) #we do get regression result but notice high standard errors


##Heteroskedasticity Simulation---------------
#for chapter 9

set.seed(1)
n <- 1000
x <- rnorm(n, mean=0, sd=1)

beta1 <- 1
beta2 <- 1

#create function to produce error terms that are correlated with the x-term
hetero.func <- function(x){
  e <- 1 + 0.5*x^2
  return(e)
}

eps <- rnorm(1000, mean=0, sd = sqrt(hetero.func(x)) #we produce epsilon from function of x
eps2 <- eps^2
y <- beta1 + beta2*x + eps
reg.hetero <- lm(y~x)
resid.hetero <- resid(reg.hetero)

plot(x, y, col="blue", main="Data and Regression Line") + abline(reg.hetero, col="red") #plotting y by x: notice that the residuals are correlated with x
# dev.copy2pdf(file="Data_and_Regression_Line.pdf")

plot(x, eps, col="blue", main="Residuals") + abline(0,0,col="red") #plotting residuals by x: notice that the residuals are correlated with x
# dev.copy2pdf(file="Residual_plot.pdf")

plot(x, eps2, col="blue", main="Residuals-Squared") #notice the correlation
# dev.copy2pdf(file="Residuals_Squared.pdf")
