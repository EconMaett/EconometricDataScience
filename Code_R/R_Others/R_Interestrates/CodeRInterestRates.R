# CodeRInterestRates.R ----

## Housekeeping ----
rm(list = ls())
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Others/R_Interestrates")

## Packages ----
library(car)
library(ggplot2)
library(gridExtra)
library(quantmod) # quantmod package allows us to download updated data set from various data sets.
library(GGally)

data <- read.csv("DataInterestRate1.csv", header = TRUE)

Y1 <- data[, "Y1"]
Y10 <- data[, "Y10"]
Y20 <- data[, "Y20"]
Y30 <- data[, "Y30"]

dev.new()
par(xaxs = "i", yaxs = "i")
Y1ts <- ts(Y1, start = c(1953, 4), frequency = 12)
ts.plot(Y1ts, main = "Figure 4.2 - 1-Year Treasury Bond Rate", ylab = "Rate", xlim = c(1960, 2005.25), ylim = c(0, 20))
dev.copy2pdf(file = "Figure 4.2 - 1-Year Treasury Bond Rate.pdf")

dev.new()
par(xaxs = "i", yaxs = "i")
Y1change <- ts(start = c(1953, 4), end = c(2005, 3), frequency = 12)
for (i in c(2:length(Y1change))) {
  Y1change[i] <- Y1ts[i] - Y1ts[i - 1]
}
ts.plot(Y1change, main = "Figure 4.3 - Change in 1-Year Treasury Bond Rate", ylab = "Change in Rate", xlim = c(1960, 2005.25), ylim = c(-4, 2))
dev.copy2pdf(file = "Figure 4.3 - Change in 1-Year Treasury Bond Rate.pdf")

dev.new()
par(xaxs = "i", yaxs = "i")
hist(Y1change, ylim = c(0, 200), xlab = "Change in Rate", ylab = "Occurrences", main = "Figure 4.5 Histogram Change in 1-Year Treasury Bond Rate", breaks = seq(-4, 2, by = .25))
print(summary(Y1change))
dev.copy2pdf(file = "Figure 4.5 Histogram Change in 1-Year Treasury Bond Rate.pdf")

dev.new()
par(xaxs = "i", yaxs = "i")
plot(Y10, Y1, xlab = "10-Year Treasury Bond Rate", ylab = "1-Year Treasury Bond Rate", main = "Figure 4.6 Scatterplot: 1-Year versus 10 Year Treasury Bond Rate", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y1 ~ Y10))
dev.copy2pdf(file = "Figure 4.6 Scatterplot 1-Year versus 10 Year Treasury Bond Rate.pdf")

dev.new()
par(mfrow = c(2, 3), xaxs = "i", yaxs = "i")
plot(Y10, Y1, xlab = "10-Year Treasury Bond Rate", ylab = "1-Year Treasury Bond Rate", main = "Y1 vs. Y10", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y1 ~ Y10))

plot(Y20, Y1, xlab = "20-Year Treasury Bond Rate", ylab = "1-Year Treasury Bond Rate", main = "Y1 vs. Y20", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y1 ~ Y20))

plot(Y30, Y1, xlab = "30-Year Treasury Bond Rate", ylab = "1-Year Treasury Bond Rate", main = "Y1 vs. Y30", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y1 ~ Y30))

plot(Y20, Y10, xlab = "20-Year Treasury Bond Rate", ylab = "10-Year Treasury Bond Rate", main = "Y10 vs. Y20", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y10 ~ Y20))

plot(Y30, Y10, xlab = "30-Year Treasury Bond Rate", ylab = "10-Year Treasury Bond Rate", main = "Y10 vs. Y30", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y10 ~ Y30))

plot(Y30, Y20, xlab = "30-Year Treasury Bond Rate", ylab = "20-Year Treasury Bond Rate", main = "Y20 vs. Y30", xlim = c(2, 16), ylim = c(0, 20))
regLine(lm(Y20 ~ Y30))
dev.copy2pdf(file = "fig_bondrate_multi_scatplot.pdf")


## Figure 2.4: Scatterplot Matrix 1-, 10-, 20-, 30- Year Government Bond Yields ----
scatmat <- cbind(Y1, Y10, Y20, Y30)
scatmat <- as.matrix(scatmat)
pairs(scatmat, main = "Figure 2.4: Scatterplot Matrix 1-, 10-, 20-, 30- Year Government Bond Yields")
dev.copy2pdf(file = "Figure 2.4 Scatterplot Matrix 1-, 10-, 20-, 30- Year Government Bond Yields.pdf")

data2 <- read.table("DataInterestRate2.csv", header = TRUE)

X1 <- data2[, "X1"]
X2 <- data2[, "X2"]
X3 <- data2[, "X3"]
X4 <- data2[, "X4"]
Y1 <- data2[, "Y1"]
Y2 <- data2[, "Y2"]
Y3 <- data2[, "Y3"]
Y4 <- data2[, "Y4"]

model1 <- lm(Y1 ~ X1)
model2 <- lm(Y2 ~ X2)
model3 <- lm(Y3 ~ X3)
model4 <- lm(Y4 ~ X4)

print(summary(model1))
print(summary(model2))
print(summary(model3))
print(summary(model4))

par(mfrow = c(2, 2), xaxs = "i", yaxs = "i")
plot(X1, Y1, main = "Y1 vs. X1", xlim = c(0, 20), ylim = c(0, 14))
regLine(model1)

plot(X2, Y2, main = "Y2 vs. X2", xlim = c(0, 20), ylim = c(0, 14))
regLine(model2)

plot(X3, Y3, main = "Y3 vs. X3", xlim = c(0, 20), ylim = c(0, 14))
regLine(model3)

plot(X4, Y4, main = "Y4 vs. X4", xlim = c(0, 20), ylim = c(0, 14))
regLine(model4)
dev.copy2pdf(file = "fig_anscombe_multi_scatplot.pdf")


## Load Data ----
data1 <- read.csv(file = "DataInterestRate3.csv")
data10 <- read.csv(file = "DataInterestRate4.csv")
data20 <- read.csv(file = "DataInterestRate5.csv")
data30 <- read.csv(file = "DataInterestRate6.csv")


## Graph1 ----
y1 <- data1$VALUE
y10 <- data10$VALUE
y20 <- data20$VALUE
y30 <- data30$VALUE
rate1 <- data.frame(y1, y10)
fig.y10.y1 <- ggplot(data = rate1, aes(x = y1, y = y10)) +
  geom_point(color = "blue") +
  xlab("1-year") +
  ylab("10-year") +
  ggtitle("Bond Yield Rate") +
  theme(text = element_text(size = 12))


## Graph2 ----
y1n <- y1[287:734]
y10n <- y10[287:734]
y20n <- y20[287:734]
rate2 <- data.frame(y1n, y10n, y20n, y30)
fig.y.all <- ggpairs(rate2, upper = list(continuous = "points"), lower = list(continuous = "points"))

## Write Out ----
pdf("bond_rate_1.pdf")
grid.arrange(fig.y10.y1, ncol = 1)
dev.off()


## Foundation ----
data <- na.omit(getSymbols("GS1", src = "FRED", auto.assign = FALSE))
# Mac and Linux users: add method="curl".
data <- na.omit(getSymbols("GS1", src = "FRED", method = "curl", auto.assign = FALSE))
per.change <- diff(data)


## 10-Year Treasury Bond Yield, Levels and Changes ----
fig.bond.rate <- autoplot.zoo(data) + ggtitle("Monthly 1-Year Treasury Bond Rate") + xlab("Month") + ylab("Bond Rate") + theme(text = element_text(size = 12))
fig.change.rate <- autoplot.zoo(per.change) + ggtitle("Percentage Change for Monthly 1-Year Treasury Bond Rate") + xlab("Month") + ylab("Percentage Change in Bond Rate") + theme(text = element_text(size = 12))

# plot in a grid and save it
pdf("bondrate_fig.pdf")
grid.arrange(fig.bond.rate, fig.change.rate, ncol = 1)
dev.off()


## 10-Year Treasury Bond Yield Histogram ----
fig.bond.hist <- ggplot(data, aes(x = GS1)) +
  geom_histogram(binwidth = .5, colour = "black", fill = "lightblue") +
  xlab("Yield (Percent)") +
  ylab("Count") +
  theme(text = element_text(size = 12))

# plot in a grid and save it
pdf("bondhist_fig.pdf")
grid.arrange(fig.bond.hist, ncol = 1)
dev.off()

# END