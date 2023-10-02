# CodeRExchangeRates.R ----

## House Keeping ----
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Others/R_ExchangeRate")
rm(list = ls())


## Read Data ----
rate <- read.csv("DataExchangeRate.csv", header = TRUE)
names(rate) <- c("TIME", "USDGBP")
row <- nrow(rate)


## Fig 17.1 USD/GBP Exchange Rate, 1971.01-2012.10 ----
pdf(file = "Fig 17. USD_GBP Exchange Rate, 1971.01-2012.10.pdf", height = 6, width = 7.5)
plot(rate, type = "l", axes = FALSE, col = "blue", xlim = c(1, row), ylim = c(0.8, 2.8), xlab = "", ylab = "", main = "USUK")
box()
axis(side = 1, at = c(seq(4 * 12, row - 9, by = 60)), lab = c(seq(1975, 2010, by = 5)), cex.axis = 0.8)
axis(side = 2, at = c(seq(0.8, 2.8, by = 0.4)), lab = c(seq(0.8, 2.8, by = 0.4)), cex.axis = 1)
dev.off()


## Fig 17.2 Log USD/GBP Exchange Rate, 1971.01-2012.10 ----
lrate <- data.frame(rate$TIME, log(rate$USDGBP))
names(lrate) <- c("TIME", "LUSDGBP")

pdf(file = "Fig 17. Log USD_GBP Exchange Rate, 1971.01-2012.10.pdf", height = 6, width = 7.5)
plot(lrate, type = "l", axes = FALSE, col = "blue", xlim = c(1, row), ylim = c(0, 1), xlab = "", ylab = "", main = "LUSUK")
box()
axis(side = 1, at = c(seq(4 * 12, row - 9, by = 60)), lab = c(seq(1975, 2010, by = 5)), cex.axis = 0.8)
axis(side = 2, at = c(seq(0.0, 1.0, by = 0.2)), lab = c(seq(0.0, 1.0, by = 0.2)), cex.axis = 1)
dev.off()


## Fig 17.3 Change in Log USD/GBP Exchange Rate, 1971.01-2012.10 ----
dlrate.v <- as.vector(NULL)
i <- 0
for (i in 1:(nrow(lrate) - 1)) {
  dlrate.v[i] <- lrate$LUSDGBP[i + 1] - lrate$LUSDGBP[i]
}
dlrate <- data.frame(lrate$TIME[-1], dlrate.v)
names(dlrate) <- c("TIME", "DLUSDGBP")

pdf(file = "Fig 17. Change in Log USD_GBP Exchange Rate, 1971.01-2012.10.pdf", height = 6, width = 7.5)
plot(dlrate, type = "l", axes = FALSE, col = "blue", xlim = c(1, row), ylim = c(-0.12, 0.12), xlab = "", ylab = "", main = "DLUSUK")
box()
axis(side = 1, at = c(seq(4 * 12, row - 9, by = 60)), lab = c(seq(1975, 2010, by = 5)), cex.axis = 0.8)
axis(side = 2, at = c(seq(-0.12, 0.12, by = 0.04)), lab = c(seq(-0.12, 0.12, by = 0.04)), cex.axis = 1)
dev.off()


## Fig 17.4 Log USD/GBP Exchange Rate, 1971.01-2012.10 Linear Model ----
attach(lrate)
lrate.mod <- lm(LUSDGBP ~ ., data = lrate)
print(summary(lrate.mod))


## Fig 17.5 Log USD/GBP Exchange Rate, 1971.01-2012.10 Residual Plot ----
lrate.act <- LUSDGBP
lrate.pred <- predict(lrate.mod, lrate)
lrate.resid <- lrate.act - lrate.pred
lrate.resid.plot <- 1.1 * lrate.resid + 0.12

pdf(file = "Fig 17. LUSUK Residual Plot.pdf", height = 6, width = 7.5)
plot(lrate.act, type = "l", axes = FALSE, ylim = c(-0.5, 1.0), xlab = "", ylab = "", main = "", col = "red")
lines(lrate.pred, col = "green")
lines(lrate.resid.plot, col = "blue")
abline(h = 0.12, col = "black")
abline(h = 0.27, col = "black", lty = "dashed")
abline(h = -0.03, col = "black", lty = "dashed")

box()
axis(side = 1, at = c(seq(4 * 12, row - 9, by = 60)), lab = c(seq(1975, 2010, by = 5)), cex.axis = 1)
axis(side = 2, at = c(-0.54, -0.32, -0.10, 0.12, 0.34, 0.56), lab = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4), cex.axis = 0.8)
axis(side = 4, at = c(seq(0.0, 1.0, by = 0.2)), lab = c(seq(0.0, 1.0, by = 0.2)), cex.axis = 0.8)

legend(row - 100, -0.15, c("Residual", "Actual", "Fitted"), lty = c(1, 1), col = c("blue", "red", "green"), cex = 0.5)
dev.off()


## Fig 17.6 Change in Log USD/GBP Exchange Rate, 1971.01-2012.10 Linear Model ----
dlrate.only <- data.frame(dlrate$DLUSDGBP)
names(dlrate.only) <- c("DLUSDGBP")
dlrate.mod <- lm(DLUSDGBP ~ ., data = dlrate.only)
print(summary(dlrate.mod))


## Fig 17.7 Change in Log USD/GBP Exchange Rate, 1971.01-2012.10 Residual Plot ----
attach(dlrate.only)
dlrate.act <- DLUSDGBP
dlrate.pred <- predict(dlrate.mod, dlrate.only)
dlrate.resid <- dlrate.act - dlrate.pred
dlrate.resid.plot <- dlrate.resid - 0.105

pdf(file = "Fig 17. DLUSUK Residual Plot.pdf", height = 6, width = 7.5)
plot(dlrate.act, type = "l", axes = FALSE, ylim = c(-0.25, 0.1), xlab = "", ylab = "", main = "", col = "red")
lines(dlrate.pred, col = "green")
lines(dlrate.resid.plot, col = "blue")
abline(h = -0.105, col = "black")
abline(h = -0.08, col = "black", lty = "dashed")
abline(h = -0.13, col = "black", lty = "dashed")

box()
axis(side = 1, at = c(seq(4 * 12, row - 9, by = 60)), lab = c(seq(1975, 2010, by = 5)), cex.axis = 1)
axis(side = 2, at = c(seq(-0.255, -0.005, by = 0.05)), lab = c(seq(-0.15, 0.10, by = 0.05)), cex.axis = 0.8)
axis(side = 4, at = c(seq(-0.15, 0.10, by = 0.05)), lab = c(seq(-0.15, 0.10, by = 0.05)), cex.axis = 0.8)

legend(1, -0.2, c("Residual", "Actual", "Fitted"), lty = c(1, 1), col = c("blue", "red", "green"), cex = 0.5)
dev.off()

# END