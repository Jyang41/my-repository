# Clean up the workspace for the new analysis
rm(list=ls()) 

# set work directory
setwd("C:/Users/jyang/OneDrive - Arizona State University/10 Classes_OneDrive/2023_STP530_Regression")

# import data
HW2.data <- read.table("CH01PR20.txt")
head(HW2.data)
colnames(HW2.data) <- c("min", "ser.no")
head(HW2.data)

# regression model
HW2.mod <- lm(min ~ ser.no, data = HW2.data)
summary(HW2.mod)
#y.hat = b0 + b1X
plot(min ~ ser.no, data = HW2.data, pch = 16, col = "darkgrey",
     main = "HW2_Copier maintenance",
     xlab = "No. of copiers serviced",
     ylab = "Mean service time")
abline(coef(HW2.mod), lwd=1, col="red")

# a. Estimate the change in the mean service time when the number of copiers serviced increases by one. Use a 90 percent confidence interval. Interpret your confidence interval. 90% confidence interval for the expected changes in the mean service time for each 1-unit increase in the number of copiers serviced = 90% CI for β1.
# 90% cI for b1 (by R code)
confint(HW2.mod, level = .90)

# 90% CI for b1 = b1 +/- t((1-a)/2,df)*sb1
b1 <- 15.0352
df <- 43
qt(1-0.1/2,43)
sb1 <- 0.4831
LL <- b1 - 1.681071*sb1 #15.0352 - qt(p=.95, df=43) * 0.4831 # lower limit
UL <- b1 + 1.681071*sb1 #15.0352 + qt(p=.95, df=43) * 0.4831 # upper limit
LL
UL
print("The estimated mean service time changed by each increase in the number of copier serviced is 15.0352 mins. With 90% confidence, we estimate that the expected mean service time changes by somewhat between 14.22307 and 15.84733 for each one-unit increase in the No. of copier served")

# e. Does bo give any relevant information here about the "start-up" time on calls-Le., about the time required before service work is begun on the copiers at a customer location?
b0 <--0.5802
p.value.b0 <- 0.837
print("The 'start-up' time on calls (the intercept) is not likely -0.5802 since it is a negative value. Also, it's not statistically significant with a p-value of 0.837.")

#5.14a. Obtain a 90 percent confidence interval for the mean gervice time on calls in which six copiers are serviced. Interpret your confidence interval.

# 90% confidence interval and prediction interval for X=6
X.6 <- data.frame("ser.no"=6)
head(X.6)
predict(HW2.mod, interval="confidence", newdata=X.6, level=.90) # confidence interval
predict(HW2.mod, interval="prediction", newdata=X.6, level=.90) # prediction interval  

# plot 90% confidence or prediction interval (CI, PI) for X=6
min_val <- min(HW2.data$ser.no, na.rm = TRUE)
max_val <- max(HW2.data$ser.no, na.rm = TRUE)
X.6 <- data.frame("ser.no" = seq(from = min_val, to = max_val, by = 1))
HW2.conf <- predict(HW2.mod, interval="confidence", newdata=X.6, level=.90)
head(HW2.conf,6)
lines(HW2.conf[, 1], lwd=2, col = "red")
lines(HW2.conf[, 2], lwd=2, lty = 3, col = "forestgreen" )
lines(HW2.conf[, 3], lwd=2, lty = 3, col = "forestgreen" )
HW2.pred <- predict(HW2.mod, interval="prediction", newdata=X.6, level=.90)
head(HW2.pred)
lines(HW2.pred[, 2], lwd=2, lty = 2, col = "blue" )
lines(HW2.pred[, 3], lwd=2, lty = 2, col = "blue" )
legend("topleft", legend = c("regression", "confidence", "prediction"),
       lty=c(1,3,2), lwd = 2, col=c("red", "forestgreen", "blue"), cex=1)

