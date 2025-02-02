# Import the dataset
CH01PR28 <- read.table("D:/Dropbox (ASU)/#0 Jiseon 2019 -/10 Classes/2023_STP530_Regression/CH01PR28.txt", quote="\"", comment.char="")

# Rename column
colnames(CH01PR28) <- c("crime.rate","education")
head(CH01PR28,3)

# a1. Obtain the estimated regression function
Regression <- lm(crime.rate ~ education, data = CH01PR28)
summary(Regression)

# a2. Scatterplot of the estimated regression function
plot(CH01PR28$education, CH01PR28$crime.rate)

# a3. The fitted regression line
abline(coef(Regression), col="red")

# b1. estimated difference in the mean crime rate for two counties whose high-school graduation rates differ by one percentage point
estimate_diff <- coef(Regression)["education"]
estimate_diff

# b2. estimated mean crime rate (y.hat) last year in counties with high school graduation percentage X=80
# y.hat <- predict(Regression)
# y.hat
last.year <- data.frame(education = 80)
y.hat <- predict(Regression, newdata=last.year) 
y.hat 

# b3. point estimation of ε10
# εi = Yi - E{Yi}, 
# ei = Yi - Yi^
y.hat <- predict(Regression)
y.hat
e <- CH01PR28$crime.rate - y.hat
e[10]

#Get residuals directly
#residuals(Regression)[10]
#Regression$residuals[10]

# b4. point estimation of σ^2 = s^2 (estimated variation of error)
# s^2 = MSE = SSE/(n-p), mean squared error 
# SSE = sum squared error = Σ(e^2)
# s= residual standard error = sqrt(sum(e ^ 2) / (84 - 2))
# S^2 = Residual variance = sum(e ^ 2) / (84 - 2)
SSE <- sum(e^2)
MSE <- SSE/(84 -2)
MSE
