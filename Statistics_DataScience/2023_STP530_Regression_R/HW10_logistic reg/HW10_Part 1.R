
# STP 530 Lab 3 Logistic regression

#-------------------------------------------

rm(list=ls())
setwd("C:/Users/jyang/OneDrive - Arizona State University/10 Classes_OneDrive/2023_STP530_Regression/R")
library(car)
# install.packages("caret")
library(ggplot2)
library(lattice)
library(caret)
# install.packages("OptimalCutpoints")
library(OptimalCutpoints)

#--------------------------------------------------------
# Load data
hire.data <- read.csv("DISCRIM.csv")

# Check data
head(hire.data )
str(hire.data )

summary(hire.data )
table(hire.data $HIRE)
table(hire.data $GENDER)
pairs(hire.data )

par(mfrow=c(1,3))
with(hire.data , sunflowerplot(EDUC, HIRE))
with(hire.data , sunflowerplot(EXP, HIRE))
with(hire.data , sunflowerplot(GENDER, HIRE))

cor(hire.data )

#--------------------------------------------------------
# Fit logistic regression model
m <- glm(HIRE ~ EDUC + EXP + GENDER, data=hire.data , family=binomial)
summary(m)

#--------------------------------------------------------
# Model Predictions

# ?predict
predict(m, type="link")
predict(m, type="response")

# A male candidate straight out of college

predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1), type="response")

# Manually calculating the quantity

my.logit <- (-14.2483 + 1.1549 * 6 + 0.9098 * 3 + 5.6037 * 1)
pi <- exp(my.logit) / (1 + exp(my.logit))
pi

# A male candidate with a master's degree and 6 years of work experience

predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1), type="response")

# Manually calculating the quantity

my.logit <- (-14.2483 + 1.1549 * 6 + 0.9098 * 3 + 5.6037 * 1)
pi <- exp(my.logit) / (1 + exp(my.logit))
pi

#--------------------------------------------------------
# Graphing of fitted model curves

png("hire_graph.png", height=500, width=700)
par(mar=c(6.1, 5.1, 4.1, 2.1))

summary(hire.data $EXP)
EXP.plot <- seq(0, 12, by=.1)

# GENDER == 0 & EDUC == 4 (Female, Bachelor's degree)

pi <- predict(m, newdata=data.frame(EDUC=4, EXP=EXP.plot, GENDER=0), type="response")
plot(EXP.plot, pi, xlim=c(0, 12), ylim=c(0, 1), 
     xlab="Years of Working Experience", 
     ylab="Probability of Being Hired", 
     type='l', col='red', lty="solid", lwd=3, 
     cex.axis=1.5, cex.lab=1.5)

# GENDER == 1 & EDUC == 4 (Male, Bachelor's degree)

pi <- predict(m, newdata=data.frame(EDUC=4, EXP=EXP.plot, GENDER=1), type="response")
lines(EXP.plot, pi, col='blue', lty="solid", lwd=3)

# GENDER == 0 & EDUC == 6 (Female, Master's degree)

pi <- predict(m, newdata=data.frame(EDUC=6, EXP=EXP.plot, GENDER=0), type="response")
lines(EXP.plot, pi, col='red', lty="dashed", lwd=3)

# GENDER == 1 & EDUC == 6 (Male, Master's degree)

pi <- predict(m, newdata=data.frame(EDUC=6, EXP=EXP.plot, GENDER=1), type="response")
lines(EXP.plot, pi, col='blue', lty="dashed", lwd=3)

legend(x="topleft", 
       legend=c("Female, Bachelor's", "Male, Bachelor's", "Female, Master's", "Male, Master's"), 
       col=c("red", "blue", "red", "blue"), lty=c("solid", "solid", "dashed", "dashed"))

# dev.off()

#--------------------------------------------------------
# Binary classification

pi <- predict(m, type="response")

predicted.class <- rep(0, nrow(hire.data ))
predicted.class[pi > .8] <- 1

observed.class <- hire.data $HIRE

side.by.side <- data.frame(predicted.pi=round(pi,2), predicted.class, observed.class)
side.by.side[order(side.by.side$predicted.pi),]

caret::confusionMatrix(data=factor(predicted.class), reference=factor(observed.class), positive="1")

# Optimal cutpoint based on Youden

ROC.data <- data.frame(pi, observed.class=hire.data $HIRE)

out <- optimal.cutpoints(X="pi", status="observed.class", tag.healthy = 0, 
										methods="Youden", data=ROC.data)
out

tmp <- out$Youden$Global$measures.acc
Youden.results <- data.frame(tmp$cutoffs, tmp$Se, tmp$Sp, tmp$Se + tmp$Sp - 1)
colnames(Youden.results) <- c("cutoffs", "Sensitivity", "Specificity", "Youden")
Youden.results

#--------------------------------------------------------
# Confidence interval for model coefficients

summary(m)$coefficients
qnorm(p=.975) # z-distribution

# 95% CI of b_EDU, using z-distribution
1.1549 - 1.96 * 0.6023 # LL
summary(m)$coefficients[2, 1] - qnorm(.975) * summary(m)$coefficients[2, 2]

1.1549 + 1.96 * 0.6023 # UL
summary(m)$coefficients[2, 1] + qnorm(.975) * summary(m)$coefficients[2, 2]



#--------------------------------------------------------
# Confidence interval for predicted probabilities (pi-hat)

# The point estimate: the predicted probability of the given case

predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1), type="response")

#----------------- HW11 ---------------------------------
# To compute the confidence interval for pi, you need to start from the logit
# scale (i.e., the "link" type).

my.pred <- predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1),level=.95, 
                   type="link", se.fit=T)
my.pred # 1.030039

z.crit <- qnorm(p=.975)
z.crit # 1.959964

LL.logit <- my.pred$fit - z.crit * my.pred$se.fit
LL.logit # -1.00459
1.01425 - 1.959964 * 1.030039 # -1.004589, manual, LL.logit

UL.logit <- my.pred$fit + z.crit * my.pred$se.fit
UL.logit # 3.03309
1.01425 + 1.959964 * 1.030039 # 3.033089, manual, UL.logit

# Then convert the lower and upper limits to the probability scale
LL.pi <- exp(LL.logit) / (1 + exp(LL.logit))
LL.pi # 0.2680399 
exp(-1) / (1 + exp(-1)) # 0.2689414, manual calculation

UL.pi <- exp(UL.logit) / (1 + exp(UL.logit))
UL.pi # 0.9540469
exp(3) / (1 + exp(3)) # 0.9525741, manual calculation

c(LL.pi, UL.pi)

# Note the point estimator of pi is not the center of the confidence interval
predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1), type="response")

#--------------------------------------------------------
# Likelihood-Ratio Test for GENDER
m.F <- glm(HIRE~EDUC+EXP+GENDER, data=hire.data , family=binomial)
summary(m.F)

m.R <- glm(HIRE~EDUC+EXP, data=hire.data , family=binomial)
summary(m.R)

# Test-statistic
26.056 - 14.735 # Test-statistic, 11.321

# DF
25 - 24

# P-value
pchisq(q=11.321, df=25-24, lower.tail=F)

#--------------------------------------------------------
#--------------------------------------------------------
# Likelihood-Ratio Test for Null vs Full
m.F <- glm(HIRE~EDUC+EXP+GENDER, data=hire.data , family=binomial)
summary(m.F)

35.165 - 14.735 # Test-statistic, 20.43
27 - 24 # DF

# P-value
pchisq(q=20.43, df=27-24, lower.tail=F)

#--------------------------------------------------------
# Likelihood-Ratio Test of model utility

ls(m)
m$null.deviance
m$deviance
m$df.null
m$df.residual
pchisq((m$null.deviance - m$deviance), df=(m$df.null-m$df.residual), lower.tail=F)

#--------------------------------------------------------
# Pseudo R-squares
# McFadden's Pseudo R-square

1 - m$deviance / m$null.deviance  # The deviances given in R outputs are -2 times of the log-likelihood
logLik(m)  # This gives the log-likelihood of the model


#--------------------------------------------------------
# Tjur'a Pseudo R-square
sel <- hire.data $HIRE == 1
hire.data [sel,]
hire.data [!sel,]

predict(m, type="response")[sel]
mean(predict(m, type="response")[sel]) #0.7428251

predict(m, type="response")[!sel]
mean(predict(m, type="response")[!sel]) #0.1218197 

(mean(predict(m, type="response")[sel]) - mean(predict(m, type="response")[!sel]))
0.7428251 - 0.1218197 #0.6210054


#--------------------------------------------------------
# Diagnostics
# Influence plot
par(mfrow=c(1,1))
car::influencePlot(m)

# Largest Cook's D = 0.7815681
p <- 4
n <- nrow(hire.data )
pf(q = .782, df1 = p, df2 = n - p) #0.4520179
0.4520179 *100 # percentile score

# DFBETA
dfbetas(m)
data.frame(hire.data , round(dfbetas(m), 2))

