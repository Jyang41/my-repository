
rm(list=ls())

# Aim: To identify factors that can explain variations in salaries among 
# programmers and engineers in the Silicon Valley. 

# install.packages("MASS")
# install.packages("lmtest")
# library(MASS)
# library(lmtest)
library(freqparcoord)
library(car)
library(Hmisc)


# Loading the dataset and inspect
data(prgeng)
head(prgeng)
str(prgeng)
# help(prgeng)

# age, with a U(0,1) variate added for jitter
# cit, citizenship; 1-4 code various categories of citizens; 
                  # 5 means noncitizen (including permanent residents
# educ: 01-09 code no college; 10-12 means some college; 13 BS degree, 14 MS, 
                # 15 a professiona deal and 16 Dr.
# engl, English proficiency
# occ, occupation
# birth, place of birth
# wageinc, wage income
# wkswrkd, number of weeks worked
# yrentry, year of entry to the U.S. (0 for natives)
# powpuma, location of work
# gender, 1 for male, 2 for female

# ===== DATA =============== DATA =============== DATA =============
# Reduce data to remove non-labor-income, volunteer, below minimum wage, and inspect
prgeng_reduce=subset(prgeng,wageinc>0 & wageinc<=200000 & wkswrkd>0 
                     & (wageinc/wkswrkd)>= 5.15)

head(prgeng_reduce)
str(prgeng_reduce)
Hmisc::describe(prgeng_reduce)

# Rearrange column and construct newdata frame
library(dplyr)
mydata <- prgeng_reduce %>%
  dplyr::select(wageinc, age, educ, engl, wkswrkd, birth, cit, sex, occ, 
                yrentry, powspuma)
head(mydata)

# pairs(mydata)
cor(mydata) # high multicolinearlity: birth, yrentry, cit


# ===== Variables ============ Variables ============= Variables =============

# wageinc(wage income): 
# Dependent variable Y 
attach(mydata)
summary(wageinc)
sd(wageinc, na.rm=T)
par(mfrow=c(1, 3))
hist(wageinc)
#boxplot(wageinc, xlab="wage.income", ylab = '$')

# age
#par(mfrow=c(1, 4))
summary(age)
hist(age)
plot(wageinc ~age, data = mydata)

# wkswrkd(Weeks worked)
#par(mfrow=c(1, 4))
summary(wkswrkd)
hist(wkswrkd)
#plot(wageinc ~ wkswrkd, data = mydata)

# educ(Education): ~09-high; 10-12 college; 13-BS; 14-MS; 15-prof; 16-Dr
par(mfrow=c(1, 4))
table(educ)
hist(educ)
plot(wageinc ~ educ)

mydata <- mydata %>% 
  mutate(educ_category = case_when(
    educ >= 01 & educ <= 09 ~ 9,   # High school or less
    educ >= 10 & educ <= 12 ~ 11,  # College (no degree, associate degree)
    educ == 13 ~ 13,               # Bachelor's degree
    educ == 14 ~ 14,               # Master's degree
    educ == 15 & educ <= 16~ 16   # Professional or Dr degree
  ))

mydata$educ_category <- factor(mydata$educ_category,
                          levels = c(9, 11, 13, 14, 16),
                          labels = c("High", "College", "BS", "MS", "Dr/Prof"))
levels(mydata$educ_category)
head(mydata)
table(mydata$educ_category)
barplot(table(mydata$educ_category), # Bar plot for educ_category
        main = "Distribution of Education Categories", 
        xlab = "Education.Category", 
        ylab = "Frequency")
plot(wageinc ~ educ_category, data = mydata)

# engl(English): 1-very-well, 2-well, 3-not-well, 4-mot-at-all
par(mfrow=c(1, 4))
table(engl)
hist(engl)
plot(wageinc ~ engl)

mydata <- mydata %>%
  mutate(engl_category = case_when(
    engl == 0 ~ 0,   # Speak only English
    engl >= 10 & engl <= 11 ~ 10,  # Very well
    engl >= 20 & engl <= 21 ~ 20,  # Well
    engl >= 30 & engl <= 31 ~ 30,  # Not well
    engl >= 40 & engl <= 41 ~ 40   # Not at all
    # Add a default case if needed, e.g.:
    # TRUE ~ "Unknown"
  ))

mydata$engl_category <- factor(mydata$engl_category,
                      levels = c(0, 10, 20, 30, 40),
                      labels = c("Only.Eng", "Very.well", 
                                 "well", "not.well", "not.at.all"))
head(mydata)
levels(mydata$engl_category)
table(mydata$engl_category)
barplot(table(mydata$engl_category), # Bar plot for educ_category
        main = "Distribution of English Categories", 
        xlab = "Eng.Category Proficiency", 
        ylab = "Frequency")
plot(wageinc ~ engl_category, data = mydata)

# birth(Place of Birth)
par(mfrow=c(1, 4))
table(birth)
hist(birth)
plot(wageinc ~ birth)

mydata <- mydata %>%
  mutate(birth_category = case_when(
    birth >= 001 & birth <= 056 ~ 1,
    birth >= 060 & birth <= 096 ~ 2,
    birth >= 100 & birth <= 157 ~ 3,
    birth >= 158 & birth <= 159 ~ 4,
    birth == 160 ~ 3,
    birth == 161 ~ 4,
    birth >= 162 & birth <= 199 ~ 3,
    birth >= 200 & birth <= 299 ~ 4,
    birth >= 300 & birth <= 399 ~ 5,
    birth >= 400 & birth <= 499 ~ 6,
    birth >= 500 & birth <= 553 ~ 7,
    TRUE ~ 8  # For values that don't fall into the above categories
  ))
str(mydata)
table(mydata$birth_category)

mydata$birth_category <- factor(mydata$birth_category, 
                  levels = c(1, 2, 3, 4, 5, 6, 7),
                  labels = c("U.S.", "US.Isld", "Europe", "Asia", 
                             "America", "Africa", "Oceania"))

barplot(table(mydata$birth_category), # Bar plot for educ_category
        main = "Distribution of Place of Birth Categories",
        xlab = "Place of Birth",
        ylab = "Frequency")
plot(wageinc ~ birth_category, data = mydata)

# cit(citizenship): 
# 1-US.born, 2-US.colonies, 3-US.parents, 4-earned.citizenship, 5-foreigner
par(mfrow=c(1, 4))
table(cit)
hist(cit)
plot(wageinc ~ cit)

mydata <- mydata %>%
  mutate(cit_category = case_when(
    cit >= 1 & cit <= 3 ~ 1,   # US citizen
    cit == 4 ~ 4,              # Acquired U.S. citizenship
    cit == 5 ~ 5               # Not a US citizen
  ))

mydata$cit_category <- factor(mydata$cit_category,
                      levels = c(1, 4, 5),
                      labels = c("US.citizen", "Acquired.US", "Not.US.citizen"))

table(mydata$cit_category)
str(mydata)

barplot(table(mydata$cit_category), # Bar plot for cit_category
        main = "Distribution of Citizenship Categories", 
        xlab = "Citizenship Category", 
        ylab = "Count")
boxplot(wageinc ~ cit_category, data = mydata)

# sex
par(mfrow=c(1, 4))
table(sex)
hist(sex)
plot(wageinc ~ sex)

mydata$sex_category <- factor(mydata$sex,
                              levels = c(1, 2),
                              labels = c("Male", "Female"))
table(mydata$sex_category)
barplot(table(mydata$sex_category), # Bar plot for sex_category
        main = "Distribution of gender Categories", 
        xlab = "gender.category", 
        ylab = "Frequency")
boxplot(wageinc ~ sex_category, data=mydata)

# occ (occupation):
# 100-Computer scientists and systems analysts (CSA)
# 101-Computer programmers (Prgrmr)
# 102-Computer software engineers (Sft.Engnr)
# 106-Database administrators (DBA)
# 141-Electrical and electronics engineers (Elc.Engnr)
# 140-Computer hardware engineers (Hrd.Engnr)
par(mfrow=c(1, 4))
table(occ)
# hist(occ)
# boxplot(wageinc ~ occ, data = mydata)

mydata$occ_category2 <- factor(mydata$occ,
              levels = c(100, 101, 102, 106, 140, 141),
              labels = c("CSA", "Prgrmr", "Sft.Engnr", "DBA", 
                         "Hrd.Engnr", "Elc.Engnr"))
table(mydata$occ_category2)

barplot(table(mydata$occ_category2), 
        main = "Distribution of occupation Categories",
        xlab = "occupation.category2", 
        ylab = "Frequency")
boxplot(wageinc ~ occ_category2, data = mydata)
table(mydata$occ_category2)

mydata <- mydata %>%
  mutate(occ_category = case_when(
    occ %in% c(102, 101, 100, 106) ~ "Software Engineer",
    occ %in% c(141, 140) ~ "Hardware Engineer",
    TRUE ~ "Other" 
  ))
table(mydata$occ_category)
str(mydata)

barplot(table(mydata$occ_category), 
        main = "Distribution of occupation Categories",
        xlab = "occupation.category", 
        ylab = "Frequency")
boxplot(wageinc ~ occ_category, data = mydata)

# yrentry(year of entry to the US, 0 for native)
par(mfrow=c(1, 4))
table(yrentry)
hist(yrentry)
boxplot(wageinc ~yrentry, data = mydata)

mydata <- mydata %>%
  mutate(yrentry_category = case_when(
    yrentry == 0 ~ "US.native",      # US native
    yrentry >= 1920 & yrentry < 1930  ~ "1920",
    yrentry >= 1930 & yrentry < 1940  ~ "1930",
    yrentry >= 1940 & yrentry < 1950  ~ "1940",
    yrentry >= 1950 & yrentry < 1960  ~ "1950",
    yrentry >= 1960 & yrentry < 1970  ~ "1960",
    yrentry >= 1970 & yrentry < 1980  ~ "1970",
    yrentry >= 1980 & yrentry < 1990  ~ "1980",
    yrentry >= 1990 & yrentry < 2000  ~ "1990",
    yrentry >= 20000 ~ "2000",
  ))
mydata$yrentry_category <- factor(mydata$yrentry_category, 
                        levels = c("US.native", "1920", "1930", "1940", "1950",
                                   "1960", "1970", "1980", "1990", "2000"))
table(mydata$yrentry_category)

barplot(table(mydata$yrentry_category), 
        main = "Distribution of Year of Entry Categories",
        xlab = "Category", 
        ylab = "Frequency")

boxplot(wageinc ~ yrentry_category, data = mydata, 
        main = "Wage Income by Year of Entry Category",
        xlab = "Year of Entry Category", 
        ylab = "Wage Income")

# powspuma (location of work)
par(mfrow=c(1, 4))
table(powspuma)
plot(powspuma)
boxplot(wageinc ~ powspuma, data = mydata) 
mydata <- mydata %>%
  mutate(location_category = case_when(
    powspuma >= 6010 & powspuma <= 6700 ~ 2, # CA
    powspuma < 6010 | powspuma > 6700 ~ 1,   # outside CA
    TRUE ~ NA_integer_ # for values that do not fit into the above categories
  ))

mydata$location_category <- factor(mydata$location_category,
                              levels = c(1, 2),
                              labels = c("Not.CA", "CA"))
table(mydata$location_category)
barplot(table(mydata$location_category), 
        main = "Distribution of location Categories",
        xlab = "Location of work", 
        ylab = "Frequency")
boxplot(wageinc ~ location_category, data = mydata)

mydata <- mydata %>%
  mutate(location_category2 = case_when(
    powspuma >= 6010 & powspuma <= 6700 ~ 1, # CA
    #powspuma < 6010 | powspuma > 6700 ~ 1,   # outside CA
    powspuma == 10100 ~ 3, # Delaware
    powspuma >= 12088 & powspuma <= 12099 ~ 4, #Florida
    powspuma >= 13060 & powspuma <= 13130 ~ 5, #Georgia
    powspuma == 15100 ~ 6, #Hawaii
    powspuma == 16300 ~ 7, #Idaho
    powspuma >= 17300 & powspuma <= 17500 ~ 8, #Illinois
    powspuma >= 18020 & powspuma <= 18090 ~ 9, #Indiana
    powspuma == 19500 ~ 10, #Iowa
    powspuma == 22800 ~ 11, #Louisiana
    powspuma == 24400 ~ 12, #Maryland
    powspuma == 25090 ~ 13, #Massachusetts
    powspuma == 26139 ~ 14, # Michigan
    powspuma >= 29400 & powspuma <= 29700 ~ 15, # Missouri
    powspuma >= 6010 & powspuma <= 6700 ~ 16, # Montana
    powspuma >= 6010 & powspuma <= 6700 ~ 17, # Nebraska
    powspuma >= 6010 & powspuma <= 6700 ~ 18, # Nevada
    powspuma >= 6010 & powspuma <= 6700 ~ 19, # New Mexico
    powspuma >= 6010 & powspuma <= 6700 ~ 20, # New York
    powspuma >= 6010 & powspuma <= 6700 ~ 21, # North Carolina
    powspuma >= 6010 & powspuma <= 6700 ~ 22, # North Dakota
    powspuma >= 6010 & powspuma <= 6700 ~ 23, # Ohio
    powspuma ==4100 & powspuma ==4100 ~ 24, #Rhode Island
    powspuma >=41200 & powspuma <= 41500 ~ 25,  #Oregon
    powspuma >=42100 & powspuma <=42180 ~ 26,   #Pennsylvania
    powspuma >=4300 ~ 27, #Arizona
    powspuma >= 47070& powspuma <=47100 ~ 28, #Tennessee
    powspuma >=48100 & powspuma <=48230 ~ 29, #Texas
    powspuma >=49300 ~ 30, #Utah
    powspuma >=50100 ~ 31, #Vermont
    powspuma >=51010 & powspuma <=51090 ~ 32, #Virginia
    powspuma >=53060 & powspuma <=53080 ~ 33, #Washington
    TRUE ~ NA_integer_ # for values that do not fit into the above categories
  ))
mydata$location_category2 <- factor(mydata$location_category2,
      levels = c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
                 25,26,27,28,29,30,31,32,33),
      labels = c("CA","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois",
                 "Indiana","Iowa","Louisiana","Maryland","Massachusetts",
                 "Michigan","Missouri","Montana","Nebraska","Nevada",
                 "New Mexico","New York","North Carolina","North Dakota",
                 "Ohio","Rhode Island","Oregon","Pennsylvania","Arizona",
                 "Tennessee","Texas","Utah","Vermont","Virginia","Washington"))
table(mydata$location_category2)
str(mydata)
par(mfrow=c(1, 2))
barplot(table(mydata$location_category2), 
        main = "Distribution of location Categories",
        xlab = "Location of work.2", 
        ylab = "Frequency")
boxplot(wageinc ~ location_category2, data = mydata, cex.axis = 0.5, las = 2)



#======== MODELING ============= MODELING =========== MODELING ==========
#======== deal with missing data
Hmisc::describe(mydata) # missing data found

# Find complete cases for the variables used in the most complex model
complete_rows <- complete.cases(mydata[c("wageinc", "age", "educ_category", 
                "engl_category", "wkswrkd", "birth_category", "cit_category", 
                "sex_category", "occ_category2", "yrentry_category", 
                "location_category")])

# Subset the data to only include complete rows
mydata_complete <- mydata[complete_rows, ]
Hmisc::describe(mydata_complete) # no missing data

#======== m0,1: Initial Model, include all
library(rgl)
library(rsq)
m0 <- lm(wageinc ~ age + educ + engl + wkswrkd + birth + cit + sex + occ + 
           yrentry + powspuma, data = mydata_complete)
summary(m0) # R^2= 0.2942
par(mfrow=c(1, 4))
plot(m0)
vif(m0) # high: cit and yrentry

# caterogies
m1 <- lm(wageinc ~ age + educ_category + engl_category + wkswrkd + 
           birth_category +cit_category + sex_category + occ_category2 + 
           yrentry_category + location_category, data = mydata_complete)
summary(m1) 
par(mfrow=c(1, 4))
plot(m1) # Diagnostics: multicollinearity, non-linear relationship; unequal error, ouliers
# 1. Residual vs fitted: not linear relationship, non-constant variance =>a sign of heteroscedasticity.
# 2. Q-Q: okay
# 3. Scale-Location: potential heteroscedasticity
# 4.Risiduals vs Leverage: Outliers, leverage points,influencers. May be influential points.

summary(m0)$r.squared # 0.2941575 
summary(m1)$r.squared # 0.3277041 ==> take
anova(m0,m1) # p<2.2e-16, statistically significantly different
rsq.partial(objF=m1, objR=m0) # 0.04752715, 4.75% practical significance => take

#======== m2: drop yrentry_category
m2 <- lm(wageinc ~ age + educ_category + engl_category + wkswrkd + 
          birth_category +cit_category + sex_category + occ_category2 + 
          location_category, data = mydata_complete)
summary(m2)
plot(m2) # Diagnostics: linearity, constant variance, and normality of residuals
vif(m2) # multicollinearity: cit & birth
# 1. Residual vs fitted: multicollinearity, non-linear relationship; unequal error, ouliers
# 2. Q-Q: ok.
# 3. Scale-Location: residuals variance, not constant => heteroscedasticity
# 4.Risiduals vs Leverage: Outliers, leverage points,influencers. May be influential points.

summary(m1)$r.squared # 0.3277041 ==> drop yrentry_category
summary(m2)$r.squared # 0.3263824 ==> reduced small, take
anova(m1,m2) # p=1.728e-05, thus statistically significantly different
rsq.partial(objF=m1, objR=m2) # 0.001962129, practically not significant 
#                               ==> to drop since more parsimonious

#======== m3: drop birth_category
m3 <- lm(wageinc ~ age + educ_category + engl_category + wkswrkd + 
           cit_category + sex_category + occ_category2 + 
           location_category, data = mydata_complete)
summary(m3)
plot(m3) # Diagnostics: multicollinearity, non-linear relationship; unequal error, ouliers
vif(m3) # multicollinearily removed. 
# 1. Residual vs fitted: multicollinearity, non-linear relationship; unequal error, ouliers
# 2. Q-Q: ok.
# 3. Scale-Location:the variance of the residuals is not constant.potential heteroscedasticity
# 4.Risiduals vs Leverage: Outliers, leverage points,influencers. May be influential points.

summary(m2)$r.squared # 0.3263824 ==> drop birth_category
summary(m3)$r.squared # 0.3253361 ==> reduced small, take
anova(m2,m3) # p=0.000153, droping birth_category lead statistically significantly different
rsq.partial(objF=m2, objR=m3) # 0.001550858, practically very small improvement 
#                               ==> to drop since more parsimonious

#======== Transformation 

# Remove rows where wageinc is NA, zero, or negative
mydata_complete.redu <- mydata_complete[!is.na(mydata_complete$wageinc) & 
                                          mydata_complete$wageinc > 0, ]

# wageinc.log
mydata_complete.redu$wageinc.log <- log(mydata_complete.redu$wageinc) 
head(mydata_complete.redu,10)
Hmisc::describe(mydata_complete.redu) 

m4 <- lm(wageinc.log ~ age + educ_category + engl_category + wkswrkd + 
            cit_category + sex_category + occ_category2 + 
            location_category, data = mydata_complete.redu)
summary(m4)
plot(m4) # Diagnostics: linearity, constant variance, and normality of residuals
vif(m4) # good
# 1. Residual vs fitted: better linear relationship, constant error variances, and outliers
# 2. Q-Q: ok.
# 3. Scale-Location:looked concentrated but not extreme heteroscedasticity
# 4.Risiduals vs Leverage: Outliers, leverage points,influencers. May be influential points.

summary(m3)$r.squared # 0.3253361 ==> drop birth_category
summary(m4)$r.squared # 0.4704995 ==> increased, take
rsq.partial(objF=m4, objR=m3) # 0.2151641 ==> 21.5% improved, take the log-Y


#======== Exploring Higher-Order Polynomial Terms and Interactions

# age2
mydata_complete.redu$age2 <- mydata_complete.redu$age ^ 2

m5 <- lm(wageinc.log ~ age + age2 + educ_category + engl_category + wkswrkd + 
           cit_category + sex_category + occ_category2 + location_category, 
         data = mydata_complete.redu)
summary(m5)
plot(m5)

summary(m4)$r.squared # 0.4704995
summary(m5)$r.squared # 0.4985745
anova(m4,m5) # p< 2.2e-16, statistically significant
rsq.partial(objF=m5, objR=m4) # 0.05302169 => 5.3 % improved, take m5

# age:educ_category; 
# benefit of having a certain level of education on income, 
# depending on the age of the individual
# impact of age on income depending on the education level of a person 
m6 <- lm(wageinc.log ~ age + age2 + educ_category + engl_category + wkswrkd 
         + cit_category + sex_category + occ_category2
         + location_category + age:educ_category, data = mydata_complete.redu)
summary(m6)
plot(m6)

summary(m5)$r.squared # 0.4985745 
summary(m6)$r.squared # 0.5051525 ==> 0.66% improved, may not necessary
anova(m5,m6) # p<2.2e-16, statistically different
rsq.partial(objF=m6, objR=m5) # 0.01311858, practically 1.3% improved => drop m6

vif(m5)

#breakpoint

#======== Automated backward selection with AIC
max.mod <- lm(wageinc.log ~ age + age2 + educ_category + engl_category + wkswrkd 
             + cit_category + sex_category + occ_category2
             + location_category, data = mydata_complete.redu)
bwd.mod <- step(max.mod, direction='backward')
# Start:  AIC=-18021.9
# wageinc.log ~ age + age2 + educ_category + engl_category + wkswrkd + 
#   cit_category + sex_category + occ_category2 + location_category
# 
# Df Sum of Sq    RSS    AIC
# <none>                           6116.8 -18022
# - location_category  1     14.88 6131.6 -17982
# - cit_category       2     17.86 6134.6 -17975
# - engl_category      4     28.10 6144.9 -17950
# - sex_category       1     46.48 6163.2 -17893
# - occ_category2      5    124.98 6241.7 -17681
# - educ_category      4    323.41 6440.2 -17137
# - age2               1    342.48 6459.2 -17079
# - age                1    461.81 6578.6 -16762
# - wkswrkd            1   2425.41 8542.2 -12233

summary(bwd.mod)
## E{wageinc.log}= 6.18 + 0.09913(age) -0.001005(age^2) + 0.1422(educ_categoryCollege)  
## + 0.3989(educ_categoryBS) + 0.4758(educ_categoryMS) + 0.2917(educ_categoryDr/Prof) - 0.03968(engl_categoryVery.well)  
## - 0.1041(engl_categorywell) -0.2379(engl_categorynot.well) -0.6588(engl_categorynot.at.all)  
## + 0.03953(wkswrkd)  + 0.04642(cit_categoryAcquired.US) + 0.1150(cit_categoryNot.US.citizen) 
## - 0.1228(sex_categoryFemale) + 0.05252(occ_category2Prgrmr) + 0.2153(occ_category2Sft.Engnr) + 0.005418(occ_category2DBA) 
## + 0.1267(occ_category2Hrd.Engnr) + 0.1395(occ_category2Elc.Engnr) + 0.1051(location_categoryCA) 


# ========= DIAGNOSTIC: Influencer, outliers ===================================
# ========= Influence plot: The area of the circles are proportional to 
#                 the influence measure Cook's distance (below)
par(mfrow=c(1, 1))
influencePlot(m5, id=list(labels=mydata_complete.redu$instant)) 
# The largest Cook's D value for this model is 0.04583435.

# -------------------------------------------------------------------------
# Cook's distance: 100 * pf(q =0.0458, df1 = p, df2 = (n - p)) is 
# to find the percentile of Cook's D value in an F-distribution.
# F(df1=p, df2=n-p) results percentile (2.519037e-09) < 0.5 indicating that 
# there is no major influencer on this regression model.

n <- nrow(mydata_complete.redu) # 17338
p <- 21
100 * pf(q =0.04583435, df1 = p, df2 = (n - p)) # 2.519037e-09

D <- cooks.distance(m5)

# Recalculate percentiles for the subsetted 'D'
percentiles.all <- 100 * pf(q = D, df1 = p, df2 = (n - p)) 

# Create the data frame with the corrected matching number of rows
Cooks.D.data <- data.frame(Cooks.D = round(D, 3), 
                           percentile = round(percentiles.all, 3), 
                           mydata_complete.redu)
Cooks.D.data <- Cooks.D.data[order(Cooks.D.data$Cooks.D, decreasing = T), ]
View(Cooks.D.data) # no major influence



#=========== Research ============ Research ============ Research ============
#============= m5, CI/PI and gender difference ===============
## Among computer programmers and engineers in the Silicon Valley in 1999, 
## excluding those with unpaid volunteer, below federal minimum wage, 
## nonlabor-income, or high-income over $200,000,

#============= BS CI & PI =======================
# 95% CI and PI of E{Y}, Male-BS
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^11.17247 =$71,144.6, e^11.21449 =$74,197.8), for a 35-years-
# old male citizen living in CA with BS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^10.02835  =$22,659.9 , e^12.35861 =$232,957), for a 35-years-old male
# citizen living in CA with BS degree who speaks Engl-only which is 
# equivalent to ($41,920.81,$430,970.4) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-BS
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^11.04446  =$62,596.2, e^11.09683=$65,961.7), for a 35-years-
# old male citizen living in CA with BS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.905414  =$20,038.6, e^12.23588=$206,051), for a 35-years-old male
# citizen living in CA with BS degree who speaks Engl-only which is 
# equivalent to ($37,071.40,$381,194.35) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 1: 
# With 95% confidence, the analysis using model m5 indicates a significant gender-based
# wage gap for 35-year-old software engineers with BS degrees in California, 
# with males earning approximately $8,398  more annually than females in average
# ($72,655.2 (=fit e^11.19348 ) for males vs. $64,257.3 (=fit e^11.07065 ) for females).



#============= MS CI & PI =======================
# 95% CI and PI of E{Y}, Male-MS
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="MS", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Male", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^11.24391 =$76,413.1, e^11.29675=$80,559.4), for a 35-years-
# old male citizen living in CA with MS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="MS", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Male", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^10.10509 =$24,467.2, e^12.43557=$251,594), for a 35-years-old male
# citizen living in CA with MS degree who speaks Engl-only which is 
# equivalent to ($45,264.32,$465,448.9) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-MS
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="MS", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Female", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^11.11681 =$67,292.9, e^11.17819=$71,552.7), for a 35-years-
# old male citizen living in CA with MS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="MS", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Female", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.982155 =$21,636.9, e^12.31284=$222,535), for a 35-years-old male
# citizen living in CA with MS degree who speaks Engl-only which is 
# equivalent to ($40,028.27,$411,689.75) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 1: 
# With 95% confidence, the analysis using model m5 indicates a significant gender-based
# wage gap for 35-year-old software engineers with MS degrees in California, 
# with males earning approximately $9,069  more annually than females in average
# ($78,458.9 (=fit e^11.27033) for males vs. $69,390.1 (=fit e^11.1475) for females).


#============= phD CI & PI =======================

# 95% CI and PI of E{Y}, Male-Dr/Prof
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="Dr/Prof", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Male", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.99463 =$59,553.5, e^11.17781=$71,525.5), for a 35-years-
# old male citizen living in CA with Dr/Prof degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="Dr/Prof", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Male", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.917688 =$20,286.0, e^12.25475=$209,976), for a 35-years-old male
# citizen living in CA with Dr/Prof degree who speaks Engl-only which is 
# equivalent to ($37,529.1,$388,456.0) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-Dr/Prof
predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="Dr/Prof", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Female", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.8704 =$52,596.2, e^11.05638=$63,346.8), for a 35-years-
# old female citizen living in CA with Dr/Prof degree who speaks Engl-only.

predict(m5, newdata=data.frame(age=35, age2=1225, educ_category="Dr/Prof", 
            engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
            sex_category="Female", occ_category2="Sft.Engnr", 
            location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.794744 =$17,939.2, e^12.13203=$185,726), for a 35-years-old female
# citizen living in CA with Dr/Prof degree who speaks Engl-only which is 
# equivalent to ($33,187.52, $343,593.00) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 2: 
# With 95% confidence, the analysis using model m5 indicates a significant gender-based
# wage gap for 35-year-old software engineers with Dr/Prof degrees in California,  
# with males earning approximately $7,544 more annually than females in avereage
# ($65,265.6 (=fit e^11.08622 ) for males vs. $57,721.8 (=fit e^10.96339 ) for females).



#============= Opportunity cost ========================================

#============= BS opportunity cost, 23-years-old =======================
# 95% CI and PI of E{Y}, Male-BS
predict(m5, newdata=data.frame(age=23, age2=529, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.67361  =$43200.6, e^10.73299 =$45843.6), for a 23-years-
# old male citizen living in CA with BS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=23, age2=529, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.537986    =$13,877.0 , e^11.86862 =$142,717), for a 23-years-old male
# citizen living in CA with BS degree who speaks Engl-only which is 
# equivalent to ($25,672.45 , $264,026.45) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-BS
predict(m5, newdata=data.frame(age=23, age2=529, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.54582   =$38,018.2, e^10.61512=$40,746.3), for a 23-years-
# old male citizen living in CA with BS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=23, age2=529, educ_category="BS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.415016   =$12,271.3, e^11.74592=$126,237), for a 23-years-old male
# citizen living in CA with BS degree who speaks Engl-only which is 
# equivalent to ($22,701.91,$233,538.45) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 4: 
# With 95% confidence, the analysis using model m5 indicates a significant gender-based
# wage gap for 23-year-old software engineers with BS degrees in California, 
# with males earning approximately $5,144  more annually than females in average
# ($44,502.5 (=fit e^10.7033) for males vs. $39,358.6 (=fit e^10.58047) for females).



#============= MS opportunity cost, 25-years-old (Study for 2 years) =======================
# 95% CI and PI of E{Y}, Male-MS
predict(m5, newdata=data.frame(age=25, age2=625, educ_category="MS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.84988  =$51,528.0, e^10.91402=54,941.3), for a 25-years-
# old male citizen living in CA with MS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=25, age2=625, educ_category="MS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.716569  =$16,590.2, e^12.04733=$170,643), for a 25-years-old male
# citizen living in CA with MS degree who speaks Engl-only which is 
# equivalent to ($30,691.90,$315,689.55) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-MS
predict(m5, newdata=data.frame(age=25, age2=625, educ_category="MS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.72263  =$45,371.1, e^10.7956=$48,805.6), for a 25-years-
# old male citizen living in CA with MS degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=25, age2=625, educ_category="MS", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.593606  =$14,670.7, e^11.92463=$150,939), for a 25-years-old male
# citizen living in CA with MS degree who speaks Engl-only which is 
# equivalent to ($27,140.795,$279,237.15) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 5: The analysis using model m5 indicates a significant gender-based
# wage gap for 25-years-old software engineers with MS degrees in California, 
# with males earning approximately $6,150  more annually than females in average
# ($53,207.3 (=fit e^10.88195 ) for males vs. $47,057.2 (=fit e^10.75912 ) for females).


#============= phD opportunity cost, 31-years-old (Study for 6 years) =======================

# 95% CI and PI of E{Y}, Male-Dr/Prof
predict(m5, newdata=data.frame(age=31, age2=961, educ_category="Dr/Prof", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.86313  =$52,215.3, e^11.04684=$62,742.9), for a 31-years-
# old male citizen living in CA with Dr/Prof degree who speaks Engl-only. 

predict(m5, newdata=data.frame(age=31, age2=961, educ_category="Dr/Prof", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Male", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.786433  =$17,790.7, e^12.12354=$184,156), for a 31-years-old male
# citizen living in CA with Dr/Prof degree who speaks Engl-only which is 
# equivalent to ($32,912.8 , $340,689) with considering the inflation index
# (1.85) from 1999 to 2023.


# 95% CI and PI of E{Y}, Female-Dr/Prof
predict(m5, newdata=data.frame(age=31, age2=961, educ_category="Dr/Prof", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="confidence", level=.95)
# With 95% confidence, the average wageincome of sft.Engnr in 1999 is estimated
# to be between (e^10.7388  =$46,110.7, e^10.9255=$55,575.6), for a 31-years-
# old female citizen living in CA with Dr/Prof degree who speaks Engl-only.

predict(m5, newdata=data.frame(age=31, age2=961, educ_category="Dr/Prof", 
                               engl_category="Only.Eng", wkswrkd=52, cit_category="US.citizen", 
                               sex_category="Female", occ_category2="Sft.Engnr", 
                               location_category="CA" ), interval="prediction", level=.95)
# With 95% confidence, the wageincome of sft.Engnr is predicted to be between  
# (e^9.663481  =$15,732.5 , e^12.00083=$162,890), for a 31-years-old female
# citizen living in CA with Dr/Prof degree who speaks Engl-only which is 
# equivalent to ($29,105.1, $301,347) with considering the inflation index
# (1.85) from 1999 to 2023.

# Conclusion 6: 
# With 95% confidence, the analysis using model m5 indicates a significant gender-based
# wage gap for 31-years-old software engineers with Dr/Prof degrees in California,  
# with males earning approximately $6,617 more annually than females in avereage.
# ($57,239.0 (=fit e^10.95499) for males vs. $50,622.4 (=fit e^10.83215) for females).

# Conclusion 7: oppertunity cost











#============= GRAPH ============== GRAPH ============== GRAPH =============
library(ggplot2)
par(mfrow=c(1, 3))
# Create a sequence for the age range you want to plot, for example from 20 to 60
age_range <- seq(20, 70, by = 1)

# Create a new data frame for predictions, expanding for each age in the age range
newdata1 <- expand.grid(
  age = age_range,
  educ_category = "BS",
  engl_category = "Only.Eng",
  wkswrkd = 52,
  cit_category = "US.citizen",
  sex_category = c("Male", "Female"),
  occ_category2 = "Sft.Engnr",
  location_category = "CA"
)
newdata1$age2 <- newdata1$age^2

# Add predicted values and intervals
newdata1$pred <- predict(m5, newdata1, interval = "confidence", level = 0.95)[, "fit"]
newdata1$pred_lwr <- predict(m5, newdata1, interval = "confidence", level = 0.95)[, "lwr"]
newdata1$pred_upr <- predict(m5, newdata1, interval = "confidence", level = 0.95)[, "upr"]
newdata1$pred_interval_lwr <- predict(m5, newdata1, interval = "prediction", level = 0.95)[, "lwr"]
newdata1$pred_interval_upr <- predict(m5, newdata1, interval = "prediction", level = 0.95)[, "upr"]


# Base plot
p1 <- ggplot(newdata1, aes(x = age)) +
  geom_line(aes(y = pred, color = sex_category), linewidth = 0.5) +
  geom_line(aes(y = pred_lwr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_line(aes(y = pred_upr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_ribbon(aes(ymin = pred_interval_lwr, ymax = pred_interval_upr, fill = sex_category), alpha = 0.1) +
  labs(x = "Age", y = "Wage Income (log-transformed)", title = "CI and PI of Predicted Wage Income with BS degree") +
  scale_color_manual(values = c("Male" = "darkgreen", "Female" = "darkorange")) +
  scale_fill_manual(values = c("Male" = "darkgreen", "Female" = "darkorange")) +
  scale_linetype_manual(values = c("Confidence Interval" = "dashed"), labels = c("Confidence Interval")) +
  theme_minimal() + theme(legend.position = "bottom")

# Print the plot
print(p1)


####################################################
# Create a new data frame for predictions, expanding for each age in the age range
newdata2 <- expand.grid(
  age = age_range,
  educ_category = "MS",
  engl_category = "Only.Eng",
  wkswrkd = 52,
  cit_category = "US.citizen",
  sex_category = c("Male", "Female"),
  occ_category2 = "Sft.Engnr",
  location_category = "CA"
)
newdata2$age2 <- newdata2$age^2

# Add predicted values and intervals
newdata2$pred <- predict(m5, newdata2, interval = "confidence", level = 0.95)[, "fit"]
newdata2$pred_lwr <- predict(m5, newdata2, interval = "confidence", level = 0.95)[, "lwr"]
newdata2$pred_upr <- predict(m5, newdata2, interval = "confidence", level = 0.95)[, "upr"]
newdata2$pred_interval_lwr <- predict(m5, newdata2, interval = "prediction", level = 0.95)[, "lwr"]
newdata2$pred_interval_upr <- predict(m5, newdata2, interval = "prediction", level = 0.95)[, "upr"]


# Base plot
p2 <- ggplot(newdata2, aes(x = age)) +
  geom_line(aes(y = pred, color = sex_category), linewidth = 0.5) +
  geom_line(aes(y = pred_lwr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_line(aes(y = pred_upr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_ribbon(aes(ymin = pred_interval_lwr, ymax = pred_interval_upr, fill = sex_category), alpha = 0.1) +
  labs(x = "Age", y = "Wage Income (log-transformed)", title = "CI and PI of Predicted Wage Income with MS degree") +
  scale_color_manual(values = c("Male" = "skyblue", "Female" = "red")) +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "red")) +
  scale_linetype_manual(values = c("Confidence Interval" = "dashed"), labels = c("Confidence Interval")) +
  theme_minimal() + theme(legend.position = "bottom")

# Print the plot
print(p2)



### Graph Dr/Prof
# Create a new data frame for predictions, expanding for each age in the age range
newdata3 <- expand.grid(
  age = age_range,
  educ_category = "Dr/Prof",
  engl_category = "Only.Eng",
  wkswrkd = 52,
  cit_category = "US.citizen",
  sex_category = c("Male", "Female"),
  occ_category2 = "Sft.Engnr",
  location_category = "CA"
)
newdata3$age2 <- newdata3$age^2

# Add predicted values and intervals
newdata3$pred <- predict(m5, newdata3, interval = "confidence", level = 0.95)[, "fit"]
newdata3$pred_lwr <- predict(m5, newdata3, interval = "confidence", level = 0.95)[, "lwr"]
newdata3$pred_upr <- predict(m5, newdata3, interval = "confidence", level = 0.95)[, "upr"]
newdata3$pred_interval_lwr <- predict(m5, newdata3, interval = "prediction", level = 0.95)[, "lwr"]
newdata3$pred_interval_upr <- predict(m5, newdata3, interval = "prediction", level = 0.95)[, "upr"]


# Base plot
p3 <- ggplot(newdata3, aes(x = age)) +
  geom_line(aes(y = pred, color = sex_category), linewidth = 0.1) +
  geom_line(aes(y = pred_lwr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_line(aes(y = pred_upr, color = sex_category, linetype = "Confidence Interval"), linewidth = 0.5) +
  geom_ribbon(aes(ymin = pred_interval_lwr, ymax = pred_interval_upr, fill = sex_category), alpha = 0.1) +
  labs(x = "Age", y = "Wage Income (log-transformed)", title = "CI and PI of Predicted Wage Income with Dr/Prof degree") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "brown")) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "brown")) +
  scale_linetype_manual(values = c("Confidence Interval" = "dashed"), labels = c("Confidence Interval")) +
  theme_minimal() + theme(legend.position = "bottom")

# Print the plot
print(p3)

# Displaying them in a grid
# install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)


##########################################################################
library(ggplot2)
library(dplyr)

# Assuming newdata1, newdata2, newdata3 are already defined as per your previous script

# Filter data for age 23
data_age35_bs <- filter(newdata1, age == 35)
data_age35_ms <- filter(newdata2, age == 35)
data_age35_drprof <- filter(newdata3, age == 35)

# Combine the data
combined_data <- rbind(data_age35_bs, data_age35_ms, data_age35_drprof)

# Base plot
p_combined <- ggplot(combined_data, aes(x = educ_category, y = pred, group = sex_category, color = sex_category)) +
  geom_line() +
  geom_point() +
  labs(x = "Education Level", y = "Predicted Wage Income (log-transformed)", title = "Comparison of Predicted Wage Income at Age 35 by Education Level") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() + theme(legend.position = "bottom")

# Print the plot
print(p_combined)

