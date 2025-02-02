
install.packages("car") # Install the 'car' package
library(car) # Load the 'car' package

data(Duncan) # load the dataset into your workspace
head(Duncan) # View the first few rows of the 'Duncan' data frame
str(Duncan) # Display the structure of the 'Duncan' data frame
help(Duncan) # Display the documentation/help file for the 'Duncan' dataset
table(Duncan$type) # Create a frequency table for the categorical variable 

# Create a histogram for the 'income' variable with main title and axis label
par(mfrow=c(1, 3))
hist(Duncan$income, main="Histogram of Income", xlab="Income")

# Create a histogram for the 'education' variable with main title and axis label
hist(Duncan$education, main="Histogram of Education", xlab="Education")

# Create a histogram for the 'prestige' variable with main title and axis label
hist(Duncan$prestige, main="Histogram of Prestige", xlab="Prestige")

# Initial check of multicollinearity 
pairs(Duncan) # Create a matrix of scatterplots of the 'Duncan' data frame

# Calculate and assign the correlation matrix of the 'income', 'education', and 'prestige' variables to 'correlation_matrix'
correlation_matrix <- cor(Duncan[c('income', 'education', 'prestige')])
correlation_matrix # Print the correlation matrix

# Fit a linear regression model predicting 
# Y ='prestige' 
# X1 = 'education'
# X2 ='income'
# X3 ='type', categorical
m <- lm(prestige ~ education + income + type, data=Duncan)
summary(m) # Print a summary of the linear regression model
vif(m) # Calculate Variance Inflation Factor (VIF) for the model

# Create residual plots with studentized residuals to check assumptions of the linear model
residualPlots(m, ~1, type="rstudent", id=list(labels=row.names(Duncan))) 
par(mfrow=c(1, 1))
hist(residuals(m)) # Create a histogram of the residuals of the model
qqPlot(m) # Create a Q-Q plot of the standardized residuals of the model

# Create an influence plot to assess the influential observations in the model
influencePlot(m, id=list(labels=row.names(Duncan)))
Cooks.d <- cooks.distance(m) # Calculate Cook's distance for the model
p <- 5 # Define the number of predictors including the intercept
n <- nrow(Duncan) # Get the number of observations in the 'Duncan' data frame

# Calculate the percentile of the Cook's distance based on the F-distribution
percentile <- 100 * pf(q=Cooks.d, df1=p, df2=n-p)

# Combine the 'Duncan' data frame with Cook's distance and percentile into a new data frame
data.frame(Duncan, Cooks.d=round(Cooks.d, 3), percentile=round(percentile, 1))
dfbetas(m) # Calculate the changes in the standardized beta coefficients for each observation
