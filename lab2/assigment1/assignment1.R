###############################################################################
##                            Assignment 1                                   ##
###############################################################################

#  ------------------------     Task 1    ----------------------------------  #
# Read csv file 
tecator = read.csv("tecator.csv")
tecator_modified = data.frame(tecator[,-1], row.names = tecator[,1])

# Split dataframe into train and test (50/50)
n = dim(tecator_modified)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = tecator_modified[id,]
test = tecator_modified[-id,]

# Fit linear regression to the training data
m_train=lm(train$Fat~., train[,1:(ncol(train)-3)])

# Estimate the training error
mean((train$Fat - predict(m_train, train)) ^ 2)
# Estimate the training error
mean((test$Fat - predict(m_train, test)) ^ 2)


#  ------------------------     Task 3    ----------------------------------  #
help("optim")
library(glmnet)

# Define response variables
response = train$Fat

# Define matrix of predictor variable
covariates = as.matrix(train[,1:(ncol(train)-3)])

# Fit LASSO regression model to training data
model_lasso = glmnet(as.matrix(covariates), response, alpha = 1, 
                     family="gaussian")

# Produce plot of coefficients by lambda value
plot(model_lasso, xvar="lambda", xlab = expression(paste("log(", lambda, ")")), )

# A summary of the glmnet path at each step is displayed if we just 
# enter the object name or use the print function:
summary = print(model_lasso)
# It shows from left to right the number of nonzero coefficients (Df), 
# the percent (of null) deviance explained (%dev) and the value of 𝜆 (Lambda).

# Get the 𝜆-value of the rows in which the nonzero coefficients (Df) are 3:
lambda = summary[summary$Df==3,3]
lambda

plot(model_lasso, xvar="lambda", xlab = expression(paste("log(", lambda, ")")))
abline(v=log(lambda[1]))
abline(v=log(lambda[2]))
abline(v=log(lambda[3]))
#  ------------------------     Task 4    ----------------------------------  #

# Fit Ridge regression model to training data
model_ridge = glmnet(as.matrix(covariates), response, alpha = 0, 
                     family="gaussian")

# Produce plot of coefficients by lambda value
plot(model_ridge, xvar="lambda")

# A summary of the glmnet path at each step is displayed if we just 
# enter the object name or use the print function:
summary = print(model_ridge)
# This shows us (like the plot also did) that all coefficients are != 0 for any 
# chosen lambda value.


#  ------------------------     Task 5    ----------------------------------  #

# Perform k-fold cross-validation to find optimal lambda value
cv_model_lasso = cv.glmnet(x = covariates, y = response, alpha=1, 
                     family="gaussian")

# Find optimal lambda value that minimizes test MSE
optimal_lambda = cv_model_lasso$lambda.min
log(optimal_lambda)
optimal_lambda


# Produce plot of test MSE by lambda value
plot(cv_model_lasso)
abline(v=log(optimal_lambda), col="orange")
abline(v=-4, col="blue")
legend("bottomright", legend = c(expression(paste("MSE by og(", lambda, ")")), 
                                 expression(paste("log(", lambda, ") = - 4")), 
                                 expression(paste("Optimal log(", lambda, ")"))), 
       col = c("red", "blue", "orange"), pch = 16)


# Create a model with the optimal lambda-value as parameter.
optimal_model_lasso = glmnet(covariates, response, alpha=1, family="gaussian", 
                             lambda = optimal_lambda)

# We can print a summary of the model to find the number of nonzero 
# coefficients (Df):
print(optimal_model_lasso)

# Get predictions of model with optimal lambda and linear regression model:
ynew_lasso = predict(optimal_model_lasso, newx = as.matrix(test[,1:100]), type = "response")
ynew_lm = predict(m_train, test)

# Scatter plot of the test versus predicted test values for model corresponding
# to optimal lambda
plot(as.matrix(test$Fat), ynew_lasso, main = "Optimal LASSO Regression", 
     xlab = "Original Values", ylab = "Predicted Values", 
     ylim = c(0,max(ynew_lasso)), xlim = c(0,max(test$Fat)))
abline(coef = c(0,1), col= "red")

# Scatter plot of the test versus predicted test values for linear regression model
plot(as.matrix(test$Fat), ynew_lm, main = "Linear Regression",  
     xlab = "Original Values", ylab = "Predicted Values", 
     ylim = c(0,max(ynew_lasso)), xlim = c(0,max(test$Fat)))
abline(coef = c(0,1), col= "red")
