###############################################################################
##                            Assignment 1                                   ##
###############################################################################

#  ------------------------     Task 1    ----------------------------------  #
f <- file.choose()
# Read csv file 
tecator = read.csv(f)
tecator_modified = data.frame(tecator[,-1], row.names = tecator[,1])

# Split dataframe into train and test (50/50)
n = dim(tecator_modified)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = tecator_modified[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.5))
test = tecator_modified[id2,]

train1 = train[,1:(ncol(train)-3)]

# Fit linear regression to the training data
m_train=lm(train$Fat~. -1, train[,1:(ncol(train)-3)])

# predict(m_train)
summary(m_train)

summary(m_train)$coefficient

predict(m_train, train)

ynew_lm = predict(m_train, test)


# Estimate the training error
mean((train$Fat - predict(m_train, train)) ^ 2)
# Estimate the training error
mean((test$Fat - predict(m_train, test)) ^ 2)


# According to https://towardsdatascience.com/ridge-and-lasso-regression-a-complete-guide-with-python-scikit-learn-e20e34bcbf0b: 
# On the other hand if we have large number of features and test score is 
# relatively poor than the training score then it’s the problem of 
# over-generalization or over-fitting."

#  ------------------------     Task 2    ----------------------------------  #

# TODO: Find 

#  ------------------------     Task 3    ----------------------------------  #

library(glmnet)

# Define response variables
response = train$Fat

# Define matrix of predictor variable
covariates = as.matrix(train[,1:(ncol(train)-3)])

# Fit LASSO regression model to training data
model_lasso = glmnet(as.matrix(covariates), response, alpha = 1, 
                     family="gaussian")

# Produce plot of coefficients by lambda value
plot(model_lasso, xvar="lambda")

# A summary of the glmnet path at each step is displayed if we just 
# enter the object name or use the print function:
summary = print(model_lasso)
# It shows from left to right the number of nonzero coefficients (Df), 
# the percent (of null) deviance explained (%dev) and the value of 𝜆 (Lambda).

# Get the 𝜆-value of the rows in which the nonzero coefficients (Df) are 3:
lambda = summary[summary$Df==3,3]
lambda

#  ------------------------     Task 4    ----------------------------------  #

# Fit Ridge regression model to training data
model_ridge = glmnet(as.matrix(covariates), response, alpha = 0, 
                     family="gaussian")

# Produce plot of coefficients by lambda value
plot(model_ridge, xvar="lambda")


#  ------------------------     Task 5    ----------------------------------  #

# Perform k-fold cross-validation to find optimal lambda value
cv_model_lasso = cv.glmnet(as.matrix(covariates), response, alpha=1, 
                     family="gaussian")

# Find optimal lambda value that minimizes test MSE
optimal_lambda = cv_model_lasso$lambda.min
log(optimal_lambda)

# Produce plot of test MSE by lambda value
plot(x = log(cv_model_lasso$lambda), y = log(cv_model_lasso$cvm), 
     xlab =expression(paste("log(", lambda, ")")), 
     ylab = " Mean cross-validated error")
abline(v=log(optimal_lambda), col="red")
abline(v=-4, col="blue")
legend("bottomright", legend = c(expression(paste("MSE by log(", lambda, ")")), 
                                 expression(paste("log(", lambda, ") = - 4")), 
                                 expression(paste("Optimal log(", lambda, ")"))), 
       col = c("black", "blue", "red"), pch = 16)


# Create a model with the optimal lambda-value as parameter.
optimal_model_lasso = glmnet(as.matrix(covariates), response, alpha=1, 
                                   family="gaussian", lambda = optimal_lambda)

# We can print a summary of the model to find the number of 
# nonzero coefficients (Df):
print(optimal_model_lasso)

help("predict")


ynew = predict(optimal_model_lasso, newx = as.matrix(test[,1:100]), type = "response")

plot(test$Fat-ynew, ylab = "Magnitutde of wrong prediction", xlab = "Channel", col = "blue")

# points(test$Fat-ynew_lm, col = "red")
abline(h=0)



mean((ynew-mean(test$Fat)))
# mean((ynew_lm-mean(test$Fat)))

plot(test$Fat, col="red")
points(ynew, col="black")

sum((ynew - mean(train$Fat))^2)/sum((train$Fat - mean(train$Fat))^2)
sum((ynew - train$Fat)^2)

sum((ynew_lm - mean(train$Fat))^2)/sum((train$Fat - mean(train$Fat))^2)
sum((ynew_lm - train$Fat)^2)


# comparison_model_lasso = glmnet(as.matrix(covariates), response, alpha=1, 
#                                 family="gaussian", lambda = exp(-4))



# TODO: plot label vs predicted values in scatter plot 
plot(x = train$Fat, y =  )


