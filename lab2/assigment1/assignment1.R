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
best_lambda = cv_model_lasso$lambda.min
best_lambda
log(best_lambda)

plot(cv_model_lasso)
# Produce plot of test MSE by lambda value
plot(x = log(cv_model_lasso$lambda), y = log(cv_model_lasso$cvm), 
     xlab =expression(paste("log(", lambda, ")")), 
     ylab = " Mean cross-validated error", ylim = c(2.5,3))
abline(v=log(best_lambda))
abline(v=-4)

help("cv.glmnet")

coeff = coef(optimal_model_lasso, s="lambda.min")
coeff

# Function to count amount of selected variables
# count_variables = function(coeff){
#   count = 0
#   dim = nrow(coeff)
#   for (i in 1:dim){
#     if (coeff[i,1] != 0){
#       count = count + 1
#     }
#   }
#   return(count)
# }

# count_variables(coefficients(optimal_model_lasso))

optimal_model_lasso = glmnet(as.matrix(covariates), response, alpha=1, 
                                   family="gaussian", lambda = best_lambda)

comparison_model_lasso = glmnet(as.matrix(covariates), response, alpha=1, 
                                                    family="gaussian", lambda = exp(-4))
print(optimal_model_lasso)
print(comparison_model_lasso)


# TODO: plot label vs predicted values in scatter plot 
plot(x = train$Fat, y =  )





# y = train$Fat
# y
# 
# ynew = predict(cv_model_lasso, newx = as.matrix(covariates), type = "response")[,1]
# ynew
#   
# 
# sum((ynew-mean(y))^2)/sum((y-mean(y))^2)
# 
# sum((ynew-y)^2)
# 
# coeff

