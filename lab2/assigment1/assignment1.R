###############################################################################
##                            Assignment 1                                   ##
###############################################################################

#  ------------------------     Task 1    ----------------------------------  #

# Read csv file 
tecator = read.csv("tecator.csv", header = TRUE)

# Split dataframe into train and test (50/50)
n = dim(tecator)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = tecator[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.5))
test = tecator[id2,]

# Fit linear regression to the training data
m_train=lm(train$Fat~., train[,1:(ncol(train)-2)])
m_test=lm(train$Fat~., test[,1:(ncol(test)-2)])

# predict(m_train)
summary(m_train)

summary(m_train)$coefficient

# Estimate the training error
mean((train$Fat - predict(m_train)) ^ 2)
# Estimate the training error
mean((train$Fat - predict(m_test)) ^ 2)


# According to https://towardsdatascience.com/ridge-and-lasso-regression-a-complete-guide-with-python-scikit-learn-e20e34bcbf0b: 
# On the other hand if we have large number of features and test score is 
# relatively poor than the training score then it’s the problem of 
# over-generalization or over-fitting."

#  ------------------------     Task 2    ----------------------------------  #

# TODO: Find 

#  ------------------------     Task 3    ----------------------------------  #

library(glmnet)

# Define response variables
response = tecator$Fat

# Define matrix of predictor variable
covariates = as.matrix(tecator[,2:(ncol(train)-3)])

# Perform k-fold cross-validation to find optimal lambda value
cv_model = cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", lambda = seq(0,1,0.001))
cv_model = cv.glmnet(as.matrix(covariates), response, alpha=1)

# Find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# Produce plot of test MSE by lambda value
plot(cv_model)

model = glmnet(as.matrix(covariates), response, alpha=1)

plot(model, xvar="lambda")






coeff = coef(cv_model, s="lambda.min")
coeff
ynew = predict(cv_model, newx=as.matrix(covariates), type="response")
ynew
y

coeff = sum((ynew-mean(y))^2)/sum(y-mean(y))^2
sum((ynew-y)^2)
sum((y-mean(y)^2))
coeff

