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

#  ------------------------     Task 2    ----------------------------------  #



#  ------------------------     Task 3    ----------------------------------  #

library(glmnet)

# Define response variables
y = tecator$Fat

# Define matrix of predictor variable
x = as.matrix(tecator[,2:(ncol(train)-2)])

# Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model, ylim=(c(0,3)))



