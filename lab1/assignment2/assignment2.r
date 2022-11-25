# TASK 1 #####################################
df = read.csv('parkinsons.csv')
set.seed(12345)
id = sample(1:nrow(df), floor(nrow(df)*0.6))
train_data = df[id,] ##
test_data = df[-id,] ##

scaler = caret::preProcess(train_data)
train_scaled = predict(scaler,train_data)
test_scaled = predict(scaler, test_data)

test = test_scaled[,-c(1,2,3,4,6)]
train = train_scaled[,-c(1,2,3,4,6)]
##############################################

# TASK 2 #####################################
linear_model = lm(formula = motor_UPDRS ~ . + 0, data=as.data.frame(train), x=TRUE) # x = true ser vilka headlines som gör ngt
summary(linear_model)

# Calculate MSE
prediction_test = predict(linear_model, as.data.frame(test))
prediction_train = predict(linear_model,as.data.frame(train))

difference_test = prediction_test - test['motor_UPDRS']
difference_train = prediction_train - train['motor_UPDRS']

MSE_test = sum(difference_test^2)/nrow(difference_test)
MSE_train = sum(difference_train^2)/nrow(difference_train)
##############################################  

# TASK 3a ####################################
# Loglikelihood (book 3.20)
loglikelihood <- function(theta, sigma, X, Y)
{
  n = dim(X)[1];
  theta = as.matrix(theta)
  first_term = -(n*log(2*pi*sigma^2)/2)
  second_term = (1/(2*sigma^2))
  summation_term = sum((X%*% theta - Y)^2)
  return (first_term - (summation_term/second_term))
}
##############################################

# TASK 3b ####################################
# Ridge regression (book 3.48)
ridge_function <- function(theta, sigma, X, Y, lambda)
{

  theta<-as.matrix(theta[1:ncol(X)])
  my_log = loglikelihood(theta=theta, sigma = sigma, X=X, Y=Y)
  result = -my_log + lambda*sum(theta^2);
  return (result)
}
##############################################

# Task 3c ####################################
# RidgeOptim
RidgeOpt <- function(lambda, X, Y, sigma)
{
  sigma = 1;
  theta = matrix(0,ncol(X));
  theta_in = theta;
  optimized <- optim(par=c(theta,sigma),fn=ridge_function, sigma=sigma,
                     X=X, Y=Y, lambda=lambda, method="BFGS");
  return (optimized);
}
##############################################

# Task 3d ####################################
# Degree of freedom (5.24 book) and Lecture 1d
DegreeFreedom <- function(lambda, X, Y)
{
  I = diag(dim(X)[2]);
  df = X %*% solve(t(X) %*% X + lambda * I) %*% t(X)
  return (sum(diag(df)))
}
##############################################

# TASK 4 #####################################
results_train = c(0,0,0)
results_test = c(0,0,0)
results_df = c(0,0,0)
lambdas = c(1,100,1000)
for (i in 1:3){
opt = RidgeOpt(lambda = lambdas[i], X = as.matrix(train[,-1]), 
               Y = as.matrix(train['motor_UPDRS']))
theta = as.matrix(unlist(opt['par']))

estimated_updrs_train = as.matrix(train[,-1])  %*% theta[-17,]
difference_train = estimated_updrs_train - train['motor_UPDRS']
MSE_train_lambda = sum(difference_train^2)/nrow(difference_train)

estimated_updrs_test = as.matrix(test[,-1]) %*% theta[-17,]
difference_test = estimated_updrs_test - test['motor_UPDRS']
MSE_test_lambda = sum(difference_test^2)/nrow(difference_test)

df = DegreeFreedom(lambda=lambdas[i], as.matrix(train[,-1]), 
                     Y = as.matrix(train['motor_UPDRS']))
results_df[i] = df
results_test[i] = MSE_test_lambda
results_train[i] = MSE_train_lambda

}
lambdas
results_train
results_test
results_df

##############################################
