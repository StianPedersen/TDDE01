df = read.csv('parkinsons.csv')
set.seed(12345)
id = sample(1:nrow(df), floor(nrow(df)*0.6))
train_data = df[id,] ##
test_data = df[-id,] ##

scaler = caret::preProcess(train_data)
train_scaled = predict(scaler,train_data)
test_scaled = predict(scaler, test_data)

# y = motor_UPDRS
data_check = test_scaled;
test_data_shaved = test_scaled[,-c(1,2,3,4,6)]
train_data_shaved = train_scaled[,-c(1,2,3,4,6)]

linear_model = lm(formula = motor_UPDRS ~ . + 0, data=as.data.frame(train_data_shaved), x=TRUE) # x = true ser vilka headlines som gör ngt
summary(linear_model)

# Calculate MSE
prediction_test = predict(linear_model, as.data.frame(test_data_shaved))
prediction_train = predict(linear_model,as.data.frame(train_data_shaved))

difference_test = prediction_test - test_data_shaved['motor_UPDRS']
difference_train = prediction_train - train_data_shaved['motor_UPDRS']

MSE_test = sum(difference_test^2)/nrow(difference_test)
MSE_train = sum(difference_train^2)/nrow(difference_train)

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

# Ridge regression (book 3.48)
ridge_function <- function(theta, sigma, X, Y, lambda)
{

  theta<-as.matrix(theta[1:ncol(X)])
  my_log = loglikelihood(theta=theta, sigma = sigma, X=X, Y=Y)
  result = -my_log + lambda*sum(theta^2);
  return (result)
}

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

# Degree of freedom (5.24 book) ASK
DegreeFreedom <- function(lambda, X, Y)
{
  I = diag(dim(X)[2]);
  df = X %*% solve(t(X) %*% X + lambda * I) %*% t(X)
  return (sum(diag(df)))
}


opt = RidgeOpt(lambda = 1, X = as.matrix(train_data_shaved[,-1]), 
               Y = as.matrix(train_data_shaved['motor_UPDRS']))
theta = as.matrix(unlist(opt['par']))  #Ask, why is this 17 not 16??
train_data_last_task = as.matrix(train_data_shaved[,-1])
theta = theta[-17,]
estimated_updrs_train = train_data_last_task  %*% theta
difference_train = estimated_updrs_train - train_data_shaved['motor_UPDRS']
MSE_train_lambda_1 = sum(difference_train^2)/nrow(difference_train)

test_data_last_task = as.matrix(test_data_shaved[,-1])
estimated_updrs_test = test_data_last_task %*% theta
difference_test = estimated_updrs_test - test_data_shaved['motor_UPDRS']
MSE_test_lambda_1 = sum(difference_test^2)/nrow(difference_test)

df_1 = DegreeFreedom(lambda=1, as.matrix(train_data_shaved[,-1]), 
                     Y = as.matrix(train_data_shaved['motor_UPDRS']))

print(df_1)
print(MSE_train_lambda_1)
print(MSE_test_lambda_1)



# opt = RidgeOpt(lambda = 100, X = as.matrix(train_data_shaved[,-1]),
#                Y = as.matrix(train_data_shaved['motor_UPDRS']))
# theta = as.matrix(unlist(opt['par']))  #Ask, why is this 17 not 16??
# train_data_last_task = as.matrix(train_data_shaved[,-1])
# theta = theta[-17,]
# estimated_updrs_train = train_data_last_task  %*% theta
# difference_train = estimated_updrs_train - train_data_shaved['motor_UPDRS']
# MSE_train_lambda_100 = sum(difference_train^2)/nrow(difference_train)
# 
# test_data_last_task = as.matrix(test_data_shaved[,-1])
# estimated_updrs_test = train_data_last_task %*% theta
# difference_test = estimated_updrs_test - test_data_shaved['motor_UPDRS']
# MSE_test_lambda_100 = sum(difference_test^2)/nrow(difference_test)
# 
# df_1 = DegreeFreedom(lambda=100, as.matrix(train_data_shaved[,-1]), 
#                      Y = as.matrix(train_data_shaved['motor_UPDRS']))
# 
# print(sum(diag(df_1)))
# print(MSE_train_lambda_100)
# print(MSE_test_lambda_100)
# 
# 
# 
# opt = RidgeOpt(lambda = 1000, X = as.matrix(train_data_shaved[,-1]),
#                Y = as.matrix(train_data_shaved['motor_UPDRS']))
# theta = as.matrix(unlist(opt['par']))  #Ask, why is this 17 not 16??
# train_data_last_task = as.matrix(train_data_shaved[,-1])
# theta = theta[-17,]
# estimated_updrs_train = train_data_last_task  %*% theta
# difference_train = estimated_updrs_train - train_data_shaved['motor_UPDRS']
# MSE_train_lambda_1000 = sum(difference_train^2)/nrow(difference_train)
# 
# test_data_last_task = as.matrix(test_data_shaved[,-1])
# estimated_updrs_test = train_data_last_task %*% theta
# difference_test = estimated_updrs_test - test_data_shaved['motor_UPDRS']
# MSE_test_lambda_1000 = sum(difference_test^2)/nrow(difference_test)
# 
# df_1 = DegreeFreedom(lambda=1000, as.matrix(train_data_shaved[,-1]), 
#                      Y = as.matrix(train_data_shaved['motor_UPDRS']))
# 
# print(sum(diag(df_1)))
# print(MSE_train_lambda_1000)
# print(MSE_test_lambda_1000)

