# http://r-statistics.co/Linear-Regression.html
# https://www.datacamp.com/tutorial/linear-regression-R
# test_mse: https://stackoverflow.com/questions/39811656/r-calculate-test-mse-given-a-trained-model-from-a-training-set-and-a-test-set
# caret lecture 1d
df = read.csv('parkinsons.csv')


set.seed(12345)
id = sample(1:nrow(df), floor(nrow(df)*0.6))
train_data = df[id,] ##
test_data = df[-id,] ##

scaler = caret::preProcess(train_data)
train_scaled = predict(scaler,train_data)
test_scaled = predict(scaler, test_data)

# y = motor_UPDRS, x = alla andra headlines?
train_data_shaved = train_data[,-c(1,2,3,4)]
linear_model = lm(formula = motor_UPDRS ~ . + 0, data=as.data.frame(train_data_shaved), x=TRUE) # x = true ser vilka headlines som gör ngt
model_summary = summary(linear_model)
model_summary
# Calculate MSE
mean(model_summary$residuals^2)

# Loglikelihood (book 3.20)
loglikelihood <- function(theta, dispersion)
{
  pi = 3.14 #pi
  n = #number of rows in training data
  x = train_data_shaved
  y = # Motor_updrs row?
  #return -(n/2) * (ln(2*pi*dispersion^2)) - (1/2*dispersion^2) * (((transpose(theta)* x) - y)^2)
}

