df <- read.csv("communities.csv", header = TRUE) #Read the data into a DF

library(dplyr)
scaler = caret::preProcess(select(df,-"ViolentCrimesPerPop")) #ViolentCrimesPerPop excluded
scaled_df = predict(scaler, df) #scales the dataframe. (ViolentCrimesPerPop is not sclaed since the scaler dosn't include that column)

PCA_matrix <- as.matrix(scaled_df) #Creates a matix of the df.
PCA_matrix_t <- t(PCA_matrix) #Transpose PCA_matrice
s_covar_matrix <- (PCA_matrix_t%*%PCA_matrix)/nrow(PCA_matrix) #Computes the sample covariance matrix.  
res <- eigen(s_covar_matrix, only.values = FALSE) #Extracts the eigenvalues from the sample covariance matrix
eigenval <-res$values
eigenvec <-res$vectors

xx <- eigenval/sum(eigenval) #To get how much of the variance a specific eigenvalue corresponds to (in decimal)
sprintf("%2.3f", xx * 100) #Prints the percentage 

i <- 0 #Iterator variable
summan <- 0 #Summary variable
while(summan <= 0.95) #Sum until 95% variance is obtained. 
{
  i <- i + 1 #Iterator var. 
  summan <- summan + xx[i] #Starts at index 1
}


###########################################################################
                   ###Question 2###
###########################################################################
res = princomp(scaled_df) #Makes a principal component analysis of scaled_df.
screeplot(res) #Prints the variance contribution of the different components.
#res$loadings[,1]  #Loading is the eigenvectors*eignenvalue^0.5, and we want the cintribution for PC1, e.g the first column 

#Following plots the PC1 and he contribution of each feature, the x axis is an index including all the features. 
library(tidyverse)
dfr_loadings = data.frame(PC1 = res$loadings[,1]) #Reads the Loadings of PC1 in to dfr 
ggplot(data = dfr_loadings, mapping = aes(x = 1:101, y = PC1)) + #Prints the PC1:s features.
  geom_point() + #Creates the dotted data
  labs(title = "Trace plot of PC1 using princomp()", x = "variable 1:101", y = "PC1 Feature contribution(loadings)", colour = "") + #Labels, can change x and y too i suppose
  theme_bw() #Theme black & White

#Following computes the five most contributing features.
library(dplyr)
absdfr = as.data.frame(abs(res$loadings[,1])) #Takes the absolute values of the PC1 scores
absdfr %>%  #Take the dataframe dfr 
  arrange(desc(absdfr[,1])) %>% #THEN arrange it desc order based on PC1
  slice(1:5) #THEN Slice it so that you only get the top 5

#summary(res)
#dim(res$scores)

#Following makes a plot of the PC1 and PC2 scores and colors using ViolentCrimesPerPop
dfr_scores = data.frame(PC1=res$score[,1], PC2 = res$scores[,2], ViolentCrimesPerPop = df$ViolentCrimesPerPop) #Creates a df with PC1 & PC2 scores
ggplot(data = dfr_scores, mapping = aes(x = PC1, y = PC2, colour = ViolentCrimesPerPop))+
  geom_point() + #Creates the dotted data
  labs(title = "PC 1 + PC 2") + #Labels
  theme_bw() #Theme black & White



###########################################################################
                              ###Question 3###
###########################################################################
df = read.csv("communities.csv", header = TRUE)#Reads in the df.
set.seed(12345) #Sets a seed to be able to reproduce the same results.
id = sample(1:nrow(df), floor(nrow(df)*0.5)) #Creates a vector with size nrow/2 e.g 50% of df.
train_data = df[id,] #picks the rows specified in id as train_data
test_data = df[-id,] #Picks the rows not specified in id as test_data

scaler = caret::preProcess(train_data) #Creates a scaler value
train_scaled = predict(scaler,train_data) #Apply the scalar to the train_data
test_scaled = predict(scaler, test_data) #Apply the data to the test_data

linear_model = lm(train_scaled$ViolentCrimesPerPop ~ ., data = train_scaled[,-1]) #Trains a linear model by using the scaled train data. ViolentCrimesPerPop as dependent var

summary(linear_model) #Renders a summary of the linear model

prediction_test = predict(linear_model, as.data.frame(select(test_scaled,-"ViolentCrimesPerPop"))) #Predicts the ViolentCrimesPerPop using the model 
prediction_train = predict(linear_model,as.data.frame(select(train_scaled,-"ViolentCrimesPerPop")))

difference_test = prediction_test - test_scaled['ViolentCrimesPerPop'] #Calculate the difference in predictions and actual values
difference_train = prediction_train - train_scaled['ViolentCrimesPerPop']

MSE_test_1 = sum(difference_test^2)/nrow(difference_test) #Calculates the MSE
MSE_train_1 = sum(difference_train^2)/nrow(difference_train)

###########################################################################
                    ###Question 4###
###########################################################################

MSE_train_list = list() #A list with MSE_train from all iteration
MSE_test_list = list() #A list with all the MSE_test from all iteration
k = 0 #Iteration var.

myf <- function(theta, train_x, train_y, test_x, test_y){
  yhat_test = as.matrix(test_x) %*% theta #Returns the estimated ViolentCrimesPerPop.
  yhat_train = as.matrix(train_x) %*% theta
  
  difference_test = yhat_test - test_y #Calcs the difference between the estimation and actual val
  difference_train = yhat_train - train_y 
  MSE_test = sum(difference_test^2)/nrow(difference_test) #Calcs the MSE
  MSE_train = sum(difference_train^2)/nrow(difference_train)
  .GlobalEnv$k = .GlobalEnv$k + 1   #Updates the glob vars.
  .GlobalEnv$MSE_test_list[[k]] = MSE_test
  .GlobalEnv$MSE_train_list[[k]] = MSE_train  
  return(MSE_train)
}

#Creates a matrix with as many rows as testdata columns(-ViolentCrim) (100) with all zeros.
theta = matrix(0,ncol(select(train_scaled,-"ViolentCrimesPerPop"))) 

res <- optim(theta, fn = myf, train_x = select(train_scaled,-"ViolentCrimesPerPop"),
      train_y = select(train_scaled, "ViolentCrimesPerPop"),
      test_x = select(test_scaled,-"ViolentCrimesPerPop"),
      test_y = select(test_scaled, "ViolentCrimesPerPop"),
      method = "BFGS") #Runs the optim function with the specified data and stores it in res.

plot(x = 1:k, y = unlist(MSE_test_list), xlim = c(500, 20000), ylim = c(0, 1), type = "l") #Plots the MSE_test iterations 500:20000
lines(x = 1:k, y = unlist(MSE_train_list), col = "red") #Plots the MSE_train iterations 500:20000 in red.

theta_opti = res$par #Extracts the optimal theta vecotor returned from opti

yhat_test = as.matrix(select(test_scaled,-"ViolentCrimesPerPop")) %*% theta_opti #Calculates y_hat with optimal parameters
yhat_train = as.matrix(select(train_scaled,-"ViolentCrimesPerPop")) %*% theta_opti 

#Point for early stoping
match(min(unlist(MSE_test_list)), MSE_test_list) #Returns the K (e.g the iteration nr) for the best MSE_test

difference_test = yhat_test - select(test_scaled,"ViolentCrimesPerPop") #Calculates the diff between estimated and actual vio crim
difference_train = yhat_train - select(train_scaled,"ViolentCrimesPerPop")
MSE_test_optim = sum(difference_test^2)/nrow(difference_test) #Calculates the MSE_ for the optimal theta parameters
MSE_train_optim = sum(difference_train^2)/nrow(difference_train)
MSE_test_optim #Following code just prints all the different MSE:s.
MSE_train_optim
MSE_test_1
MSE_train_1

