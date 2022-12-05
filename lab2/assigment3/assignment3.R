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

pro_1_2 <- (xx[1] + xx[2])*100 #Calculates the proportion of variation explained by the first two principals components

###########################################################################
                   ###Question 2###
###########################################################################
res = princomp(scaled_df) #Makes a principal component analysis of scaled_df.
screeplot(res) #Prints the variance contribution of the different components.
res$loadings[,1]  #Loading is the eigenvectors, and we want the eigenvector for PC1, e.g the first column 
plot(res$loadings[,1]) #plots the result




library(tidyverse)
absdfr = as.data.frame(abs(res$loadings[,1])) #Takes the absolute values of the PC1 scores
dfr = as.data.frame(res$loadings[,1:2]) #Creates a df with PC1 & PC2
colnames(dfr)<-(c("PC1", "PC2")) #Renames the columns
ggplot(data = dfr, mapping = aes(x = 1:101, y = PC1)) + #Prints the PC1 
  geom_point() + #Creates the dotted data
  labs(title = "Trace plot of PC1 using princomp()", x = "variable 1:101", y = "PC1 score", colour = "") + #Labels, can change x and y too i suppose
  theme_bw() #Theme black & White

ggplot(data = dfr, mapping = aes(x = PC1, y = PC2, colour = 1)) +
  geom_point() + #Creates the dotted data
  labs(title = "PC 1 + PC 2", colour = "") + #Labels, can change x and y too i suppose
  theme_bw() #Theme black & White

library(dplyr)
absdfr %>%  #Take the dataframe dfr 
  arrange(desc(absdfr[,1])) %>% #THEN arrange it desc order based on PC1
  slice(1:5) #THEN Slice it so that you only get the top 5

###########################################################################
                              ###Question 3###
###########################################################################
df = read.csv("communities.csv", header = TRUE)
set.seed(12345)
id = sample(1:nrow(df), floor(nrow(df)*0.5)) #Creates a vector with size nrow/2 e.g 50% of df.
train_data = df[id,] #picks the rows specified in id as train_data
test_data = df[-id,] #Picks the rows not specified in id as test_data

scaler = caret::preProcess(train_data)
train_scaled = predict(scaler,train_data)
test_scaled = predict(scaler, test_data)

linear_model = lm(train_scaled$ViolentCrimesPerPop ~ ., data = train_scaled)
summary(linear_model)

prediction_test = predict(linear_model, as.data.frame(test_data))
prediction_train = predict(linear_model,as.data.frame(train_data))

difference_test = prediction_test - test_data['ViolentCrimesPerPop']
difference_train = prediction_train - train_data['ViolentCrimesPerPop']

MSE_test = sum(difference_test^2)/nrow(difference_test)
MSE_train = sum(difference_train^2)/nrow(difference_train)

###########################################################################
###Question 3###
###########################################################################

coeff = linear_model$coefficients
