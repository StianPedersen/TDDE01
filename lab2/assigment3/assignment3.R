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