df <- read.csv("communities.csv", header = TRUE) #Read the data into a DF

library(dplyr)
scaler = caret::preProcess(select(df,-"ViolentCrimesPerPop")) #ViolentCrimesPerPop excluded
scaled_df = predict(scaler, df) #scales the dataframe. (ViolentCrimesPerPop is not sclaed since the scaler dosn't include that column)

PCA_matrix <- as.matrix(scaled_df)
eigen(PCA_matrix, only.values = FALSE)
