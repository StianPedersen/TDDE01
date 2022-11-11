
#Plasma glucose as a function of age. 
#Plasma = col V2 (Y) 
#Age = col V8 (X) 
#xlab, ylab = labels
#main = Title
#xlim,Ylim = 

install.packages("tidyverse")
library(tidyverse)
set.seed(12345)

#Q1:

Dataframe = read.csv("pima-indians-diabetes.csv", header = FALSE)
colnames(Dataframe) <- c('Tpregnant', 'PlaGluCon', 'BloodPr', 'TriSkinT', 'Insulin', 'BMI', 'DPedFunc', 'Age', 'Diabetes')
input <- Dataframe[, c('Age', 'PlaGluCon', 'Diabetes')]
ggplot(data = input, mapping = aes(x = Age, y = PlaGluCon, colour = Diabetes)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()
#Compact way of writing the line above
#ggplot(input, aes(Age, PlaGluCon, colour = Diabetes)) + geom_point()

#Q1 answer: No i don't think that it is easy to to classify diabetes using logistic regression
# with only these two features. The reason being that the diabetes and non-diabetes dotes is all over the place. 

#Q2:

