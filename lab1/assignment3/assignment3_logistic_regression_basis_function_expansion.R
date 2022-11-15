
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
plot(x = input$Age, y = input$PlaGluCon, col= (input$Diabetes + 1))
ggplot(data = input, mapping = aes(x = Age, y = PlaGluCon, colour = Diabetes)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()
#Compact way of writing the line above
#ggplot(input, aes(Age, PlaGluCon, colour = Diabetes)) + geom_point()

#Q1 answer: No i don't think that it is easy to to classify diabetes using logistic regression
# with only these two features. The reason being that the diabetes and non-diabetes dotes is all over the place. 

#Q2: glm(). lm() linnear regression
n <- nrow(input)
set <-input[sample(1:n,replace = FALSE), 1:3] #1:n, nrow(input), replace = TRUE, prob = c(0.8, 0.2))
training <- set[(1:(0.8*n)),] #80 % training data.
test <- set[((0.8*n)+1):n,] #20 % Test data

m = glm(Diabetes ~ Age + PlaGluCon,
        data = training, family = "binomial")

summary(m)
