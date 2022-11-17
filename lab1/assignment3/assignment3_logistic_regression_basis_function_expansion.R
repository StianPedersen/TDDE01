#install.packages("tidyverse")
library(tidyverse)
set.seed(12345)

#Q1:

Dataframe = read.csv("pima-indians-diabetes.csv", header = FALSE, na.strings = c("")) #Reads all the values to a dataframe, if a value is missing it is set to NA by na.strings
sapply(Dataframe, function(x) sum(is.na(x))) #For each tabelcolumn checks if there is values missing
colnames(Dataframe) <- c('Tpregnant', 'PlaGluCon', 'BloodPr', 'TriSkinT', 'Insulin', 'BMI', 'DPedFunc', 'Age', 'Diabetes') #Gives names to the columns
input <- data.frame(Dataframe[, c('Age', 'PlaGluCon', 'Diabetes')],
                    Z1 = (Dataframe$Age^0)*(Dataframe$PlaGluCon^4), 
                    Z2 = (Dataframe$Age^1)*(Dataframe$PlaGluCon^3), 
                    Z3 = (Dataframe$Age^2)*(Dataframe$PlaGluCon^2), 
                    Z4 = (Dataframe$Age^3)*(Dataframe$PlaGluCon^1), 
                    Z5 = (Dataframe$Age^4)*(Dataframe$PlaGluCon^0))
#Creates a df with all the values needed in the assignment.

ggplot(data = input, mapping = aes(x = Age, y = PlaGluCon, colour = Diabetes > 0.5)) +
  geom_point() + #Creates the dotted data
  labs(title = "Plasma Glucose Concentration as a function of age", colour = "Diabetes") + #Labels, can change x and why too i suppose
  theme_bw() #Theme black & White


#Q1 answer: No i don't think that it is easy to to classify diabetes using logistic regression
# with only these two features. The reason being that the diabetes and non-diabetes dotes is all over the place. 
#However the PLAGLUCON AND Age can give a OK indication.

#Q2.1 --> Train a logistic regression model
n <- nrow(input)#Number of rows in the input df
set <-input[sample(1:n,replace = FALSE), 1:8] #1:n, nrow(input), replace = TRUE, prob = c(0.8, 0.2))
training <- set[(1:(0.8*n)),] #80 % training data.
test <- set[((0.8*n)+1):n,] #20 % Test data

mymodel = glm(Diabetes ~ PlaGluCon + Age,
        data = training, family = "binomial")


#Q2.2 --> p = p(y = 1 | x) = e^(θ0+θ1x1+θ2x2)/(1 + e^(θ0+θ1x1+θ2x2)
#Where θ = (-6.057385, 0.034036, 0.035762) ("Estimated" found in summary(mymodel)
summary(mymodel)
coef(mymodel)

res <- as.matrix(predict(mymodel, test, type = "response"))
Threshold = 0.5
confmatrix <- table(Actual_Value = test$Diabetes, Predicted_Value = res > Threshold)
confmatrix



#Accuracy 
accuracy = (confmatrix[1,1] + confmatrix[2,2])/sum(confmatrix)

#Q2.3 --> Compute the training missclassification error
#Missclassification
missclass1 = (1 - sum(diag(confmatrix))/nrow(test))
missclass1
dfres = as.data.frame(res)
#test_mat = matrix(c(res, as.matrix(Age = test$Age), as.matrix(PlaGluCon = test$PlaGluCon)))
dataPlot1 = data.frame(test,est= res > Threshold)

#Q2.4 --> Plot of the same kind as in Q1 but with predicted values of diabetes instead
ggplot(data = dataPlot1, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()

#Q2.5 --> comment on the quality of the classification by using the results:
# I guess that the quality ain't too good since approx one in 4 is missclassified


#Q3.1 --> Report the decision boundary equation:
#X1 = AGE
#X2 = PlaGluCon = Y
#Dec.Bound = f(x) = 1- f(x) --> e^(θTx)/(1 + e^(θTx)) = 1/(1 + e^(θTx))
#<=> e^(θTx) = 1 <=> θTx = 0 <=> x2 = -(θ0/θ2 + (θ1/θ2)*x1) => y = kx + m.
coef(mymodel) # = θT 
θ0 = coef(mymodel)[1]
θ1 = coef(mymodel)[3]
θ2 = coef(mymodel)[2]

#

#Q3.2 --> Add curve showing dec bound
ggplot(data = dataPlot1, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw() +
  geom_abline(intercept = (-θ0/θ2), slope = (-θ1/θ2), color = "red")

#Q.3.3 --> Comment whether the dec.bound seems to catch the data distribution well.
#IDK like ofc it does? Since it is drawn in the est scatter plot? Feels like I am missing something.


#Q4.1 --> Make the same kind of plot as in Q2 but with r=0.2
Threshold = 0.2
dataPlot2 = data.frame(test,est= res > Threshold)
ggplot(data = dataPlot2, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()

#Q4.2 --> Make the same kind of plot as in Q2 but with r=0.2
Threshold = 0.8
dataPlot3 = data.frame(test,est= res > Threshold)
ggplot(data = dataPlot3, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()
#Q4.3 --> when r increase, less ppl are estimated to have diabetes

#5.1 --> BASIS FFUNCTION EXPANSION. NEW FEATURES Z1-Z5

#Z1 = training$PlaGluCon^4
#Z2 = (training$Age^3)*(training$PlaGluCon^1)
#Z3 = (training$Age^2)*(training$PlaGluCon^2)
#Z4 = (training$Age^1)*(training$PlaGluCon^3)
#Z5 = (training$Age^0)*(training$PlaGluCon^4)
mymodel2 = glm(Diabetes ~ PlaGluCon + Age + Z1 + Z2 + Z3 + Z4 + Z5,
                data = training, family = "binomial")
summary(mymodel2)
coef(mymodel2)

res <- as.matrix(predict(mymodel2, test, type = "response"))
Threshold = 0.5
confmatrix <- table(Actual_Value = test$Diabetes, Predicted_Value = res > Threshold)
confmatrix

#Missclassification
missclass = (1 - sum(diag(confmatrix))/nrow(test))  
missclass


dataPlot4 = data.frame(test,est= res > Threshold)
ggplot(data = dataPlot4, mapping = aes(x = Age, y = PlaGluCon, colour = res > 0.5)) +
  geom_point() + 
  labs(title = "Plasma Glucose Concentration as a function of age") +
  theme_bw()
