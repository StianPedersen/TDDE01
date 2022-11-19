#install.packages("tidyverse")
library(tidyverse)
set.seed(12345)

#Q1:

Dataframe = read.csv("pima-indians-diabetes.csv", header = FALSE, na.strings = c("")) #Reads all the values to a dataframe, if a value is missing it is set to NA by na.strings
colnames(Dataframe) <- c('Tpregnant', 'PlaGluCon', 'BloodPr', 'TriSkinT', 'Insulin', 'BMI', 'DPedFunc', 'Age', 'Diabetes') #Gives names to the columns
sapply(Dataframe, function(x) sum(x == 0)) #For each tabelcolumn checks if there is values equal to 0
df_dropped <- Dataframe[Dataframe[,"PlaGluCon"] != 0,] #drops all the rows where PlaGluCon = 0.

input <- data.frame(df_dropped[, c('Age', 'PlaGluCon', 'Diabetes')],
                    Z1 = (df_dropped$Age^0)*(df_dropped$PlaGluCon^4), 
                    Z2 = (df_dropped$Age^1)*(df_dropped$PlaGluCon^3), 
                    Z3 = (df_dropped$Age^2)*(df_dropped$PlaGluCon^2), 
                    Z4 = (df_dropped$Age^3)*(df_dropped$PlaGluCon^1), 
                    Z5 = (df_dropped$Age^4)*(df_dropped$PlaGluCon^0))
#Creates a df with all the values needed in the assignment.

ggplot(data = input, mapping = aes(x = Age, y = PlaGluCon, colour = Diabetes > 0.5)) +
  geom_point() + #Creates the dotted data
  labs(title = "PlasmaGlucoseConcentration as a function of age", colour = "Diabetes") + #Labels, can change x and why too i suppose
  theme_bw() #Theme black & White


#Q1 answer: No i don't think that it is easy to to classify diabetes using logistic regression
# with only these two features. The reason being that the diabetes and non-diabetes dotes is all over the place. 
#However the PLAGLUCON AND Age can give a OK indication.

mymodel = glm(Diabetes ~ PlaGluCon + Age,
        data = df_dropped, family = "binomial")


#Q2.2 --> p = p(y = 1 | x) = 1/(1 + e^(θ0+θ1x1+θ2x2)
#Where θ = (-6.24034649, 0.03850024, 0.02332926) ("Estimated" found in summary(mymodel)
summary(mymodel)
coef(mymodel)

res <- predict(mymodel, input, type = "response")
Threshold = 0.5
confmatrix <- table(Actual_Value = df_dropped$Diabetes, Predicted_Value = res > Threshold)

#Q2.3 --> Compute the training missclassification error
#Missclassification
missclass1 = (1 - sum(diag(confmatrix))/nrow(df_dropped))

#dfres = as.data.frame(res)
#test_mat = matrix(c(res, as.matrix(Age = test$Age), as.matrix(PlaGluCon = test$PlaGluCon)))
dataPlot1 = data.frame(df_dropped,est= res > Threshold)

#Q2.4 --> Plot of the same kind as in Q1 but with predicted values of diabetes instead
ggplot(data = dataPlot1, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "PlasmaGlucoseConcentration as a function of age, model1, r=0.5") +
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
  labs(title = "PlasmaGlucoseConcentration as a function of age, model1, r=0.5") +
  theme_bw() +
  geom_abline(intercept = (-θ0/θ2), slope = (-θ1/θ2), color = "red")

#Q.3.3 --> Comment whether the dec.bound seems to catch the data distribution well.
#IDK like ofc it does? Since it is drawn in the est scatter plot? Feels like I am missing something.


#Q4.1 --> Make the same kind of plot as in Q2 but with r=0.2
Threshold = 0.2
dataPlot2 = data.frame(df_dropped,est= res > Threshold)
ggplot(data = dataPlot2, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "PlasmaGlucoseConcentration as a function of age, model1, r=0.2") +
  theme_bw()

#Q4.2 --> Make the same kind of plot as in Q2 but with r=0.2
Threshold = 0.8
dataPlot3 = data.frame(df_dropped,est= res > Threshold)
ggplot(data = dataPlot3, mapping = aes(x = Age, y = PlaGluCon, colour = est)) +
  geom_point() + 
  labs(title = "PlasmaGlucoseConcentration as a function of age, model1, r=0.8") +
  theme_bw()
#Q4.3 --> when r increase, less ppl are estimated to have diabetes

#5.1 --> BASIS FFUNCTION EXPANSION. NEW FEATURES Z1-Z5

#Z1 = input$PlaGluCon^4
#Z2 = (input$Age^3)*(input$PlaGluCon^1)
#Z3 = (input$Age^2)*(input$PlaGluCon^2)
#Z4 = (input$Age^1)*(input$PlaGluCon^3)
#Z5 = (input$Age^0)*(input$PlaGluCon^4)
mymodel2 = glm(Diabetes ~ PlaGluCon + Age + Z1 + Z2 + Z3 + Z4 + Z5,
                data = input, family = "binomial")
summary(mymodel2)
coef(mymodel2)

res <- as.matrix(predict(mymodel2, input, type = "response"))
Threshold = 0.5
confmatrix <- table(Actual_Value = input$Diabetes, Predicted_Value = res > Threshold)
confmatrix

#Missclassification
missclass = (1 - sum(diag(confmatrix))/nrow(input))  
missclass


dataPlot4 = data.frame(input,est= res > Threshold)
ggplot(data = dataPlot4, mapping = aes(x = Age, y = PlaGluCon, colour = res > 0.5)) +
  geom_point() + 
  labs(title = "PlasmaGlucoseConcentration as a function of age using model 2") +
  theme_bw()
