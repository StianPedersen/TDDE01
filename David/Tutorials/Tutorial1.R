##install.packages("dplyr")
##install.packages("tidyr")
##install.packages("readxl")

#Create data frame birth by reading birthstatistics.csv into R and by using read.csv. Note that these data has headers.
df_birth <- read.csv("birthstatistics.csv", header = TRUE)
#Create data frame blog by reading blogData_test.csv into R and by using read.csv. Note that headers (column names) are missing.
df_blog <- read.csv("blogData_test.csv", header = FALSE)
#Create data frame tecator by reading tecator.xls into R by using readxl package.
library("readxl")
df_tecator <- read_excel("tecator.xls")
#Save tecator to tecator.csv with write.csv and make sure that row names are not saved
write.csv(df_tecator, file = "tecator.csv", row.names = FALSE) 
#Convert tecator to a data frame and call it tecator1
tecator1 <- as.data.frame(df_tecator) 
#Change row names in tecator1 to the values of Sample column plus 10
rownames(tecator1) = tecator1$Sample+10
#Change column name in tecator1 from Sample to ID
library("dplyr")
colnames(tecator1) <- c("ID", colnames(select(tecator1, -Sample))) #Changes tecator1 colnames
colnames(tecator1)[1]="ID"
colnames(tecator1)[which(names(tecator1) == "Protein")] <- "BicepsJuice" #Change by name
#Extract rows in tecator1 such that Channel1 > 3 and Channel2 > 3 and columns between number 5 and number 8
modified_tecator <- tecator1[tecator1$Channel1 > 3 & tecator1$Channel2 > 3, 5:8]
#Remove column ID in tecator1
no_ID_tecator <- select(tecator1, -ID)
tecator1$ID=c()


#Update tecator1 by dividing its all Channel columns with their respective means per column
library(stringr) #Load stringr lib
index = str_which(colnames(tecator1), "Channel") #Creates index for all channel cols
tecatorChannel = tecator1[,index] #Makes a df with only channels
means = colMeans(tecatorChannel) #Takes the means of the Col.
tecator1[,index] = tecator1[,index]/matrix(means, 
                                           nrow=nrow(tecatorChannel),
                                           ncol=ncol(tecatorChannel),
                                           byrow=TRUE) #Dela tecator channels med mean.
#Compute a sum of squares for each row between 1 and 5 in tecator1 without writing loops and make it as a matrix with one column. (APPLY)
sumsq <- apply(X = as.matrix(tecator1[1:5,]), 
               MARGIN = 1,
               FUN = function(x) return(sum(x^2))) #Apply FUN to each row in X 
tecator2 <- matrix(sumsq, ncol = 1) #Create a matrix with sumsq and only one col.

#Extract X as all columns except of columns 101-103 in tecator1, y as column Fat and compute (XTX)−1XTy
X <- as.matrix(tecator1[,-(101:103)])
Y <- as.matrix(select(tecator1, Fat))
result <- solve(t(X)%*%X, t(X)%*%Y) #
det(t(X)%*%X)#Det = zero so there is no inverse ...
#Use column Channel1 in tecator1 to compute new column ChannelX which is 
#a factor with the following levels: “high” if Channel1>1 and “low” otherwise
tecator1$ChannelX <- as.factor(ifelse(tecator1$Channel1 > 1,
                                      "high", #ifelse is smart
                                      "low")) #factor finds Levels
#Write a for loop that computes regressions Fat as function of Channeli,i=1,...100 and 
#then stores the intercepts into vector Intercepts. Print Intercepts.
intercept = numeric(100)
for (i in 1:100)
{
  linear_model <- lm(paste("Fat~Channel", i, sep = ""), data = tecator1)#lm(Fat~tecator[i+1])
  intercept[i] = coef(linear_model)[1] 
}
intercept
#Given equation y=5x+1, plot this dependence for x between 1 and 3
x=c(1,3)
y=5*x+1
plot(x,y, type="l", col="blue") #Type L is line.

#dplyr och tidyr
library("dplyr")
library("tidyr")
#Convert data set birth to a tibble birth1
birth1 <- tibble(df_birth)
#Select only columns X2002-X2020 from birth1 and save into birth2
birth2 <- select(birth1, X2002:X2020)
#alt
birth2 <- birth1 %>%
  select(X2002:X2020)
#Create a new variable Status in birth1 that is equal to “Yes” if the record says “born in Sweden with two parents born in Sweden” and “No” otherwise
birth1$Status <- ifelse(birth1$foreign.Swedish.background == "born in Sweden with two parents born in Sweden",
                        "Yes",
                        "No")
#alt
birth1 <- birth1 %>% 
  mutate(Status = ifelse(birth1$foreign.Swedish.background == "born in Sweden with two parents born in Sweden",
                "Yes",
                "No"))
#DROP COLUMN
birth1 <- subset(birth1,select = -24) #Drop column 24
#Count the amount of rows in birth 1 corresponding to various combinations of sex and region
birth1 %>% count(sex, region) #Counts the intersect.

#Assuming that X2002-X2020 in birth1 show amounts of persons born respective year, 
#compute total amount of people born these years irrespective gender, given Status and region. Save the result into birth3
birth3 <- birth1 %>% 
  select(X2002:X2020, Status, region) %>%
  group_by(Status, region) %>%
  summarize_all(sum) %>%
  ungroup()
#By using birth3, compute percentage of people in 2002 having Status=Yes in different counties. 
#Report a table with column region and Percentage sorted by Percentage.
birth4 <- birth3 %>% 
  group_by(region) %>%
  mutate(Percentage = X2002/sum(X2002)*100) %>%
  filter(Status == "Yes") %>%
  select(region, Percentage) %>%
  ungroup() %>%
  arrange(Percentage)
birth4  
#By using birth1, transform the table to a long format: make sure that years are shown in column Year and values from the respective X2002-X2020 are stored in column Born.
#Make sure also that Year values show years as numerical values and store the table as birth5.
birth5 = birth1%>%
  group_by(region, sex, foreign.Swedish.background, Status)%>%
  pivot_longer(X2002:X2020, names_to="Year", values_to = "Born")%>%
  mutate(Year=as.numeric(stringr::str_remove(Year, "X"))) 
#By using birth5, transform the table to wide format: make sure that years are shown as separate columns and their corresponding values are given by Born.
#Columns should be named as “Y_2002” for example
birth6 = birth5%>%
  group_by(region, sex, foreign.Swedish.background, Status)%>%
  pivot_wider(names_from = Year, values_from = Born, names_prefix = "Y_")

#By using blog data, filter out columns that have zeroes everywhere.
blogS <- tibble(df_blog) %>% select_if(function(x) !all(x==0))
blogS
#Sample and print random 5 rows from birth data without replacement, use seed 123.
set.seed(123)
smp=birth[sample(nrow(birth), 5, replace=FALSE), ]
print(smp)
#Generate a vector with 5 elements from a Normal distribution with mean 5 and standard deviation 0.1
rnorm(5, mean=5, sd=0.1)
#Generate a vector with 168 elements from a Normal distribution with means equal to X2002 values in birth data and standard deviations 0.1
rnorm(168, mean=birth$X2002, sd=0.1)
#Compute probability density values of standard normal distribution in points x=-1, 0, 1.
dnorm(c(-1,0,1))
#Assuming in birth data X2003=w⋅X2002+w0+ϵ where ϵ∼Exponential(1), write down a minus log-likelihood formula for this model as 
#a function of parameters w and w0 and implement an R function for the minus log-likelihood
#ANSWER: From the formula, we get X2003−w⋅X2002−w0=ϵ; it means that for each observation X2003−w⋅X2002−w0∼Exponential(1).
#The pdf of this exponential distribution is p(ϵ)=e−ϵ so p(X2003|X2002,w,w0)=exp−(X2003−w⋅X2002−w0) and therefore log-probability 
#is logp(X2003|X2002,w,w0)=−(X2003−w⋅X2002−w0). The minus log-likelihood is the minus sum of log-probability over all observations.
loglik = function(w, w0){
  Probs=-(birth$X2003-w*birth$X2002-w0)
  return (-sum(Probs))
}
#testing
loglik(1,1)
#Compute a linear regression model with target X2020 and features X2002-X2004 by using data birth. 
#Report the regression coefficients and the MSE and probabilistic model.
df=birth%>%select(X2020, X2002:X2004)
m1=lm(X2020~., df)
coef(m1)
Preds=predict(m1)
MSE=mean((df$X2020-Preds)^2)
MSE
#Probabilistic model X2020∼N(1872−1.33X2002−10.35X2003+12.67X2004,9568343)

#Use package fastDummy to create dummy variables for variables region and sex in birth data
#install.packages("fastDummies"). 
#A dummy column is one which has a value of one when a categorical event occurs and a zero when it doesn't occur.
library("fastDummies")
dummies = dummy_cols(df_birth, select_columns = c("region", "sex"))
#Use rows 1-50 of birth data with columns X2002-X2020 as training data and rows 51-100 of the same data and test data. Scale training and test data appropriately with caret package
#install.packages("caret")
library("caret")
train=birth%>%select(X2002:X2020)%>%slice(1:50)
test=birth%>%select(X2002:X2020)%>%slice(51:100)
params=preProcess(train)
trainS=predict(params, train)
testS=predict(params,test)
#Compute logistic regression by using birth data and sex as target and X2002-X2004 as features. Assuming probability threshold 0.5,
#compute and print the confusion matrix. Report probabilistic model.
train=birth%>%select(X2002:X2004, sex)
m1=glm(as.factor(sex)~., train, family = "binomial")
Prob=predict(m1, type="response")
Pred=ifelse(Prob>0.5, "girls", "boys")
table(train$sex, Pred)
summary(m1)
#Probabilistic model P(sex=girls)=1/1+exp(−0.0172+0.0009X2002−0.0018X2003+0.0009X2004)

#Compute k nearest neighbor classification with k=10 by using birth data and sex as target and X2002-X2010 as features. 
#Print the predicted class labels for the training data.
train=birth%>%select(X2002:X2010, sex)
library(kknn)
m1=kknn(as.factor(sex)~., train,train, k=10,kernel="rectangular")
Pred=m1$fitted.values
Pred
#Compute the optimal parameter values and the optimal function value for minx1,x2(x1−1)^2+(x2−2)^2.
df=data.frame(x1=1,x2=2)
to_optimize<-function(x){
  x1=x[1]
  x2=x[2]
  return((x1-df$x1)^2+(x2-df$x2)^2)
}
res=optim(c(0,0), fn=to_optimize, method="BFGS")
res$par
res$value