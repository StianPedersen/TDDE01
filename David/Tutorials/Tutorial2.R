#Extract data crabs from library MASS, save it as crabs1 and read about these data in help.
library(MASS)
crabs1=crabs
?crabs

#Compute logistic regression model with sex as target and RW–CW measurements as features in data set crabs1,
#use cost matrix (1 row=F, 2 row=M). [0,2;3,0]
library(MASS)
library(dplyr)
crabs2=crabs1%>%select(sex, RW:CW)
levels(crabs1$sex)
m1=glm(sex~., data=crabs2, family="binomial")#if more than 2 classes use multinom from nnet
ProbM=predict(m1, type="response")
ProbF=1-ProbM
#alternative A: good for 2 classes
Pred=ifelse(ProbF/ProbM>3/2, "F", "M")
table(crabs2$sex, Pred)
#alternative B: fits to 2 or more classes 
Probs=cbind(ProbF, ProbM)
Losses=Probs%*%matrix(c(0,3,2,0), nrow=2)
bestI=apply(Losses, MARGIN=1, FUN = which.min)
Pred=levels(crabs2$sex)[bestI]
table(crabs2$sex, Pred)
#Compute a decision tree model with sex as target and all crab measurements as features in data set crabs1,
#one with default settings and another with 40 as a smallest allowed node size. 
#Compute an aggregated prediction from these two trees and then the confusion matrix.
library(tree)
crabs2=crabs1%>%select(sex, FL:BD)
tree0=tree(as.factor(sex)~., data=crabs2)
tree2=tree(as.factor(sex)~., data=crabs2, control = tree.control(nrow(crabs2), minsize = 40))
Probs=(predict(tree0, newdata=crabs2)+predict(tree2, newdata=crabs2))/2
bestI=apply(Probs, MARGIN=1, FUN = which.max)
Pred=levels(crabs2$sex)[bestI]
table(crabs2$sex, Pred)
#Compute a decision tree model fullT with sex as target and all crab measurements as features in data set crabs1, 
#and for the decision threshold p(male)/m(female)=0.7 compute TPR, FPR, precision, recall. Assume “F” is a positive class.
crabs2=crabs1%>%select(sex, FL:BD)
fullT=tree(as.factor(sex)~., data=crabs2)
Probs=predict(fullT)
Decision=ifelse(Probs[,2]/Probs[,1]>0.7, "M", "F")
tab=table(crabs2$sex, Decision)
TP=tab[1,1]
TN=tab[2,2]
FP=tab[2,1]
FN=tab[1,2]
TPR=TP/(TP+FN)
FPR=FP/(FP+TN)
prec=TP/(TP+FP)
rec=TP/(TP+FN)
cat(TPR,FPR, prec, rec)
#Use tree fullT and find out amount of leaves, how many variables are used by the tree and proportion of the labels in the root node
#ANSWWER: for proportions, look in the first row, within brackets
print(fullT)
#ANSWER: for variables used and amount of leaves(terminal nodes), look here
summary(fullT)
#Use cross-validation to find optimal amount of leaves in fullT, prune the tree and plot the result.
result=cv.tree(fullT)
leaves=result$size[which.min(result$dev)]
tree1=prune.tree(fullT, best=leaves)
plot(tree1)
text(tree1)
#Use crab measurements from crab1 to run PCA. Plot data in first 2 PC coordinates, answer which component has greatest contribution to PC1,
#provide equation of PC1 in the original coordinates, compress the data by using the first two PC.
crabs2=crabs1%>%select(FL:BD)
res2=princomp(crabs2)
plot(res2$scores[,1], res2$scores[,2])
colnames(res2$loadings)[which.max(res2$loadings[,1])]
res2$loadings[,1]
#PC1=0.29FL+0.2RW+0.6CL+0.66CW+0.28BD
compressed=res2$scores[,1:2]%*%t(res2$loadings[,1:2])+ 
  matrix(colMeans(crabs2), nrow=nrow(crabs2), ncol=ncol(crabs2), byrow=T)
#Use crab measurements from crab1 to run Lasso regression for with target CW and all other crab measurements as features and lambda=1.
#Compute the amount of features selected and report the predictive equation.
crabs2=crabs1%>%select(FL:BD)

library(glmnet)
x=as.matrix(crabs2%>%select(-CW))
y=as.matrix(crabs2%>%select(CW))
mB=glmnet(x, y,family="gaussian", lambda = 1, alpha=1)
coef(mB)
#1 feature selected and predictive equation CW=5.61+0.96CL

#Use crab measurements from crab1 to run cross-validated Lasso regression with target CW and all other crab measurements as features. 
#Report the optimal penalty.
crabs2=crabs1%>%select(FL:BD)
library(glmnet)
x=as.matrix(crabs2%>%select(-CW))
y=as.matrix(crabs2%>%select(CW))
model=cv.glmnet(x, y, alpha=1,family="gaussian")
model$lambda.min
#Assuming MSEtrain=(w−1)^2+(w−2)^2 and MSEtest=(w−4)^2+(w+1)^2, use BFGS optimization to plot the training and test MSE dependencies on 
#iteration number. Is early stopping needed?
TestE=list()
TrainE=list()
k=0

mseTrain= function(w){
  MSE_train=(w-1)^2+(w-2)^2
  MSE_test=(w-4)^2+(w+1)^2
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$TrainE[[k]]=MSE_train
  .GlobalEnv$TestE[[k]]=MSE_test
  return(MSE_train)
}

res=optim(c(0), fn=mseTrain,  method="BFGS")

plot(as.numeric(TrainE), type="l", col="blue", ylim=c(0,60), ylab="Error")
points(as.numeric(TestE), type="l", col="red")
#Early stopping can be done at 8th iteration to get the smallest test error.