data = read.csv2("bank-full.csv", header=TRUE, stringsAsFactors = TRUE)
data = subset(data,select = -c(duration))

n=dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
train = data[id,]

id1 = setdiff(1:n,id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
valid = data[id2,]

id3 = setdiff(id1,id2)
test = data[id3,]

# Task 2a
library(tree)
fit1 = tree(y~., data=train)
fit2 = tree(y~., data=train, minsize=7000)
fit3 = tree(y~., data=train, mindev=0.0005)

text(fit1, pretty=0)
plot(fit1)
summary(fit1)

text(fit2, pretty=0)
plot(fit2)
summary(fit2)

text(fit3, pretty=0)
plot(fit3)
summary(fit3)

yfit1_train = predict(fit1,newdata = train, type="class")
table(train$y,yfit1_train)
yfit1_valid = predict(fit1,newdata = valid, type="class")
table(valid$y,yfit1_valid)
mean(yfit1_train != train$y)
mean(yfit1_valid != valid$y)

yfit2_train = predict(fit2,newdata = train, type="class")
table(train$y,yfit2_train)
yfit2_valid = predict(fit2,newdata = valid, type="class")
table(valid$y,yfit2_valid)
mean(yfit2_train != train$y)
mean(yfit2_valid != valid$y)

yfit3_train = predict(fit3,newdata = train, type="class")
table(train$y,yfit3_train)
yfit3_valid = predict(fit3,newdata = valid, type="class")
table(valid$y,yfit3_valid)
mean(yfit3_train != train$y)
mean(yfit3_valid != valid$y)


# Task 3
trainScore = rep(0,50)
testScore = rep(0,50)
for(i in 2:50){
  prunedTree = prune.tree(fit3, best=i)
  pred = predict(prunedTree, newdata = valid, type = "tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
  }
  plot(2:50, trainScore[2:50], type = 'b', col='red',
       ylim=c(min(testScore[-1]),max(trainScore[-1])))
  points(2:50, testScore[2:50], type = 'b', col="blue")
  
# Best amount of leaves is 22?
#TO_DO SVARA Pa ANALYS FRGOR PART 3
  
# Part 4
finaltree = prune.tree(fit3, best=22)
text(finaltree, pretty=0)
summary(finaltree)
plot(finaltree)
Yfit_finaltree = predict(finaltree, newdata=test, type="class")
cm = table(test$y, Yfit_finaltree) # Confusion matrix

accuracy = sum(cm[1],cm[4])/sum(cm[1:4])
precision = cm[4] / sum(cm[4],cm[2])
sensitivity = cm[4] / sum(cm[4],cm[3])

fscore = (2*(sensitivity*precision))/(sensitivity + precision)

accuracy #  0.8922884
fscore # 0.2848752

# Part 5
# loss_matrix = matrix(c(0,5,1,0),nrow = 2)
fit_part5 = predict(finaltree, newdata = test, type="vector")
final_part5 = fit_part5[,1]
for (row in 1:nrow(fit_part5))
{
  reassurance = fit_part5[row,1] / fit_part5[row, 2]
  if(reassurance < 5){
    final_part5[row] = "yes"
  }
  else
  {
    final_part5[row] = "no"
  }
}

tbl_part5 = table(test$y,final_part5)
accuracy_5 = sum(tbl_part5[1],tbl_part5[4])/sum(tbl_part5[1:4])
precision_5 = tbl_part5[4] / sum(tbl_part5[4],tbl_part5[2])
sensitivity_5 = tbl_part5[4] / sum(tbl_part5[4],tbl_part5[3])

fscore_5 = (2*(sensitivity_5*precision_5))/(sensitivity_5 + precision_5)

accuracy_5
fscore_5

# Task 6
pi_sq = seq(from=0.05, to=0.95, by=0.05)

tree_part6 = tree(y~., data=train, mindev=0.0005)
opt_tree = prune.tree(tree_part6, best=22) ## NBNB hadde fit3
predict_task6 = predict(tree_part6,newdata = test, type="vector") # OPT tree or tree_part6??
final_part6 = predict_task6[,1]
true_positive_rate = rep(0,length(pi_sq))
false_positive_rate = rep(0,length(pi_sq))

logistic_part6 = glm(y ~ .,
              data = train, family = "binomial")
predict_logistic = predict(logistic_part6, newdata = test, type="response")
true_positive_rate_logistic = rep(0,length(pi_sq))
false_positive_rate_logistic = rep(0,length(pi_sq))

for (i in 1:length(pi_sq))
{
  # Decision tree
  for (row in 1:nrow(predict_task6))
  {
    if(predict_task6[row,2] > pi_sq[i])
    {
      final_part6[row] = "yes"
    }
    else
    {
      final_part6[row] = "no"
    }
  }
  tbl_part6 = table(test$y,final_part6)
  print(table(final_part6))
  if (ncol(tbl_part6) > 1)
  {
    true_positive_rate[i] = (tbl_part6[2,2])/(tbl_part6[2,2]+tbl_part6[2,1])
    false_positive_rate[i] = (tbl_part6[1,2])/(tbl_part6[1,2]+tbl_part6[1,1])
  }
  
  # Logistic regression
  logistic_pred = ifelse(predict(logistic_part6, newdata = test,
                                  type = "response") > pi_sq[i], "yes", "no")
  print(table(logistic_pred))
  logistic_tbl = table(test$y, logistic_pred)
  if (ncol(tbl_part6) > 1)
  {
    true_positive_rate_logistic[i] = (logistic_tbl[2,2])/(logistic_tbl[2,2]+logistic_tbl[2,1])
    false_positive_rate_logistic[i] = (logistic_tbl[1,2])/(logistic_tbl[1,2]+logistic_tbl[1,1])
  }
  
  
  #readline(prompt="Press [enter] to continue")
  
}

plot(false_positive_rate, true_positive_rate, xlim = c(0,1), ylim = c(0,1),
     type = "b",xlab="False Positive Ratio (FPR)",ylab="True Positive Ratio (TRP)")
lines(false_positive_rate_logistic, true_positive_rate_logistic, col="green")


