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
train_m = as.matrix(train)
fit1 = tree(y~., data=train)
fit2 = tree(y~., data=train, minsize=7000)
fit3 = tree(y~., data=train, mindev=0.0005)

plot(fit1)
plot(fit2)
plot(fit3)

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










