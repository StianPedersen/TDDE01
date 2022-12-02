# TASK 1
data = read.csv2("bank-full.csv", 
                 header=TRUE, stringsAsFactors = TRUE) #Read the data as L2d
data = subset(data,select = -c(duration)) #Remove Duration

# Read and split data into 40/30/30
n=dim(data)[1]
set.seed(12345) #Set seed first
id = sample(1:n, floor(n*0.4))
train = data[id,]

id1 = setdiff(1:n,id)
set.seed(12345) #Set seed second
id2 = sample(id1, floor(n*0.3))
valid = data[id2,]

id3 = setdiff(id1,id2)
test = data[id3,]

# TASK 2
library(tree) # Import library tree
decisiontree_1 = tree(y~., data=train) # Decision tree  2a
decisiontree_2 = tree(y~., data=train,
                      minsize=7000) # Decision tree 2b, node-size 7000
decisiontree_3 = tree(y~., data=train, 
                      mindev=0.0005) # Decision tree 2c, deviance 0.0005


plot(decisiontree_1) # Plot the first decision tree
text(decisiontree_1, pretty=0) # Add decision text to first tree
summary(decisiontree_1) # Print the summary of the first tree 
                        # (i.e., variable importance)

plot(decisiontree_2) # Plot the second decision tree
text(decisiontree_2, pretty=0) # Add decision text to third tree
summary(decisiontree_2) # Print the summary of the second tree 
                        # (i.e., variable importance)

plot(decisiontree_3) # Plot the third decision tree
text(decisiontree_3, pretty=0) # Add decision text to third tree
summary(decisiontree_3) # Print the summary of the third tree 
                        # (i.e., variable importance)
# FROM NOW ON
# D1 = Decisiontree_1
# D2 = Decisiontree_2
# D3 = Decisiontree_3

## Calculating Missclassification rate for D1 for Train/Validation data
ypred_D1_tr = predict(decisiontree_1, newdata = train, 
                      type="class") # Predict on train using D1
table(train$y,ypred_D1_tr) # Print confusion matrix for prediction
ypred_D1_va = predict(decisiontree_1, newdata = valid, 
                      type="class") # Predict on validation using D1
table(valid$y,ypred_D1_va) # Print confusion matrix for prediction
mean(ypred_D1_tr != train$y) # Missclassification rate for train data 
mean(ypred_D1_va != valid$y) # Missclassification rate for validation data 

## Calculating Missclassification rate for D2 for Train/Validation data
ypred_D2_tr = predict(decisiontree_2,newdata = train, 
                      type="class") # Predict on train using D2
table(train$y,ypred_D2_tr) # Print confusion matrix for prediction
ypred_D2_va = predict(decisiontree_2,newdata = valid, 
                      type="class") # Predict on validation using D2
table(valid$y,ypred_D2_va) # Print confusion matrix for prediction
mean(ypred_D2_tr != train$y) # Missclassification rate for train data 
mean(ypred_D2_va != valid$y) # Missclassification rate for validation data 

## Calculating Missclassification rate for D2 for Train/Validation data
ypred_D3_tr = predict(decisiontree_3,newdata = train, 
                      type="class") # Predict on train using D3
table(train$y,ypred_D3_tr) # Print confusion matrix for prediction
ypred_D3_va = predict(decisiontree_3,newdata = valid, 
                      type="class") # Predict on validation using D3
table(valid$y,ypred_D3_va) # Print confusion matrix for prediction
mean(ypred_D3_tr != train$y) # Missclassification rate for train data 
mean(ypred_D3_va != valid$y) # Missclassification rate for validation data 


# TASK 3
trainScore = rep(0,50) # Create empty vector of size 50
testScore = rep(0,50) # Create empty vector of size 50

for(i in 2:50){ # Loop 50 times since we want to investigate the first 50 nodes 
  prunedTree = prune.tree(decisiontree_3,
                          best=i) # Prune the D3 tree from Task 2
  prediction_pruned = predict(prunedTree, newdata = valid, 
                 type = "tree") # Predict on validation data using prunedTree
  trainScore[i] = deviance(prunedTree) # Calc. the deviance for the prunedTree
  testScore[i] = deviance(prediction_pruned) # Calculate deviance for prediction
}

  plot(2:50, trainScore[2:50], type = 'b', col='red', 
       ylim=c(min(testScore[-1]),max(trainScore[-1]))) # Plot the Tree deviance
  points(2:50, testScore[2:50], type = 'b', 
         col="blue") # Add prediction deviance to plot
  
# TASK 4
optimal_tree = prune.tree(decisiontree_3, 
                          best=22) # Use the best amount of leaves found in 3
summary(optimal_tree) # Print which variables are most important
plot(optimal_tree) # Plot the tree
text(optimal_tree, pretty=0) # Add text to the tree
ypred_optimaltree = predict(optimal_tree, newdata=test, 
                            type="class") # Predict using the optimal tree
cm = table(test$y, ypred_optimaltree) # Confusion matrix for prediction
accuracy = sum(cm[1],cm[4])/sum(cm[1:4]) # Calculate accuracy from table
precision = cm[4] / sum(cm[4],cm[2]) # Calculate precision from table
sensitivity = cm[4] / sum(cm[4],cm[3]) # Calculate sensitivity from table

fscore = (2*(sensitivity*precision)) /
          (sensitivity + precision) # Calculate fscore

# Print results
print(accuracy) # -> 0.8922884
print(fscore) # -> 0.2848752

# TASK 5
ypred_task5_te = predict(optimal_tree, newdata = test, 
                    type="vector") # Predict using opt_tree from Task 4
final_prediction = ypred_task5_te[,1] # Create vector same size as prediction

for (row in 1:nrow(ypred_task5_te)) # Loop over all our predictions
{
  reassurance = ypred_task5_te[row,1] / 
                ypred_task5_te[row, 2] # Calculate how sure we are of "no"
  if(reassurance < 5) # If we are less than 5x as sure for a "no" 
  {
    final_prediction[row] = "yes" # Predict yes
  }
  else
  {
    final_prediction[row] = "no" # Else, predict no
  }
}

tbl_part5 = table(test$y,final_prediction) # Create confusion matrix 
print(tbl_part5) # Print confusion matrix
accuracy_task5 = sum(tbl_part5[1],tbl_part5[4]) /
                  sum(tbl_part5[1:4]) # Calculate accuracy for task 5
precision_task5 = tbl_part5[4] / 
                  sum(tbl_part5[4],tbl_part5[2]) # Calculate precision task 5
sensitivity_5 = tbl_part5[4] / 
                sum(tbl_part5[4],tbl_part5[3]) # Calculate sensitivity task 5

fscore_task5 = (2*(sensitivity_5*precision_task5)) / 
            (sensitivity_5 + precision_task5) # Calculate fscore task 5

# Print results
print(accuracy_task5) # -> 0.8731937
print(fscore_task5) # -> 0.4862605

# TASK 6
pi_sq = seq(from=0.05, to=0.95, by=0.05) # Create sequence from 0.05 -> 0.95

tree_part6 = tree(y~., data=train, mindev=0.0005) # Same tree as 2c
opt_tree = prune.tree(tree_part6, best=22) # Same optimal tree as task 4
                                           # Duplicate code for readability(?)

pred_opttree = predict(opt_tree,newdata = test, 
                        type="vector") # Predict using the optimal tree
final_prediction_part6 = pred_opttree[,1] # Create vector size of prediction

true_positive_rate = rep(0,length(pi_sq)) # Create empty TPR vector for opttree
false_positive_rate = rep(0,length(pi_sq)) # Create empty FPR vector for opttree

logistic_part6 = glm(y ~ ., data = train, 
                     family = "binomial") # Create logistic regression model
predict_logistic = predict(logistic_part6, newdata = test, 
                           type="response") # Predict using the model
true_positive_rate_logistic = rep(0,length(pi_sq)) # Empty TPR for logi. model
false_positive_rate_logistic = rep(0,length(pi_sq)) # Empty FPR for logi. model

for (i in 1:length(pi_sq)) # Loop over the elements in pi_sq (0.05, 0.1 .. 0.95)
{
  # Check if above pi for decision tree
  for (row in 1:nrow(pred_opttree)) # Loop over all predictions
  {
    if(pred_opttree[row,2] > pi_sq[i]) # If confidence over pi element we are at
    {
      final_prediction_part6[row] = "yes" # Predict yes
    }
    else
    {
      final_prediction_part6[row] = "no" # Predict no
    }
  }
  tbl_opttree_part6 = table(test$y,final_prediction_part6) # Confusion matrix
  
  # Later PI predicts never predicts no, if statement controls this
  if (ncol(tbl_opttree_part6) > 1) # If both yes and no predictions exists
  {
    true_positive_rate[i] = (tbl_opttree_part6[2,2]) / # Calculate TPR
                            (tbl_opttree_part6[2,2]+tbl_opttree_part6[2,1])
    
    false_positive_rate[i] = (tbl_opttree_part6[1,2]) / # calculate FPR
                             (tbl_opttree_part6[1,2]+tbl_opttree_part6[1,1])
  }
  
  # Logistic regression
  # If the response is higher than pi element predict yes, otherwise no
  # Can possibly be done for decision tree as well (:
  logistic_pred = ifelse(predict(logistic_part6, newdata = test,
                                  type = "response") > pi_sq[i], "yes", "no")
  
  logistic_tbl = table(test$y, logistic_pred) # Create confusion matrix logi.
  if (ncol(tbl_opttree_part6) > 1) # Same as for decision tree
  {
    true_positive_rate_logistic[i] = (logistic_tbl[2,2]) / # TPR
                                     (logistic_tbl[2,2]+logistic_tbl[2,1])
    
    false_positive_rate_logistic[i] = (logistic_tbl[1,2]) / # FPR
                                      (logistic_tbl[1,2]+logistic_tbl[1,1])
  }
}

# Plot the TPR and FPR for the decision tree and Logistic regression model
plot(false_positive_rate, true_positive_rate, xlim = c(0,1), ylim = c(0,1), type='l',
     col='blue', xlab="False Positive Ratio (FPR)",ylab="True Positive Ratio (TRP)")
lines(false_positive_rate_logistic, true_positive_rate_logistic, col="green")


