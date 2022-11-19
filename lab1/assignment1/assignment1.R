library(kknn)
library(Formula)

# 1.1
# Import the data into R and divide it into training, validation and test sets.
# I also cleaned up the data by renaming target column and normalizing the data. 

optdigits = read.csv("optdigits.csv")
optdigits = transform(optdigits, X0.26=as.factor(X0.26))

#mod_opdigits = read.csv("optdigits.csv")

# Renamed our target column to Digit
#Colnames(mod_opdigits)[65]="Digit"

#Normalize data set
#normalize = function(x) {
#  if (max(x) == 0){
#    return(0)
#  }else{
#    return( (x-min(x))/(max(x)-min(x))) 
#  }
#}
#norm_mod_digits = as.data.frame(lapply(mod_opdigits[,c(1:64)], normalize))

n = dim(optdigits)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = optdigits[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
valid = optdigits[id2,]

id3 = setdiff(id1,id2)
test = optdigits[id3,]

# KKNN model with train and test data
m_test = kknn(formula=X0.26~., train=train, test=test, k=30, kernel="rectangular")
fit_test = fitted(m_test)

# Confusion matrix for m_test
table(Predicted_value=fit_test, Actual_value=test$X0.26)

# Missclassification errors for test data
miss_class_error_test = mean(fit_test != test$X0.26)
miss_class_error_test

# KKNN model with only train
m_train = kknn(formula=X0.26~., train=train, test=train, k=30, kernel="rectangular")
fit_train = fitted(m_train)

# Confusion matrix for m_train
table(Predicted_value=fit_train, Actual_value=train$X0.26)

# Missclassification errors for training data
miss_class_error_train = mean(fit_train != train$X0.26)
miss_class_error_train

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1.3.

p = m_train$prob

index_all_eights = which(train[,65]==8)

p_of_eights = p[index_all_eights,9]

three_lowest_p = sort(p_of_eights)[1:3]
print(three_lowest_p)
m = length(p_of_eights)
two_highest_p = sort(p_of_eights)[(m-2):m]

index_two_highest_p = matrix(c(which(train[,65]==8 & p[,9]==two_highest_p)[1:2]), nrow=2, ncol = 1)

index_three_lowest_p = 
  matrix(c(which(train[,65]==8 & p[,9]==three_lowest_p[1]),
           which(train[,65]==8 & p[,9]==three_lowest_p[2]),
           which(train[,65]==8 & p[,9]==three_lowest_p[3])
           ),nrow = 3,ncol = 1)

# print(index_two_highest_p)
# print(index_three_lowest_p)


heatmaps_hard = list(
  matrix(train[index_three_lowest_p[1],-65],nrow = 8, ncol = 8),
  matrix(train[index_three_lowest_p[2],-65],nrow = 8, ncol = 8))
print(heatmaps_hard[2])

heatmaps_easy = list(
  matrix(as.numeric(train[index_two_highest_p[1],-65],nrow = 8, ncol = 8)),
  matrix(as.numeric(train[index_two_highest_p[2],-65],nrow = 8, ncol = 8)),
  matrix(as.numeric(train[index_two_highest_p[3],-65],nrow = 8, ncol = 8)))
print(heatmaps_easy[2])  

hm_easy1 = t(matrix(as.numeric(train[index_two_highest_p[1],-65]),nrow = 8, ncol = 8))
hm_easy2 = t(matrix(as.numeric(train[index_two_highest_p[2],-65]),nrow = 8, ncol = 8))

hm_hard1 = t(matrix(as.numeric(train[index_three_lowest_p[1],-65]),nrow = 8, ncol = 8))
hm_hard2 = t(matrix(as.numeric(train[index_three_lowest_p[2],-65]),nrow = 8, ncol = 8))
hm_hard3 = t(matrix(as.numeric(train[index_three_lowest_p[3],-65]),nrow = 8, ncol = 8))

heatmap = heatmap(hm_hard3, Colv = NA, Rowv = NA)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1.4.

all_mce_train = c()
all_mce_valid = c()
for (i in 1:30){

  m_train = kknn(formula=X0.26~., train=train, test=train, k=i, kernel="rectangular")
  fit_train = fitted(m_train)
  mce_train = mean(fit_train != train$X0.26)
  all_mce_train[i] = mce_train
  
  m_valid = kknn(formula=X0.26~., train=train, test=valid, k=i, kernel="rectangular")
  fit_valid = fitted(m_valid)
  mce_valid = mean(fit_valid != valid$X0.26)
  all_mce_valid[i] = mce_valid
  
  }

plot(x=all_mce_train, xlab="k-value", ylim=c(0,0.06),
     ylab="MCE", col="black", type = "b", pch=16,
     main="Misclassification errors: Tests and Valid")
lines(all_mce_valid, type="b", col="red", pch=16)
legend("bottomright", legend = c("Train", "Valid"), 
       col = c("black", "red"), pch = 16)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1.5.

# Calculate cross-entropy
cross_entropy=function(n, probability, label){
  w = c()
  for (i in 1:n){
    w[i]=log(1e-15+probability[i, label[i]])
  }
  return(-sum(w))
}

ce_loss =c()
for (i in 1:30){
  m_valid_CE = kknn(formula=X0.26~., train=train, test=valid, k=i, distance = 1, kernel="rectangular")

  ce_loss[i] = cross_entropy(length(predict((m_valid_CE))), m_valid_CE$prob, valid$X0.26)
}

plot(x=ce_loss, xlab="k-value", ylim = (c(0,1000)),
     ylab="Cross-Entropy", col="red", type = "b", pch=16,
     main="Cross Entropy loss function of Valid-data")