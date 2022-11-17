library(kknn)
library(Formula)
library(Misclass)


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

# Misclassification errors for test data
miss_class_error_test = mean(fit_test != test$X0.26)
miss_class_error_test
#------------------------------------------------------------------------------#
# KKNN model with only train
m_train = kknn(formula=X0.26~., train=train, test=train, k=30, kernel="rectangular")
fit_train = fitted(m_train)

miss_class_error_train = mean(fit_train != train$X0.26)


# Confusion matrix for m_train
table(Predicted_value=fit_train, Actual_value=train$X0.26)

# Misclassification errors for training data
miss_class_error_train = mean(fit_train != train$X0.26)
miss_class_error_train

