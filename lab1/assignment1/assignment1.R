library(kknn)
library(Formula)

install.packages('caret')
library(caret)

optdigits = read.csv("optdigits.csv")

mod_opdigits = read.csv("optdigits.csv")
colnames(mod_opdigits)[65]="Digit"
summary(mod_opdigits[,c(1:65)])

normalize = function(x) {
  return( (x-min(x))/(max(x)-min(x)))
}

norm_mod_digits = as.data.frame(lapply(mod_opdigits[,c(1:64)], normalize))

str(norm_mod_digits)

summary(norm_mod_digits)

n = dim(norm_mod_digits)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = norm_mod_digits[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
valid = norm_mod_digits[id2,]

id3 = setdiff(id1,id2)
test = norm_mod_digits[id3,]

train_target = mod_opdigits[id,65]
test_target = mod_opdigits[id3, 65]

m1 = kknn(formula=formula(train), train=train, test=test, k=20)

# -----------------------------

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

optdigits_knn = kknn(train, test, valid, k = 30, kernel = "rectangular")
fit=fitted(optdigits_knn)
print(fit)
confusion = table(Truth=test, Predition=optdigits_knn)

help("kknn")
