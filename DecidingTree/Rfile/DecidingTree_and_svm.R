library(e1071)
library(rpart)
data(Glass, package="mlbench")
## split data into a train and test set
index     <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset   <- Glass[testindex,]
trainset  <- Glass[-testindex,]
## svm
best.svm(x = trainset[, -10], y = trainset[, 10])
svm.model <- svm(Type ~ ., data = trainset, cost = 1, gamma = 0.1111111)
svm.pred  <- predict(svm.model, testset[,-10])
svm.table = table(pred = svm.pred, true = testset[,10])
## rpart
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred  <- predict(rpart.model, testset[,-10], type = "class")
rpart.table = table(pred = rpart.pred,true = testset[,10])

classAgreement(svm.table)
classAgreement(rpart.table)
