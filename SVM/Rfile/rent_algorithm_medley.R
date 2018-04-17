library(caret)
library(mlbench)
dataset <- train_rent

library(mice)
md.pattern(dataset)

metric <- "RMSE" #Rsquared
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
set.seed(seed)
# fit.lda <- train(rent~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(rent~., data=dataset, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(rent~., data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(rent~., data=dataset, method="rvmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(rent~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
# set.seed(seed)
# fit.nb <- train(rent~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(rent~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(rent~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(rent~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(rent~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# neural net work neuralnet
set.seed(seed)
fit.nn <- train(rent~., data=dataset, method="nnet", metric=metric,preProc=c("center", "scale"),trControl=control)

results <- resamples(list(linear=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn,cart=fit.cart,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm,nn = fit.nn))
# Table comparison
summary(results)

tune.grid = expand.grid(n.trees = c(50,100),interaction.depth = 1:5,shrinkage = 0.1^(1:3),n.minobsinnode = c(5,10))
fit.gbm.grid <- train(rent~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE,tuneGrid = tune.grid)
