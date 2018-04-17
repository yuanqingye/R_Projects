library(mlbench)
library(caret)
library(caTools)
# The caret package supports parallel processing in order to decrease the compute time for a given 
# experiment. It is supported automatically as long as it is configured. 
# This is used for the loops for the repeats of cross validation for each parameter combination.
library(doMC)
registerDoMC(cores=4)

vignette("caret")
# load data
data(PimaIndiansDiabetes)
# rename dataset to keep code below generic
dataset <- PimaIndiansDiabetes

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

metric <- "Accuracy"

preProcess=c("center", "scale")

set.seed(seed)
fit.lda <- train(diabetes~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(diabetes~., data=dataset, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(diabetes~., data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(diabetes~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(diabetes~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(diabetes~., data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(diabetes~., data=dataset, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(diabetes~., data=dataset, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(diabetes~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(diabetes~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(diabetes~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
plot(fit.gbm)
plot(fit.svmRadial)

#the resamples compare all the results involved in training, 10 diff cv way and repeats 3 times
#each could correspond to diff parameters
results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)
bwplot(results)
densityplot(results, metric = "Accuracy")

pSpecies <- predict(fit.lda,dataset[,!colnames(dataset) %in% "diabetes"],type='prob')
colAUC(pSpecies,dataset$diabetes,plot=TRUE)

library(caret)
library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,
                                 ## the outcome data are needed
                                   p = .75,
                                 ## The percentage of data in the
                                   ## training set
                                   list = FALSE)

## The format of the results
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]

#The notes from caret vignette
set.seed(123)
ctrl <- trainControl(method = "repeatedcv",
                        repeats = 3,
                        classProbs = TRUE,#this is for AUC calculation
                        summaryFunction = twoClassSummary)

plsFit <- train(Class ~ .,
                   data = training,
                   method = "pls",
                   tuneLength = 15,
                   trControl = ctrl,
                   metric = "ROC",
                   preProc = c("center", "scale"))

plsFit

plot(plsFit)

rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,
                  + data = training,
                  + method = "rda",
                  + tuneGrid = rdaGrid,
                  + trControl = ctrl,
                  + metric = "ROC")



