library(rpart.plot)
data("bodyfat", package = "TH.data")

dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]

set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train,control = rpart.control(minsplit = 10))

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)
prp(bodyfat_rpart)

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
prp(bodyfat_rpart)
printcp(bodyfat_rpart)

DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed",ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

set.seed(4321)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

library(party)
ct = ctree(Species~.,data=trainData)
table(predict(ct,testData),testData$Species)

library(randomForest)
# Species ~ .指的是Species与其他所有属性之间的等式
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf,testData), testData$Species)