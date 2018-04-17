library(caret)
head(iris)
library(klaR)

x = iris[,-5]
y = iris$Species

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)

table(predict(model$finalModel,x)$class,y)

naive_iris <- NaiveBayes(iris$Species ~ ., data = iris)
plot(naive_iris)

library(naivebayes)

#try svm for one class 
library(e1071)
one_sample_train = iris[iris$Species=="setosa",]
svm_one_sample_model = svm(Species~.,one_sample_train,type="one-classification",nu=0.5,
                           scale=TRUE,
                           kernel="radial")
result = predict(svm_one_sample_model,iris[,-5])
result = as.numeric(result)
result = as.factor(result)
ori_result = ifelse(iris$Species=="setosa",1,0)
ori_result = as.factor(ori_result)
sum(result==ori_result)/length(result)

specificity(data = result,reference = ori_result)
sensitivity(data = result,reference = ori_result)
