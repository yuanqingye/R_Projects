library(lattice)
library(e1071)
library(data.table)
xyplot(Petal.Length ~ Petal.Width, data = iris, groups = Species, auto.key=list(corner=c(1,0)))

data(iris)
attach(iris)
subdata = iris[iris$Species != 'virginica',]
sampledata = iris[sample(1:nrow(iris),nrow(iris)/3),]
subdata$Species = factor(subdata$Species)
# model = svm(Species ~ Petal.Length + Petal.Width,data = subdata)

model = svm(Species ~ Petal.Length + Petal.Width,data = iris,subdata = sampledata)
plot(model, iris, Petal.Length ~ Petal.Width)
pred = predict(model,x)
table(pred,y)

model2 <- svm(Species ~ ., data = iris)
pred2 = predict(model2,x)
table(pred2,y)
plot(model2,iris,Petal.Length ~ Petal.Width)

x = iris[,-5]
y = iris[,5]
model3 = svm(x,y,kernel = "radial",gamma = if(is.vector(x)) 1 else 1/ncol(x))
pred3 = predict(model3,x)
pred4 = predict(model3,x,decision.values = TRUE)
table(pred3,y)

plot(model3,iris,Petal.Length ~ Petal.Width)

plot(cmdscale(dist(iris[,-5])),col = c("orange","blue","green")[as.integer(iris[,5])],pch = c("o","+")[1:150 %in% model3$index + 1])  

legend(1.8, -0.8, c("setosa","versicolor","virgincia"),col = c("orange","blue","green"), lty = 1)  

#使用svm范例
library(rpart)
# install.packages("mlbench")
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
table(pred = svm.pred, true = testset[,10])
table(pred = rpart.pred, true = testset[,10])
classAgreement(tab = table(pred = svm.pred, true = testset[,10]))


# Tips on practical use
# ❼ Note that SVMs may be very sensible to the proper choice of parameters,
# so allways check a range of parameter combinations, at least on a
# reasonable subset of your data.
# ❼ For classification tasks, you will most likely use C-classification with the
# RBF kernel (default), because of its good general performance and the
# few number of parameters (only two: C and γ). The authors of libsvm
# suggest to try small and large values for C—like 1 to 1000—first, then to
# decide which are better for the data by cross validation, and finally to try
# several γ’s for the better C’s.
# ❼ However, better results are obtained by using a grid search over all parameters.
# For this, we recommend to use the tune.svm() function in
# e1071.
# ❼ Be careful with large datasets as training times may increase rather fast.
# ❼ Scaling of the data usually drastically improves the results. Therefore,
# svm() scales the data by default.

