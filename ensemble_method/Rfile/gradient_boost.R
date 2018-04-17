# install.packages("gbm")
# ref:https://datascienceplus.com/gradient-boosting-in-r/
require(gbm)
require(MASS)#package with the boston housing dataset

#separating training and test data
train=sample(1:506,size=374)
Boston.boost=gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
Boston.boost
summary(Boston.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
plot(Boston.boost,i="lstat") 
#Inverse relation with lstat variable
plot(Boston.boost,i="rm") 
#as the average number of rooms increases the the price increases
cor(Boston$lstat,Boston$medv)#negetive correlation coeff-r
cor(Boston$rm,Boston$medv)#positive correlation coeff-r
n.trees = seq(from=100 ,to=10000, by=100) #num of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost,Boston[-train,],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(Boston[-train,],apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

library(randomForest)
rf = randomForest(medv ~ . ,data = Boston[train,])
rf.pred = predict(rf,Boston[-train,-14])
e.rf = crossprod(rf.pred - unlist(Boston[-train,14]))/nrow(Boston[-train,])
#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = e.rf,col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


# install.packages("adabag")
library(adabag)
adadata<-read.csv('~/data/bank-full.csv',
                  header=TRUE,sep=";")
adaboost<-boosting(y~age+job+marital+education+default+balance+
                     housing+loan+contact+day+month+duration+campaign+pdays+previous+
                     poutcome, data=adadata, boos=TRUE, mfinal=20,coeflearn='Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,adadata)
predict(adaboost,adadata)
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)

library(rpart)
data(iris)
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~ ., data=iris[sub,], mfinal=10)
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-sub,])
iris.predboosting$prob