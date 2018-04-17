library("neuralnet")

# rp = rpart(rent~.,data = new_trainset,control = rpart.control(cp = 0.01,minsplit = 6, minbucket = round(6/3)),
#            parms = list(prior = c(.65,.35), split = "gini"),method = "anova")
# n <- names(dt)
# f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

f <- reformulate(setdiff(colnames(new_trainset), "rent"), response="rent")
f2 = rent~sale+area
nn2 = neuralnet(f2,data = traindata,hidden = 15,lifesign = "full",threshold = 300000000,stepmax = 1e+7,algorithm = "rprop+",linear.output = TRUE)
nn2.cont = neuralnet(f2,data = traindata,hidden = 15 ,lifesign = "full", threshold = 100000000,
                     stepmax = 1e+7,algorithm = "rprop+",linear.output = TRUE,startweights = nn2$weights)

pred2 = compute(nn2,scale(new_testset[,c(2,5)]))$net.result


pred2 = compute(nn2,new_testset[,c(2,5)])$net.result
pred.train =compute(nn2,traindata[,c(2,5)])$net.result

nn3 = neuralnet(Sqrt~Var1,sqrt.data,hidden = 15,lifesign = "full",threshold = 0.01,algorithm = "rprop+")

nn <- neuralnet(f, data=new_trainset, hidden=20,threshold = 492000000,startweights = rnorm(20),lifesign = "full",algorithm = "sag")
nn <- neuralnet(f, data=new_trainset, hidden=8,threshold = 1,startweights = initweight,lifesign = "full",algorithm = "slr")

set.seed(1)
initweight = rnorm(1000)

pred = compute(nn,new_testset[,-3])
pred.train = prediction(nn)
491975411
492675470.8
plot(nn)
gwplot(nn)
confidence.interval(nn)
prediction(nn)

Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
print(net.sqrt <- neuralnet(Sqrt~Var1, data = sqrt.data, hidden=10, threshold=0.01))
compute(net.sqrt, (1:10)^2)$net.result

# data(infert, package="datasets")
# print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, 
#                               err.fct="ce", linear.output=FALSE, likelihood=TRUE))

scaled.dat <- scale(new_trainset)
library(standardize)
new_trainset_sd = standardize(f, data=new_trainset)
traindata = data.frame(rent = new_trainset$rent,new_trainset_sd$data[,-1])
nn <- neuralnet(f, data=traindata, hidden=12,threshold = 489262279,lifesign = "full",algorithm = "slr")
pred = compute(nn,new_testset[,-3])$net.result

set.seed(500)
library(MASS)
data <- Boston

apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

#scale for preparation of neural network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)

pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  pbar$step()
}

mean(cv.error)

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

#scale for preparation of neural network
#new_trainset is the original train set,new_testset is the original test set 
maxs <- apply(new_trainset, 2, max) 
mins <- apply(new_trainset, 2, min)
train.scaled = as.data.frame(scale(new_trainset, center = mins, scale = maxs - mins))
test.scaled = as.data.frame(scale(new_testset,center = mins,scale = maxs - mins))
max_rent = maxs["rent"]
min_rent = mins["rent"]

library(neuralnet)
n <- names(train.scaled)
f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))

source("Rfile/nn_para.R")
neuralnet_para_m = neuralnet_para(train.scaled,test.scaled)
# neuralnet_para_table = nn_para_percent(train.scaled,test.scaled)
nn <- neuralnet(f,data=train.scaled,hidden = rep(12,5),act.fct = 'logistic',linear.output=T) #9,2也是个好的选择
pr.nn <- compute(nn,test.scaled[,-3])
pr.nn_ <- pr.nn$net.result*(max_rent-min_rent)+min_rent
test.r <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
MSE.nn <- sum((test.r - pr.nn_)^2)/length(test.r)
compare.test2 = cbind(pr.nn_,test.r)
