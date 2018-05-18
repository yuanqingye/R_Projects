new_general_info_2015 = new_general_info[new_general_info$year == 2015,]
new_general_info_2016 = new_general_info[new_general_info$year == 2016,]
new_general_info_2017 = new_general_info[new_general_info$year == 2017,]
train_next = merge(new_general_info_2015,new_general_info_2016[,c(1,4)],by = "mall_name",all.x = TRUE)
test_next = merge(new_general_info_2016,new_general_info_2017[,c(1,4)],by = "mall_name",all.x = TRUE)
train_next = train_next[,c(-1,-24)]
test_next = test_next[,c(-1,-24)]
names(train_next)[24] = "rent"
names(test_next)[24] = "rent"
setwd("~/R_Projects/SVM")
source("./Rfile/test_svm.R")
try_para = svm_para_percent(train_next,test_next)
svm.model.next = svm(rent~.,data = train_next,cost = 10,gamma = 0.015625,cross = 5)
svm.test.next = predict(svm.model.next,test_next[,-24])
compare.result = cbind(mall_name = new_general_info_2016$mall_name,real_rent = test_next[,"rent"],svm.test.next)
MSE.svm = sum((svm.test.next - test_next[,24])^2)/length(svm.test.next)

source("~/R_Projects/neuralNetwork/Rfile/nn_para.R")
neuralnet_para_m = neuralnet_para(train_next,test_next)
n <- names(train_next)
f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))
maxs <- apply(train_next, 2, max)
mins <- apply(train_next, 2, min)
train.scaled = as.data.frame(scale(train_next, center = mins, scale = maxs - mins))
test.scaled = as.data.frame(scale(test_next,center = mins,scale = maxs - mins))
max_rent = maxs["rent"]
min_rent = mins["rent"]
nn <- neuralnet(f,data=train.scaled,hidden = rep(9,3),act.fct = 'logistic') 
rentind = which(names(test.scaled) %in% c("rent"))
pr.nn <- compute(nn,test.scaled[,-rentind])
pr.nn_ <- pr.nn$net.result*(max_rent-min_rent)+min_rent
test.r <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
MSE.nn <- sum((test.r - pr.nn_)^2)/length(test.r)
compare.result2 = cbind(pr.nn_,test.r)



#new data,new method 2017/11
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(rent~., data=train_rent, method="rf", preProcess="scale", trControl=control,
               importance = T)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

# load libraries
library(mlbench)
library(caret)

# load data
data(PimaIndiansDiabetes)
dataset <- PimaIndiansDiabetes

require(gbm)
# rent.boost = gbm(rent ~ . ,data = train_next,distribution = "gaussian",n.trees = 10000,nTrain = 100,bag.fraction = 0.8,n.minobsinnode = 10)
source('~/R_Projects/ensemble_method/Rfile/boosting_para.R')
gbm_para_m = boosting_para(train_rent,test_rent) #takes a lot time
ptm = proc.time()
rent.boost = gbm(rent ~ . ,data = train_rent,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)
ptm = proc.time() - ptm
rent.boost
para_rank1 = summary(rent.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
n.trees = seq(from=100 ,to=100000, by=100) #num of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(rent.boost,test_rent,n.trees = n.trees)

#Calculating The Mean squared Test Error
test.error<-with(test_rent,apply((predmatrix-rent)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged
compare.result.gbm = cbind(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = predmatrix[,1000])
compare.result.gbm = data.table(compare.result.gbm)
compare.result.gbm[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.result.gbm[,diff:=(real_rent-pred_rent)]
MSE.gbm.MALLS = compare.result.gbm[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),gbm_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
MSE.gbm = test.error[1000]
#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

destmatrix.gbm<-predict(rent.boost,dest_rent,n.trees = 100000)
dest_result = cbind(mall_name=dest_mall_names,dest_rent = destmatrix.gbm)

source("~/R_Projects/SVM/Rfile/test_svm.R")
try_para = svm_para_percent(train_rent,test_rent)
# svm.model.next = svm(rent~.,data = train_rent,cost = 10,gamma = 0.00391,cross = 5)
svm.model.next = svm(rent~.,data = train_rent,cost = 10,gamma = 7.81250e-03,cross = 5)
rentind = names(test_rent) == "rent"
svm.test.next = predict(svm.model.next,test_rent[,!rentind])
compare.result.svm = cbind(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = svm.test.next)
compare.result.svm = data.table(compare.result.svm)
compare.result.svm[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.result.svm[,diff:=(real_rent-pred_rent)]
# MSE.svm.MALLS = compare.result.svm[,.(svm_err = sqrt(sum(diff^2)/3)),by = "mall_name"]
MSE.svm.MALLS = compare.result.svm[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),svm_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
MSE.svm = sum((svm.test.next - test_rent[,rentind])^2)/length(svm.test.next)

svm.dest.next = predict(svm.model.next,dest_rent[,!rentind])
dest_result = cbind(mall_name=dest_mall_names,dest_rent = svm.dest.next,test_rent = svm.test.next[(1:length(svm.dest.next))%%2==1])

source("~/R_Projects/neuralNetwork/Rfile/nn_para.R")
neuralnet_para_m = neuralnet_para(train_rent,test_rent)
n <- names(train_rent)
f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))
maxs <- apply(train_rent, 2, max)
mins <- apply(train_rent, 2, min)
train.scaled = as.data.frame(scale(train_rent, center = mins, scale = maxs - mins))
test.scaled = as.data.frame(scale(test_rent,center = mins,scale = maxs - mins))
max_rent = maxs["rent"]
min_rent = mins["rent"]
nn <- neuralnet(f,data=train.scaled,hidden = rep(6,7),act.fct = 'logistic') 
rentind = which(names(test.scaled) %in% c("rent"))
pr.nn <- compute(nn,test.scaled[,-rentind])
pr.nn_ <- pr.nn$net.result*(max_rent-min_rent)+min_rent
pr.nn_ = pr.nn_[,1]
test.r <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
MSE.nn <- sum((test.r - pr.nn_)^2)/length(test.r)
# compare.result.nn = cbind(pr.nn_,test.r)
compare.result.nn = cbind(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = pr.nn_)
compare.result.nn = data.table(compare.result.nn)
compare.result.nn[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.result.nn[,diff:=(real_rent-pred_rent)]
# MSE.nn.MALLS = compare.result.nn[,.(svm_err = sqrt(sum(diff^2)/3)),by = "mall_name"]
MSE.nn.MALLS = compare.result.nn[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),nn_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]

library(randomForest)
rf <- randomForest(rent ~ ., data=train_rent, ntree=10000, proximity=TRUE)
rentind = which(names(test_rent) %in% c("rent"))
rf.test.next= predict(rf,test_rent[-rentind])
compare.result.rf = cbind(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = rf.test.next)
compare.result.rf = data.table(compare.result.rf)
compare.result.rf[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.result.rf[,diff:=(real_rent-pred_rent)]
MSE.rf.MALLS = compare.result.rf[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),rf_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
# MSE.rf.MALLS = compare.result.rf[,.(svm_err = sqrt(sum(diff^2)/3)),by = "mall_name"]
MSE.rf <- sum((test_rent$rent - rf.test.next)^2)/length(test_rent$rent)


MSE.all.MALLS = cbind(MSE.rf.MALLS,nn_perc = MSE.nn.MALLS$perc,svm_perc = MSE.svm.MALLS$perc,gbm_perc = MSE.svm.MALLS$perc)


#get the rent from specified city beijing
train_beijing = train_rent[train_rent$city==1,]
test_beijing = test_rent[test_rent$city==1,]
test_beijing_df = test_df[test_df$city==1,]
rentind = which(names(test_rent) %in% c("rent"))
train_beijing_svm = train_beijing[,!(names(train_beijing)%in%c("position","city"))]
test_beijing_svm = test_beijing[,!(names(test_beijing)%in%c("position","city"))]


setwd("~/R_Projects/SVM")
source("./Rfile/test_svm.R")
try_para_beijing = svm_para_percent(train_beijing_svm,test_beijing_svm)
svm.model.beijing = svm(rent~.,data = train_beijing_svm,cost = 10,gamma = 0.00391,cross = 5,kernel = "polynomial")
svm.test.beijing = predict(svm.model.beijing,test_beijing_svm[,-rentind])
compare.beijing.svm = cbind(mall_name = test_beijing_df[,"MALL_NAME"],real_rent = test_beijing$rent,pred_rent = svm.test.beijing)
compare.beijing.svm = data.table(compare.beijing.svm)
compare.beijing.svm[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.beijing.svm[,diff:=abs(real_rent-pred_rent)]
# MSE.svm.MALLS = compare.result.svm[,.(svm_err = sqrt(sum(diff^2)/3)),by = "mall_name"]
MSE.svm.beijing.MALLS = compare.beijing.svm[,.(svm_err = sum(diff),real_rent=sum(real_rent),perc = sum(diff)/sum(real_rent)),by = "mall_name"]


require(gbm)
source("~/R_Projects/ensemble_method/Rfile/boosting_para.R")
gbm_para_beijing = boosting_para(train_beijing_svm,test_beijing_svm)
rent.boost.beijing = gbm(rent ~ . ,data = train_beijing_svm,distribution = "gaussian",n.trees = 100000,interaction.depth = 5,shrinkage = 0.01)
n.trees = 100000
predmatrix<-predict(rent.boost.beijing,test_beijing_svm,n.trees = n.trees)
test.error=mean((predmatrix-test_beijing$rent)^2)
compare.beijing.gbm = cbind(mall_name = test_beijing_df[,"MALL_NAME"],real_rent = test_beijing$rent,pred_rent = predmatrix)
compare.beijing.gbm = data.table(compare.beijing.gbm)
compare.beijing.gbm[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.beijing.gbm[,diff:=abs(real_rent-pred_rent)]
MSE.gbm.beijing.MALLS = compare.beijing.gbm[,.(gbm_err = sum(diff),pred_rent=sum(pred_rent),real_rent=sum(real_rent),perc = sum(diff)/sum(real_rent)),by = "mall_name"]


library(randomForest)
rf.beijing <- randomForest(rent ~ ., data=train_beijing, ntree=10000, proximity=TRUE)
rf.beijing.next= predict(rf.beijing,test_beijing[,-rentind])
compare.beijing.rf = cbind(mall_name = test_beijing_df[,"MALL_NAME"],real_rent = test_beijing$rent,pred_rent = rf.beijing.next)
compare.beijing.rf = data.table(compare.beijing.rf)
compare.beijing.rf[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.beijing.rf[,diff:=abs(real_rent-pred_rent)]
MSE.rf.beijing.MALLS = compare.beijing.rf[,.(rf_err = sum(diff),real_rent=sum(real_rent),perc = sum(diff)/sum(real_rent)),by = "mall_name"]


source("~/R_Projects/neuralNetwork/Rfile/nn_para.R")
neuralnet_para_beijing = neuralnet_para(train_beijing_svm,test_beijing_svm)
n <- names(train_beijing_svm)
f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))
maxs <- apply(train_beijing_svm, 2, max)
mins <- apply(train_beijing_svm, 2, min)
train.scaled = as.data.frame(scale(train_beijing_svm, center = mins, scale = maxs - mins))
test.scaled = as.data.frame(scale(test_beijing_svm,center = mins,scale = maxs - mins))
max_rent = maxs["rent"]
min_rent = mins["rent"]
nn.beijing <- neuralnet(f,data=train.scaled,hidden = rep(18,8),act.fct = 'logistic') 
pr.nn.beijing <- compute(nn.beijing,test.scaled[,-rentind])
pr.nn.beijing <- pr.nn.beijing$net.result*(max_rent-min_rent)+min_rent
pr.nn.beijing = pr.nn.beijing[,1]
test.rent.beijing <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
MSE.nn.beijing <- sum((test.rent.beijing - pr.nn.beijing)^2)/length(test.rent.beijing)
compare.beijing.nn = cbind(mall_name = test_beijing_df[,"MALL_NAME"],real_rent = test_beijing_svm$rent,pred_rent = pr.nn.beijing)
compare.beijing.nn = data.table(compare.beijing.nn)
compare.beijing.nn[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
compare.beijing.nn[,diff:=abs(real_rent-pred_rent)]
MSE.nn.beijing.MALLS = compare.beijing.nn[,.(nn_err = sum(diff),real_rent=sum(real_rent),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
