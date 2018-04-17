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

#
source("Rfile/nn_para.R")
neuralnet_para_m = neuralnet_para(train.scaled,test.scaled)
# neuralnet_para_table = nn_para_percent(train.scaled,test.scaled)
nn <- neuralnet(f,data=train.scaled,hidden = rep(12,5),act.fct = 'logistic',linear.output=T) #9,2也是个好的选择
pr.nn <- compute(nn,test.scaled[,-3])
pr.nn_ <- pr.nn$net.result*(max_rent-min_rent)+min_rent
test.r <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
MSE.nn <- sum((test.r - pr.nn_)^2)/length(test.r)
compare.test2 = cbind(pr.nn_,test.r)