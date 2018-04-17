library(nnet)
f = rent~.
f2 = rent~sale+area
cities = substring(new_general_info$mall_name,1,2)
cities_code = as.numeric(factor(cities))
year = new_general_info$year
data = new_general_info[,c(-1,-24)]
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
scaled$city = cities_code
scaled$year = year
new_train <- scaled[year!=2017,-ncol(scaled)]
new_test <- scaled[year==2017,-ncol(scaled)]

# traindata = scale(new_trainset)
# traindata = cbind(rent = new_trainset$rent,traindata[,-3])
# traindata = as.data.frame(traindata)
# res <- nnet(f2,data=traindata,
#             size=35, linout=TRUE, skip=TRUE, MaxNWts=1000000, trace=FALSE, maxit=1000000)
# 
# testdata = scale(new_testset[,c(2,5)])
# testdata = as.data.frame(testdata)
# pred.nnet = predict(res,traindata[,c(3,5)])
# pred.test = predict(res,testdata)
# compare.train = cbind(pred.nnet,traindata$rent)
# compare.test = cbind(pred.test,new_testset$rent)
# e.test = crossprod(pred.test - unlist(new_testset$rent))/nrow(new_testset)
# checkdata = cbind(testdata[,c("sale","area")],traindata[1:19,c("sale","area")])

#combine scaled train data with actual rent: new_traindata,new_testdata
#original data: new_trainset,new_testset
#scaled data new_train,new_test
library(tibble) # add_column(dataset, d = 4:8, .after = 2)

new_traindata = cbind(rent = new_trainset$rent,new_train[,-3])
new_testdata = cbind(rent = new_testset$rent,new_test[,-3])
dt_ori = nn_para(new_trainset,new_testset) #try paras with original data 
dt_scale = nn_para(new_traindata,new_testdata) #try paras with scaled data
new_res <- nnet(rent~.,data=new_traindata,
            size=17, linout=TRUE, skip=TRUE, MaxNWts=1000000, trace=FALSE, maxit=10)
pred.nnet.new = predict(new_res,new_train[,c(-3)])
pred.test.new = predict(new_res,new_test[,c(-3)])
compare.train.new = cbind(pred.nnet.new,new_trainset$rent)
compare.test.new = cbind(pred.test.new,new_testset$rent)
e.test = crossprod(pred.test.new - unlist(new_testset$rent))/nrow(new_testset)

# checkdata = cbind(testdata[,c("sale","area")],traindata[1:19,c("sale","area")])


nn_para = function(traindata,testdata) {
  min = Inf
  v = vector(mode = "numeric",length = 0)
  dt = data.table(cost = v,size = v,iter = v)
  for (size in 5:35) {
    for (maxit in c(1e+1,1e+2,1e+3,1e+4,1e+5,1e+6,1e+7)) {
      res <- nnet(
        f,data = traindata,
        size = size, linout = TRUE, skip = TRUE, MaxNWts = 100000, trace =
          FALSE, maxit = maxit
      )
      pred.nnet = predict(res,traindata[,-which(names(traindata) %in% c("rent"))])
      pred.test = predict(res,testdata[,-which(names(testdata) %in% c("rent"))])
      e.test = crossprod(pred.test - unlist(new_testset$rent)) / nrow(new_testset)
      temp.dt = data.table(cost = e.test[1,1],size = size,iter = maxit)
      dt = rbind(dt,temp.dt)
      if (e.test[1,1] < min) {
        min = e.test[1,1]
        bestsize = size
        bestiter = maxit
      }
    }
  }
  temp.dt = data.table(cost = min,size = bestsize,iter = bestiter)
  dt = rbind(dt,temp.dt)
}
