# 例如动量梯度下降（QuickProp）、Nesterov 加速动量（NAG）梯度下降、
# 自适应梯度算法（AdaGrad）、弹性反向传播（RProp）和均方根反向传播（RMSProp）

# 这些算法并不能完全解决局部最小值问题，而且在同时优化神经网络的架构和权重时用处也不大。
# 为了得到所需的全局优化算法。两种流行的全局优化算法是粒子群优化算法（PSO）和遗传算法（GA）。
# 下面是两者可被用来训练神经网络的方式

# install.packages("RSNNS")
library(Rcpp)
library(RSNNS)
library(data.table)
load("~/data/rentmodel.RData")
data(iris)
iris = iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
#定义网络输入 
irisValues= iris[,1:4]
#定义网络输出，并将数据进行格式转换 
irisTargets = decodeClassLabels(iris[,5])
#从中划分出训练样本和检验样本 
iris = splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
#数据标准化 
iris = normTrainingAndTestSet(iris)
#利用mlp命令执行前馈反向传播神经网络算法 
model = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest) 
#利用上面建立的模型进行预测 
predictions = predict(model,iris$inputsTest)
#生成混淆矩阵，观察预测精度 
confusionMatrix(iris$targetsTest,predictions)

rent.model = mlp(new_trainset[,-3], new_trainset[,3], size=c(5), learnFunc="Quickprop", 
                maxit=100, 
                inputsTest=new_testset[,-3], targetsTest=new_testset[,3],outputActFunc = 'linear') 
rent.predict = predict(rent.model,new_testset[,-3])

data(snnsData)
inputs <- snnsData$laser_1000.pat[,inputColumns(snnsData$laser_1000.pat)]
inputs <- snnsData$eight_016.pat[,inputColumns(snnsData$eight_016.pat)]
outputs <- snnsData$eight_016.pat[,outputColumns(snnsData$eight_016.pat)]

par(mfrow=c(1,2))
modelElman <- elman(inputs, outputs, size=c(8,9), learnFuncParams=c(0.1), maxit=1000)
modelElman
modelJordan <- jordan(inputs, outputs, size=8, learnFuncParams=c(0.1), maxit=1000)
modelJordan
plotIterativeError(modelElman)
plotIterativeError(modelJordan)

rent.model.Elman = elman(new_trainset[,-3],new_trainset[,3],learnFuncParams = c(0.1),size = 8,maxit = 1000)
rent.predict.Elman = predict(rent.model.Elman,new_testset[,-3])
e.Elman = crossprod(rent.predict.Elman - unlist(new_testset[,3]))/nrow(new_testset)
result_compare.Elman = cbind(rent.predict.Elman,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.Elman = data.table(result_compare.Elman)
result_compare.Elman[,c("diff","percent"):= list(rent.predict-rent,(rent.predict-rent)/rent)]
