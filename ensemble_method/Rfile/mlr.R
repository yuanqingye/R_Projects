# https://mlr-org.github.io/mlr-tutorial/release/html/
library(mlr)
iris.task = classif.task = makeClassifTask(id = "iris-example", data = iris, target = "Species")
resamp = makeResampleDesc("CV", iters = 10L)

lrn = makeLearner("classif.rpart")

control.grid = makeTuneControlGrid() 
#you can pass resolution = N if you want the algorithm to 
#select N tune params given upper and lower bounds to a NumericParam
#instead of a discrete one
ps = makeParamSet(
  makeDiscreteParam("cp", values = seq(0,0.1,0.01)),
  makeDiscreteParam("minsplit", values = c(10,20))
)

#you can also check all the tunable params
getParamSet(lrn)

#and the actual tuning, with accuracy as evaluation metric
res = tuneParams(lrn, task = iris.task, resampling = resamp, control = control.grid, par.set = ps, measures = list(acc,timetrain))
opt.grid = as.data.frame(res$opt.path)
print(opt.grid)

library(mlr)
data(iris)

## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
task = makeClassifTask(data = iris, target = "Species")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn = makeLearner("classif.lda")

n = nrow(iris)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = train(lrn, task, subset = train.set)

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred = predict(model, task = task, subset = test.set)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred, measures = list(mmce, acc))