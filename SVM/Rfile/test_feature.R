library(magrittr)
set.seed(12)
n1 = rnorm(10000)
n2 = abs(n1)*50
n3 = matrix(n2,ncol = 100)
n4 = round(rowMeans(n3))
hist(n4%%7)

set.seed(12)
rnorm(10000)%>%abs%>%'*'(50)%>%matrix(ncol = 100)%>%rowMeans%>%round%>%'%%'(7)%>%hist


# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
rfe_results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
rfe_results <- rfe(new_trainset[,-3], new_trainset[,3]$rent, sizes=c(1:16), rfeControl=control)
# summarize the results
print(rfe_results)
# list the chosen features
predictors(rfe_results)
# plot the results
plot(rfe_results, type=c("g", "o"))
