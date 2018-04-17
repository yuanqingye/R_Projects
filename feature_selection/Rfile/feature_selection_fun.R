library(mlbench)
library(caret)

#For accuracy, you must input a factor, else you use RMSE
rfe_selection = function(x,y,sizes = 1:8){
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
rfe_results <- rfe(x, y, sizes=sizes, rfeControl=control)
# list the chosen features
# predictors(rfe_results)
# plot(rfe_results, type=c("g", "o"))
return(rfe_results)
}

importance_selection = function(testdata,test_formula,test_method_name = "lvq"){
  test_formula = as.formula(test_formula)
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  test_model <- train(test_formula, data=testdata, method=test_method_name, preProcess="scale", trControl=control,
                      importance = T)
  # estimate variable importance
  importance <- varImp(test_model, scale=FALSE)
  # summarize importance
  # print(importance)
  # plot(importance)
  return(importance)
}