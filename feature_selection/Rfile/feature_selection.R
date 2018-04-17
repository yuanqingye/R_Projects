set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
test_model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control,
               importance = T)
# estimate variable importance
importance <- varImp(test_model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#k features by importance using the caret r packageR

# In the train() statement, you need to specify the option importance=T. 
# This will get passed to the underlying random forest call. Otherwise, 
# importance is not calculated, giving you that error.

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#using gbm to get the para rank
rent.boost = gbm(rent ~ . ,data = train_rent,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)
rent.boost
para_rank1 = summary(rent.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance

