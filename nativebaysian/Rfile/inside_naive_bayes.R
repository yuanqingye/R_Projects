# http://www.learnbymarketing.com/tutorials/naive-bayes-in-r/
# https://www.r-bloggers.com/naive-bayes-classification-in-r-part-2/
library(e1071)
model <- naiveBayes(Species ~ ., data = iris)
class(model)
summary(model)
print(model)

# The model has class “naiveBayes” and the summary tells us that the model
# provides a - priori probabilities of no - recurrence and recurrence events as
# well as conditional probability tables across all attributes. To examine the
# conditional probability tables just print the model. One of our tasks for this
# assignment was to create code which would give us the same conditional
# probabilities as those output by the naiveBayes() function. I went about this
# in the following way:

tbl_list <- sapply(iris[-5], table, iris[,5])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)

# If continuous features do not have normal distribution, we should use
# transformation or different methods to convert it in normal distribution. 

# If test data set has zero frequency issue, apply smoothing techniques “Laplace
# Correction” to predict the class of test data set. 

# Remove correlated features,
# as the highly correlated features are voted twice in the model and it can lead
# to over inflating importance. 

# Naive Bayes classifiers has limited options for
# parameter tuning like alpha=1 for smoothing, fit_prior=[True|False] to learn
# class prior probabilities or not and some other options (look at detail here).
# I would recommend to focus on your  pre-processing of data and the feature
# selection. 

# You might think to apply some classifier combination technique like
# ensembling, bagging and boosting but these methods would not help. Actually,
# “ensembling, boosting, bagging” won’t help since their purpose is to reduce
# variance. Naive Bayes has no variance to minimize.