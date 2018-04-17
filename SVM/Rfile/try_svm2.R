library(kernlab)
irismodel <- ksvm(Species ~ ., data = iris,              
                  type = "C-bsvc", kernel = "rbfdot",                    
                  kpar = list(sigma = 0.1), C = 10,                    
                  prob.model = TRUE) 
irismodel
predict(irismodel, iris[c(3, 10, 56, 68, 107, 120), -5], type = "probabilities")


