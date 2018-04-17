library(mlbench)
library(caret)
library(plyr)
cal_model_accuracy = function(model,newset,keycol,calf,caltype)
{
  newset = newset[complete.cases(newset),]
  model_result = predict(object = model,newdata = newset)
  model_cal = calf(newset[,keycol],model_result,caltype)
  return(model_cal)
}

go_around_model = function(dataset,test_formula){
  pbar <- create_progress_bar('text')
  k = 11
  pbar$init(k)
  pbar$step()
  test_formula = as.formula(test_formula)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "Accuracy"
  preProcess=c("center", "scale")
  pbar$step()
  #LDA
  set.seed(seed)
  ptm <- proc.time()
  fit.lda <- train(test_formula, data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
  lda_time = proc.time() - ptm
  # Logistic Regression
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.glm <- train(test_formula, data=dataset, method="glm", metric=metric, trControl=control)
  glm_time = proc.time() - ptm
  # GLMNET
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.glmnet <- train(test_formula, data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
  glmnet_time = proc.time() - ptm
  # SVM Radial
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.svmRadial <- train(test_formula, data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
  svm_time = proc.time() - ptm
  # kNN
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.knn <- train(test_formula, data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
  knn_time = proc.time() - ptm
  # Naive Bayes
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.nb <- train(test_formula, data=dataset, method="nb", metric=metric, trControl=control)
  nb_time = proc.time() - ptm
  # CART
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.cart <- train(test_formula, data=dataset, method="rpart", metric=metric, trControl=control)
  cart_time = proc.time() - ptm
  # C5.0
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.c50 <- train(test_formula, data=dataset, method="C5.0", metric=metric, trControl=control)
  c50_time = proc.time() - ptm
  # Bagged CART
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.treebag <- train(test_formula, data=dataset, method="treebag", metric=metric, trControl=control)
  treebag_time = proc.time() - ptm
  # Random Forest
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.rf <- train(test_formula, data=dataset, method="rf", metric=metric, trControl=control)
  rf_time = proc.time() - ptm
  # Stochastic Gradient Boosting (Generalized Boosted Modeling)
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.gbm <- train(test_formula, data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_time = proc.time() - ptm
  pbar$step()
  results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                            svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                            bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
  time_records = list(lda_time = lda_time,glm_time = glm_time,glmnet_time = glmnet_time,
                      glmnet_time = glmnet_time,svm_time = svm_time,knn_time = knn_time,
                      nb_time = nb_time,cart_time = cart_time,c50_time = c50_time,
                      treebag_time = treebag_time,rf_time = rf_time,gbm_time = gbm_time)
  return(list(results,time_records))
  # summary(results)
  # bwplot(results)
}

go_around_model_with_test = function(dataset,newset,keycol,calf,caltype = "accuracy"){
  pbar <- create_progress_bar('text')
  k = 11
  pbar$init(k)
  pbar$step()
  test_formula = paste0(keycol,"~.")
  test_formula = as.formula(test_formula)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "Accuracy"
  preProcess=c("center", "scale")
  pbar$step()
  #LDA
  set.seed(seed)
  ptm <- proc.time()
  fit.lda <- train(test_formula, data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
  lda_cal = cal_model_accuracy(model = fit.lda,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  lda_time = proc.time() - ptm
  # Logistic Regression
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.glm <- train(test_formula, data=dataset, method="glm", metric=metric, trControl=control)
  glm_cal = cal_model_accuracy(model = fit.glm,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  glm_time = proc.time() - ptm
  # GLMNET
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.glmnet <- train(test_formula, data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
  glmnet_cal = cal_model_accuracy(model = fit.glmnet,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  glmnet_time = proc.time() - ptm
  # SVM Radial
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.svmRadial <- train(test_formula, data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
  svm_cal = cal_model_accuracy(model = fit.svmRadial,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  svm_time = proc.time() - ptm
  # kNN
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.knn <- train(test_formula, data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
  knn_cal = cal_model_accuracy(model = fit.knn,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  knn_time = proc.time() - ptm
  # Naive Bayes
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.nb <- train(test_formula, data=dataset, method="nb", metric=metric, trControl=control)
  nb_cal = cal_model_accuracy(model = fit.nb,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  nb_time = proc.time() - ptm
  # CART
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.cart <- train(test_formula, data=dataset, method="rpart", metric=metric, trControl=control)
  cart_cal = cal_model_accuracy(model = fit.cart,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  cart_time = proc.time() - ptm
  # C5.0
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.c50 <- train(test_formula, data=dataset, method="C5.0", metric=metric, trControl=control)
  c50_cal = cal_model_accuracy(model = fit.c50,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  c50_time = proc.time() - ptm
  # Bagged CART
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.treebag <- train(test_formula, data=dataset, method="treebag", metric=metric, trControl=control)
  treebag_cal = cal_model_accuracy(model = fit.treebag,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  treebag_time = proc.time() - ptm
  # Random Forest
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.rf <- train(test_formula, data=dataset, method="rf", metric=metric, trControl=control)
  rf_cal = cal_model_accuracy(model = fit.rf,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  rf_time = proc.time() - ptm
  # Stochastic Gradient Boosting (Generalized Boosted Modeling)
  pbar$step()
  set.seed(seed)
  ptm <- proc.time()
  fit.gbm <- train(test_formula, data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_cal = cal_model_accuracy(model = fit.gbm,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
  gbm_time = proc.time() - ptm
  pbar$step()
  results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                            svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                            bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
  cal_results = list(lda_cal = lda_cal,glm_cal = glm_cal,
                     glmnet_cal = glmnet_cal,svm_cal = svm_cal,knn_cal = knn_cal,
                     nb_cal = nb_cal,cart_cal = cart_cal,c50_cal = c50_cal,
                     treebag_cal = treebag_cal,rf_cal = rf_cal,gbm_cal = gbm_cal)
  time_records = list(lda_time = lda_time,glm_time = glm_time,
                      glmnet_time = glmnet_time,svm_time = svm_time,knn_time = knn_time,
                      nb_time = nb_time,cart_time = cart_time,c50_time = c50_time,
                      treebag_time = treebag_time,rf_time = rf_time,gbm_time = gbm_time)
  return(list(results,cal_results,time_records))
  # summary(results)
  # bwplot(results)
}

go_around_model_with_test_simple_version = function(dataset,newset,keycol,modeldf,
  calf,caltype = "accuracy",metric = "Accuracy",preProc = c("center","scale")){
  K = nrow(modeldf)
  resample_list = list()
  time_records = list()
  cal_results = list()
  test_formula = paste0(keycol,"~.")
  test_formula = as.formula(test_formula)
  pbar = create_progress_bar("text")
  pbar$init(K)
  seed = 6
  #when method=none then use entire set for training
  trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3)
  for(md in modeldf[[1]]){
    set.seed(seed)
    ptm <- proc.time()
    if(md %in% c("lda","glmnet","svmRadial","knn")){
      tr_model = train(form = test_formula,data = dataset,method = md,metric = metric,trControl = trControl,preProc = preProc)
    }
    else if(md %in% c("gbm")){
      tr_model = train(form = test_formula,data = dataset,method = md,metric = metric,trControl = trControl,verbose = FALSE)
    }
    else{
      tr_model = train(form = test_formula,data = dataset,method = md,metric = metric,trControl = trControl)
    }
    effect = cal_model_accuracy(model = tr_model,newset = newset,keycol = keycol,calf = calf,caltype = caltype)
    resample_list[[md]] = tr_model
    cal_results[[md]] = effect
    time_records[[md]] = proc.time() - ptm
    pbar$step()
    }
    results <- resamples(resample_list)
    return(list(results,cal_results,time_records))
}

temp = proc.time()
temp = proc.time() - temp