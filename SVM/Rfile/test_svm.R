library(data.table)
library(e1071)
library(plyr)
svm_para = function(trainset,testset){
  min = Inf
  v = vector(mode = 'numeric',length = 0)
  c = vector(mode = 'character',length = 0)
  dt = data.table(sigma = v,C = v,e = v,kernel = c)
  rentind = which(names(trainset)=="rent")
  for(index in -5:10){
    sigma = 0.125 * 2^index
    for(i in -1:4){
      C = 1 * 10^i
      for(kernel in c('polynomial','radial','sigmoid')){
      svm.model = svm(rent~.,data = trainset,gamma = sigma,cost = C,kernel = kernel)
      svm.pred = predict(svm.model,testset[,-rentind])
      e = crossprod(svm.pred - unlist(testset[,rentind]))/nrow(testset)
      temp.dt = data.table(sigma = sigma,C = C,e = e[1,1],kernel = kernel)
      dt = rbind(dt,temp.dt)
      if(e[1,1]<min){
        bestsigma = sigma
        bestC = C
        bestKernal = kernel
        min = e[1,1]
        }
      }
    }
  }
  min.dt = data.table(sigma = bestsigma,C = bestC,e = min,kernel = bestKernal)
  dt = rbind(dt,min.dt)
  return(dt)
} 

svm_para_class = function(trainset,testset,keycolname,f,type){
  test_formula = as.formula(paste0(keycolname,"~."))
  max = -Inf
  v = vector(mode = 'numeric',length = 0)
  c = vector(mode = 'character',length = 0)
  dt = data.table(sigma = v,C = v,accuracy = v,kernel = c)
  # keyind = which(names(trainset)==keycolname)
  indexes = c(-5,3,10)
  k = length(indexes)
  pbar <- create_progress_bar('text')
  pbar$init(k)
  for(index in c(-5,3,10)){
    sigma = 0.125 * 2^index
    for(i in -1:4){
      C = 1 * 10^i
      for(kernel in c('radial','sigmoid')){
        svm.model = svm(test_formula,data = trainset,gamma = sigma,cost = C,kernel = kernel)
        svm.pred = predict(svm.model,testset[,!(names(testset) %in% keycolname)])
        r = f(testset[,keycolname],svm.pred,type)
        temp.dt = data.table(sigma = sigma,C = C,accuracy = r,kernel = kernel)
        dt = rbind(dt,temp.dt)
        if(r>max){
          bestsigma = sigma
          bestC = C
          bestKernal = kernel
          max = r
        }
      }
    }
    pbar$step()
  }
  max.dt = data.table(sigma = bestsigma,C = bestC,accuracy = max,kernel = bestKernal)
  dt = rbind(dt,max.dt)
  return(dt)
} 


svm_para_percent = function(trainset,testset){
  minavge = Inf
  mine2 = Inf
  minmax = Inf
  v = vector(mode = 'numeric',length = 0)
  c = vector(mode = 'character',length = 0)
  dt = data.table(sigma = v,C = v,e = v,kernel = c,avge = v,maxe = v,sde = v)
  rentind = which(names(trainset)=="rent")
  for(index in -5:5){
    sigma = 0.125 * 2^index
    for(i in -1:2){
      C = 1 * 10^i
      for(kernel in c('radial')){
        svm.model = svm(rent~.,data = trainset,gamma = sigma,cost = C,kernel = kernel)
        svm.pred = predict(svm.model,testset[,-rentind])
        e = (svm.pred - unlist(testset[,rentind]))/unlist(testset[,rentind])
        crosse = (crossprod(svm.pred - unlist(testset[,rentind]))/nrow(testset))[1,1]
        avge = sum(abs(e))/nrow(testset)
        maxe = max(e)
        sde = sd(e)
        temp.dt = data.table(sigma = sigma,C = C,e = crosse,kernel = kernel,avge = avge,maxe = maxe,sde = sde)
        dt = rbind(dt,temp.dt)
        if(abs(avge)<abs(minavge)){
          bestsigma = sigma
          bestC = C
          bestKernal = kernel
          pickedmax = maxe
          pickedsd = sde
          mine = crosse
          minavge = avge
        }
        if(crosse<mine2){
          bestsigma2 = sigma
          bestC2 = C
          bestKernal2 = kernel
          pickedmax2 = maxe
          pickedsd2 = sde
          mine2 = crosse
          minavge2 = avge
        }
        if(maxe<minmax){
          bestsigmamax = sigma
          bestCmax = C
          bestKernalmax = kernel
          minmax = maxe
          pickedsdmax = sde
          minemax = crosse
          minavgemax = avge
        }
        
      }
    }
  }
  min.dt = data.table(sigma = bestsigma,C = bestC,e = mine,kernel = bestKernal,avge = minavge,maxe = pickedmax,sde = pickedsd)
  min.cross.dt = data.table(sigma = bestsigma2,C = bestC2,e = mine2,kernel = bestKernal2,avge = minavge2,maxe = pickedmax2,sde = pickedsd2)
  min.max.dt = data.table(sigma = bestsigmamax,C = bestCmax,e = minemax,kernel = bestKernalmax,avge = minavgemax,maxe = minmax,sde = pickedsdmax)
  dt = rbind(dt,min.dt,min.cross.dt,min.max.dt)
  return(dt)
} 