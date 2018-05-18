library(neuralnet)
# n <- names(new_trainset)
# f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))
# maxs <- apply(new_trainset, 2, max) 
# mins <- apply(new_trainset, 2, min)
# train.scaled = as.data.frame(scale(new_trainset, center = mins, scale = maxs - mins))
# test.scaled = as.data.frame(scale(new_testset,center = mins,scale = maxs - mins))
# max_rent = maxs["rent"]
# min_rent = mins["rent"]

neuralnet_para = function(trainset,testset){
  trainname <- names(trainset)
  fff <- as.formula(paste("rent ~", paste(trainname[!(trainname %in% "rent")], collapse = " + ")))
  maxs <- apply(trainset, 2, max) 
  mins <- apply(trainset, 2, min)
  trainset = as.data.frame(scale(trainset, center = mins, scale = maxs - mins))
  testset = as.data.frame(scale(testset,center = mins,scale = maxs - mins))
  max_rent = maxs["rent"]
  min_rent = mins["rent"]
  k = 3
  library(plyr) 
  pbar <- create_progress_bar('text')
  pbar$init(k*7)
  min = Inf
  v = vector(mode = 'numeric',length = 0)
  c = vector(mode = 'character',length = 0)
  dt = data.table(size = v,depth = v,e = v,actfun = c)
  rentind = which(names(trainset)=="rent")
  for(depth in seq(from = 5, to = 9, length = k)){
    for(size in seq(3,21,by = 3)){
      # for(actfun in c('logistic','tanh')){
        for(actfun in c('logistic')){
        neural.model = neuralnet(fff,data = trainset,hidden = rep(size,depth),act.fct = actfun)
        neural.pred = compute(neural.model,testset[,-rentind])$net.result
        neural.pred <- neural.pred*(max_rent-min_rent)+min_rent
        test.r <- (testset$rent)*(max_rent-min_rent)+min_rent
        e <- sum((test.r - neural.pred)^2)/length(test.r)
        temp.dt = data.table(size = size,depth = depth,e = e,actfun = actfun)
        dt = rbind(dt,temp.dt)
        if(e<min){
          bestsize = size
          bestdepth = depth
          bestactfun = actfun
          min = e
        }
        }
      pbar$step()
    }
  }
  min.dt = data.table(size = bestsize,depth = bestdepth,e = min,actfun = bestactfun)
  dt = rbind(dt,min.dt)
  return(dt)
}
