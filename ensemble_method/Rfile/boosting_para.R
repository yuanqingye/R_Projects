boosting_para = function(trainset,testset,distribs="gaussian",n.trees = 100000){
  minavge = Inf
  mine2 = Inf
  minmax = Inf
  v = vector(mode = 'numeric',length = 0)
  c = vector(mode = 'character',length = 0)
  dt = data.table(shrink = v,interdepth = v,e = v,distribution = c,avge = v,maxe = v,sde = v)
  rentind = which(names(trainset)=="rent")
  k=5
  library(plyr) 
  pbar <- create_progress_bar('text')
  pbar$init(k)
  for(distrib in distribs){
    # sigma = 0.125 * 2^index
    for(interdepth in 1:5){
      for(shrinkind in 0:4){
        shrink = 10^(-shrinkind)
        gbm.model = gbm(rent ~ . ,data = train_rent,distribution = distrib,n.trees = n.trees,interaction.depth = interdepth,shrinkage = shrink)
        #Generating a Prediction matrix for each Tree
        predmatrix<-predict(gbm.model,test_rent,n.trees = n.trees)
        e = predmatrix - test_rent$rent
        crosse<-mean((e)^2)
        avge = sum(abs(e))/nrow(test_rent)
        maxe = max(e)
        sde = sd(e)
        temp.dt = data.table(shrink = shrink,interdepth = interdepth,e = crosse,distribution = distrib,avge = avge,maxe = maxe,sde = sde)
        dt = rbind(dt,temp.dt)
        if(abs(avge)<abs(minavge)){
          bestshrink = shrink
          bestinterdepth = interdepth
          bestdistrib = distrib
          pickedmax = maxe
          pickedsd = sde
          mine = crosse
          minavge = avge
        }
        if(crosse<mine2){
          bestshrink2 = shrink
          bestinterdepth2 = interdepth
          bestdistrib2 = distrib
          pickedmax2 = maxe
          pickedsd2 = sde
          mine2 = crosse
          minavge2 = avge
        }
        if(maxe<minmax){
          bestshrinkmax = shrink
          bestinterdepthmax = interdepth
          bestdistribmax = distrib
          minmax = maxe
          pickedsdmax = sde
          minemax = crosse
          minavgemax = avge
        }
      }
      pbar$step()
    }
  }
  min.dt = data.table(shrink = bestshrink,interdepth = bestinterdepth,e = mine,distribution = bestdistrib,avge = minavge,maxe = pickedmax,sde = pickedsd)
  min.cross.dt = data.table(shrink = bestshrink2,interdepth = bestinterdepth2,e = mine2,distribution = bestdistrib2,avge = minavge2,maxe = pickedmax2,sde = pickedsd2)
  min.max.dt = data.table(shrink = bestshrinkmax,interdepth = bestinterdepthmax,e = minemax,distribution = bestdistribmax,avge = minavgemax,maxe = minmax,sde = pickedsdmax)
  dt = rbind(dt,min.dt,min.cross.dt,min.max.dt)
  return(dt)
}
temp = dirname(rstudioapi::getActiveDocumentContext()$path)
