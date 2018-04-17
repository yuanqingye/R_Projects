getYearPara = function(v,f){
  t = vector(mode = "numeric",length = 0L)
  for(i in 1:(length(v)-11)){
    t[i] = f(v[i:(i+11)])
  }
  return(t)
}

getYearReal = function(v,f){
  t = vector(mode = "numeric",length = 0L)
  for(i in 13:(length(v)-11)){
    t[i-12] = f(v[i:(i+11)])
  }
  return(t)
}


getYearParaV = function(v,f){
  t = vector(mode = "numeric",length = 0L)
  m = diag(1:(length(v)-12))%*%matrix(rep(1,12*(length(v)-12)),nrow = (length(v)-12),ncol = 12)
  m = 1:12+
  apply()
  return(t)
}