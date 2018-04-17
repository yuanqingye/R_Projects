x = 1:10
y = filter(x,filter = c(1,2,3,4),sides = 2,method = "convolution")

y2 = filter(x,filter = c(1,2,3,4),sides = 1, method = "convolution")

y3 = filter(x,filter = c(1,2,3,4),method = "recursive")

firstsmooth = function(x,initial,alpha){
  s = 0
  s0 = initial
  s[1] = alpha*x[1]+(1-alpha)*s0
  num = length(x)
  for(i in 2:num){
    s[i] = alpha*x[i] + (1-alpha)*s[i-1]
  }
  MSE = mean((s-x)^2)
  predict = alpha*x[num]+(1-alpha)*s[num]
  return(list(s=s,mse = MSE,predict=predict))
}

secondsmooth = function(x,b0,s0,alpha,beta,m){
  s = 0
  b = 0
  s[1] = alpha*x[1] + (1-alpha)*(s0+b0)
  b[1] = beta*(s[1]-s[0])+(1-beta)*b0
  num = length(x)
  for(i in 2:num){
    s[i] = alpha*x[i]+(1-alpha)*(s[i-1]+b[i-1])
    b[i] = beta*(s[i]-s[i-1])+(1-beta)*(b[i-1])
  }
  MSE = mean((x-s)^2)
  predict = x[num]+b[num]*1:m
  return(list(predict=predict,mse = MSE,s=s))
}

