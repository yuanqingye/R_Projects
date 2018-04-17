main = function(){
m = matrix(rnorm(100000*2),ncol = 2)
max_v = pmax(m[,1],m[,2])
result = mean(max_v)
}

main2 = function(){
  sum = 0
  for(i in 1:100000){
    xy = rnorm(2)
    max = max(xy)
    sum = sum + max
  }
  result = sum/100000
}

sim1 = function(N){
  result = 0
  nblue = 10
  n1 = 12
  n2 = 13
  for(i in 1:N){
    nblue2 = ifelse((runif(1)>10/18),6,7)
    result = result + ifelse(runif(1)>nblue2/13,1,0)
  }
  result = result/N
  return(result)
}

sim2 = function(N){
  result = 0
  nblue = 10
  n1 = 12
  n2 = 13
  m = matrix(runif(2*N),ncol = 2)
  result = m[,1]>10/18 & m[,2]<6/13 | m[,1]<10/18 & m[,2]>6/13
  sum(result)/N
}

createTimesMatrix = function(x,n){
  m = matrix(0,nrow = length(x),ncol = n)
  prod = x
  m[,1] = prod
  for(i in 2:n){
    prod = x*prod
    m[,i] = prod
  }
  return(m)
}

Rprof()
invisible(createTimesMatrix(1:10,8))
Rprof(NULL)
summaryRprof()
