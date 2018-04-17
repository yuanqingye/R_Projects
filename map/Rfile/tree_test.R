resultL = list()

for (j in 1:200){
  r = j*0.1
  max = -Inf
  min = Inf
for(i in 1:100){
  set.seed(i)
  df = data.frame(x = runif(69,min = 0,max = 20),y = runif(69,min = 0,max = 20))
  mdf = as.matrix(df)
  m = dist(mdf)
  I = r>m
  K = sum(I)/(69^2)*20*20
  L = sqrt(K/pi)-r
  if(L>max)
    max = L
  if(L<min)
    min = L
}
  resultL[[j]] = c(min,max)
}