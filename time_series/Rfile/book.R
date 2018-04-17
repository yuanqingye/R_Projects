price = c(101,82,66,35,31,7)
price = ts(price,start = c(2005,1),frequency = 12)

price2 = scan()

yield = c(15.2,16.9,15.3,14.9,15.7,15.1,16.7)
yield = ts(yield,start = 1884)
plot(yield)
plot(yield,type = "s")
plot(yield,type = "h")
plot(yield,type = "o",pch = 7,lty = 2,lwd = 2,col = "purple",main="time series", sub = "wheal")
abline(v = c(1885,1889), lty = 2)
abline(h = c(15.5,16.5), lty = 4,col = "green")
acf(yield)

whitenoise = rnorm(100)
whitenoise = ts(whitenoise)
plot(whitenoise)
acf(whitenoise)

Box.test(whitenoise,lag = 6)
Box.test(whitenoise,lag = 12)

##page 55

sha = read.table("~/ts_data/file4.csv",sep = ",", header = TRUE)
output = ts(sha$output,start = 1964)
plot(output)
acf(output,lag = 25)

a = read.table("~/ts_data/file5.csv",sep = ",", header = TRUE)
milk = ts(a$milk,start = c(1962,1),frequency = 12)
plot(milk)
acf(milk)

b = read.table("~/ts_data/file6.csv",sep = ",", header = TRUE)
temp = ts(b$temp,start = 1949)
plot(temp)
acf(temp)
for(i in 1:2) print(Box.test(temp,lag = 6*i))

c = read.table("~/ts_data/file7.csv",sep = ",", header = TRUE)
prop = ts(c$prop,start = 1950)
plot(prop)
acf(prop)
for(i in 1:2) print(Box.test(prop,lag = 6*i))

exer2.5.1 = ts(1:20)
acf(exer2.5.1)

mu = mean(exer2.5.1)
n = length(exer2.5.1)
xn = exer2.5.1 - mu

getasc = function(xn,k){
part = xn[1:(n-k)]*xn[(1+k):n]
result = sum(part)/(n-k)
return(result)
}

getvar = function(xn){
  part = xn*xn
  result = sum(part)/(n-1)
  return(result)
}

getasc(xn,3)
