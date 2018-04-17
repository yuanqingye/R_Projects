setwd("D:/R") 
data0 = read.csv("D:/R/mall.csv",header=T) 
head(data0)
dim(data0)

avg_rental = data0$finalprice/data0$area
data = cbind(data0,avg_rental)
dim(data)
data.2016 = data[1:16,]
data.2015 = data[17:32,]
data.2014 = data[33:48,]
library(ggplot2)
library(plotly)
help(plotly)
plot_ly(data.2016,y=data.2016$avg_rental,type="box",name="avg.2016") %>%
  add_trace(data.2015,y=data.2015$avg_rental,type="box",name="avg.2015")%>%
  add_trace(data.2014,y=data.2014$avg_rental,type="box",name="avg.2014")%>%
  layout(title="Outliers and Boxplot")
which(data$avg_rental>300)
data00 = data[c(-11,-27,-43),1:25]
test.mall_name = data00[1:15,1]
data1 = data[c(-11,-27,-43),2:25] 
data = data[c(-11,-27,-43),2:24]
dim(data)

test = data[1:15,]
m.test = dim(test)[1]
train = data[16:45,]
m.train = dim(train)[1]


##LASSO
library(glmnet)
names <- c("open_year","area","customer_num","sale","jiaju","jiancai",
           "ruanzhuang","jinkou",
           "xinyetai","brand_num","gdp","population","region_area",
           "density","avg_gdp","avg_salary","highway_distance","road_distance",
           "location","subway_distance","shangquan_distance","shangquan_num")
X <- as.matrix(train[names])
y <- as.matrix(train["finalprice"])
corr_matrix <- cor(X)
eigen_values <- eigen(corr_matrix)$values
condition_number <- max(eigen_values) / min(eigen_values)
lasso_reg = glmnet(X,y,alpha = 1)
cv_reg = cv.glmnet(X,y,alpha = 1)  
plot(cv_reg)
lambda = cv_reg$lambda.min
model.final = cv_reg$glmnet.fit
model.coef = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.1se)
coeff = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.min)
all.coef <- coef(cv_reg$glmnet.fit, s =  min(model.final$lambda))


X <- as.matrix(test[names])
y <- as.matrix(test["finalprice"])
pre = predict(cv_reg, newx = X, s= cv_reg$lambda.min) 
error = (pre-y)/y
pre.lasso =data.frame(y,pre,error)
lasso = data.frame(test.mall_name,pre.lasso)
err.lasso = mean((pre-y)^2) 
err.lasso
plot(pre,y)
abline(a=0,b=1)
subset(lasso,abs(error)<0.1)
which(abs(error)<0.1)


##ridge
library(glmnet)
names <- c("open_year","area","customer_num","sale","jiaju","jiancai",
           "ruanzhuang","jinkou",
           "xinyetai","brand_num","gdp","population","region_area",
           "density","avg_gdp","avg_salary","highway_distance","road_distance",
           "location","subway_distance","shangquan_distance","shangquan_num")
X <- as.matrix(train[names])
y <- as.matrix(train["finalprice"])
corr_matrix <- cor(X)
eigen_values <- eigen(corr_matrix)$values
condition_number <- max(eigen_values) / min(eigen_values)

ridge_reg = glmnet(X,y,alpha = 0)
cv_reg = cv.glmnet(X,y,,alpha = 0)  
plot(cv_reg)
lambda = cv_reg$lambda.min
model.final = cv_reg$glmnet.fit
model.coef = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.1se)
coeff = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.min)
all.coef <- coef(cv_reg$glmnet.fit, s =  min(model.final$lambda))

X <- as.matrix(test[names])
y <- as.matrix(test["finalprice"])
pre = predict(model.final,newx=X,s=cv_reg$lambda.min)
error = (pre-y)/y
pre.ridge = data.frame(y,pre,error)
ridge = data.frame(test.mall_name,pre.ridge)
err.ridge = mean((pre-y)^2) 
err.ridge
plot(pre,y)
abline(a=0,b=1)
error = (pre-y)/y
subset(ridge,abs(error)<0.1)
which(abs(error)<0.1)


##elastic net
library(glmnet)
#Condition Number
names <- c("open_year","area","customer_num","sale","jiaju","jiancai",
           "ruanzhuang","jinkou",
           "xinyetai","brand_num","gdp","population","region_area",
           "density","avg_gdp","avg_salary","highway_distance","road_distance",
           "location","subway_distance","shangquan_distance","shangquan_num")
X <- as.matrix(train[names])
y <- as.matrix(train["finalprice"])
corr_matrix <- cor(X)
eigen_values <- eigen(corr_matrix)$values
condition_number <- max(eigen_values) / min(eigen_values)
lasso_reg = glmnet(X,y,alpha = 0.5)
cv_reg = cv.glmnet(X,y,,alpha = 0.5) 
plot(cv_reg)
lambda = cv_reg$lambda.min
model.final = cv_reg$glmnet.fit
model.coef = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.1se)
coeff = coef(cv_reg$glmnet.fit, s = cv_reg$lambda.min)
all.coef <- coef(cv_reg$glmnet.fit, s =  min(model.final$lambda))

X <- as.matrix(test[names])
y <- as.matrix(test["finalprice"])
pre = predict(model.final,newx=X,s=cv_reg$lambda.min)
error = (pre-y)/y
pre.elastic = data.frame(y,pre,error)
elastic = data.frame(test.mall_name,pre.elastic)
err.elastic = mean((pre-y)^2) 
plot(pre,y)
abline(a=0,b=1)
subset(elastic,abs(error)<0.1)
which(abs(error)<0.1)

