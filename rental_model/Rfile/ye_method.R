data<-read.csv("~/data/booth_rental_shanghai1.csv")

rentable_area = (data$rentable_area-min(data$rentable_area))/(max(data$rentable_area)-min(data$rentable_area))  
mall_rental = (data$mall_rental-min(data$mall_rental))/(max(data$mall_rental)-min(data$mall_rental))  
ctg1_avg_rental = (data$ctg1_avg_rental-min(data$ctg1_avg_rental))/(max(data$ctg1_avg_rental)-min(data$ctg1_avg_rental))
ctg2_avg_rental = (data$ctg2_avg_rental-min(data$ctg2_avg_rental))/(max(data$ctg2_avg_rental)-min(data$ctg2_avg_rental))
ctg3_avg_rental = (data$ctg3_avg_rental-min(data$ctg3_avg_rental))/(max(data$ctg3_avg_rental)-min(data$ctg3_avg_rental))
booth_score = (data$booth_score-min(data$booth_score))/(max(data$booth_score)-min(data$booth_score))  
ctg1_mall_rental = (data$ctg1_mall_rental-min(data$ctg1_mall_rental))/(max(data$ctg1_mall_rental)-min(data$ctg1_mall_rental))
ctg2_mall_rental = (data$ctg2_mall_rental-min(data$ctg2_mall_rental))/(max(data$ctg2_mall_rental)-min(data$ctg2_mall_rental))
ctg3_mall_rental = (data$ctg3_mall_rental-min(data$ctg3_mall_rental))/(max(data$ctg3_mall_rental)-min(data$ctg3_mall_rental))
brand_score = (data$brand_score-min(data$brand_score))/(max(data$brand_score)-min(data$brand_score))  
adj_rental = (data$adj_rental-min(data$adj_rental))/(max(data$adj_rental)-min(data$adj_rental))

data.std.w = cbind(ctg1_mall_rental,
                   ctg2_mall_rental,
                   ctg3_mall_rental,
                   booth_score,
                   rentable_area)
dim(data.std.w)

##主成分
library(psych)
library(GPArotation)
m1 = data.std.w
options(digits=3) 
m1.cov = cov(m1)
covariances = m1.cov 
correlations = cov2cor(covariances)  
correlations
fa.parallel(correlations,n.obs=112,fa="both",n.iter=100,main="Scree plots with parallel analysis") 
pc = principal(correlations, nfactors = 2, score = TRUE)
rc = principal(covariances, nfactors = 2, rotate="varimax")  
rc
round(unclass(rc$weights),2) 

lambda = pc$values
total_lambda = sum(lambda)
prop.var1 = lambda[1]/total_lambda
prop.var2 = lambda[2]/total_lambda
comp.1 = pc$loadings[,1]
comp.2 = pc$loadings[,2]
coef.1 = comp.1/sqrt(lambda[1])
coef.2 = comp.2/sqrt(lambda[2])
score = (prop.var1*coef.1+prop.var2*coef.2)/(prop.var1+prop.var2)
total_score = sum(score)
n = dim(data.std.w)[2]
weight=0
for (j in 1:n) {
  weight[j] = score[j]/total_score
}
weight

###权重模型
n = dim(data.std.w)[2]
score = 0
for (i in 1:n) {
  score = score+weight[i]*data.std.w[,i]
}
head(score)
area.score = score*data$rentable_area
booth.score = cbind(data,score,area.score)
mall.score = aggregate(booth.score$area.score,by=list(mall_name=booth.score$mall_name),sum)
score = merge(booth.score,mall.score,by="mall_name")
head(score)
total.score = score$x
booth.month.rental = score$mall_rental*score$area.score/total.score
booth.avg.rental = booth.month.rental/score$rentable_area
err = booth.avg.rental-score$adj_rental
mse = mean(err^2)

booth.pred = cbind(score,booth.month.rental,booth.avg.rental,err,err/score$avg_rental) 
head(booth.pred)
write.csv(booth.pred,file="D:/R/booth_rental_shanghai_pred1.csv")
