#import data
# 几大插值准则：
# 1去除缺失太多且上下左右均不靠的数据（无法有效估计）
# 2使用频率比较高的值或者平均值，中间值进行插值
# 3使用相关变量进行估计
# 4使用临近原则进行插值
# 5其它办法

data ("BostonHousing", package="mlbench")
original <- BostonHousing  # backup original data

set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] = NA

library(mice)
#check missing value pattern
md.pattern(BostonHousing)  # pattern or missing values in data.

lm(medv ~ ptratio + rad, data=BostonHousing, na.action=na.omit)

#简单插值
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # replace with mean
impute(BostonHousing$ptratio, median)  # median
impute(BostonHousing$ptratio, 20)  # replace with specific number
# or if you want to impute manually
BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, na.rm = T)  # not run

#比较插值后的误差
library(DMwR)
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- rep(mean(BostonHousing$ptratio, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)

#高阶插值
knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput)
#如果值只允许整数,稍后需要进行近似

#Knn插值效果
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals, predicteds)

#使用决策树进行插值
library(rpart)
class_mod <- rpart(rad ~ . - medv, data=BostonHousing[!is.na(BostonHousing$rad), ], method="class", na.action=na.omit)  # since rad is a factor
anova_mod <- rpart(ptratio ~ . - medv, data=BostonHousing[!is.na(BostonHousing$ptratio), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad),!(colnames(BostonHousing)%in%c("rad"))])
ptratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

library(DMwR)
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- ptratio_pred
regr.eval(actuals, predicteds)

actuals <- original$rad[is.na(BostonHousing$rad)]
predicteds <- as.numeric(colnames(rad_pred)[apply(rad_pred, 1, which.max)])
mean(actuals != predicteds)  # compute misclass error.

#使用随机森林
library(mice)
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

#线性插值和样条插值
library(zoo)
a = 1:7
a[4] = NA
y1 = na.approx(a)
y2 = na.spline(a)
