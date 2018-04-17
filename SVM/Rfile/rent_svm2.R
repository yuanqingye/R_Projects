library(readxl)
library(lattice)
library(e1071)
library(data.table)

business_district_info = read_xlsx("~/data/final_result_2017.xlsx")
shop_info_2015 = read_xlsx("~/data/rent_recent_3years.xlsx",sheet = '2015')
shop_info_2016 = read_xlsx("~/data/rent_recent_3years.xlsx",sheet = '2016')
shop_info_2017 = read_xlsx("~/data/rent_recent_3years.xlsx",sheet = '2017')

shop_info_2017_1 = shop_info_2017[,1:15]
traffic_info = shop_info_2017[,c(2,22:25)] 
  

shop_info_2015 = data.frame(shop_info_2015,year = 2015)
shop_info_2016 = data.frame(shop_info_2016,year = 2016)
shop_info_2017_1 = data.frame(shop_info_2017_1,year = 2017)

shop_info = rbindlist(list(shop_info_2015,shop_info_2016,shop_info_2017_1),use.names = TRUE)
general_info = merge(shop_info,traffic_info,by = "商场名称")
general_info = merge(general_info,business_district_info,by.x = "商场名称",by.y = "name")
general_info_copy = general_info
names(general_info) = c("shop_name","shop_code","age","city","city_level","area","rent","num_ordered_custom",
                        "sales","furniture","construction","soft","import","new_business","brand_num","year",
                        "dist_from_highway","dist_from_big_road","position","dist_from_subway","dist_from_business_district",
                        "num_business_within_2500","a","b","c","d")
general_info = general_info[,1:22]
general_info_copy2 = general_info
general_info = general_info_copy2

rm_index = c(-10,-16,-49,-50,-51,-58)
general_info = general_info[rm_index,]

general_info$city_level = as.numeric(as.factor(general_info$city_level))
general_info_copy3 = general_info
general_info$rent = as.numeric(general_info$rent)
general_info$num_ordered_custom = as.numeric(general_info$num_ordered_custom)
general_info$sales = as.numeric(general_info$sales)

new_general_info = read_xlsx('~/data/mall_data_0802.xlsx',sheet = 1)

names(new_general_info) = c("mall_name","age","area","rent","ordered_custom_num","sale","furniture","construction","soft","import",
                            "new_business","brand_num","gdp","population","region_area","density","avg_gdp","avg_sale","dist_from_highway",
                            "dist_from_big_road","position","dist_from_subway","dist_from_business_district","num_business_within_2500")

new_general_info = new_general_info[,-11]
new_general_info$year = 2017
new_general_info[20:38,"year"] = 2016
new_general_info[39:nrow(new_general_info),"year"] = 2015
cities = substring(new_general_info$mall_name,1,2)
cities_code = as.numeric(factor(cities))
new_general_info$city = cities_code
new_trainset = new_general_info[new_general_info$year!=2017,c(-1,-24)]

mall_fac = as.numeric(as.factor(new_general_info$mall_name))
new_trainset_with_name = new_general_info
new_trainset_with_name$mall_fac = mall_fac
new_trainset_with_name = new_trainset_with_name[new_general_info$year!=2017,]

new_trainset$mall_fac = mall_fac
new_testset = new_general_info[new_general_info$year==2017,c(-1,-24)]

#want to test if along with m increasing, the model get much better
set.seed(1)
half_index = sample(nrow(new_trainset),ceiling(nrow(new_trainset)/2))
lllll = svm_para(new_trainset[half_index,],new_testset)

View(svm.model.new$SV)

trainset = general_info[,c(-1,-2,-4,-16)]
svm.model.1 = svm(rent~.,data = trainset)  #1.41e+12
svm.model.1000 <- svm(rent ~ ., data = trainset, cost = 1000, gamma = 1)

trainset_var_2015_2016 = general_info[year!=2017,c("rent","age","area","city_level","dist_from_big_road","num_ordered_custom",
                               "position","sales")]
# Variables actually used in tree construction:
# age  area  city_level  dist_from_big_road  num_ordered_custom  position  sales     

svm.model = svm(rent~.,data = trainset,gamma = 0.5)
svm.model.10.2 = svm(rent ~ ., data = trainset, cost = 10, gamma = 2)
svm.model.new = svm(rent~.,data = new_trainset_with_name,cost = 10,gamma = 0.0078125,cross = 5)
# svm.model.100 <- svm(rent ~ ., data = trainset, cost = 100, gamma = 1)
# svm.model.1000 <- svm(rent ~ ., data = trainset, cost = 1000, gamma = 1)

testset = general_info[,c(-1,-2,-4,-16)]
svm.pred.1 = predict(svm.model.1,testset[,-4])
svm.pred.1000 = predict(svm.model.1000,testset[,-4])
svm.pred = predict(svm.model,testset[,-4])
svm.pred.10.2 = predict(svm.model.10.2,testset[,-4])

general_pred_info = data.table(general_info[,-7],rent=svm.pred.1,type='predict')
general_real_info = data.table(general_info,type = "real")

library(caret)
init.model = train(rent~.,data = new_trainset,method = 'rf',preProcess = "scale",importance=T)
importance <- varImp(init.model)
print(importance)
svm.model.new = svm(rent~.,data = new_trainset,cost = 10,gamma = 0.0078125,cross = 5)
svm.model.bigC = svm(rent~.,data = new_trainset,cost = 1000,gamma = 0.0078125,cross = 5)
svm.model.best = best.svm(x=new_trainset[,-3],y =new_trainset[,3])
svm.model.new.half = svm(rent~.,data = new_trainset[half_index,],cost = 10,gamma = 0.0078125,cross = 5)
svm.model.percent = svm(rent~.,data = new_trainset,cost = 1,gamma = 0.015625)
svm.model.city = svm(rent~.,data = new_trainset,cost = 10,gamma = 0.0078125,cross = 5)

svm.test = predict(svm.model.new,new_testset[,-3])
svm.train = predict(svm.model.new,new_trainset[,-3])
svm.test.best = predict(svm.model.best,new_testset[,-3])
svm.train.best = predict(svm.model.best,new_trainset[,-3])
svm.test.half = predict(svm.model.new.half,new_testset[,-3])
svm.train.half = predict(svm.model.new.half,new_trainset[half_index,-3])
svm.percent = predict(svm.model.percent,new_testset[,-3])
svm.train.bigC = predict(svm.model.bigC,new_trainset[,-3])
svm.test.bigC = predict(svm.model.bigC,new_testset[,-3])
svm.test.city = predict(svm.model.city,new_testset[,-3])

e.test = crossprod(svm.test - unlist(new_testset[,3]))/nrow(new_testset)
e.test.bigC = crossprod(svm.test.bigC - unlist(new_testset[,3]))/nrow(new_testset)
e.train = crossprod(svm.train - unlist(new_trainset[,3]))/nrow(new_trainset)
e.train.bigC = crossprod(svm.train.bigC - unlist(new_trainset[,3]))/nrow(new_trainset)
e.test.best = crossprod(svm.test.best - unlist(new_testset[,3]))/nrow(new_testset)
e.train.best = crossprod(svm.train.best - unlist(new_trainset[,3]))/nrow(new_trainset)
e.test.half = crossprod(svm.test.half - unlist(new_testset[,3]))/nrow(new_testset)
e.train.half = crossprod(svm.train.half - unlist(new_trainset[half_index,3]))/length(half_index)
e.percent = crossprod(svm.percent - unlist(new_testset[,3]))/nrow(new_testset)
e.test.city = crossprod(svm.test.city - unlist(new_testset[,3]))/nrow(new_testset)

result_compare.test = cbind(svm.test,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.test = data.table(result_compare.test)
result_compare.test[,c("diff","percent"):= list(svm.test-rent,(svm.test-rent)/rent)]

result_compare.percent = cbind(svm.percent,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.percent = data.table(result_compare.percent)
result_compare.percent[,c("diff","percent"):= list(svm.percent-rent,(svm.percent-rent)/rent)]

result_compare.city = cbind(svm.test.city,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.city = data.table(result_compare.city)
result_compare.city[,c("diff","percent"):= list(svm.test.city-rent,(svm.test.city-rent)/rent)]
result_compare.city[,c("vanilla_diff","vanilla_percent"):=list(result_compare.test$diff,result_compare.test$percent)]

result_compare = cbind(svm.pred.1,svm.pred.1000,general_info[,c("rent","shop_name","year")])
# e.1 = crossprod(result_compare[year == 2017,svm.pred.1] - unlist(result_compare[year == 2017,rent])) / nrow(result_compare[year==2017,])
e.1000 = crossprod(svm.pred.1000 - unlist(testset[,4])) / nrow(testset)
e = crossprod(svm.pred - unlist(testset[,4]))/nrow(testset)
e.1 = crossprod(svm.pred.1 - unlist(testset[,4]))/nrow(testset)
e.10.2 = crossprod(svm.pred.10.2 - unlist(testset[,4]))/nrow(testset)

plot3dset = rbind(trainset,testset)

realvspred = rbind(general_pred_info,general_real_info)

library(plotly)
plot_ly(realvspred[year==2017], x = ~dist_from_business_district, y = ~position, z = ~rent, color = ~type, colors = c('red', 'blue')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'dist_from_business_district'),
                      yaxis = list(title = 'position'),
                      zaxis = list(title = 'rent')))

plot_ly(general_info, x = ~city_level, y = ~num_ordered_custom, z = ~rent, color = ~year, colors = c('red', 'blue','purple')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'city_level'),
                      yaxis = list(title = 'num_ordered_custom'),
                      zaxis = list(title = 'rent')))

plot_ly(general_info, x = ~dist_from_highway, y = ~dist_from_big_road, z = ~rent, color = ~year, colors = c('red', 'blue','purple')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'dist_from_highway'),
                      yaxis = list(title = 'dist_from_big_road'),
                      zaxis = list(title = 'rent')))

plot_ly(general_info, x = ~num_business_within_2500, y = ~dist_from_business_district, z = ~rent, color = ~year, colors = c('red', 'blue','purple')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'num_business_within_2500'),
                      yaxis = list(title = 'dist_from_business_district'),
                      zaxis = list(title = 'rent')))

plot_ly(plot3dset, x = ~area, y = ~avg_gdp, z = ~rent, color = ~year, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'area'),
                      yaxis = list(title = 'people income'),
                      zaxis = list(title = 'rent')))

# 如图论推理算法（Graph Inference）或者拉普拉斯支持向量机（Laplacian SVM.）等。 

#检测出异常值，并把其脱离开模型

#决策树并没有产生更好的模型
library(rpart)
# rp = rpart(rent~.,data = new_trainset)
# rp = best.rpart(rent~.,data = new_trainset)
rp = rpart(rent~.,data = new_trainset,control = rpart.control(cp = 0.01,minsplit = 6, minbucket = round(6/3)),
                parms = list(prior = c(.65,.35), split = "gini"),method = "anova")
rp.pred = predict(rp,new_testset[,-3])
e.rp = crossprod(rp.pred - unlist(new_testset[,3]))/nrow(new_testset)
result_compare.rp = cbind(rp.pred,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.rp = data.table(result_compare.rp)
result_compare.rp[,c("diff","percent"):= list(rp.pred-rent,(rp.pred-rent)/rent)]
library(rpart.plot)
prp(rp)

#尝试随机森林,效果好于决策树，但不如支持向量机
library(randomForest)
rf = randomForest(rent~.,data = new_trainset)
rf.pred = predict(rf,new_testset[,-3])
e.rf = crossprod(rf.pred - unlist(new_testset[,3]))/nrow(new_testset)
result_compare.rf = cbind(rf.pred,new_general_info[new_general_info$year == 2017,c("rent","mall_name","year")])
result_compare.rf = data.table(result_compare.rf)
result_compare.rf[,c("diff","percent"):= list(rf.pred-rent,(rf.pred-rent)/rent)]
plot(rf)

#               maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#               surrogatestyle = 0, maxdepth = 30, ...)