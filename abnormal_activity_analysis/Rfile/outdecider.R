date = Sys.Date()-1
date.str = format(date,'%Y%m%d')
spec.date.str = "20171112"
pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc")

calf1 = function(real,pred){
accuracy = sum(real==pred)/length(real)
#precise定义为当你认为是真时，实际是真的比例
precise = sum(real==1&pred==1)/sum(pred==1)
#spec定义为实际是假时，你也认为是假的比例
spec = sum(real==0&pred==0)/sum(real==0)
#当实际是真时，你也认为是真的比例recall/sensitivity
recall=sens = sum(real==1&pred==1)/sum(real==1)
f1 = 2*precise*recall/(precise+recall)
return(list(f1=f1,accuracy = accuracy,precise = precise,recall = recall))
  }

calAUC = function(real,pred){
  library(AUC)
  test_roc = roc(pred,real)
  test_auc = auc(test_roc)
  return(test_auc)
}
source('~/Rfile/R_impala.R')

city_sql = paste0("select x.*,x.city_count_new/x.city_count_all city_perc from
(select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = '",spec.date.str,"') m group by m.l_city,m.dt) x")

model_sql = paste0("select x.*,x.md_count_new/x.md_count_all md_perc from
(select m.d_model,m.dt,count(*) md_count_all,count(if(m.isnew = 'new','new',NULL)) md_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = '",spec.date.str,"') m group by m.d_model,m.dt) x")

umid_sql = "select u_mid from test.train_sample"

umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in ('漯河','漯河市','luohe') and dt = '20171203' and path = 'z'"

spec.date.str = "20171203"

base_sql = paste0("select base.*,umid_count_same_ip from 
(select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
  from dm.dm_app_pageview_info where u_mid in (",umid_sql,") and dt <= '",spec.date.str,"' and path = 'z'
  group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info where u_mid in 
                                       (",umid_sql,") 
                                       and dt <= '",spec.date.str,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")

check_sql = "select * from test.train_sample"



base = read_data_impala_general(base_sql)
model = read_data_impala_general(model_sql)
city = read_data_impala_general(city_sql)
check = read_data_impala_general(check_sql)
#ip = read_data_impala_general(ip_sql)

base$sign = ifelse(base$umid_count_same_ip<20,0,1)
base_city = merge(base,city[city$dt == spec.date.str,],by.x = "l_city",by.y = "l_city",all.x = TRUE)
base_city_model = merge(base_city,model[model$dt == spec.date.str,],by.x = "d_model",by.y = "d_model",all.x = TRUE)

train_set = base_city_model[,pickedColums]
x = train_set[,!(names(train_set) %in% "sign")]
train_set$sign = as.factor(train_set$sign)
y = train_set$sign
one_sample_train_set = train_set[train_set$sign == "1",]
# library(mice)
# md.pattern(x)
# View(x[is.na(x)])

#This can't handle the missing value part
library(caret)
library(klaR)
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
result = predict(model$finalModel,x)

library(naivebayes)
model_nb = naive_bayes(sign~.,train_set)
result_nb = predict(model_nb,x)
nb_list = calf1(y,result_nb)
nb_f1 = nb_list$f1
nb_AUC = calAUC(y,result_nb)

library(randomForest)
model_random_forest = randomForest(sign~.,train_set[complete.cases(train_set),])
result_random_forest = predict(model_random_forest,train_set[complete.cases(train_set),!(names(train_set) %in% "sign")])
rf_list = calf1(train_set[complete.cases(train_set),]$sign,result_random_forest)
rf_AUC = calAUC(train_set[complete.cases(train_set),]$sign,result_random_forest)

library(e1071)
model_svm = svm(sign~.,train_set)
result_svm = predict(model_svm,x)
svm_list = calf1(train_set[complete.cases(train_set),]$sign,result_svm)
svm_AUC = calAUC(train_set[complete.cases(train_set),]$sign,result_svm)
summary(model_svm)

# library(EMCluster)
library(mclust)
BIC <- mclustBIC(train_set[complete.cases(train_set),!(names(train_set)%in%c("sign"))])
plot(BIC)
summary(BIC)

ICL = mclustICL(train_set[complete.cases(train_set),!(names(train_set)%in%c("sign"))])
plot(ICL)
summary(ICL)
#test EM
# msEst <- mstep(modelName = "EEE", data = iris[,-5], 
#                z = unmap(iris[,5]))
# names(msEst)
# 
# result_em_iris = em(modelName = msEst$modelName, data = iris[,-5],
#    parameters = msEst$parameters)

# specificity(data = result,reference = ori_result)
# sensitivity(data = result,reference = ori_result)
#EEV
#VII
#The best for our company's data is VEV AUC:0.999093
train_base = train_set[complete.cases(train_set),]
set.seed(1000)
train_index = sample(nrow(train_base),2/3*nrow(train_base))
train_sample = train_base[train_index,]
test_sample = train_base[-train_index,]

mEst = mstep(modelName = "VEV", data = train_sample[,!(names(train_set)%in%c("sign","city_perc"))],z = unmap(train_sample[,"sign"]))
result_em_user = em(modelName = mEst$modelName,data = test_sample[,!(names(train_set)%in%c("sign","city_perc"))],
                    parameters = mEst$parameters)
View(result_em_user$z)
em_result = ifelse(result_em_user$z[,1]-result_em_user$z[,2]>0,0,1)
em_list = calf1(test_sample[,"sign"],em_result)
em_AUC = calAUC(test_sample[,"sign"],em_result)

test = read_data_impala_general(base_sql)
test$sign = 1
test_city = merge(test,city[city$dt == spec.date.str,],by.x = "l_city",by.y = "l_city",all.x = TRUE)
test_city_model = merge(test_city,model[model$dt == spec.date.str,],by.x = "d_model",by.y = "d_model",all.x = TRUE)

test_set = test_city_model[,pickedColums]
x = test_set[,!(names(test_set) %in% "sign")]
test_set$sign = factor(test_set$sign,levels = c('0','1'))
y = as.character(test_set$sign)
test_em_user = em(modelName = mEst$modelName,data = test_set[,!(names(test_set)%in%c("sign","city_perc","md_perc"))],
                    parameters = mEst$parameters)
em_test_result = ifelse(test_em_user$z[,1]-test_em_user$z[,2]>0,0,1)
em_test_list = calf1(y,em_test_result)
em_test_AUC = calAUC(y,em_test_result)

result_nb = predict(model_nb,x)
result_nb = as.character(result_nb)
nb_list = calf1(y,result_nb)
nb_f1 = nb_list$f1
nb_AUC = calAUC(y,result_nb)
#draw a picture through cluster
X = train_set[complete.cases(train_set),!(names(train_set)%in%c("sign","city_perc"))]
BIC <- mclustBIC(X,G=2) #We only need to consider the cases for 2 clusters
plot(BIC)
summary(BIC)
mod2 <- Mclust(X, x = BIC)
# summary(mod2, parameters = TRUE)
plot(mod2, what = "classification")
#che
class = train_set[complete.cases(train_set),"sign"]
clPairs(X, class)

#VVV,EVV,VVE,VVI not correct

mEst = mstep(modelName = "VEV", data = train_set[,!(names(train_set)%in%c("sign","city_perc"))],z = unmap(train_set$sign))
result_em_user = em(modelName = mEst$modelName,data = train_set[,!(names(train_set)%in%c("sign","city_perc"))],
                    parameters = mEst$parameters)
View(result_em_user$z)

mEst = mstep(modelName = "VEI", data = train_set[,!(names(train_set)%in%c("sign","city_perc"))],z = unmap(train_set$sign))
result_em_user = em(modelName = mEst$modelName,data = train_set[,!(names(train_set)%in%c("sign","city_perc"))],
                    parameters = mEst$parameters)
View(result_em_user$z)

# ip_sql = paste0("select x.*,x.ip_count_new/x.ip_count_all ip_perc from
# (select m.l_ip,m.dt,count(*) ip_count_all,count(if(m.isnew = 'new','new',NULL)) ip_count_new from 
#                 (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
#                 where f.dt = '",spec.date.str,"') m group by m.l_ip,m.dt) x")


# base_sql_qingyuan = paste0("select base.*,umid_count_same_ip from 
# (select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
#                            from dm.dm_app_pageview_info where l_city in ('清远','清远市','qingyuan','qingyuanshi') and dt = '",spec.date.str,"' and path = 'z'
#                            group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info where l_city in 
#                            ('清远','清远市','qingyuan','qingyuanshi') 
#                            and dt = '",spec.date.str,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")

#using one sign classification from SVM
library(e1071)
model_svm_one_sample = svm(sign~.,one_sample_train_set,type = "one-classification")
result_svm_one_sample = predict(model_svm_one_sample,x)
#test if this can work for multiclass
model_svm2 = svm(sign~.,train_set,type = "one-classification")
result_svm2 = predict(model_svm2,train_set)

getSrcDirectory(function(x) {x})
temp = dirname(rstudioapi::getActiveDocumentContext()$path)
rstudioapi::getActiveDocumentContext()$path


