source('~/Rfile/R_impala.R')
lastdate = Sys.Date()-1
last.date.str = format(lastdate,'%Y%m%d')
spec.date.str = "20171112" #using 20171203 before
spec.date.dash.str = "2017-11-12"
city_name = ""

#dataset to be tested with umid,ip,city,model,stay time,pv count,# of umid per ip for z point and 
#time before spec date,for the same umid,model can't be diff but ip and city can  
base_sql = paste0("select base.*,umid_count_same_ip from 
                  (select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
                  from dm.dm_app_pageview_info p where p.u_mid in (",umid_sql,") and dt <= '",spec.date.str,"' and path = 'z'
                  group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
                  (",umid_sql,") and dt <= '",spec.date.str,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")

#get umid which introduce the order, which is unlikely, to be abnormal umid
order_umid_sql = paste0("select distinct u_mid from ods.ods_tx_order_tx_order_dt o 
inner join ods.ods_app_pageview_info a on 
                        o.purchaser_id = a.u_id where substr(o.create_date,1,10)= '",spec.date.dash.str,"' 
                        and o.order_status not in (1,7,19) and o.order_type=1 and a.l_city like '",city_name,"%'")

#get umid which revisit our app during a long time
last_long_umid_sql = "select distinct u_mid from 
(select u_mid,(max(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss'))-min(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss')))/(3600*24) time_inv from dm.dm_app_pageview_info group by u_mid) t where time_inv>60"

#This sql search for target umid, the sample rely on which we build the model
umid_sql = "select u_mid from test.train_sample"

umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('泉州','泉州市','quanzhou') and dt = '20171112' and path = 'z'"

qz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('泉州','泉州市','quanzhou') and dt = '20171112' and path = 'z'"

wz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('温州','温州市','wenzhou') and dt = '20171203' and path = 'z'"

umid_sql = paste0("select distinct u_mid from (",qz_umid_sql," union ",wz_umid_sql,") df")
neg_umid_sql = paste0("select distinct u_mid from (",order_umid_sql," union ",last_long_umid_sql,") df")

check_sql = "select u_mid,flag from test.train_sample"
check_umid_sql = "select u_mid from test.train_sample"

train_sample_big = get_advbase_set(umid_sql,neg_umid_sql,complexity = TRUE)
# check = read_data_impala_general(check_sql)
model = run_flexible_sql(MODEL_SQL)
city = run_flexible_sql(CITY_SQL)
page_city_dt = run_flexible_sql(PAGE_CITY_DT_FIRST_SQL)
page_umid = run_flexible_sql(PAGE_UMID_FIRST_SQL)
pv_umid_unit_min = run_flexible_sql(PV_UMID_UNIT_MIN_SQL)

#base umid with city attributes on a given day
base_city = merge(train_sample_big,city[city$dt == spec.date.str,],by.x = "l_city",by.y = "l_city",all.x = TRUE)
#base umid with city and model attributes on a given day
base_city_model = merge(base_city,model[model$dt == spec.date.str,],by.x = "d_model",by.y = "d_model",all.x = TRUE)
base_city_model = data.table(base_city_model)
base_city_model_summary = base_city_model[,c(lapply(.SD[,sum_col,with=FALSE],sum),lapply(.SD[,avg_col,with=FALSE],mean),lapply(.SD[,min_col,with = FALSE],min),lapply(.SD[,max_col,with = FALSE],max)),by = "u_mid"]
base_city_model_summary = as.data.frame(base_city_model_summary)
base_city_model_move = merge(base_city_model_summary,umid_move,by.x = "u_mid",by.y = "u_mid",all.x = TRUE,all.y = FALSE)

move = get_base_with_restrict(umid_sql,mode = "umidmove")
sample_train_base = merge(base_city_model_summary,move,by.x = "u_mid",by.y = "u_mid")
sample_sign_SQL = "select u_mid,flag as sign from test.train_sample"
sample_sign_df = read_data_impala_general(sample_sign_SQL)
sample_train = merge(sample_train_base,sample_sign_df,by.x = "u_mid",by.y = "u_mid")
sample_train = preprocess_train_data(sample_train)
model_nb = naive_bayes(sign~.,sample_train[complete.cases(sample_train),])

train_set = base_city_model_move[,pickedColums]
train_base = train_set[complete.cases(train_set),]
train_base = as.data.frame(train_base)
x = train_base[,!(names(train_base) %in% "sign")]
train_base$sign = as.factor(train_base$sign)
y = train_base$sign

#this can handle the miss value case
library(naivebayes)
model_nb = naive_bayes(sign~.,train_set[complete.cases(train_set),])
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
#VEV
#"VII","VEV"
#VOLUME SHAPE ORIENTATION
mEst = mstep(modelName = "EEI", data = train_set[,!(names(train_set)%in%c("sign"))],z = unmap(train_set[,"sign"]))
result_em_user = em(modelName = mEst$modelName,data = test_set[,!(names(test_set)%in%c("sign"))],
                    parameters = mEst$parameters)
em_result = ifelse(result_em_user$z[,1]-result_em_user$z[,2]>0,0,1)
em_list = calf1(test_set[,"sign"],em_result)
em_AUC = calAUC(test_set[,"sign"],em_result)

test = read_data_impala_general(check_sql)
colnames(test) = c("u_mid","sign")
test_base = get_advbase_set(check_umid_sql)
test_base_set = merge(test,test_base,by.x = "u_mid",by.y="u_mid")
test_set = test_base_set[,pickedColums]
test_set = test_set[complete.cases(test_set),]
x = test_set[,!(names(test_set) %in% "sign")]
test_set$sign = factor(test_set$sign,levels = c('0','1'))
y = as.character(test_set$sign)

test_em_user = em(modelName = "VII",data = test_set[,!(names(test_set)%in%c("sign"))],
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
X = train_set[complete.cases(train_set),!(names(train_set)%in%c("sign"))]
BIC_2 <- mclustBIC(X,G=2) #We only need to consider the cases for 2 clusters
plot(BIC_2)
summary(BIC_2)
mod2 <- Mclust(X, x = BIC_2)
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
