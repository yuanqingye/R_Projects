source('~/Rfile/R_impala.R')
library(caret)
library(ggplot2)
luohe_set = unique(base[base$l_city %in% c("漯河","漯河市","luohe","luoheshi"),]$l_ip)
View(base[!(base$l_ip %in% luohe_set),])
source('~/R_Projects/abnormal_activity_analysis/Rfile/test_func.R', encoding = 'UTF-8')
#
city = ''
date.str = '20171105'
date.dash = '2017-11-05'

page_perc_limit = paste0("select count(*) p_count,p_type from dm.dm_app_pageview_info where dt = '",
                         date.str,"' and l_city like '",city,"%' and path = 'z' group by p_type")
page_perc_limit2 = paste0("select count(*) all_count from dm.dm_app_pageview_info where dt = '",
                         date.str,"' and l_city like '",city,"%' and path = 'z'")
page_perc_sql = paste0("select t.p_count,ac.all_count,t.p_count/ac.all_count rate from
(",page_perc_limit,") t cross join (",page_perc_limit2,") ac order by t.p_count desc")

sytemp = read_data_impala_general(page_perc_sql)

#??
#'10.11.29.197','10.11.29.146','10.11.30.9','10.11.29.155'

page_perc_limit = "select count(*) p_count,p_type from dm.dm_app_pageview_info where l_ip in 
('10.11.29.197','10.11.29.146','10.11.30.9','10.11.29.155') and path = 'z' and dt > '20171220'"
page_perc_limit2 = "select count(*) all_count from dm.dm_app_pageview_info where l_ip in 
('10.11.29.197','10.11.29.146','10.11.30.9','10.11.29.155') and path = 'z' and dt > '20171220'"
page_perc_sql = paste0("select t.p_count,ac.all_count,t.p_count/ac.all_count rate from
(",page_perc_limit,"group by p_type) t cross join (",page_perc_limit2,") ac order by t.p_count desc")

ztemp = read_data_impala_general(page_perc_sql)

#orginal sql
page_perc_sql = "select t.p_count,ac.all_count,t.p_count/ac.all_count rate from
(select count(*) p_count,p_type from dm.dm_app_pageview_info where dt = '20171106' and l_city like '娌堥槼%' 
and path = 'z' group by p_type) t cross join (select count(*) all_count from dm.dm_app_pageview_info where dt = '20171106' and l_city like '娌堥槼%' 
and path = 'z') ac order by t.p_count desc"

city ='閲嶅簡'
spec.date.str = '20171209'
pv_umid_unit_min_sql = paste0("select u_mid,pcount,stay_time,pcount/stay_time pv from (
select u_mid,case when count(*)>1 then (max(cast(ts as bigint))-min(cast(ts as bigint)))/1000/60 
                else cast(max(p_stay_time) as integer)/1000/60 end stay_time,
                count(*) pcount from dm.dm_app_pageview_info where l_city like '",city,"%' and dt = '",spec.date.str,"' 
                and path = 'z' group by u_mid,session) t")
pv_umid_unit_min = read_data_impala_general(pv_umid_unit_min_sql)

city = ''
spec.date.str = '2017-12-20'
order_umid_sql = paste0("select distinct u_mid from ods.ods_tx_order_tx_order_dt o 
inner join ods.ods_app_pageview_info a on 
o.purchaser_id = a.u_id where substr(o.create_date,1,10)= '",spec.date.str,"' 
and o.order_status not in (1,7,19) and o.order_type=1 and a.l_city like '",city,"%'")

last_long_umid_sql = "select distinct u_mid from 
(select u_mid,(max(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss'))-min(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss')))/(3600*24) time_inv from dm.dm_app_pageview_info group by u_mid) t where time_inv>60"

order_umid = read_data_impala_general(order_umid_sql)

library(mclust)
BIC <- mclustBIC(test_set[complete.cases(test_set),!(names(test_set)%in%c("sign"))])
plot(BIC)
summary(BIC)

temp_mEst = mstep(modelName = "VII", data = test_set[,!(names(test_set)%in%c("sign"))],z = unmap(test_set[,"sign"]))
test_em_user = em(modelName = temp_mEst$modelName,data = test_set[,!(names(test_set)%in%c("sign"))],
                  parameters = temp_mEst$parameters)
em_test_result = ifelse(test_em_user$z[,1]-test_em_user$z[,2]>0,0,1)
em_test_list = calf1(y,em_test_result)
em_test_AUC = calAUC(y,em_test_result)

set.seed(1000)
rand_z = runif(nrow(train_sample),0,1)
rand_z = cbind(rand_z,(1-rand_z))
rand_z = ifelse(rand_z>0.5,1,0)
#"VII","VEV"
#VOLUME SHAPE ORIENTATION
mEst = mstep(modelName = "EEI", data = train_set[,!(names(train_set)%in%c("sign"))],z = unmap(train_set[,"sign"]))
result_em_user = em(modelName = mEst$modelName,data = test_set[,!(names(test_set)%in%c("sign"))],
                    parameters = mEst$parameters)
em_result = ifelse(result_em_user$z[,1]-result_em_user$z[,2]>0,0,1)
em_list = calf1(test_set[,"sign"],em_result)
em_AUC = calAUC(test_set[,"sign"],em_result)

#part has the same result as whole above
#but with test the recall rate is down which is wanted since spec and precise both up
mtEst = mstep(modelName = "VEV", data = train_sample[,!(names(train_sample)%in%c("sign"))],z = unmap(train_sample[,"sign"]))
result_emt_user = em(modelName = mtEst$modelName,data = test_sample[,!(names(train_set)%in%c("sign"))],
                     parameters = mtEst$parameters)
emt_result = ifelse(result_emt_user$z[,1]-result_emt_user$z[,2]>0,0,1)
emt_list = calf1(test_sample[,"sign"],emt_result)

me_base = get_advbase_set(my_sql,'20171230')
me_base$sign = factor('0',levels = c('0','1'))
me_set = me_base[,pickedColums]
me_set = rbind(me_set,me_set)
result_em_user = em(modelName = mEst$modelName,data = me_set[,!(names(me_set)%in%c("sign"))],
                    parameters = mEst$parameters)
em_result = ifelse(result_em_user$z[,1]-result_em_user$z[,2]>0,0,1)
em_list = calf1(me_set[,"sign"],em_result)
em_AUC = calAUC(me_set[,"sign"],em_result)

BIC_TRAIN = mclustBIC(train_base[complete.cases(train_base),!(names(train_base)%in%c("sign"))])
BIC_TEST = mclustBIC(test_set[complete.cases(test_set),!(names(test_set)%in%c("sign"))])
BIC_M <- mclustBIC(train_set[complete.cases(train_set),!(names(train_set)%in%c("sign"))])

result_nb = predict(model_nb,me_set[complete.cases(me_set),!(names(me_set) %in% "sign")])
nb_list = calf1(me_set[,"sign"],result_nb)
nb_f1 = nb_list$f1

result_random_forest = predict(model_random_forest,test_set[complete.cases(test_set),!(names(test_set) %in% "sign")])
rf_list = calf1(test_set[complete.cases(test_set),]$sign,result_random_forest)
rf_AUC = calAUC(test_set[complete.cases(test_set),]$sign,result_random_forest)

result_svm = predict(model_svm,x)
svm_list = calf1(test_set[complete.cases(test_set),]$sign,result_svm)
svm_AUC = calAUC(test_set[complete.cases(test_set),]$sign,result_svm)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
nbmodel <- train(sign~., data=train_base, method="naive_bayes", preProcess="scale", trControl=control,
               importance = T)
# estimate variable importance
importance <- varImp(nbmodel, scale=FALSE)
# summarize importance
print(importance)



my_sql = "select u_mid from dm.dm_app_pageview_info where u_mid like '9F96D1CD-108B-460B-B6C4-3E577AFE9271%'"

qz_sql = "select u_mid from dm.dm_app_pageview_info where dt = '20171203' and l_city like '泉州%'"
qz_base = get_advbase_set(qz_sql)
qz_base$sign = factor('1',levels = c('0','1'))
qz_set = qz_base[,pickedColums]
result_nb = predict(model_nb,qz_set[complete.cases(qz_set),!(names(qz_set) %in% "sign")])
View(cbind(qz_set[complete.cases(qz_set),!(names(qz_set) %in% "sign")],result_nb))
nb_list = calf1(qz_set[,"sign"],result_nb)
nb_f1 = nb_list$f1

#沈阳11月5日，6日；11月9日，10日，12日，13日,29,30,12-02
#沈阳1105 is normal(20%+ abnormal),1106 is normal(40%+ abnormal);the rest is abnormal
sy_result = get_test_sample_result("沈阳",20171202)
#清远11月6,9,10,12,13
#清远 all is abnormal
qy_result = get_test_sample_result("清远",20171112)
#苏州 11月26,27,29,30,12-02,03
#苏州 1126,1127,1129,1130 is normal(but the stay time is so consistent),
sz_result = get_test_sample_result("苏州",20171203)
#上海 12-01,02,03,seems all is normal
sh_result = get_test_sample_result("上海",20171203)
#12-02,03,09,10,11,12
#漯河 all is abnormal(at least 80% is positive)
lh_result = get_test_sample_result("漯河",20171212)
#温州1202 1203
#温州1202 is abnormal,1203 is abnormal
wz_result = get_test_sample_result("温州",20171203)
#南京 is normal
nj_result = get_test_sample_result("南京",20171226)
#重庆 is normal
cq_result = get_test_sample_result("重庆",20171209)

qy_1106_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '清远%' and dt = '20171106'")
qy_1109_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '清远%' and dt = '20171109'")
qy_1110_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '清远%' and dt = '20171110'")
qy_1112_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '清远%' and dt = '20171112'")
qy_1113_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '清远%' and dt = '20171113'")
qy_inter_umid = intersect(qy_1109_umid[[1]],qy_1110_umid[[1]])
qy_inter_umid = Reduce(intersect,list(qy_1106_umid[[1]],qy_1109_umid[[1]],qy_1110_umid[[1]],qy_1112_umid[[1]],qy_1113_umid[[1]]))

sh_1201_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '上海%' and dt = '20171201'")
sh_1202_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '上海%' and dt = '20171202'")
sh_1203_umid = read_data_impala_general("select distinct u_mid from dm.dm_app_pageview_info where 
                                        l_city like '上海%' and dt = '20171203'")

sh_inter_umid = Reduce(intersect,list(sh_1201_umid[[1]],sh_1202_umid[[1]],sh_1203_umid[[1]]))

#so we have some thing unusual,no intersection for 清远
test_distribution_sql = "select u_mid,avg(cast(stay_time AS bigint)) stay_time from
(select u_mid,dt,l_city,p_stay_time stay_time from dm.dm_app_pageview_info where path = 'z' and platf_lv1 = 'APP') t1 inner join
(select l_city,dt,count(*) num from dm.dm_app_pageview_info group by l_city,dt having num <20) t2
on t1.l_city = t2.l_city and t1.dt = t2.dt group by u_mid"

test_distribution = read_data_impala_general(test_distribution_sql)
test_distrib = test_distribution[test_distribution$stay_time<80000,]
ggplot(data = test_distrib,aes(x = stay_time)) + geom_histogram()

n = nrow(test_distrib)
exp_sep = c(0,500,1000,2000,4000,8000,16000,32000,80000)
lambda = 1/mean(test_distrib$stay_time)
exp_theo = diff(pexp(exp_sep,rate = lambda))*n
exp_real = table(cut(test_distrib$stay_time,breaks = exp_sep))
theo_real_diff = exp_theo - exp_real

a <- chisq.test(exp_real, p=exp_theo, rescale.p=TRUE, simulate.p.value=TRUE)

#Cramér–von Mises criterion
sample_size = 100000
exp_theo_sample = rexp(sample_size,lambda)
res <- CramerVonMisesTwoSamples(test_distrib$stay_time,exp_theo_sample)
pvalue = 1/6*exp(-res)

#Kolmogorov-Smirnov test
ks.test(exp_theo_sample,test_distrib$stay_time)

chisq.test(x=qy_result[[2]]$pv,y=qy_result[[2]]$avg_stay)

x2 = train_set[,!(names(train_set) %in% "sign")]
train_set$sign = as.factor(train_set$sign)
y2 = train_set$sign
model_nb2 = naive_bayes(sign~.,train_set[complete.cases(train_set),])

qz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('泉州','泉州市','quanzhou') and dt = '20171112' and path = 'z'"
wz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('温州','温州市','wenzhou') and dt = '20171203' and path = 'z'"
#umid_sql = paste0("select distinct u_mid from (",qz_umid_sql," union ",wz_umid_sql,") df")
#neg_umid_sql = paste0("select distinct u_mid from (",order_umid_sql," union ",last_long_umid_sql,") df")
dateset = c("20171112","20171203")
#negdateset should be the same, due to the function setting we don't need to specify the same date
umid_sql = c(qz_umid_sql,wz_umid_sql)
neg_umid_sql = c(order_umid_sql,last_long_umid_sql)
general_pn = get_advbase_set(umid_sql,neg_umid_sql,dateset,renew = TRUE,complexity = TRUE)
train_pn = preprocess_train_data(general_pn)
model_nb_pn = naive_bayes(sign~.,train_pn[complete.cases(train_pn),])

test_umid_sql = "select u_mid from dm.dm_app_pageview_info where l_city like '清远%' and dt = '20171112'"
qy_test = get_advbase_set(test_umid_sql,specDate = "20171112")
qy_test = qy_test[,pickedColums]

# write.xlsx(train_pn,"~/data/test_abnormal_uv.xlsx")
library(readxl)
train_pn = read_xlsx("~/data/test_abnormal_uv.xlsx")

source("~/R_Projects/SVM/Rfile/test_svm.R")
ptm = proc.time()
test_svm_para = svm_para_class(train_pn,qy_test,"sign",calAR,"recall")
svm_time = proc.time() - ptm
write.xlsx(test_svm_para,"./Rdata/test_svm_para.xlsx")

ptm <- proc.time()
model_svm = svm(sign~.,train_pn[complete.cases(train_pn),])
proc.time() - ptm
#沈阳11月5日，6日；11月9日，10日，12日，13日,29,30,12-02
#沈阳1105 is normal(20%+ abnormal),1106 is normal(40%+ abnormal);the rest is abnormal
#recall:0.844
sy_result = get_test_sample_result("沈阳",20171202,test_model = model_nb_pn)
#nb:0.967923
sy_result = get_test_sample_result("沈阳",20171202,test_model = model_svm)
#svm:0.355

#清远11月6,9,10,12,13
#清远 all is abnormal:recall:0.994
qy_result = get_test_sample_result("清远",20171112,test_model = model_nb_pn)
#nb:0.00043
qy_result = get_test_sample_result("清远",20171112,test_model = model_svm)
#svm:0

#苏州 11月26,27,29,30,12-02,03
#苏州 1126,1127,1129,1130 is normal(but the stay time is so consistent),recall:0.124
sz_result = get_test_sample_result("苏州",20171203,test_model = model_nb_pn)
#nb:0.00112
sz_result = get_test_sample_result("苏州",20171203,test_model = model_svm)
#svm:0.000887

#上海 12-01,02,03,seems all is normal:recall:0.1
sh_result = get_test_sample_result("上海",20171203,test_model = model_nb_pn)
#nb:0.000383
sh_result = get_test_sample_result("上海",20171203,test_model = model_svm)
#svm:0

#12-02,03,09,10,11,12
#漯河 all is abnormal(at least 80% is positive):recall:0.894
lh_result = get_test_sample_result("漯河",20171212,test_model = model_nb_pn)
#nb:0.996
lh_result = get_test_sample_result("漯河",20171212,test_model = model_svm)
#svm:0.981

#温州1202 1203
#温州1202 is abnormal,1203 is abnormal:recall:0.759
wz_result = get_test_sample_result("温州",20171203,test_model = model_nb_pn)
#nb:0.807
wz_result = get_test_sample_result("温州",20171203,test_model = model_svm)
#svm:0.759

#南京 is normal recall:0.0312
nj_result = get_test_sample_result("南京",20171226,test_model = model_nb_pn)
#nb:0.000488
nj_result = get_test_sample_result("南京",20171226,test_model = model_svm)
#svm:0

#重庆 is normal recall:0.0186
cq_result = get_test_sample_result("重庆",20171209,test_model = model_nb_pn)
#nb:0.00772
cq_result = get_test_sample_result("重庆",20171209,test_model = model_svm)
#svm:0.00585

#泉州 
qz_result = get_test_sample_result("泉州",20171112,test_model = model_nb_pn)
#nb:0.993
qz_result = get_test_sample_result("泉州",20171112,test_model = model_svm)
#svm:0.991

#result from my created sample is general the same result as the original trains

umid_sql_list = c(qz_umid_sql,wz_umid_sql)
date_list = c(20171112,20171203)
temp = get_base_with_restrict_union(umid_sql_list,date_list)

source("~/R_Projects/feature_selection/Rfile/feature_selection_fun.R")
source("~/R_projects/ensemble_method/Rfile/Go_around_model_fun.R")
model_results = go_around_model(dataset = train_pn,test_formula = "sign~.")
bwplot(model_results)
# write.xlsx(model_results,"./Rdata/model_results.xlsx")

test_formula = as.formula("sign~.")
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
preProcess=c("center", "scale")
ptm <- proc.time()
fit.lda <- train(test_formula, data=train_pn, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
lda_time = proc.time() - ptm
lda_result = predict(object = fit.lda,newdata = qy_test)
lda_result = predict(object = fit.lda,newdata = qy_test,type = "prob")
accr = ifelse(lda_result$`1`>lda_result$`0`,1,0)
lda_accr = length(accr[accr==1])/length(accr)

ptm <- proc.time()
fit.c50 <- train(test_formula, data=train_pn, method="c50", metric=metric, preProc=c("center", "scale"), trControl=control)
c50_time = proc.time() - ptm
c50_result = predict(object = fit.c50,newdata = qy_test,type = "prob")
lda_result = predict(object = fit.lda,newdata = qy_test)

ptm <- proc.time()
fit.rf <- train(test_formula, data=train_pn, method="rf", metric=metric, trControl=control)
rf_time = proc.time() - ptm

qy_test$sign = factor(qy_test$sign,level = c("0","1"))
model_results2 = go_around_model_with_test(dataset = train_pn,newset = qy_test,keycol = "sign",calf = calAR)

model_results2[[3]]
formals(calAR)

model_results3 = go_around_model_with_test_simple_version(dataset = train_pn,newset = qy_test,keycol = "sign",
modeldf = data.frame(modelname = c("lda","rpart","glm","nb")),calf = calAR,caltype = "accuracy",
metric = "Accuracy",preProc = c("center","scale"))

CV_Folds <- createMultiFolds(y, k = 10, times = 3)

tr_model = train(form = test_formula,data = dataset,method = md,metric = metric,trControl = trControl,preProc = preProc)

# svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
trControl = trainControl(method = "none",classProbs = TRUE,summaryFunction = twoClassSummary)
svmGrid = data.frame(sigma = 3.90625e-03, C = c(0.1,1,10,1000,10000))
#svm_model = train(test_formula, data=train_pn, method="svmRadial", metric="Accuracy", preProc=c("center", "scale"), trControl=control, fit=FALSE)
svm_model = train(test_formula,train_pn,method = "svmRadial",trControl = control,tuneGrid = svmGrid)
svm_cal = cal_model_accuracy(model = svm_model,newset = qy_test,keycol = "sign",calf = calAR,caltype = "accuracy")
svm_pred = predict(svm_model,qy_test)

nb_model = train(test_formula,train_pn,method = "nb",trControl = control)
nb_cal = cal_model_accuracy(model = nb_model,newset = qy_test,keycol = "sign",calf = calAR,caltype = "accuracy")
nb_pred = predict(nb_model,qy_test)

model_nb_pn = naive_bayes(test_formula,train_pn)
nb_pred_pn = predict(model_nb_pn,qy_test)
nb_pn_cal = cal_model_accuracy(model = model_nb_pn,newset = qy_test,keycol = "sign",calf = calAR,caltype = "accuracy")

library(doMC)
registerDoMC(cores=8)
ptm = proc.time()
naive_bayes_model = train(test_formula,train_pn,method = "naive_bayes",trControl = control)
naive_bayes_cal = cal_model_accuracy(model = naive_bayes_model,newset = qy_test,keycol = "sign",calf = calAR,caltype = "accuracy")
naive_bayes_pred = predict(nb_model,qy_test)
ptm = proc.time() - ptm

naive_bayes_model = train(test_formula,train_pn,method = "naive_bayes",trControl = control)
#long time
svm_model = train(test_formula,train_pn,method = "svmRadial",trControl = control,tuneGrid = svmGrid)

#long time
c50_model <- train(test_formula, data=train_pn, method="C5.0", metric=metric, preProc=c("center", "scale"), trControl=control)

#short time
rpart_model <- train(test_formula, data=train_pn, method="rpart", metric=metric, trControl=control)

#short time
glm_model <- train(test_formula, data=train_pn, method="glm", metric=metric, trControl=control)


#沈阳11月5日，6日；11月9日，10日，12日，13日,29,30,12-02
#沈阳1105 is normal(20%+ abnormal),1106 is normal(40%+ abnormal);the rest is abnormal
#recall:0.844
sy_test = get_test_sample("沈阳",20171202)
naive_bayes_cal["shenyang"] = cal_model_accuracy(model = naive_bayes_model,newset = sy_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.967923

#清远11月6,9,10,12,13
#清远 all is abnormal:recall:0.994
qy_test = get_test_sample("清远",20171112)
naive_bayes_cal["qingyuan"] = cal_model_accuracy(model = naive_bayes_model,newset = qy_test,keycol = "sign",calf = calAR,caltype = "accuracy")
qy_result = get_test_sample_result("清远",20171112,test_model = model_svm)
#svm:0

#苏州 11月26,27,29,30,12-02,03
#苏州 1126,1127,1129,1130 is normal(but the stay time is so consistent),recall:0.124
sz_test = get_test_sample("苏州",20171203)
naive_bayes_cal["suzhou"] = cal_model_accuracy(model = naive_bayes_model,newset = sz_test,keycol = "sign",calf = calAR,caltype = "accuracy")
sz_result = get_test_sample_result("苏州",20171203,test_model = model_svm)
#svm:0.000887

#上海 12-01,02,03,seems all is normal:recall:0.1
sh_test = get_test_sample("上海",20171203)
naive_bayes_cal["shanghai"] = cal_model_accuracy(model = naive_bayes_model,newset = sh_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.000383
sh_result = get_test_sample_result("上海",20171203,test_model = model_svm)
#svm:0

#12-02,03,09,10,11,12
#漯河 all is abnormal(at least 80% is positive):recall:0.894
lh_test = get_test_sample("漯河",20171212)
naive_bayes_cal["luohe"] = cal_model_accuracy(model = naive_bayes_model,newset = lh_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.996
lh_result = get_test_sample_result("漯河",20171212,test_model = model_svm)
#svm:0.981

#温州1202 1203
#温州1202 is abnormal,1203 is abnormal:recall:0.759
wz_test = get_test_sample("温州",20171203)
naive_bayes_cal["wenzhou"] = cal_model_accuracy(model = naive_bayes_model,newset = wz_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.807
wz_result = get_test_sample_result("温州",20171203,test_model = model_svm)
#svm:0.759

#南京 is normal recall:0.0312
nj_test = get_test_sample("南京",20171226)
naive_bayes_cal["nanjing"] = cal_model_accuracy(model = naive_bayes_model,newset = nj_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.000488
nj_result = get_test_sample_result("南京",20171226,test_model = model_svm)
#svm:0

#重庆 is normal recall:0.0186
cq_test = get_test_sample("重庆",20171209)
naive_bayes_cal["chongqing"] = cal_model_accuracy(model = naive_bayes_model,newset = cq_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.00772
cq_result = get_test_sample_result("重庆",20171209,test_model = model_svm)
#svm:0.00585

#泉州 
qz_test = get_test_sample("泉州",20171112)
naive_bayes_cal["quanzhou"] = cal_model_accuracy(model = naive_bayes_model,newset = qz_test,keycol = "sign",calf = calAR,caltype = "accuracy")
#nb:0.993
qz_result = get_test_sample_result("泉州",20171112,test_model = model_svm)
#svm:0.991

test_set_names = ls(pattern = "^.._test$")
# model_result #svm
model_result_1 = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","svm_model","c50_model","rpart_model","glm_model"))
library(readxl)
name_para = read_xlsx("~/data/name_para.xlsx")
collect_test_set(name_para,factordf = 1,renew = TRUE)

get_var_sample

naive_bayes_model2 = train(form = test_formula,data = train_pn2,method = "naive_bayes",metric = metric,
                           trControl = control,preProcess = preProcess)
model_result_2 = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model2"))

train_pn3 = get_var_sample(oringin_set = train_pn,negdiv = 0.2)
naive_bayes_model3 = train(form = test_formula,data = train_pn3,method = "naive_bayes",metric = metric,
                           trControl = control,preProcess = preProcess)
model_result_3 = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model2","naive_bayes_model3"))

temp = predict(naive_bayes_model,newdata = sy_test,type = "prob")

#check why original one can't work for qy_test
naive_bayes_model1 = naive_bayes(sign~.,train_pn[complete.cases(train_pn),])
temp = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model1","naive_bayes_model2","naive_bayes_model3"))


#get everyday's feature set
dateset = c("20180129")
#negdateset should be the same, due to the function setting we don't need to specify the same date
umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where  dt = '20180129' and path = 'z'"
general_today = get_advbase_set(umid_sql,specDate = dateset)
train_today = preprocess_train_data(general_today)
temp = predict(naive_bayes_model,newdata = train_today)
temp2 = predict(naive_bayes_model,newdata = train_today,type = "class")
today_result = cbind(train_today,pred = temp)

name_para = read_xlsx("~/data/name_para.xlsx")
collect_test_set(name_para,factordf = 1,renew = FALSE)
test_set_names = ls(pattern = "_test(\\d){4}$")
model_result_1_new = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model1","naive_bayes_model2","naive_bayes_model3"))

train_pn_simple = train_pn[,pickedColums]
naive_bayes_model = train(form = test_formula,data = train_pn,method = "naive_bayes",metric = metric,
                               trControl = control,preProcess = preProcess)
naive_bayes_model_simp = train(form = test_formula,data = train_pn_simple,method = "naive_bayes",metric = metric,
                           trControl = control,preProcess = preProcess)
pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc","mobile_city_count","mobile_gps_count")
naive_bayes_model_no_rate = train(form = test_formula,data = train_pn[,pickedColums],method = "naive_bayes",metric = metric,
                               trControl = control,preProcess = preProcess)

pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc","mobile_city_count","rate")
naive_bayes_model_no_gps_count = train(form = test_formula,data = train_pn[,pickedColums],method = "naive_bayes",metric = metric,
                                  trControl = control,preProcess = preProcess)

model_result_simple = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model_simp","naive_bayes_model_no_rate","naive_bayes_model_no_gps_count","naive_bayes_model_real"))

naive_bayes_model_real = train(form = test_formula,data = train_pn,method = "naive_bayes",metric = metric,
                          trControl = control,preProcess = preProcess)
naive_bayes_model_real_ori = naive_bayes(test_formula,train_pn)
model_result_simple = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model_simp","naive_bayes_model_no_rate","naive_bayes_model_no_gps_count","naive_bayes_model_real","naive_bayes_model_real_ori"))

# transform of the data
P = ecdf(train_pn$max_stay)
P(50000)
temp = qnorm(P(train_pn$max_stay))

sz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in ('苏州','苏州市') and dt = '20171203' and path = 'z'"
sh_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in ('上海','上海市') and dt = '20171201' and path = 'z'"
umid_sql = c(sz_umid_sql,sh_umid_sql)
dateset = c("20171203","20171201")
temp =  get_complex_train_set(dateset = dateset,umid_sql = umid_sql)
naive_bayes_model_szsh = naive_bayes(test_formula,temp)
model_result_all = test_around_set(test_set_names = test_set_names,model_names = c("naive_bayes_model","naive_bayes_model_simp","naive_bayes_model_no_rate","naive_bayes_model_no_gps_count","naive_bayes_model_real","naive_bayes_model_real_ori","naive_bayes_model_szsh"))

train_pn$sign = as.factor(train_pn$sign)
test_imp_model <- train(test_formula, data=train_pn, method="naive_bayes", trControl=control,
                    importance = T)
# estimate variable importance
importance <- varImp(test_imp_model, scale=FALSE)

today_result = pred_day_result(naive_bayes_model_no_rate)
today_result = today_result[complete.cases(today_result),]
today_result$u_mid = as.character(today_result$u_mid)
today_result$dt = as.character(today_result$dt)
today_result$normal_prob = round(today_result$normal_prob,2)
today_result$abnorm_prob = round(today_result$abnorm_prob,2)
today_result[] = lapply(today_result,as.character)

dbSendUpdate(con,"create table test.test_umid1 (name string,sign string,abnorm_prob string,normal_prob string) partitioned by (dt string)")
dbWriteTable(con,"test.test_umid",today_result,overwrite = TRUE,append = TRUE)

# getOption("pkgType")
