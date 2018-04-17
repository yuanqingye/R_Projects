#比较今天和前几天的u_mid交集~访问增长,几乎所有的异常都是从异常的增长开始的
#访问时间长度居然相同(shanghai,suzhou)
#异常不止一种，所以需要多个变量
#输入方式
#找到正常访问的分布，最好能判断出分布类型(并不是指数分布)
#需要把过于线性相关的变量排除(max_stay~avg_stay)
#This R file is original and is used as archive or relic resources
#is that small city only has < 50 visit, no need to be abnormal, should be normal, for a big city <200 should be considered normal
#shenyang has hign umidperip but was diagnosed as normal in model(which could be wrong),solution:raise the weight of umidperip
#No need to get all the sample from one set, part of it will give good model
#Naive bayes may not handle this case, SVM or neural network may be better model
#The model is too focus on some feature
#May need to remove outlier,since in suzhou 1127 a record's stay time is too high and made that point 'abnormal'
#MAX stay = min stay ? calculation problem
#mobile gps count over 100 is a normal thing or abnormal thing
#avg stay is bigger than the min stay(problem)
#重庆 1209 is a dedicated case

date = Sys.Date()-1
date.str = format(date,'%Y%m%d')
spec.date.str = "20171203" #using 20171112 before
spec.date.dash.str = "2017-12-03"
pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc")
city_name = ""

CITY_SQL = "select x.*,x.city_count_new/x.city_count_all city_perc from
                  (select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
                  (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
                  from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
                  where f.dt = 'date_to_be_replaced') m group by m.l_city,m.dt) x"

MODEL_SQL = "select x.*,x.md_count_new/x.md_count_all md_perc from
                   (select m.d_model,m.dt,count(*) md_count_all,count(if(m.isnew = 'new','new',NULL)) md_count_new from 
                   (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
                   from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
                   where f.dt = 'date_to_be_replaced') m group by m.d_model,m.dt) x"

PAGE_CITY_FIRST_SQL= "select * from
(select t.l_city,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.l_city order by p_count desc) rnum from
                             (select l_city,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by l_city,p_type) t 
                             left join (select l_city,count(*) all_count from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by l_city) ac 
                             using(l_city)) tt where tt.rnum = 1"

PAGE_UMID_FIRST_SQL = "select * from
(select t.u_mid,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.u_mid order by p_count desc) rnum from
                             (select u_mid,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by u_mid,p_type) t 
                             left join (select u_mid,count(*) all_count from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by u_mid) ac 
                             using(u_mid)) tt where tt.rnum = 1"

PV_UMID_UNIT_MIN_SQL = "select u_mid,pcount,stay_time,pcount/stay_time pv from (
select u_mid,case when count(*)>1 then (max(cast(ts as bigint))-min(cast(ts as bigint)))/1000/60 
else cast(max(p_stay_time) as integer)/1000/60 end stay_time,
count(*) pcount from dm.dm_app_pageview_info where l_city like 'city_to_be_replaced%' and dt = 'date_to_be_replaced' 
and path = 'z' group by u_mid,session) t"

source('~/Rfile/R_impala.R')

#Calculate the f1, the important criteria for classification model
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

#Given the real and pred result, calculate the AUC
calAUC = function(real,pred){
  library(AUC)
  test_roc = roc(pred,real)
  test_auc = auc(test_roc)
  return(test_auc)
}
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
last_long_umid_sql = "select u_mid from 
(select u_mid,(max(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss'))-min(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss')))/(3600*24) time_inv from dm.dm_app_pageview_info group by u_mid) t where time_inv>30"

#This sql search for target umid, the sample rely on which we build the model
umid_sql = "select u_mid from test.train_sample"

umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('漯河','漯河市','luohe') and dt = '20171203' and path = 'z'"

#city and its umid count and new umid count and the rate of new count/all count,group by 
#city and the visit dt,only include umid before a specific date(BY LIMIT FIRSTONLINETIME TABLE)
city_sql = paste0("select x.*,x.city_count_new/x.city_count_all city_perc from
                  (select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
                  (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
                  from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
                  where f.dt = '",spec.date.str,"') m group by m.l_city,m.dt) x")

#model and its umid count and new umid count and the rate of new count/all count group by 
#model and the visit dt,only include the umid before a specific date(BY LIMIT FIRSTONLINETIME TABLE)
model_sql = paste0("select x.*,x.md_count_new/x.md_count_all md_perc from
                   (select m.d_model,m.dt,count(*) md_count_all,count(if(m.isnew = 'new','new',NULL)) md_count_new from 
                   (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
                   from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
                   where f.dt = '",spec.date.str,"') m group by m.d_model,m.dt) x")

#a umid may shows up in more than one city
umid_move_sql = paste0("select u_mid,count(distinct l_city) mobile_city_count,count(l_gps) mobile_gps_count from dm.dm_app_pageview_info p 
                       where p.u_mid in (",umid_sql,") and dt <= '",spec.date.str,"' and path = 'z' group by u_mid")

check_sql = "select * from test.train_sample"

#the percentage of each city's ranking page against all count(including all count)
# page_city_sql = paste0("select t.l_city,t.p_count,ac.all_count,t.p_count/ac.all_count rate from
# (select l_city,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  
# and path = 'z' group by l_city,p_type) t left join (select l_city,count(*) all_count from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  
# and path = 'z' group by l_city) ac using(l_city) order by t.l_city,t.p_count desc")

#the percentage of each city's first ranking page count against the all count
page_city_first_sql = paste0("select * from
(select t.l_city,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.l_city order by p_count desc) rnum from
(select l_city,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  and path = 'z' group by l_city,p_type) t 
left join (select l_city,count(*) all_count from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  and path = 'z' group by l_city) ac 
using(l_city)) tt where tt.rnum = 1")

#the percentage of each umid's first ranking page count against the all count
page_umid_first_sql = paste0("select * from
(select t.u_mid,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.u_mid order by p_count desc) rnum from
(select u_mid,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  and path = 'z' group by u_mid,p_type) t 
left join (select u_mid,count(*) all_count from dm.dm_app_pageview_info where dt = '",spec.date.str,"'  and path = 'z' group by u_mid) ac 
using(u_mid)) tt where tt.rnum = 1")

#get each umid's page view count per minute
pv_umid_unit_min_sql = paste0("select u_mid,pcount,stay_time,pcount/stay_time pv from (
select u_mid,case when count(*)>1 then (max(cast(ts as bigint))-min(cast(ts as bigint)))/1000/60 
                              else cast(max(p_stay_time) as integer)/1000/60 end stay_time,
                              count(*) pcount from dm.dm_app_pageview_info where l_city like '",city_name,"%' and dt = '",spec.date.str,"' 
                              and path = 'z' group by u_mid,session) t")


#get the 1-level depth rate for each umid, without dt different
#We can consider group also by date and then using max,min,avg to get the calculation
#currently not use since the table can hardly be maintened
# depth_rate_sql = "select a.u_mid,count(case when(cast(b.depth as tinyint) = 1) then 1 else NULL end) as 1depth_count,count(*) as depth_count,
# count(case when(cast(b.depth as tinyint) = 1) then 1 else NULL end)/count(*) 1depth_rate from 
# dm.dm_app_umid_step a  
# left outer join 
# test.pagelevel b on a.page_name_zh=b.page_name where length(b.depth)=1 and dt > '20171220' group by a.u_mid"

get_base_with_restrict = function(umid_sql,specDate = spec.date.str,mode = "base"){
  if(mode == "base"){
  sql = paste0("select base.*,umid_count_same_ip from 
(select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
                    from dm.dm_app_pageview_info p where p.u_mid in (",umid_sql,") and dt <= '",specDate,"' and path = 'z' and cast(p_stay_time as int) > 1000*3600*6
                    group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
                    (",umid_sql,") and dt <= '",specDate,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")
  }
  else if(mode == "umidmove"){
  sql = paste0("select u_mid,count(distinct l_city) mobile_city_count,count(l_gps) mobile_gps_count from dm.dm_app_pageview_info p 
                       where p.u_mid in (",umid_sql,") and dt <= '",specDate,"' and path = 'z' group by u_mid")
  }
  base = read_data_impala_general(sql)
  return(base)
}

run_flexible_sql = function(SQL,specDate = spec.date.str,specCity = city_name){
  SQL = str_replace_all(SQL,'date_to_be_replaced',specDate)
  SQL = str_replace_all(SQL,'city_to_be_replaced',specCity)
  result = read_data_impala_general(SQL)
  return(result)
}

get_sample_set = function(umid_sql,specDate = spec.date.str){
  base = get_base_with_restrict(umid_sql,spec.date.str)
  move = get_base_with_restrict(umid_sql,mode = "umidmove")
  #base umid with city attributes on a given day
  base_city = merge(base,city[city$dt == spec.date.str,],by.x = "l_city",by.y = "l_city",all.x = TRUE)
  #base umid with city and model attributes on a given day
  base_city_model = merge(base_city,model[model$dt == spec.date.str,],by.x = "d_model",by.y = "d_model",all.x = TRUE)
  base_city_model = data.table(base_city_model)
  #duplicated umid should be take care of
  base_city_model_summary = base_city_model[,c(lapply(.SD[,sum_col,with=FALSE],sum),lapply(.SD[,avg_col,with=FALSE],mean),lapply(.SD[,min_col,with = FALSE],max),lapply(.SD[,max_col,with = FALSE],max)),by = "u_mid"]
  base_city_model_summary = as.data.frame(base_city_model_summary)
  base_city_model_move = merge(base_city_model_summary,umid_move,by.x = "u_mid",by.y = "u_mid",all.x = TRUE,all.y = FALSE)
  return(base_city_model_move)
}

# get_resource_by_name = function(spec.date.str){
#   city_sql = paste0("select x.*,x.city_count_new/x.city_count_all city_perc from
#                   (select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
#                   (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
#                   from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
#                   where f.dt = '",spec.date.str,"') m group by m.l_city,m.dt) x")
#   city = read_data_impala_general(spec.date.str)
# }

positive_base = get_base_with_restrict(umid_sql,spec.date.str)
negative_base = get_base_with_restrict(neg_umid_sql,spec.date.str)
negative_base1 = get_base_with_restrict(order_umid_sql,spec.date.str)
negative_base2 = get_base_with_restrict(last_long_umid_sql,spec.date.str)
train_sample_big = rbind(positive_base,negative_base1,negative_base2)
positive_move = get_base_with_restrict(umid_sql,mode = "umidmove")
negative_move1 = get_base_with_restrict(order_umid_sql,mode = "umidmove")
negative_move2 = get_base_with_restrict(last_long_umid_sql,mode = "umidmove")
negative_move = rbind(negative_move1,negative_move2)
negative_move = negative_move[!duplicated(negative_move$u_mid),]
positive_move$sign = 1
negative_move$sign = 0
umid_move = rbind(positive_move,negative_move)

positive_base = get_base_with_restrict(umid_sql,spec.date.str)
negative_base1 = get_base_with_restrict(order_umid_sql,spec.date.str)
negative_base2 = get_base_with_restrict(last_long_umid_sql,spec.date.str)
check = read_data_impala_general(check_sql)
model = run_flexible_sql(MODEL_SQL)
city = run_flexible_sql(CITY_SQL)
move = run_flexible_sql(UMID_MOVE_SQL)
page_city = run_flexible_sql(PAGE_CITY_FIRST_SQL)
page_umid = run_flexible_sql(PAGE_UMID_FIRST_SQL)
pv_umid_unit_min = run_flexible_sql(PV_UMID_UNIT_MIN_SQL)

base = read_data_impala_general(base_sql)
model = read_data_impala_general(model_sql)
city = read_data_impala_general(city_sql)
move = read_data_impala_general(umid_move_sql)
page_city = read_data_impala_general(page_city_first_sql)
page_umid = read_data_impala_general(page_umid_first_sql)
check = read_data_impala_general(check_sql)
pv_umid_unit_min = read_data_impala_general(pv_umid_unit_min_sql)
order_umid = read_data_impala_general(order_umid_sql)
last_long_umid = read_data_impala_general(last_long_umid_sql)
#ip = read_data_impala_general(ip_sql)

test_sql = paste0("select base.*,umid_count_same_ip from 
(select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
             from dm.dm_app_pageview_info p where p.u_mid in (",order_umid_sql,") and dt <= '",spec.date.str,"' and path = 'z'
             group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
             (",order_umid_sql,") and dt <= '",spec.date.str,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")


#!!!Not the final rule, only for the current version
base$sign = ifelse(base$umid_count_same_ip<20,0,1)
#base umid with city attributes on a given day
base_city = merge(base,city[city$dt == spec.date.str,],by.x = "l_city",by.y = "l_city",all.x = TRUE)
#base umid with city and model attributes on a given day
base_city_model = merge(base_city,model[model$dt == spec.date.str,],by.x = "d_model",by.y = "d_model",all.x = TRUE)

base_city_model_move = merge(base_city_model,move,by.x = "u_mid",by.y = "u_mid")

train_set = base_city_model[,pickedColums]
x = train_set[,!(names(train_set) %in% "sign")]
train_set$sign = as.factor(train_set$sign)
y = train_set$sign
#get the train sample from original, but with only positive sample, result is poor so far
one_sample_train_set = train_set[train_set$sign == "1",]
# library(mice)
# md.pattern(x)
# View(x[is.na(x)])

#Naive bayes This can't handle the missing value part
library(caret)
library(klaR)
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
result = predict(model$finalModel,x)

#this can handle the miss value case
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
