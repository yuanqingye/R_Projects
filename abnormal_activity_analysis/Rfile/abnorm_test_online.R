#! /usr/bin/env Rscript
source('~/Rfile/R_impala.R')
library(stringr)
library(naivebayes)
library(mclust)
library(e1071)
library(randomForest)
library(readxl)
library(caret)
library(data.table)
library(abnormTestOnlineFunc)

# pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc","mobile_city_count","mobile_gps_count","rate")
pickedColums = c("max_stay","min_stay","avg_stay","pv","umid_count_same_ip","sign","city_perc","md_perc","mobile_city_count","mobile_gps_count")
sum_col = c("pv")
avg_col = c("city_perc","md_perc","avg_stay")
max_col = c("max_stay","umid_count_same_ip")
min_col = c("min_stay")
lastdate = Sys.Date()-1
last.date.str = format(lastdate,'%Y%m%d')
spec.date.str = "20171112" #using 20171203 before
spec.date.dash.str = "2017-11-12"
city_name = ""
test_formula = as.formula("sign~.")
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
preProcess=c("center", "scale")
svmGrid = data.frame(sigma = 3.90625e-03, C = c(0.1,1,10,1000,10000))
# train_pn = read_xlsx("~/data/test_abnormal_uv.xlsx")

#get umid which introduce the order, which is unlikely, to be abnormal umid
order_umid_sql = paste0("select distinct u_mid from ods.ods_tx_order_tx_order_dt o 
                        inner join ods.ods_app_pageview_info a on 
                        o.purchaser_id = a.u_id where substr(o.create_date,1,10)= '",spec.date.dash.str,"' 
                        and o.order_status not in (1,7,19) and o.order_type=1 and a.l_city like '",city_name,"%'")

#get umid which revisit our app during a long time
last_long_umid_sql = "select distinct u_mid from 
(select u_mid,(max(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss'))-min(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss')))/(3600*24) time_inv from dm.dm_app_pageview_info group by u_mid) t where time_inv>60"

#city and its umid count and new umid count and the rate of new count/all count,group by 
#city and the visit dt,only include umid before a specific date(BY LIMIT FIRSTONLINETIME TABLE)
#duplicated umid is allowed, which means this is essentially 
CITY_SQL = "select x.*,x.city_count_new/x.city_count_all city_perc from
(select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = 'date_to_be_replaced') m group by m.l_city,m.dt) x"

#model and its umid count and new umid count and the rate of new count/all count group by 
#model and the visit dt,only include the umid before a specific date(BY LIMIT FIRSTONLINETIME TABLE)
MODEL_SQL = "select x.*,x.md_count_new/x.md_count_all md_perc from
(select m.d_model,m.dt,count(*) md_count_all,count(if(m.isnew = 'new','new',NULL)) md_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = 'date_to_be_replaced') m group by m.d_model,m.dt) x"

#the percentage of each city's first ranking page count against the all count
PAGE_CITY_FIRST_SQL= "select * from
(select t.l_city,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.l_city order by p_count desc) rnum from
(select l_city,count(*) p_count,p_type from dm.dm_app_pageview_info where dt <= 'date_to_be_replaced'  and path = 'z' group by l_city,p_type) t 
left join (select l_city,count(*) all_count from dm.dm_app_pageview_info where dt <= 'date_to_be_replaced'  and path = 'z' group by l_city) ac 
using(l_city)) tt where tt.rnum = 1"

PAGE_CITY_DT_FIRST_SQL= "select * from
(select t.l_city,t.dt,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.l_city,t.dt order by p_count desc) rnum from
(select l_city,dt,count(*) p_count,p_type from dm.dm_app_pageview_info where dt <= 'date_to_be_replaced'  and path = 'z' group by l_city,dt,p_type) t 
left join (select l_city,dt,count(*) all_count from dm.dm_app_pageview_info where dt <= 'date_to_be_replaced'  and path = 'z' group by l_city,dt) ac 
using(l_city,dt)) tt where tt.rnum = 1"

#the percentage of each umid's first ranking page count against the all count
PAGE_UMID_FIRST_SQL = "select * from
(select t.u_mid,t.p_count,ac.all_count,t.p_count/ac.all_count rate,row_number() over (partition by t.u_mid order by p_count desc) rnum from
(select u_mid,count(*) p_count,p_type from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by u_mid,p_type) t 
left join (select u_mid,count(*) all_count from dm.dm_app_pageview_info where dt = 'date_to_be_replaced'  and path = 'z' group by u_mid) ac 
using(u_mid)) tt where tt.rnum = 1"

#get each umid's page view count per minute
PV_UMID_UNIT_MIN_SQL = "select u_mid,pcount,stay_time,pcount/stay_time pv from (
select u_mid,case when count(*)>1 then (max(cast(ts as bigint))-min(cast(ts as bigint)))/1000/60 
else cast(max(p_stay_time) as integer)/1000/60 end stay_time,
count(*) pcount from dm.dm_app_pageview_info where l_city like 'city_to_be_replaced%' and dt = 'date_to_be_replaced' 
and path = 'z' group by u_mid,session) t"
order_umid_sql = paste0("select distinct u_mid from ods.ods_tx_order_tx_order_dt o 
inner join ods.ods_app_pageview_info a on 
                        o.purchaser_id = a.u_id where substr(o.create_date,1,10)= '",spec.date.dash.str,"' 
                        and o.order_status not in (1,7,19) and o.order_type=1 and a.l_city like '",city_name,"%'")

#get umid which revisit our app during a long time
last_long_umid_sql = "select distinct u_mid from 
(select u_mid,(max(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss'))-min(unix_timestamp(`system_time`, 'yyyy-MM-dd HH:mm:ss')))/(3600*24) time_inv from dm.dm_app_pageview_info group by u_mid) t where time_inv>60"

qz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('泉州','泉州市','quanzhou') and dt = '20171112' and path = 'z'"

wz_umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where l_city in 
('温州','温州市','wenzhou') and dt = '20171203' and path = 'z'"

dateset = c("20171112","20171203")
#negdateset should be the same, due to the function setting we don't need to specify the same date
umid_sql = c(qz_umid_sql,wz_umid_sql)
neg_umid_sql = c(order_umid_sql,last_long_umid_sql)
general_pn = get_advbase_set(umid_sql,neg_umid_sql,dateset,renew = TRUE,complexity = TRUE)
train_pn = preprocess_train_data(general_pn)
#model_nb_pn = naive_bayes(sign~.,train_pn[complete.cases(train_pn),])
naive_bayes_model = train(form = test_formula,data = train_pn,method = "naive_bayes",metric = metric,
                          trControl = control,preProcess = preProcess)

# general_today = get_advbase_set(umid_sql,specDate = last.date.str)
# train_today = preprocess_train_data(general_today)
# temp = predict(naive_bayes_model,newdata = train_today)
today_result = pred_day_result(naive_bayes_model)
today_result = today_result[complete.cases(today_result),]
# today_result$u_mid = as.character(today_result$u_mid)
# today_result$dt = as.character(today_result$dt)
today_result$normal_prob = round(today_result$normal_prob,2)
today_result$abnorm_prob = round(today_result$abnorm_prob,2)
today_result[] = lapply(today_result,as.character)

write.table(today_result,"~/result/today_result.txt",col.names = FALSE,row.names = FALSE,sep = "\t")
dbDisconnect(con)
