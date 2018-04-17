date = Sys.Date()-1
date.str = format(date,'%Y%m%d')
spec.date.str = "20171112"

source('~/Rfile/R_impala.R')

city_sql = paste0("select x.*,x.city_count_new/x.city_count_all city_perc from
(select m.l_city,m.dt,count(*) city_count_all,count(if(m.isnew = 'new','new',NULL)) city_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = '",date.str,"') m group by m.l_city,m.dt) x")

ip_sql = paste0("select x.*,x.ip_count_new/x.ip_count_all ip_perc from
(select m.l_ip,m.dt,count(*) ip_count_all,count(if(m.isnew = 'new','new',NULL)) ip_count_new from 
  (select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
    where f.dt = '",date.str,"') m group by m.l_ip,m.dt) x")

model_sql = paste0("select x.*,x.md_count_new/x.md_count_all md_perc from
(select m.d_model,m.dt,count(*) md_count_all,count(if(m.isnew = 'new','new',NULL)) md_count_new from 
(select i.*,case when (regexp_replace(to_date(firstonlinetime),'-','')) = i.dt then 'new' else 'old' end isnew 
from dm.dm_app_pageview_info i inner join dl.umid_firstonlinetime f on i.u_mid = f.u_mid
where f.dt = '",date.str,"') m group by m.d_model,m.dt) x")

base_sql = paste0("select base.*,umid_count_same_ip from 
(select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
                  from dm.dm_app_pageview_info where l_city in ('清远','清远市','qingyuan','qingyuanshi') and dt = '",spec.date.str,"' and path = 'z'
                  group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info where l_city in 
                  ('清远','清远市','qingyuan','qingyuanshi') 
                  and dt = '",spec.date.str,"' and path = 'z' group by l_ip) iptb on base.l_ip = iptb.l_ip")

base = read_data_impala_general(base_sql)
model = read_data_impala_general(model_sql)
city = read_data_impala_general(city_sql)
ip = read_data_impala_general(ip_sql)

base$sign = ifelse(base$umid_count_same_ip<20,0,1)
base_city = merge(base,city[city$dt == "20171112",],by.x = "l_city",by.y = "l_city",all.x = TRUE)
base_city_model = merge(base_city,model[model$dt == "20171112",],by.x = "d_model",by.y = "d_model",all.x = TRUE)

train_set = base_city_model[,c(5,6,7,8,9,10,14,18)]
x = train_set[,!(names(train_set) %in% "sign")]
y = as.factor(train_set$sign)

library("caret")
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)
