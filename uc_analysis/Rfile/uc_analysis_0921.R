source('~/Rfile/R_impala.R')
source("~/Rfile/R_hive.R")
source("~/Rfile/R_impala3.R")

sql = "select * from test.umid_lujing_split where dt=20170930"

sql = "select * from dl.umid_pv p left join 
(select i.u_mid,i.l_city,i.l_province,i.d_model,i.dt,row_number() over 
(partition by i.dt,i.u_mid order by dt desc) rn from ods.ods_app_pageview_info i 
where i.dt>= 20170916)
i2
on p.u_mid = i2.u_mid and p.dt = i2.dt
where i2.rn = 1"



data_930 = read_data_hive_general(sql)

data_930 = read_data_impala_general(sql)
  
data_921  = read_data_impala_general(sql)
setDT(data_921)
setnames(data_921,"_c3","path")
data_921[,splitted_path:=str_split(path,"->")]
temp_first_page = sapply(data_921$splitted_path,`[[`,1)
path_length = sapply(data_921$splitted_path,length)
temp_last_page = mapply(`[[`,data_921$splitted_path,path_length)
data_921$first_page = temp_first_page
data_921$last_page = temp_last_page
data_921$path_length = path_length
freq_first_page = data_921[,.N,by=c("isnew","first_page")]
freq_first_page = freq_first_page[order(isnew,N,decreasing = TRUE),]
freq_last_page = data_921[,.N,by=c("isnew","last_page")]
freq_last_page = freq_last_page[order(isnew,N,decreasing = TRUE),]

library(tidyr)
seperated_pages = strsplit(data_921$path,"->")
sep_pages_df = data.frame(seq = rep(1:length(seperated_pages),sapply(seperated_pages,length)),page = unlist(seperated_pages))
#separate_rows
# table_basis = data.frame(page_name = unique(sep_pages_df$page),stringsAsFactors = FALSE)
# table_basis$depth = as.vector(NA,mode = "numeric")
# table_basis[grep("详情",table_basis$page_name),"depth"] = 3
# table_basis[grep("列表",table_basis$page_name),"depth"] = 2
# table_basis[grep("首页",table_basis$page_name),"depth"] = 1
# table_basis[grep("主页",table_basis$page_name),"depth"] = 1
# openxlsx::write.xlsx(table_basis,"~/data/table_basis.xlsx")
table_basis2 = readxl::read_xlsx("~/data/table_basis.xlsx")
table_basis2 = as.data.frame(table_basis2)
sep_pages_df = merge(sep_pages_df,table_basis2,all.x = TRUE,by.x = "page",by.y = "page_name")

sep_pages_df = sep_pages_df[order(sep_pages_df$seq),]
depth_result = aggregate(depth~seq, data = sep_pages_df, FUN = max)
depth_avg = aggregate(depth~seq,data = sep_pages_df, FUN = mean )
data_921$seq = 1:nrow(data_921)
data_921 = merge(data_921,depth_result,by = "seq",all.x = TRUE) 
avg_depth = aggregate(depth~isnew,data = data_921,FUN = mean)
avg_length = aggregate(path_length~isnew,data = data_921,FUN = mean)

#地域,机型,渠道

# select k.l_city,isnew,count(distinct u_mid) pv,k.dt
# from
# (
#   Select i2.*,p.isnew from dl.umid_pv p left join 
#   (select i.u_mid,i.l_city,i.l_province,i.d_model,i.dt 
#     from ods.ods_app_pageview_info i where i.dt>= 20170916)
#   i2
#   on p.u_mid = i2.u_mid and p.dt = i2.dt)k
# group by k.l_city,isnew,k.dt

# select k.d_model,isnew,count(distinct u_mid) pv,k.dt
# from
# (
#   Select i2.*,p.isnew from dl.umid_pv p left join 
#   (select i.u_mid,i.l_city,i.l_province,i.d_model,i.dt 
#     from ods.ods_app_pageview_info i where i.dt>= 20170916)
#   i2
#   on p.u_mid = i2.u_mid and p.dt = i2.dt)k
# group by k.d_model,isnew,k.dt
