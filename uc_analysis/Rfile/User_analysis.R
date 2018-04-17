library(ggplot2)
library(reshape2)
library(scales)
library(plyr)
library(treemap)
source('~/Rfile/R_impala.R')
dateend = Sys.Date()-1
datestart = Sys.Date()-14
datestart.str = format(datestart,'%Y%m%d')
dateend.str = format(dateend,'%Y%m%d')
dates = datestart.str:dateend.str
ldateend = Sys.Date()-15
ldatestart = Sys.Date()-28
ldatestart.str = format(ldatestart,'%Y%m%d')
ldateend.str = format(ldateend,'%Y%m%d')

pvuv_sql = paste0("select dt,
count(1) uv,
                 sum(pv) pv ,
                 sum(pv)/count(1) perpv,
                 count(if(isnew='new',1,null)) newuv ,
                 sum(if(isnew='new',pv,0)) newpv, 
                 sum(if(isnew='new',pv,0))/count(if(isnew='new',1,null)) newperpv,
                 count(if(isnew='old',1,null)) olduv ,
                 sum(if(isnew='old',pv,0)) oldpv, 
                 sum(if(isnew='old',pv,0))/count(if(isnew='old',1,null)) oldperpv
                 from 
                 dl.umid_pv where dt>='",datestart.str,
                 "'group by dt")

pvuv = read_data_impala_general(pvuv_sql)
pvuv = pvuv[order(pvuv$dt),]
pvuv$fdt = as.factor(pvuv$dt)
matrix = as.matrix(cbind(newuv = pvuv$newuv,olduv = pvuv$olduv))
matrix = t(matrix)
maxy = max(pvuv$uv)
par(bg = 'white')
colors <- c("green","red")
dates <- pvuv$dt
stacks <- c("New","Old")
# Create the bar chart.
barplot(matrix,main="Every day uv",names.arg=dates,xlab="month",ylab="uv",col=colors)
# Add the legend to the chart.
legend(x = 0,y = maxy, stacks, cex=0.7, fill=colors,xjust = 0, yjust = 1)


par(bg = 'black')
colors = rainbow(3)
ylarge = max(max(pvuv$perpv),max(pvuv$newperpv),max(pvuv$oldperpv))
#考虑人均pv
plot(as.numeric(pvuv$fdt),pvuv$perpv,col = colors[[1]],type = 'l',ylim = c(0,ceiling(ylarge)))
lines(as.numeric(pvuv$fdt),pvuv$newperpv,col = colors[[2]],type = 'l')
lines(as.numeric(pvuv$fdt),pvuv$oldperpv,col = colors[[3]], type = 'l')
legend('topright',c("perpv","newperpv","oldperpv"),cex = 0.7,fill = colors,text.col= 'pink')
#需要加入坐标轴
title(main = '近两周人均pv',xlab = 'dates',ylab = '人均pv',col.main = 'blue',
      col.lab = 'purple')
axis(1,col = 'purple')
at = str_sub(pvuv$dt,5,8)[seq(from = 2,to = 14,by = 2)]
mtext(side = 1, text = at, at = seq(from = 2,to = 14,by = 2), col = "purple", line = 1)

axis(2,col = 'purple')
at = axTicks(2)
mtext(side = 2,text = at,at = at,col = 'purple',line = 1)

pm <- ggplot(pvuv[8:nrow(pvuv),], aes(as.Date(dt,format = "%Y%m%d"), pv)) + labs(x = "DATE",y = "PV") + ylim(min(pvuv$pv,na.rm = T),max(pvuv$pv,na.rm = T))
mainplot <- pm + geom_line() + 
  labs(title = "pv trend compare to last 1 month")
mainplot
# p1 <- p + geom_rect(aes(xmin = as.Date(pvuv$dt[11],format = "%Y%m%d"), xmax = as.Date(pvuv$dt[14],format = "%Y%m%d"),
#                         ymin = min(pvuv$pv, na.rm = TRUE), ymax = (max(pvuv$pv, na.rm = TRUE)+min(pvuv$pv, na.rm = TRUE))/2),
#                     fill = alpha("lightblue", 0.2)) + scale_x_continuous(breaks = NA) +
#   scale_y_continuous(breaks = NA) + labs(y = NULL)
p = ggplot(pvuv,aes(as.Date(dt,format = "%Y%m%d"), pv))+labs(x = "DATE",y = "PV") + ylim(min(pvuv$pv,na.rm = T),max(pvuv$pv,na.rm = T))
p1 <- p + geom_rect(aes(xmin = as.Date(pvuv$dt[8],format = "%Y%m%d"), xmax = as.Date(pvuv$dt[14],format = "%Y%m%d"),
                        ymin = min(pvuv$pv, na.rm = TRUE), ymax = max(pvuv$pv, na.rm = TRUE)),
                    fill = alpha("lightblue", 0.2))
p1
  # opts(title = "Full data: 1880-2008") +
  # opts(plot.title = theme_text(face = "bold")) +
  # opts(panel.border = theme_blank())
subplot <- p1 + geom_line(colour = I("grey"),
                          size = 0.8) 
library(grid)
vp <- viewport(width = 0.4, height = 0.4, x = 1,
               y = unit(0.7, "lines"), just = c("right",
                                                "bottom"))
# theme_white <- function() {
#   theme_update(panel.background = element_rect(fill = "transparent", color = "gray")
#                )
# }

full <- function() {
  print(mainplot)
  # theme_set(theme_bw(base_size = 8))
  # theme_white()
  print(subplot, vp = vp)
  # theme_set(theme_bw())
}
full()
theme(panel.background = element_rect(fill = "transparent", color = "gray"), legend.key = element_rect(fill = "transparent", color = "transparent"), axis.text = element_text(color = "red"))

#用户时长
time_span_sql = paste0("select dt,
  avg(persvg) totalavg,
  avg(case when isnew ='new' then persvg else null end) newavg,
  avg(case when isnew ='old' then persvg else null end) oldavg
from
(
  select a.dt,a.u_mid,
  sum(CAST(p_stay_time AS INT))/1000/60 persvg,
  case when regexp_replace(to_date(firstonlinetime),'-','')=CAST(a.dt AS STRING) then 'new' else 'old' end isnew 
  from 
  ods.ods_app_pageview_info a 
  left outer join 
  dl.umid_firstonlinetime b on a.u_mid=b.u_mid 
  where a.dt>=",datestart.str," and b.dt='",dateend.str,"' and
  p_domain='mmall.com' and service like '%staytime%' and substr(a.u_mid,1,2)!='a_' and path='z'  and l_city!='测试'
  and p_type not in ('page.closedown','page.wakeup','page.activate.main') and length(p_stay_time)<=7
  group by a.dt,a.u_mid,case when regexp_replace(to_date(firstonlinetime),'-','')=CAST(a.dt AS STRING) then 'new' else 'old' end 
)a group by dt")
  time_span = read_data_impala_general(time_span_sql)
  time_span = time_span[order(time_span$dt),]
  # write.xlsx(time_span,"~/data/uc_analysis/time_span.xlsx")
  time_span$fdt = as.factor(time_span$dt)
  time_span.new = time_span[,-1]
  timespan.m <- melt(time_span.new)
  timespan.m <- ddply(timespan.m, .(variable), transform,rescale = rescale(value))
  (p <- ggplot(timespan.m, aes(fdt,variable)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white",high = "purple"))

  #考虑time span,
  par(bg = 'white')
  colors = rainbow(3)
  ylarge = max(time_span[,2:4])
  plot(time_span$dt,time_span$totalavg,col = colors[[1]],type = 'l',ylim = c(0,ceiling(ylarge)))
  lines(time_span$dt,time_span$newavg,col = colors[[2]],type = 'l')
  lines(time_span$dt,time_span$oldavg,col = colors[[3]], type = 'l')
  legend('topright',c("totalavg","newavg","oldavg"),cex = 0.7,fill = colors,text.col= 'pink')
  #需要加入坐标轴
  title(main = '近两周用户活跃时长',xlab = 'dates',ylab = '',col.main = 'blue',
        col.lab = 'purple')
  axis(1,col = 'purple')
  at = axTicks(1)
  mtext(side = 1,text = at,at = at,col = "purple",line = 1)
  axis(2,col = 'purple')
  at = axTicks(2)
  mtext(side = 2,text = at,at = at,col = 'purple',line = 1)


#2persistency/stickness survival
  survival_sql = paste0("select 
  a.dt,ndv(a.u_mid) t,
  ndv(case when datediff(concat(substr(b.dt,1,4),'-',substr(b.dt,5,2),'-',substr(b.dt,7,2)),
  concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))=1 then a.u_mid else null end)	t1,
  ndv(if(a.isnew='new',a.u_mid,null)) newuv ,
  ndv(case when datediff(concat(substr(b.dt,1,4),'-',substr(b.dt,5,2),'-',substr(b.dt,7,2)),
  concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))=1 and a.isnew='new' then a.u_mid else null end) newt1,
  ndv(if(a.isnew='old',a.u_mid,null)) olduv,
  ndv(case when  datediff(concat(substr(b.dt,1,4),'-',substr(b.dt,5,2),'-',substr(b.dt,7,2)),
  concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))=1  and a.isnew='old' then a.u_mid else null end) noldt1
  from dl.umid_pv a
  left outer join
  dl.umid_pv b on a.u_mid=b.u_mid		
  where  a.dt>='",datestart.str,"' group by a.dt")

survival = read_data_impala_general(survival_sql)
survival = survival[order(survival$dt),]
survival = survival[-nrow(survival),]
par(bg = 'black')
colors = rainbow(3)
#考虑survival,!!须注意最后一天
ylarge = max(max(survival$t1/survival$t),max(survival$newt1/survival$newuv),max(survival$noldt1/survival$olduv))
plot(survival$dt,survival$t1/survival$t,col = colors[[1]],type = 'l',ylim = c(0,ceiling(ylarge)))
lines(survival$dt,survival$newt1/survival$newuv,col = colors[[2]],type = 'l')
lines(survival$dt,survival$noldt1/survival$olduv,col = colors[[3]], type = 'l')
legend('topright',c("survival","newsurvival","oldsurvival"),cex = 0.7,fill = colors,text.col= 'pink')
#需要加入坐标轴
title(main = '近两周次日留存率',xlab = 'dates',ylab = '留存率',col.main = 'blue',
      col.lab = 'purple')
axis(1,col = 'purple')
at = axTicks(1)
mtext(side = 1, text = at, at = at, col = "purple", line = 1)
axis(2,col = 'purple')
at = axTicks(2)
mtext(side = 2,text = at,at = at,col = 'purple',line = 1)

# umid_lujing_split_sql = "select * from test.umid_lujing_split"
# umid_lujing_split  = read_data_impala_general(umid_lujing_split_sql)
# setDT(umid_lujing_split)
# setnames(umid_lujing_split,"_c3","path")
# umid_lujing_split[,splitted_path:=str_split(path,"->")]
# temp_first_page = sapply(umid_lujing_split$splitted_path,`[[`,1)
# path_length = sapply(umid_lujing_split$splitted_path,length)
# temp_last_page = mapply(`[[`,umid_lujing_split$splitted_path,path_length)
# umid_lujing_split$first_page = temp_first_page
# umid_lujing_split$last_page = temp_last_page
# umid_lujing_split$path_length = path_length
# freq_first_page = umid_lujing_split[,.N,by=c("isnew","first_page")]
# freq_first_page = freq_first_page[order(isnew,N,decreasing = TRUE),]
# freq_last_page = umid_lujing_split[,.N,by=c("isnew","last_page")]
# freq_last_page = freq_last_page[order(isnew,N,decreasing = TRUE),]

library(tidyr)
# seperated_pages = strsplit(umid_lujing_split$path,"->")
# sep_pages_df = data.frame(seq = rep(1:length(seperated_pages),sapply(seperated_pages,length)),page = unlist(seperated_pages))
# table_basis2 = readxl::read_xlsx("~/data/table_basis.xlsx")
# table_basis2 = as.data.frame(table_basis2)
# sep_pages_df = merge(sep_pages_df,table_basis2,all.x = TRUE,by.x = "page",by.y = "page_name")
# sep_pages_df = sep_pages_df[order(sep_pages_df$seq),]
# depth_result = aggregate(depth~seq, data = sep_pages_df, FUN = max)
# depth_avg = aggregate(depth~seq,data = sep_pages_df, FUN = mean )
# umid_lujing_split$seq = 1:nrow(umid_lujing_split)
# umid_lujing_split = merge(umid_lujing_split,depth_result,by = "seq",all.x = TRUE) 
# avg_depth = aggregate(depth~isnew,data = umid_lujing_split,FUN = mean)
# avg_length = aggregate(path_length~isnew,data = umid_lujing_split,FUN = mean)


#1daily activity depth
depth_sql = paste0("select a.dt,isnew,a.p_channel,b.depth,count(1) from 
dm.dm_app_umid_step a  
left outer join 
test.pagelevel b on a.page_name_zh=b.page_name where length(b.depth)=1 and a.dt>'",datestart.str,"' group by a.dt,isnew,b.depth,a.p_channel")
depth = read_data_impala_general(depth_sql)
setDT(depth)
depth$depth = as.numeric(depth$depth)
avg_depth2 = depth[,.(avg_depth = sum(depth*expr_0)/sum(expr_0)),by = c("isnew","dt")][order(dt,isnew)]
avg_depth_dcast = dcast(avg_depth2,dt~isnew)
ymax = max(max(avg_depth_dcast$new),max(avg_depth_dcast$old))
depth_dates = avg_depth_dcast$dt
avg_depth_pic = avg_depth_dcast[,c(2,3)]
barplot(t(as.matrix(avg_depth_pic)),beside = TRUE,col = c("green","brown4"),axes = FALSE,ylim=c(0,ymax + 0.6))
axis(2)
axis(1,at=seq(from = 2,to = 38,length.out = length(depth_dates)),labels = depth_dates,tick=FALSE)
legend('topright',c("new user","old user"),cex = 0.7,fill = c("green","brown4"),text.col= 'blue')
title(main = '近两周新老用户访问平均深度',xlab = 'dates',ylab = '深度',col.main = 'blue',
      col.lab = 'purple')

avg_depth_total = depth[,.(avg_depth = sum(depth*expr_0)/sum(expr_0)),by = c("isnew")]
barplot(t(as.matrix(avg_depth_total[,-1])),beside = TRUE,col = c("brown4","green"),axes = FALSE,xlim=c(0,ymax + 0.6),horiz = TRUE)
legend('topright',c("new user","old user"),cex = 0.7,fill = c("green","brown4"),text.col= 'blue')
axis(1)
axis(2,at=c(1.5,3.5),labels = c("old","new"),tick = TRUE)
title(main = '近两周新老用户访问平均深度',xlab = '深度',ylab = '新老用户',col.main = 'blue',
      col.lab = 'purple')

#daily activity depth by person
depth_p_sql = paste0("select t.isnew,t.dt,sum(cast(u_depth as INT))/count(t.u_mid) avg_depth from 
(select a.dt,isnew,max(b.depth) as u_depth,a.u_mid from 
dm.dm_app_umid_step a  
left outer join 
test.pagelevel b on a.page_name_zh=b.page_name where length(b.depth)=1 and dt > '",datestart.str,"' group by a.dt,a.isnew,a.u_mid) t group by t.isnew,t.dt")
depth_p = read_data_impala_general(depth_p_sql)
depth_p = depth_p[order(depth_p$dt),]
p  = ggplot(depth_p,aes(dt,avg_depth))
p + geom_bar(aes(fill = isnew),stat = "identity",position = "dodge")

im_sql = paste0("select stat_date dt,split_part(send,'_',2) openid, 'im' type from 
dm.im_info a where split_part(send,'_',1)='1' and stat_date>='",datestart.str,"' and split_part(rec,'_',1)='2'")
im = read_data_impala_general(im_sql)

new_user_by_model_date_city_sql = 
paste0("select count(b.u_mid) numbers,firstdt,d_model,l_city from
(select f.u_mid,substr(firstonlinetime,1,10) firstdt,a.d_model,a.l_city from dl.umid_firstonlinetime f left join
  (select distinct u_mid,d_model,l_city from ods.ods_app_pageview_info where dt>= ",datestart.str,") a
  using(u_mid) where substr(firstonlinetime,1,10) >= '",as.character(datestart),"' 
  and f.dt = '",dateend.str,"') b group by firstdt,d_model,l_city")
new_user_by_model_date_city = read_data_impala_general(new_user_by_model_date_city_sql)
new_user_by_model_date_city = data.table(new_user_by_model_date_city)
new_user_by_model_date = new_user_by_model_date_city[d_model!="null" & str_trim(d_model)!="",.(numbers = sum(numbers)),by = c("d_model","firstdt")]
new_user_by_model = new_user_by_model_date_city[d_model!="null" & str_trim(d_model)!="",.(numbers = sum(numbers)),by = "d_model"][order(numbers,decreasing = T)]
model_list = new_user_by_model[1:100,]$d_model
new_user_by_model_date_top100 = new_user_by_model_date[d_model %in% model_list,]
new_user_by_model_date_top100$model = factor(new_user_by_model_date_top100$d_model,levels = model_list)
new_user_by_model_date_top100_ordered = new_user_by_model_date_top100[order(firstdt,model)]
new_user_by_model_date_ordered_reshape = reshape(new_user_by_model_date_top100_ordered, idvar = "model", timevar = "firstdt", direction = "wide")
new_user_by_model_date_ordered_reshape = dcast(new_user_by_model_date_top100_ordered, model ~ firstdt, value.var = "numbers")
new_user_by_model_date_ordered_reshape$sum = rowSums(new_user_by_model_date_ordered_reshape[,-1],na.rm = T)
melt_df_model_top20 = melt(new_user_by_model_date_ordered_reshape[1:20,-ncol(new_user_by_model_date_ordered_reshape)], id.vars = "model", measure.vars = 2:(ncol(new_user_by_model_date_ordered_reshape)-1), variable.name = "date", value.name = "value")
treemap(melt_df_model_top20,index = c("date","model"),vSize = "value")

new_user_by_model_date_city$l_city = str_replace(new_user_by_model_date_city$l_city,"市","")
new_user_by_city_dt = new_user_by_model_date_city[!l_city %in% c("null","局域网","未知"),.(numbers = sum(numbers)),by = c("l_city","firstdt")]
new_user_by_city = new_user_by_model_date_city[!l_city %in% c("null","局域网","未知"),.(numbers = sum(numbers)),by = "l_city"][order(numbers,decreasing = T),]
city_list = new_user_by_city[1:101,]$l_city
new_user_by_city_date_top100 = new_user_by_city_dt[l_city %in% city_list,]
new_user_by_city_date_top100$city = factor(new_user_by_city_date_top100$l_city,levels = city_list)
new_user_by_city_date_top100_ordered = new_user_by_city_date_top100[order(firstdt,city)]
new_user_by_city_date_ordered_reshape = dcast(new_user_by_city_date_top100_ordered, city ~ firstdt, value.var = "numbers")
new_user_by_city_date_ordered_reshape$sum = rowSums(new_user_by_city_date_ordered_reshape[,c(-1,-14)],na.rm = T)
melt_df_city_top20 = melt(new_user_by_city_date_ordered_reshape[1:20,-ncol(new_user_by_city_date_ordered_reshape)], id.vars = "city", measure.vars = 2:(ncol(new_user_by_city_date_ordered_reshape)-1), variable.name = "date", value.name = "value")
treemap(melt_df_city_top20,index=c("date","city"),vSize = "value")

channel_sql = paste0("select * from dl.dl_channel_umid_openid where substr(umid_firstonlinetime,1,10) > '",as.character(datestart),"' and dt = ",dateend.str)
new_user_channel_unique = read_data_impala_general(channel_sql)
colnames(new_user_channel_unique) = str_replace(colnames(new_user_channel_unique),"dl_channel_umid_openid.","")
# nrow(new_user_channel[!duplicated(new_user_channel$dl_channel_umid_openid.u_mid),])
# new_user_channel_unique = new_user_channel[!duplicated(new_user_channel$dl_channel_umid_openid.u_mid),]
new_user_channel_unique = data.table(new_user_channel_unique)
new_user_channel_unique$date = str_sub(new_user_channel_unique$umid_firstonlinetime,1,10)
new_user_by_channel_date = new_user_channel_unique[,.(numbers = .N),by = c("channel","date")]
new_user_by_channel = new_user_channel_unique[,.(numbers = .N),by = c("channel")][order(numbers,decreasing = T)]
channel_list = new_user_by_channel$channel
new_user_by_channel_date_top = new_user_by_channel_date
new_user_by_channel_date_top$channels = factor(new_user_by_channel_date_top$channel,levels = channel_list)
new_user_by_channel_date_top_ordered = new_user_by_channel_date_top[order(date,channels)]
new_user_by_channel_date_ordered_reshape = dcast(new_user_by_channel_date_top_ordered, channels ~ date, value.var = "numbers")
new_user_by_channel_date_ordered_reshape$sum = rowSums(new_user_by_channel_date_ordered_reshape[,c(-1)],na.rm = T)
melt_df_model_top20 = melt(new_user_by_channel_date_ordered_reshape[1:20,-ncol(new_user_by_channel_date_ordered_reshape)], id.vars = "channels", measure.vars = 2:(ncol(new_user_by_channel_date_ordered_reshape)-1), variable.name = "date", value.name = "value")
treemap(melt_df_channel_top20,index=c("date","channels"),vSize = "value")


online_order_sql = paste0("select count(distinct purchaser_id) as o_num,substr(b.create_date,1,10) as dt from
(select purchaser_id,create_date from ods.ods_tx_order_tx_order_dt o inner join ods.ods_app_pageview_info a on 
                          o.purchaser_id = a.u_id where substr(o.create_date,1,10)> '",datestart,"' and o.order_status not in (1,7,19) and o.order_type=1) b
                          group by substr(b.create_date,1,10) order by substr(b.create_date,1,10)")
online_order = read_data_impala_general(online_order_sql)

total_order_sql = paste0("select count(distinct purchaser_id) as t_num,substr(b.create_date,1,10) as dt from
(select purchaser_id,create_date from ods.ods_tx_order_tx_order_dt o where substr(o.create_date,1,10)> '",datestart,"' and o.order_status not in (1,7,19) and o.order_type=1) b
                         group by substr(b.create_date,1,10) order by substr(b.create_date,1,10)")
total_order = read_data_impala_general(total_order_sql)

online_book_sql = paste0("select substr(d.create_date,1,10) dt,count(distinct d.openid) b_num from (select b.create_date,c.openid from
                   ods.ods_jz_business_jz_activity_user_dt b inner join 
ods.ods_db_user_center_users_dt c on b.user_mobile=c.mobile
where b.is_del=0 and substr(create_date,1,10)>='",datestart,"') d
group by substr(d.create_date,1,10) order by substr(d.create_date,1,10)")
online_book = read_data_impala_general(online_book_sql)

coupon_sql = paste0("select substr(d.create_time,1,10) dt,count(distinct d.open_id) c_num 
from (select create_time,open_id from ods.ods_marketing_center_mmc_user_coupon_dt where 
to_date(create_time)>='",datestart,"'
and channel_id not in (2,4) and open_id!='') d
group by substr(d.create_time,1,10) order by substr(d.create_time,1,10)")
coupon = read_data_impala_general(coupon_sql)


