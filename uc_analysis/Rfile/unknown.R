sql = "select count(*) numbers,u_phone from
(select c.u_mid,a.l_city,c.channel,substr(c.umid_firstonlinetime,1,10) firstdt,a.* from dl.dl_channel_umid_openid c left join 
(select * from 
(select *,ROW_NUMBER() OVER (
PARTITION BY u_mid,l_city
ORDER BY dt) as level from ods.ods_app_pageview_info where dt>= 20171111) t where t.level = 1) a
using(u_mid) where substr(c.umid_firstonlinetime,1,10) >= '2017-11-11' and c.dt = 20171112 and l_city in ('泉州','泉州市')) g group by u_phone"

source('~/Rfile/R_impala.R')
uphone = read_data_impala_general(sql)

sql = "select count(*) numbers,l_ip from
(select c.channel,substr(c.umid_firstonlinetime,1,10) firstdt,a.* from dl.dl_channel_umid_openid c left join 
(select * from 
(select *,ROW_NUMBER() OVER (
PARTITION BY u_mid,l_city
ORDER BY dt) as level from ods.ods_app_pageview_info where dt>= 20171111) t where t.level = 1) a
using(u_mid) where substr(c.umid_firstonlinetime,1,10) >= '2017-11-11' and c.dt = 20171112 and l_city in ('泉州','泉州市')) g
group by l_ip"

uphone = read_data_impala_general(sql)

library(cowplot)
ggdraw()+draw_plot(p,x=0,y=0,width=.5,height = .5)

vp <- viewport(width = 0.4, height = 0.4, x = 1,
               y = unit(0.7, "lines"), just = c("right","bottom"))
full <- function() {
  print(mainplot)
  print(subplot, vp = vp)
}
full()
