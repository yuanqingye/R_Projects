library(plotrix)
library(data.table)
dateend = Sys.Date()-1
datestart = Sys.Date()-14
datestart.str = format(datestart,'%Y%m%d')
dateend.str = format(dateend,'%Y%m%d')
dates = datestart.str:dateend.str
source('~/Rfile/R_impala.R')
query1 = paste0("select * from dm.dm_totaldailyreport where dt>=",datestart.str," and dt<=",dateend.str)
total_daily_report = read_data_impala_general(query1)
total_daily_report = data.table(total_daily_report)
setorder(total_daily_report,cols = 'dt')

ldigits = nchar(median(total_daily_report$tuv))-1
rdigits = nchar(median(total_daily_report$tpv))-1
lrange = range(total_daily_report$tuv)+c(-`^`(10,ldigits),`^`(10,ldigits))
rrange = range(total_daily_report$tpv)+c(-`^`(10,rdigits),`^`(10,rdigits))

par(bg = 'black')
twoord.plot(dates,total_daily_report$tuv,dates,total_daily_report$tpv,ylab='uv',
            rylab = 'pv',xlab = 'date',main = "近两周流量情况",
            lylim = lrange,rylim = rrange,
            lcol='green',
            rcol ='red'
)

legend('topleft',legend = c('uv','pv'),fill = c('green','red'),text.col = 'pink',cex = 1.5)

axis(1,col = 'blue')
at = axTicks(1)
mtext(side = 1, text = at, at = at, col = "blue", line = 1)

title(main = '近两周流量情况',col.main = 'blue')

avgpv = round(mean(total_daily_report$tpv))
avguv = round(mean(total_daily_report$tuv))
maxpv = max(total_daily_report$tpv)
maxuv = max(total_daily_report$tuv)
text(dates[7],mean(c(lrange[2],maxuv)),paste0('本周UV峰值',maxuv,' 均值',avguv,
      ' 本周PV峰值',maxpv,' 均值',avgpv),col = 'orange')

# plot(x=dates,y=total_daily_report$tuv,ylim = c(0,10000),xlab = 'date',ylab = 'uv',
#      type = 'o',col = 'blue',main = "近两周流量情况",pch = 3)
# lines(x=dates,y=total_daily_report$tpv/10,col = 'red',pch = 4,type = 'b')

query2 = paste0("select * from dm.dm_totaldailyactive where dt>=",datestart.str,
               " and dt<=",dateend.str)
total_daily_active = read_data_impala_general(query2)
total_daily_active = data.table(total_daily_active)
setorder(total_daily_active,cols = 'dt')

col_num = ncol(total_daily_active)
total_daily_active$sum = rowSums(total_daily_active[,-col_num,with = F])
digits = nchar(round(median(total_daily_active$sum)))-1
range = range(total_daily_active$sum)+c(-`^`(10,digits),`^`(10,digits))
total_daily_active_draw = total_daily_active
total_daily_active_draw[,c('marketpromotion','offlinemarket','dt'):=NULL,]
colors = rainbow(6)
par(bg = 'black')
plot(x=dates,y=total_daily_active_draw[,1][[1]],ylim = c(0,range[2]),
     type = 'o',col = colors[[1]],pch = 3,axes = FALSE)
lines(x=dates,y=total_daily_active_draw[,2][[1]],type = 'b',col = colors[[2]],pch = 4)
lines(x=dates,y=total_daily_active_draw[,3][[1]],type = 'b',col = colors[[3]],pch = 5)
lines(x=dates,y=total_daily_active_draw[,4][[1]],type = 'b',col = colors[[4]],pch = 6)
lines(x=dates,y=total_daily_active_draw[,5][[1]],type = 'b',col = colors[[5]],pch = 7)
lines(x=dates,y=total_daily_active_draw[,6][[1]],type = 'b',col = colors[[6]],pch = 8)

legend('topleft',colnames(total_daily_active_draw),cex = 1.3,fill = colors,text.col= 'pink')

#需要加入坐标轴
title(main = '近两周激活情况',xlab = 'dates',ylab = '激活数',col.main = 'blue',
      col.lab = 'purple')
axis(1,col = 'purple')
at = axTicks(1)
mtext(side = 1, text = at, at = at, col = "purple", line = 1)

axis(2,col = 'purple')
at = axTicks(2)
mtext(side = 2,text = at,at = at,col = 'purple',line = 1)


#计算激活指标
lastweek_total_active = sum(total_daily_active[1:7,'sum'])
thisweek_total_active = sum(total_daily_active[8:14,'sum'])
link_relative_ratio_active = round(thisweek_total_active/lastweek_total_active,2)
#上周激活数8427，本周激活数14838，周环比76%
text(dates[7],mean(c(range[2],max(total_daily_active$sum))),
     paste0('上周激活数   ',lastweek_total_active,'   本周激活数   ',thisweek_total_active,'   周环比   ',link_relative_ratio_active*100,'%'),col = 'orange')

query3 = paste0("select * from dl.openid_firstonlinetime where dt='",format(Sys.Date()-1,'%Y%m%d'),"' and substring(firstonlinetime,1,10)>='",format(Sys.Date()-14,'%Y-%m-%d'),"' and substring(firstonlinetime,1,10)<='",format(Sys.Date()-1,'%Y-%m-%d'),"'")
user_registry = read_data_impala_general(query3)
user_registry = data.table(user_registry)
setorder(user_registry,cols = 'firstonlinetime')
user_registry[,date:=substr(firstonlinetime,1,10)]
user_registry[,date:=as.Date(date,format = '%Y-%m-%d')]
user_registry_count = user_registry[,.N,by = 'date']
par(bg = 'black')
plot(dates,user_registry_count$N,type = 'b',col= 'red',xlab = 'dates',
     ylab = '注册数',axes = FALSE,lwd = 2,pch = '*')

axis(1,col = 'blue')
at = axTicks(1)
mtext(side = 1, text = at, at = at, col = "blue", line = 1)

axis(2,col = 'blue')
at = axTicks(2)
mtext(side = 2, text = at, at = at, col = "blue", line = 1)

title(main = '近两周新注册数',xlab = 'dates',ylab = '注册数',col.main = 'red',
      col.lab = 'blue')

#计算注册指标
lastweek_total_registry = sum(user_registry_count[1:7,'N'])
thisweek_total_registry = sum(user_registry_count[8:14,'N'])
link_relative_ratio_registry = round(thisweek_total_registry/lastweek_total_registry,2)

#上周新增注册数2003 ，本周新增注册数7592，周环比279%
text(dates[7],max(user_registry_count$N),
     paste0('上周新增注册数  ',lastweek_total_registry,'  本周新增注册数 ',thisweek_total_registry,' 周环比 ',link_relative_ratio_registry*100,'%'),col = 'orange')

