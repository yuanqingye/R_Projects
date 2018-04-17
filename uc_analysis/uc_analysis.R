-- 建立字典表
"CREATE TABLE test.path_dict
AS
SELECT DISTINCT split(b.value,' ')[0] AS path, 
split(c.value,' ')[0] AS p_domain,
split(d.value,' ')[0] AS p_channel,
a.page_mark,
a.page_name_en AS p_type,
a.p_item,a.page_name_zh as page_name
FROM dl.dl_data_point_rule_t_rule_dt a 
LEFT JOIN dl.dl_data_point_rule_t_code_dt b 
ON a.path_type_value=b.id AND b.status=1 
LEFT JOIN dl.dl_data_point_rule_t_code_dt c
ON a.p_domain_type_value=c.id AND c.status=1 
LEFT JOIN dl.dl_data_point_rule_t_code_dt d 
ON a.p_channel_type_value=d.id AND d.status=1 
WHERE a.status=1 -- AND split(b.value,' ')[0] in ('f','p')
ORDER BY path,p_domain,p_channel,p_type"

# 关联得出包含中文名的数据
"create table test.uc_record_to_531 AS
select page_name,p.p_type,p.path,p.p_item,p.p_channel,dt,app_v,service,
p_stay_time,p_live_time,d_os,ts,p_url,page_mark,u_mid from 
ods.ods_app_pageview_info p inner join test.path_dict d
on p.p_type = d.p_type and p.page = d.page_mark and p.path = d.path and 
p.p_item = d.p_item where p.app_v in ('2.0.4','2.0.5','2.0.6') and 
p.p_domain = 'mmall.com' and 
(p.p_type like '%user' or page_name like '%中心%' or p.p_type like '%.uc.%') and 
dt between 20170508 and 20170531"
-- 数据清洗
uc_record_to_531_copy = uc_record_to_531
uc_record_to_531[,value :=1]

-- 各个页面 #pv uv
uc_record_pv_by_pagename = uc_record_to_531[,.(pv = .N),by=c('page_name','p.p_type','p.path')]
uc_record_pv_by_pagename_p = uc_record_to_531[p.path=='p',.(pv = sum(value)),by=c('page_name','p.p_type')]
uc_record_pv_by_pagename_f = uc_record_to_531[p.path=='f',.(pv = sum(value)),by=c('page_name','p.p_type')]
uc_record_pv_by_pagename_sorted = uc_record_pv_by_pagename[order(uc_record_pv_by_pagename$pv,decreasing = T),]
uc_record_pv_by_pagename_p_sorted = uc_record_pv_by_pagename_p[order(uc_record_pv_by_pagename_p$pv,decreasing = T),]
uc_record_pv_by_pagename_f_sorted = uc_record_pv_by_pagename_f[order(uc_record_pv_by_pagename_f$pv,decreasing = T),]


uc_record_to_531_user = unique(uc_record_to_531,by = c('page_name','u_mid','p.p_type','p.path','p.p_item','p.p_channel'))
uc_record_uv_by_pagename_p = uc_record_to_531_user[p.path=='p',.(uv = .N),by = c('page_name','p.p_type')]
uc_record_uv_by_pagename_f = uc_record_to_531_user[p.path=='f',.(uv = .N),by = c('page_name','p.p_type')]
uc_record_uv_by_pagename_p_sorted = uc_record_uv_by_pagename_p[order(uc_record_uv_by_pagename_p$uv,decreasing = T),]
uc_record_uv_by_pagename_f_sorted = uc_record_uv_by_pagename_f[order(uc_record_uv_by_pagename_f$uv,decreasing = T),]


#pv 作图
pv_pic = uc_record_pv_by_pagename_p_sorted[1:7,]
# library(plotrix)
# pie(pv_pic$pv,labels=pv_pic$page_name,main="用户中心各主要页面pv")
cols=rainbow(length(page_name))
barplot(pv_pic$pv,names.arg = page_name,col = cols,xlab = '页面名称',ylab='pv值',main='pv值分页面统计')

#uv 作图
uv_pic = uc_record_uv_by_pagename_p_sorted[1:7,]
barplot(uv_pic$uv,names.arg = page_name,col = cols,xlab = '页面名称',ylab='uv值',main='uv值分页面统计')

--总跳转率
rate_uc2mc = uc_record_pv_by_pagename_p[page_name=='个人中心首页',pv]/uc_record_pv_by_pagename_p[page_name=='个人中心首页-会员中心主页',pv]
rate_mc2uc = 1/rate_uc2mc

mc_pv = uc_record_pv_by_pagename_p[page_name=='个人中心首页-会员中心主页',pv]
uc_pv = uc_record_pv_by_pagename_p[page_name=='个人中心首页',pv]

pie3D(c(mc_pv,(uc_pv-mc_pv)),labels = c(paste('会员中心点击数',100*(round(rate_mc2uc,3)),'%'),'用户中心点击数'),main = '跳转率pv')

mc_uv = uc_record_uv_by_pagename_p[page_name=='个人中心首页-会员中心主页',uv]
uc_uv = uc_record_uv_by_pagename_p[page_name=='个人中心首页',uv]

pie3D(c(mc_uv,(uc_uv-mc_uv)),labels = c(paste('会员中心点击人数',100*(round(mc_uv/uc_uv,3)),'%'),'用户中心点击人数'),main = '跳转率uv')

-- 用户任务完成情况
# DELIVERY_ADDRESS
# 803
# FIRST_COMMENT
# 22
# FIRST_SHOPPING
# 132274
# PERSONAL_INFO
# 1933
# select * from dl.fct_ordr where date_id between date('2017-05-08') and date('2017-05-31') and sale_type = 'C端' and ordr_status not in (1,7,19);

task_complete = data.table(task_name = c('地址完善','第一次评论','第一次购物','个人信息完善'),num = c(803,22,32,1933))
colors <- c("green","orange","brown","blue")
barplot(task_complete$num,main="任务完成状态",names.arg=task_complete$task_name,xlab="taskname",ylab="number",col=colors)

-- 重返情况
uc_record_main = uc_record_to_531[page_name == '个人中心首页',]
uc_record_pagecount_by_user = uc_record_main[,.(count = .N),by = c('u_mid')]
uc_record_pagecount_by_user_date = uc_record_main[,.(count = .N),by = c('u_mid','dt')]
uc_record_comeback = uc_record_pagecount_by_user_date[,.(count = .N),by = c('u_mid')]
not_comeback_num = nrow(uc_record_comeback[count == 1,])#20919
total_num = nrow(uc_record_comeback) #24264
comeback_num = total_num - comeback_num
#13.8% 重返率
#做饼图
pie3D(c(comeback_num,not_comeback_num),labels = c('重返用户中心用户13.8%','不重返用户中心用户86.2%'),main = '重返率饼状图')

--平均时长
uc_record_stay_time = uc_record_to_531[!is.na(p_stay_time),]
uc_record_stay_time[,p_stay_time := as.numeric(p_stay_time)]
uc_record_stay_time_summarize = uc_record_stay_time[,.(totaltime = sum(p_stay_time)/1000,count=.N),by = c('page_name')]
uc_record_stay_time_avg = uc_record_stay_time_summarize[,avg:=totaltime/count]
cols = rainbow(nrow(uc_record_stay_time_avg))
page_name = c('我的福利包页面','会员中心主页','会员等级页面','积分兑换页面','会员权益页面')
barplot(uc_record_stay_time_avg$avg,main="页面访问平均时长(秒) ",names.arg=page_name,xlab="页面名称",ylab="停留时长",col=cols)

-- 每日签到数
# uc_sign = read_xlsx('~/data/sign.xlsx')
# uc_sign = data.table(uc_sign)
  uc_sign_bydate = setorder(uc_sign[,.(count=.N),by = 'sign_date'],sign_date)
#  uc_sign_byuser = setorder(uc_sign[,.(count=.N),by = 'user_id'],-count)
  注意没有任何一个人签到过两次

#使用线点结合作图
plot(y=uc_sign_bydate_may$count,x=11:31,type = 'h',main='每日签到数',xlab = '日期',ylab='签到人数',col='red',lwd = 10)

-- 权益获取
uc_rights = read_xlsx('~/data/rights.xlsx')
uc_rights = data.table(uc_rights[1:6,c('rights_name','repertory_total','already_use_total')])
uc_rights[,already_use_total:=as.numeric(already_use_total)]
uc_rights[,repertory_total:=as.numeric(repertory_total)]
uc_rights[,rate :=  already_use_total/repertory_total]
#打算使用堆积柱表
rights_name = c('床垫除螨上海','床垫除螨北京','爱奇异50元抵用券','爱奇异五折','电影折扣','地板保养上海')
Values = t(as.matrix(uc_rights[,c('repertory_total','already_use_total')]))
row3 = Values[1,]-Values[2,]
Values = rbind(Values,row3)
Values = Values[c(3,2),]
colors = c('green','purple')
usage= c('未使用','已使用')
barplot(Values,main="权益领取",names.arg=rights_name,xlab="权益名称",ylab="数量",col=colors)
legend("topleft", usage, cex=1.3, fill=colors)

-- 积分兑换礼物
uc_gifts = read_xlsx('~/data/gifts.xlsx')
uc_gifts_repertory = read_xlsx('~/data/gifts_repertory.xlsx')
uc_gifts_wide = merge(uc_gifts,uc_gifts_repertory,by.x='id',by.y='gift_id')
uc_gifts_wide_copy = uc_gifts_wide
uc_gifts_wide = data.table(uc_gifts_wide)
uc_gifts_wide[,use_start_time:=as.Date(use_start_time)]
uc_gifts_wide[,use_end_time:=as.Date(use_end_time)]
uc_gifts_check = uc_gifts_wide[use_start_time!='\\N--hive-import',]
# uc_gifts_use = uc_gifts_wide_copy[already_use_total>0,]
uc_gifts_use = uc_gifts_check[,c('name','specification','cost_score','use_start_time','use_end_time','quantity','already_use_total')]
uc_gifts_use[,already_use_total := as.numeric(already_use_total)]
uc_gifts_use[,quantity:=as.numeric(quantity)]
uc_gifts_use[,rate:=already_use_total/quantity]
#打算使用堆积柱表
gifts_name = c('12日发放电影券','26日发放电影券')
Values2 = t(as.matrix(uc_gifts_pick[,c('quantity','already_use_total')]))
Row3 = Values2[1,]-Values2[2,]
Values2 = rbind(Values2,Row3)
Values2 = Values2[c(3,2),c(2,1)]
colors = c('green','purple')
usage2= c('未兑换','已兑换')
barplot(Values2,main="积分兑换礼物",names.arg=gifts_name,xlab="礼物名称",ylab="数量",col=colors)
legend("topleft", usage2, cex=1.3, fill=colors)