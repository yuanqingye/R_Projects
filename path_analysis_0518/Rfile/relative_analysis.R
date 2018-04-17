source('~/Rfile/R_impala.R')

query = "select u_mid,path,page,p_item,p_channel,p_type,dt from ods.ods_app_pageview_info where dt between 20170615 and 20170617
and p_domain = 'mmall.com' and path = 'p' and p_type not in ('page.closedown','page.wakeup','page.activate.main') and 
service in ('h5.pvuv','ios.pvuv','android.pvuv') and substr(u_mid,1,2)!='a_' and l_city!='≤‚ ‘' and l_ip!='210.13.91.146'"
page_view = read_data_impala_general(query)
page_view = data.table(page_view)

dict_query = "select * from test.path_dict where path = 'p'"
dict2 = read_data_impala_general(dict_query)
dict2 = data.table(dict2)

page_view_with_dict = merge(page_view,dict2,by.x = c('page','p_channel','p_type'),
                            by.y = c('page_mark','p_channel','p_type'))
page_view_raw = page_view_with_dict[,c('p_channel','p_type','u_mid','p_domain','page_name')]
page_view_count = page_view_raw[,.N,by = c('u_mid','page_name')]
setorder(page_view_count,u_mid,page_name)
page_view_count_group = aggregate(page_name~u_mid,data = page_view_count,paste,sep = '')
page_view_group_unique = page_view_count_group[!duplicated(page_view_count_group$page_name),]

