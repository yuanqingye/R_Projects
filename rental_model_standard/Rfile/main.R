library(readxl)
library(data.table)
library(pinyin)
library(geosphere)
# Read info about the mall
redstar_points = read_xlsx("~/data/selfmanaged_business.xlsx",sheet = '商场总名单')
redstar_points = data.table(redstar_points)
redstar_points = redstar_points[商场类型=='自营',]
redstar_location = redstar_points[,c('商场名称','city','longitude','latitude')]
colnames(redstar_location)[[1]] = 'mall_name'
redstar_result = redstar_location #redstar_result don't have an order
city = unique(redstar_location$city)
city = paste0(city,"市")

#find the closest subways around the mall
source("~/R_Projects/rental_model_standard/Rfile/functions.R")
Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
closest_subway = find_closest_subways(redstar_location,place_key)
subway_distance = distHaversine(redstar_location[,c('longitude','latitude')],closest_subway[,c('X2','X1')])
# redstar_location = cbind(redstar_location,subway_distance)
result_subway = cbind(mall_name = redstar_location$mall_name,subway_distance)
redstar_result = merge(redstar_result,result_subway,by = "mall_name")

#get the mall info from the dianping web scrap
source("~/R_Projects/page_destructure/Rfile/Rvest.R")
Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")
Sys.unsetenv("ftp_proxy")
city_list = unique(redstar_points$city)
city_list_pinyin = lapply(city_list,pinyin,method = "toneless", sep = "")
city_list_pinyin[[9]] = "eerduosi"
city_list_pinyin[[14]] = "huhehaote"
city_list_pinyin[[16]] = "kunming"
city_list_pinyin[[17]] = "hefei"
city_list_pinyin[[19]] = "wuxi"
city_list_pinyin[[20]] = "chengdu"
city_list_pinyin[[21]] = "shenyang"
city_list_pinyin[[23]] = "yantai"
city_list_pinyin[[29]] = "nanjing"
city_list_pinyin[[32]] = "jinan"
city_list_pinyin[[34]] = "xiamen"
city_list_pinyin[[38]] = "shenzhen"

city_pinyin_df = data.frame(city_name = city_list,city_pinyin = unlist(city_list_pinyin))
business_region = dianping_scrapper2(city_list_pinyin)
business_region_final = merge(business_region,city_pinyin_df,by.x = "city",by.y = "city_pinyin")
pv = paste0(business_region_final$city_name,business_region_final$region)
business_region_final$location = pv

# geocode only support 2500 connection
commerce_cord_ch = list()
commerce_cord_ch[[1]] = lapply(pv[1:2000],geocode)
commerce_cord_ch[[2]] = lapply(pv[2001:4000],geocode)
commerce_cord_ch[[3]] = lapply(pv[4001:4500],geocode)
commerce_cord_ch[[4]] = lapply(pv[4501:length(pv)],getSimplifiedGeoData)
# The rest 
commerce_cord_ch[[1]] = rbindlist(commerce_cord_ch[[1]])
commerce_cord_ch[[2]] = rbindlist(commerce_cord_ch[[2]])
commerce_cord_ch[[3]] = rbindlist(commerce_cord_ch[[3]])
# commerce_cord_ch[[3]] = commerce_cord_ch[[3]][1:500]
commerce_cord_ch[[4]] = rbindlist(commerce_cord_ch[[4]])
colnames(commerce_cord_ch[[4]]) = c("lat","lon","lat2","lon2")
commerce_cord_result = rbindlist(commerce_cord_ch[1:4],fill = TRUE)
commerce_cord_result = cbind(pv,commerce_cord_result)
extra_pv = commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),]$pv
# The rest na part need to be refilled
commerce_cord_ch[[5]] = lapply(extra_pv,getSimplifiedGeoData)
commerce_cord_ch[[5]] = rbindlist(commerce_cord_ch[[5]])
colnames(commerce_cord_ch[[5]]) = c("lat","lon","lat2","lon2")
commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[5]][,c("lat","lon","lat2","lon2")]
extra_pv2 = commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),]$pv
commerce_cord_ch[[6]] = lapply(extra_pv2,getDaodeGeoData)
commerce_cord_ch[[6]] = rbindlist(commerce_cord_ch[[6]])
commerce_cord_ch[[6]] = cbind(commerce_cord_ch[[6]],lat2 = NA,lon2 = NA)
# a = lapply(commerce_cord_ch[[6]],as.character)
# a = lapply(a,as.numeric)
# commerce_cord_ch[[6]] = as.data.frame(a)
commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[6]][,c("lat","lon","lat2","lon2")]
extra_pv3 = commerce_cord_result[str_detect(commerce_cord_result$pv,"其他$"),]$pv
extra_pv3 = str_replace_all(extra_pv3,"其他","中心")
commerce_cord_ch[[7]] = lapply(extra_pv3,getDaodeGeoData)
commerce_cord_ch[[7]] = rbindlist(commerce_cord_ch[[7]])
commerce_cord_ch[[7]] = cbind(commerce_cord_ch[[7]],lat2 = NA,lon2 = NA)
commerce_cord_result[str_detect(commerce_cord_result$pv,"其他$"),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[7]][,c("lat","lon","lat2","lon2")]
commerce_cord_final_result = commerce_cord_result[,c("pv","lon","lat")]
commerce_cord_final_result = cbind(city = business_region_final$city_name,commerce_cord_final_result)
commerce_cord_final_result$city = as.character(commerce_cord_final_result$city)
commerce_cord_final_result = merge(redstar_location,commerce_cord_final_result,by = "city",allow.cartesian=TRUE)
commerce_cord_final_result$commerce_distance = distHaversine(commerce_cord_final_result[,c('longitude','latitude')],commerce_cord_final_result[,c('lon','lat')])
commerce_distance_df = commerce_cord_final_result[,.(commerce_distance = min(commerce_distance)),by = "mall_name"]
redstar_result = merge(redstar_result,commerce_distance_df,by = "mall_name")
redstar_result$mall_name = factor(redstar_result$mall_name,levels = redstar_location$mall_name,ordered = TRUE)
redstar_result = redstar_result[order(redstar_result$mall_name),]

# shop_commerce_count = commerce_cord_final_result[commerce_distance<2500,.N,by = mall_name]
shop_commerce_count = commerce_cord_final_result[,.(distance_commerce_in_2500 = sum(commerce_distance<2500)),by = mall_name]
redstar_result = merge(redstar_result,shop_commerce_count,by = "mall_name")

closest_road_list = getClosestRoadPointFromDF(redstar_location)
closest_road_df = rbindlist(closest_road_list)
closest_road_distance = distHaversine(redstar_location[,c('longitude','latitude')],closest_road_df[,c('lng','lat')])
closest_road_distance_df = cbind(mall_name = redstar_location$mall_name,closest_road_distance)
redstar_result = merge(redstar_result,closest_road_distance_df,by = "mall_name")

district_location_list = lapply(city_list,getGaoDeDistrictInfo)
district_location_df = rbindlist(district_location_list)
district_lat_lon_matrix = str_split_fixed(district_location_df$center_location,",",2)
class(district_lat_lon_matrix) = "numeric"
district_location_df$lon = district_lat_lon_matrix[,1]
district_location_df$lat = district_lat_lon_matrix[,2]
redstar_district_mixed_df = merge(redstar_location,district_location_df,by = "city",allow.cartesian=TRUE)
redstar_direction_df = list()
redstar_direction_df[[1]] = getDirectionInfoFromDF(redstar_district_mixed_df[1:40,])
redstar_direction_df[[2]] = getDirectionInfoFromDF(redstar_district_mixed_df[41:80,])
for(i in 2:20){
  redstar_direction_df[[i+1]] = getDirectionInfoFromDF(redstar_district_mixed_df[(40*i+1):(40*(i+1)),])
}
# redstar_direction_df_copy = redstar_direction_df
redstar_direction_final_df = rbindlist(redstar_direction_df)
redstar_direction_final_df = redstar_direction_final_df[1:835,]

redstar_four_direction_df = makeFourDirectionFromDF(redstar_location)
redstar_four_direction_mixed_df = merge(redstar_location,redstar_direction_df,by = "mall_name",allow.cartesian=TRUE)
redstar_four_direction_mixed_df = as.data.frame(redstar_four_direction_mixed_df)
redstar_four_direction_df = list()
for(i in 1:7){
  redstar_four_direction_df[[i]] = getDirectionInfoFromDF(redstar_four_direction_mixed_df[(40*(i-1)+1):(40*i),])
}
redstar_four_direction_final_df = rbindlist(redstar_four_direction_df)
redstar_four_direction_final_df = redstar_four_direction_final_df[1:260,]

combined_highway_raw_df = rbind(redstar_direction_final_df,redstar_four_direction_final_df)
combined_highway_raw_dt = data.table(combined_highway_raw_df)
# highway_shortest_distance_result = combined_highway_raw_dt[,.(distance = min(highway_distance)),by = "start_point"]
shortest_distance_result = combined_highway_raw_dt[,.(highway_distance = min(highway_distance),district_center_distance = min(distance,na.rm = TRUE)),by = "start_point"]
redstar_result = merge(redstar_result,shortest_distance_result,by.x = "mall_name",by.y = "start_point")

source("~/Rfile/R_impala.R")
community_data_sql = "select id,city,province,area,name,address,plate,areamonut,roommount,buildingdate,longitude,latitude,buildingamount,pricesection,ownermallname from ods.ods_db_dragoneye_xiwa_redstar_community_dt"
community_data = read_data_impala_general(community_data_sql)
#using string bfore "市" to do the check
community_data$join_city = ifelse(!str_detect(community_data$city,"(\\w+)市$"),community_data$city,str_extract(community_data$city,"(\\w+)(?=市)"))
redstar_community_mixed_df = merge(y=redstar_location,x=community_data,by.y = "city",by.x = "join_city",all.y = TRUE)
redstar_community_distance = distHaversine(redstar_community_mixed_df[,c('longitude.x','latitude.x')],redstar_community_mixed_df[,c('longitude.y','latitude.y')])
redstar_community_distance = cbind(redstar_community_mixed_df[,c("city","province","mall_name","area","name","address","plate","roommount","pricesection")],distance = redstar_community_distance)
redstar_community_distance = data.table(redstar_community_distance)
# redstar_community_5km_number = redstar_community_distance[,sum(distance<=5000,na.rm = TRUE),by = mall_name]
# redstar_community_50km_number = redstar_community_distance[,sum(distance<=50000,na.rm = TRUE),by = mall_name]
redstar_community_5km_roominfo = redstar_community_distance[distance<=5000,.(communitynum = .N,roomnum = sum(roommount,na.rm = TRUE),roomnumavailablerate = sum(roommount!=0)/.N,pricemorethan50k = sum(pricesection>50000)/sum(pricesection!=0),pricemorethan10k = sum(pricesection>10000)/sum(pricesection!=0)),by = mall_name]
redstar_community_50km_roominfo = redstar_community_distance[distance<=50000,.(communitynum = .N,roomnum = sum(roommount,na.rm = TRUE),roomnumavailablerate = sum(roommount!=0)/.N,pricemorethan50k = sum(pricesection>50000)/sum(pricesection!=0),pricemorethan10k = sum(pricesection>10000)/sum(pricesection!=0)),by = mall_name]
redstar_community_5km_roomavailable = redstar_community_distance[distance<=5000,.(totalprice = sum(roommount*pricesection,na.rm = TRUE),totalnum = sum(.SD[(roommount*pricesection!=0),roommount])),by = mall_name]
redstar_community_5km_roomavailable[,avg_price:=totalprice/totalnum]
community_data_focus = community_data[community_data$city %in% city,]
community_data_focus = data.table(community_data_focus)

library(readxl)
rent_original = read_xlsx("C:/Users/qingye.yuan/Desktop/工作需要/租金定价/租金定价模型二期数据.xlsx")
mall_name = rent_original$MALL_NAME
rent_original[] = lapply(rent_original,as.numeric)
rent_original$MALL_NAME = mall_name
rent_original$avg_price = rent_original$ZX_PRICE/rent_original$RENT_AREA_WA
