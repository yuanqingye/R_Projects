library(readxl)
library(data.table)
library(pinyin)
library(geosphere)
library(openxlsx)
#mall code is key info for updating, need to make sure new info in the excel
mall_code = m$商场代码
options(timeout= 4000000)

addupdate = function(mall_code = NULL){
  #this programm need to be run after the main programm
  load("redstar_result.RData")
  redstar_location_update <<- getRedstarMallLocationSet(mallcode = mall_code) 
  # city_update = unique(redstar_location_update$city)
  # city_update = paste0(city_update,"市")
  #find the closest subways around the mall(need update cause new subways get build fast)
  source("~/R_Projects/rental_model_standard/Rfile/functions.R",encoding = "utf-8")
  #put in the proxy setting in R
  redstar_result_update <<- redstar_location_update
  city_pinyin_update <<- getCityPinyinList(redstar_location_update)
  business_region_update <<- scrap_business_info(city_pinyin_update)
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  redstar_result_update <<- calSubwayDistance(redstar_location_update,redstar_result_update)
  calBusinessDistanceGeocode(business_region_update$location)
  commerce_cord_result_update <<- calBusinessDistanceAPI(business_region_update$location)
  redstar_result_update <<- calBusinessDistance(commerce_cord_result_update,redstar_location_update,redstar_result_update)
  redstar_result_update <<- calClosestWayDistance(redstar_location_update,redstar_result_update)
  redstar_result_update <<- calHighWayDistance(redstar_location_update,redstar_result_update)
  redstar_result_update <<- calCommunityInfo(redstar_location_update,redstar_result_update)
  redstar_result_update <<- data.frame(redstar_result_update)
  if(!exists('redstar_result')){
    redstar_result = redstar_result_update
  }
  else{
    redstar_result = data.frame(redstar_result)
    for (mallname in redstar_location_update$mall_name) {
      if (mallname %in% redstar_result$mall_name) {
        for (colname in colnames(redstar_result)) {
          # print(redstar_result[[redstar_result$mall_name == mallname,colname]])
          redstar_result[[which(redstar_result$mall_name == mallname), colname]] = redstar_result_update[[which(redstar_result_update$mall_name == mallname), colname]]
        }
      }
      else{
        redstar_result = rbind(redstar_result, redstar_result_update[redstar_result_update$mall_name == mallname,])
      }
    }
  }
  save(redstar_result,file = "redstar_result.RData")
  write.xlsx(redstar_result,"~/data/redstar_result.xlsx")
  return(redstar_result)
    }

getRedstarMallLocationSet = function(default_path = "~/data/selfmanaged_business.xlsx",sheet_name = "商场总名单",mallcode = NULL,pickedtype = '自营'){
  redstar_points = read_xlsx(default_path,sheet = sheet_name)
  redstar_points = data.table(redstar_points)
  redstar_points = redstar_points[商场类型==pickedtype,]
  redstar_location = redstar_points[,c('商场名称','商场代码','city','longitude','latitude')]
  colnames(redstar_location)[colnames(redstar_location) == '商场名称'] = 'mall_name'
  colnames(redstar_location)[colnames(redstar_location) == '商场代码'] = 'mall_code'
  if(!is.null(mallcode)){
    redstar_location = redstar_location[mall_code %in% mallcode,]
  }
  return(redstar_location)
}

calSubwayDistance <- function(redstar_location,redstar_result) {
  #get the closest subway's longitude and latitude(some city missing some)
  closest_subway = find_closest_subways(redstar_location,place_key)
  #get the distance between the mall and the closest subway
  subway_distance = distHaversine(redstar_location[,c('longitude','latitude')],closest_subway[,c('X2','X1')])
  #put mall_name and distance together
  result_subway = cbind(mall_name = redstar_location$mall_name,subway_distance)
  #combine the final result----redstar_result with subway distance result
  redstar_result = merge(redstar_result,result_subway,by = "mall_name")
  return(redstar_result)
}

getCityPinyinList = function(redstar_location){
  #get city pin dictionary
  all_city_pin = read.table("C:/Users/qingye.yuan/Desktop/工作需要/工作信息/city_pinyin.txt",sep = " ",fileEncoding = "utf-8",nrows = 500)
  colnames(all_city_pin) = c("city_name","city_pinyin","city_abbr")
  all_city_pin$city_pinyin = tolower(all_city_pin$city_pinyin)
  #get the unique city list about the mall
  city_list = unique(redstar_location$city)
  city_list = data.frame(city_name = city_list)
  #combind city name and its pinyin name
  city_pinyin_df = merge(city_list,all_city_pin,by = "city_name",all.x = TRUE)
  return(city_pinyin_df)
}

scrap_business_info = function(city_pinyin_df) {
  #source the file contains the web scraping coding
  source("~/R_Projects/page_destructure/Rfile/Rvest.R")
  #unsetting proxy setting before web scraping, since that may slow down the scraping speed
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  Sys.unsetenv("ftp_proxy")
  #get each city's famous business name(may need to update due to city's expansion)
  business_region = dianping_scrapper2(city_pinyin_df$city_pinyin)
  #combine city and region name using city's pinyin
  business_region_final = merge(business_region,
                                city_pinyin_df,
                                by.x = "city",
                                by.y = "city_pinyin")
  #get full region name using city name and region name
  pv = paste0(business_region_final$city_name,
              business_region_final$region)
  business_region_final$location = pv
  return(business_region_final)
}

calBusinessDistanceGeocode = function(pv){
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  num = length(pv)%/%2000+1
  for(i in 1:num){
    if(i<num){
       commerce_cord_ch[[i]] <<- lapply(pv[((i-1)*2000+1):(i*2000)],geocode_general)
    }
    else{
       commerce_cord_ch[[i]] <<- lapply(pv[((i-1)*2000+1):length(pv)],geocode_general)
    }
      commerce_cord_ch[[i]] <<- rbindlist(commerce_cord_ch[[i]])
  }
}
  
calBusinessDistanceAPI = function(pv){
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  num = length(pv)%/%2000+1
  # combine four part together with column filling(if less than 4 columns then expand automatically)
  commerce_cord_result = rbindlist(commerce_cord_ch[1:num],fill = TRUE)
  # put commercial center's name and position together
  commerce_cord_result = cbind(pv,commerce_cord_result)
  # find name of commercial center with no position
  extra_pv = commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),]$pv
  if(length(extra_pv)>0){
  commerce_cord_ch[[num+1]] <<- lapply(extra_pv,getSimplifiedGeoData)
  commerce_cord_ch[[num+1]] <<- rbindlist(commerce_cord_ch[[num+1]])
  colnames(commerce_cord_ch[[num+1]]) <<- c("lat","lon","lat2","lon2")
  # put the requested answer back to original location-coordinate table
  commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[num+1]][,c("lat","lon","lat2","lon2")]
  }
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  Sys.unsetenv("ftp_proxy")
  # if still missing, using gaode API to fill in
  extra_pv2 = commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),]$pv
  if(length(extra_pv2)>0){
  commerce_cord_ch[[num+2]] <<- lapply(extra_pv2,getDaodeGeoData)
  commerce_cord_ch[[num+2]] <<- rbindlist(commerce_cord_ch[[num+2]])
  commerce_cord_ch[[num+2]] <<- cbind(commerce_cord_ch[[num+2]],lat2 = NA,lon2 = NA)
  # put the requested answer back to original location-coordinate table
  commerce_cord_result[apply(is.na(commerce_cord_result[,-1]),1,all),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[num+2]][,c("lat","lon","lat2","lon2")]
  }
  # replace "其他" with "中心" and using gaode to request coordination.
  extra_pv3 = commerce_cord_result[str_detect(commerce_cord_result$pv,"其他$"),]$pv
  if(length(extra_pv3)>0){
  extra_pv3 = str_replace_all(extra_pv3,"其他","中心")
  commerce_cord_ch[[num+3]] <<- lapply(extra_pv3,getDaodeGeoData) # using proxy it is hard and slow to get
  commerce_cord_ch[[num+3]] <<- rbindlist(commerce_cord_ch[[num+3]])
  commerce_cord_ch[[num+3]] <<- cbind(commerce_cord_ch[[num+3]],lat2 = NA,lon2 = NA)
  # put the requested answer back to original location-coordinate table(using regexpr)
  commerce_cord_result[str_detect(commerce_cord_result$pv,"其他$"),c("lat","lon","lat2","lon2")] = commerce_cord_ch[[num+3]][,c("lat","lon","lat2","lon2")]
  }
  return(commerce_cord_result)
}

calBusinessDistance = function(commerce_cord_result,redstar_location,redstar_result){
  # extract city,commercial center name,and coordinate together
  commerce_cord_final_result = commerce_cord_result[,c("pv","lon","lat")]
  commerce_cord_final_result = cbind(city = business_region_update$city_name,commerce_cord_final_result)
  commerce_cord_final_result$city = as.character(commerce_cord_final_result$city)
  # merge the shopping mall's info and commercial center info
  commerce_cord_final_result = merge(redstar_location,commerce_cord_final_result,by = "city",allow.cartesian=TRUE)
  # calculate the distance between them
  commerce_cord_final_result$commerce_distance = distHaversine(commerce_cord_final_result[,c('longitude','latitude')],commerce_cord_final_result[,c('lon','lat')])
  # get the closest commercial center distance from the shopping mall
  commerce_distance_df = commerce_cord_final_result[,.(commerce_distance = min(commerce_distance)),by = "mall_name"]
  # merge closest distance back to the final result set(may disturb the original order)
  redstar_result = merge(redstar_result,commerce_distance_df,by = "mall_name")
  # make mall_name vector a factor which ordered by original order(in case we can always trace)
  redstar_result$mall_name = factor(redstar_result$mall_name,levels = redstar_location$mall_name,ordered = TRUE)
  redstar_result = redstar_result[order(redstar_result$mall_name),]
  # compile statistics of the commercial center inside 2500 meter
  # shop_commerce_count = commerce_cord_final_result[commerce_distance<2500,.N,by = mall_name]
  shop_commerce_count = commerce_cord_final_result[,.(distance_commerce_in_2500 = sum(commerce_distance<2500)),by = mall_name]
  # merge number inside 2500 m back to the final result set(need to consider order???)
  redstar_result = merge(redstar_result,shop_commerce_count,by = "mall_name")
  return(redstar_result)
}

calClosestWayDistance <- function(redstar_location, redstar_result) {
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  # get closest road from shopping mall's location
  closest_road_list = getClosestRoadPointFromDF(redstar_location)
  closest_road_df = rbindlist(closest_road_list)
  # calculate the distance between the closest road and shopping mall
  closest_road_distance = distHaversine(redstar_location[, c('longitude', 'latitude')], closest_road_df[, c('lng', 'lat')])
  closest_road_distance_df = cbind(mall_name = redstar_location$mall_name, closest_road_distance)
  # merge closest distance result with final result set
  redstar_result = merge(redstar_result, closest_road_distance_df, by = "mall_name")
}

calHighWayDistance <- function(redstar_location, redstar_result) {
  # get every city's district info(name,coordinate) from gaode map system api
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  Sys.unsetenv("ftp_proxy")
  city_list = unique(redstar_location$city)
  district_location_list = lapply(city_list,getGaoDeDistrictInfo)
  district_location_df = rbindlist(district_location_list)
  district_lat_lon_matrix = str_split_fixed(district_location_df$center_location,",",2)
  class(district_lat_lon_matrix) = "numeric"
  district_location_df$lon = district_lat_lon_matrix[,1]
  district_location_df$lat = district_lat_lon_matrix[,2]
  # merge shopping mall info with city's district info
  redstar_district_mixed_df = merge(redstar_location,district_location_df,by = "city",allow.cartesian=TRUE)
  # get distance from shopping mall and city's district center(mainly for the highway access info)
  availablenum = nrow(redstar_district_mixed_df)
  redstar_direction_df = list()
  availabletime = availablenum %/% 40
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  for(i in 0:availabletime){
    redstar_direction_df[[i+1]] = getDirectionInfoFromDF(redstar_district_mixed_df[(40*i+1):(40*(i+1)),])
  }
  redstar_direction_final_df = rbindlist(redstar_direction_df)
  redstar_direction_final_df = redstar_direction_final_df[1:availablenum,]
  # get coordinate for the four direction from shopping mall about 0.5 degree diff in lon or lat
  redstar_four_direction_df = makeFourDirectionFromDF(redstar_location)
  redstar_four_direction_mixed_df = merge(redstar_location,redstar_four_direction_df,by = "mall_name",allow.cartesian=TRUE)
  redstar_four_direction_mixed_df = as.data.frame(redstar_four_direction_mixed_df)
  # get distance from shopping mall and its four direction expand(mainly for the highway access info)
  redstar_four_direction_df = list()
  availablenum2 = nrow(redstar_four_direction_mixed_df)
  availabletime2 = availablenum2 %/% 40
  for(i in 1:(availabletime2+1)){
    redstar_four_direction_df[[i]] = getDirectionInfoFromDF(redstar_four_direction_mixed_df[(40*(i-1)+1):(40*i),])
  }
  redstar_four_direction_final_df = rbindlist(redstar_four_direction_df)
  redstar_four_direction_final_df = redstar_four_direction_final_df[1:availablenum2,]
  #combine district info and four direction info together to get closest highway
  combined_highway_raw_df = rbind(redstar_direction_final_df,redstar_four_direction_final_df)
  combined_highway_raw_dt = data.table(combined_highway_raw_df)
  # get shortest distance from highway
  shortest_distance_result = combined_highway_raw_dt[,.(highway_distance = min(highway_distance),district_center_distance = min(distance,na.rm = TRUE)),by = "start_point"]
  # merge highway distance info back to final result set
  redstar_result = merge(redstar_result,shortest_distance_result,by.x = "mall_name",by.y = "start_point")
}

calCommunityInfo <- function(redstar_location,redstar_result) {
  city = unique(redstar_location$city)
  city = paste0(city,"市")
  source("~/Rfile/R_impala.R") ##!!need to include that file
  community_data_sql = "select id,city,province,area,name,address,plate,areamonut,roommount,buildingdate,longitude,latitude,buildingamount,pricesection,ownermallname from ods.ods_db_dragoneye_xiwa_redstar_community_dt"
  community_data = read_data_impala_general(community_data_sql)
  # always takes string before "市" to do the check
  community_data$join_city = ifelse(!str_detect(community_data$city,"(\\w+)市$"),community_data$city,str_extract(community_data$city,"(\\w+)(?=市)"))
  # merge shopping mall info with community's info
  redstar_community_mixed_df = merge(y=redstar_location,x=community_data,by.y = "city",by.x = "join_city",all.y = TRUE)
  # calculate distance from shopping mall to community
  redstar_community_distance = distHaversine(redstar_community_mixed_df[,c('longitude.x','latitude.x')],redstar_community_mixed_df[,c('longitude.y','latitude.y')])
  redstar_community_distance = cbind(redstar_community_mixed_df[,c("city","province","mall_name","area","name","address","plate","roommount","pricesection")],distance = redstar_community_distance)
  redstar_community_distance = data.table(redstar_community_distance)
  redstar_community_distance = redstar_community_distance[(!(city %in% c('北京市','上海市','深圳市','厦门市')) & pricesection < 100000)|(pricesection < 250000),]
  # redstar_community_5km_number = redstar_community_distance[,sum(distance<=5000,na.rm = TRUE),by = mall_name]
  # redstar_community_50km_number = redstar_community_distance[,sum(distance<=50000,na.rm = TRUE),by = mall_name]
  redstar_community_5km_roominfo = redstar_community_distance[distance<=5000,.(communitynum = .N,roomnum = sum(roommount,na.rm = TRUE),roomnumavailablerate = sum(roommount!=0)/.N,pricemorethan50k = sum(pricesection>50000)/sum(pricesection!=0),pricemorethan10k = sum(pricesection>10000)/sum(pricesection!=0)),by = mall_name]
  redstar_community_50km_roominfo = redstar_community_distance[distance<=50000,.(communitynum = .N,roomnum = sum(roommount,na.rm = TRUE),roomnumavailablerate = sum(roommount!=0)/.N,pricemorethan50k = sum(pricesection>50000)/sum(pricesection!=0),pricemorethan10k = sum(pricesection>10000)/sum(pricesection!=0)),by = mall_name]
  # calculate community's average price data
  temp_5km = redstar_community_distance[distance<=5000,.(totalprice = sum(roommount*pricesection,na.rm = TRUE),totalnum = sum(.SD[(roommount*pricesection!=0),roommount])),by = mall_name]
  temp_5km[,avg_price:=totalprice/totalnum]
  temp_50km = redstar_community_distance[distance<=50000,.(totalprice = sum(roommount*pricesection,na.rm = TRUE),totalnum = sum(.SD[(roommount*pricesection!=0),roommount])),by = mall_name]
  temp_50km[,avg_price:=totalprice/totalnum]
  redstar_community_5km_roominfo$avg_price = temp_5km$avg_price
  redstar_community_50km_roominfo$avg_price = temp_50km$avg_price
  redstar_community_50km_roominfo = data.frame(communitynum50 = redstar_community_50km_roominfo$communitynum,roomnum50 = redstar_community_50km_roominfo$roomnum,avg_price_50 = redstar_community_50km_roominfo$avg_price)
  community_result = cbind(redstar_community_5km_roominfo,redstar_community_50km_roominfo)
  redstar_result = merge(redstar_result,community_result,by = "mall_name")
  return(redstar_result)
}

testfun = function(redstar_result, redstar_location_update) {
  for (mallname in redstar_location_update$mall_name) {
    for (colname in colnames(redstar_result)) {
      # print(redstar_result[[redstar_result$mall_name == mallname, colname]])
      redstar_result[[which(redstar_result$mall_name == mallname),colname]] = redstar_result_update[[which(redstar_result_update$mall_name == mallname),colname]]
    }
  }
}