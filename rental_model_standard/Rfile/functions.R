library(ggmap)
library(geosphere)
library(googleway)
library(baidumap)
library(plyr)
library(RJSONIO)
library(RCurl)

place_key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
# proxy_url <- "http://127.0.0.1:59327/" #蓝灯
proxy_url <- "http://127.0.0.1:50837/" #Psiphon 3
options(baidumap.key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc')
baidu_key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc'
gaode_key = 'de4e66af67591588df24da020bb3d3eb'
tencent_key = 'IESBZ-JKIRS-XVTOJ-6A2LQ-F2NAT-OABVW'

# Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)

# get the closest subway station from the coordination of the mall,without input for subway stations
# The key method google_places need latitude first and longitude next
find_closest_subways = function(coordinate_df,place_key){
  coordinate_df$lat_lon <- do.call(Map, c(f= c, unname(coordinate_df[,c("latitude","longitude")])))
  i = 1
  result_df = data.frame(matrix(,nrow=nrow(coordinate_df), ncol=2))
  for(v in coordinate_df$lat_lon){
    df = google_places(key = place_key,place_type = "subway_station",location = v,radius = 5000,rankby = "distance")
    if(!is.na(df$results$geometry$location$lat) && length(df$results$geometry$location$lat)>0){
      result_df[i,] = c(df$results$geometry$location$lat[1],df$results$geometry$location$lng[1])
    }
    i = i+1
  }
  return(result_df)
  #apply(DF[, c("height", "weight")], 1, f)
}


getGeoData <- function(location,key = place_key){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=",place_key,sep=""))
  raw_data_2 <- fromJSON(geo_data)
  #fromJSON get back a not so friendly result, seperate the longitude and latitude
  return(raw_data_2)
}

getSimplifiedGeoData = function(location,key = place_key){
  v = vector(mode = "numeric",length = 0L)
  raw_data = google_geocode(location,key = place_key)
  if(length(raw_data$results) == 0){
    result = data.frame(lat = NA,lng = NA,lat2 = NA,lng2 = NA)
  }
  else if(nrow(raw_data$results) == 1){
    result = raw_data$results$geometry$location
    result = cbind(result,lat2 = NA,lng2 = NA)
  }
  else if(nrow(raw_data$results) >1){
    result = raw_data$results[1,]$geometry$location
    result2 = raw_data$results[2,]$geometry$location
    result = cbind(result,lat2 = result2$lat,lng2 = result2$lng)
  }
  else{
    result = data.frame(lat = NA,lng = NA,lat2 = NA,lng2 = NA)
  }
  return(result)
}

getBaiduGeoData = function(location,key = baidu_key){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("http://api.map.baidu.com/geocoder/v2/?address=",location,"&output=json&ak=",key,"&callback=showLocation",sep=""))
  raw_data_2 <- fromJSON(geo_data)
  #fromJSON get back a not so friendly result, seperate the longitude and latitude
  return(raw_data_2)
}

getDaodeGeoData = function(location,key = gaode_key){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("http://restapi.amap.com/v3/geocode/geo?address=",location,"&output=json&key=",key,sep=""))
  raw_data <- fromJSON(geo_data)
  #fromJSON get back a not so friendly result, seperate the longitude and latitude
  result = data.frame(lon = NA,lat = NA)
  if (exists("raw_data")) {
    if (!is.atomic(raw_data)) {
      if (length(raw_data$geocodes) > 0) {
        result = raw_data$geocodes[[1]]$location
        result = str_split_fixed(result, ",", 2)
        class(result) = "numeric"
        result = as.data.frame(result)
        colnames(result) = c("lon", "lat")
      }
    }
  }
  return(result)
}

geocode_like = function(search_string){
  
}

getClosestRoadPoint = function(start_location,end_location = start_location,key = place_key){
  result = NA
  response = google_directions(origin = start_location,destination = end_location,key = place_key)
  result = response$routes$legs[[1]]$end_location
  print(result)
  return(result)
}

getClosestRoadPointFromDF = function(location_df){
  result_df = data.frame(matrix(,nrow=nrow(location_df), ncol=2))
  location_df$lat_lon = do.call(Map, c(f= c, unname(location_df[,c("latitude","longitude")])))
  result = lapply(location_df$lat_lon,getClosestRoadPoint)
  return(result)
}

getGaoDeDistrictInfo = function(city,key = gaode_key){
  city = enc2utf8(city)
  location <- gsub(' ','+',city)
  # http://restapi.amap.com/v3/config/district?keywords=北京&subdistrict=2&key=<用户的key>
  geo_data <- getURL(paste("http://restapi.amap.com/v3/config/district?keywords=",location,"&subdistrict=1&key=",key,sep=""))
  raw_data <- fromJSON(geo_data)
  n = length(raw_data$districts)
  result = raw_data$districts[[n]]$districts  
  result_df = data.frame(matrix(,nrow=length(result), ncol=3))
  colnames(result_df) = c("city","district","center_location")
  result_df$city = city
  i = 1
  for(ele in result){
   result_df[i,"district"] = ele$name
   result_df[i,"center_location"] = ele$center
   i = i+1
  }
  return(result_df)
}
#寻找高速，先要使用限速应用,用不了则只能间接使用导航API的距离与时间 
getTencentRouteInfo = function(start_location,end_location,key = tencent_key){
  geo_data = getURL(paste("http://apis.map.qq.com/ws/distance/v1/?mode=driving&from=",start_location,"&to=",end_location,"&key=",key,sep = ""))
  raw_data <- fromJSON(geo_data)
  result = raw_data
  return(result)
}

getDirectionInfo = function(city,start_location,end_location,start_point = start_location,end_point = end_location,speed_limit = 65,key = place_key){
  raw_data = google_directions(start_location,end_location,key = key)
  result = raw_data$routes$legs[[1]]$steps[[1]]
  speed = result$distance$value/result$duration$value*3.6
  indexes = which(speed>speed_limit)
  if (length(indexes) > 0) {
    highway_index = indexes[1]
    if (highway_index > 1) {
      highway_distance = sum(result$distance$value[1:(highway_index-1)])
    }
    else{
      highway_distance = 0
    }
  }
  else{
    highway_distance = Inf
  }
  distance = raw_data$routes$legs[[1]]$distance$value
  duration = raw_data$routes$legs[[1]]$duration$value
  if(is.null(distance)){
    distance = NA
  }
  if(is.null(duration)){
    duration = NA
  }
  result_df = data.frame(city = city,start_point = start_point,end_point = end_point,
                         distance = distance,duration = duration,highway_distance = highway_distance)
}

getDirectionInfoFromDF = function(location_df){
  nv = vector(mode = "numeric",length = 0L)
  cv = vector(mode = "character",length = 0L)
  result_df = data.frame(city = cv,start_point = cv,end_point = cv,
                         distance = nv,duration = nv,highway_distance = nv)
  location_df$start_location = do.call(Map, c(f= c, unname(location_df[,c("latitude","longitude")])))
  location_df$end_location = do.call(Map, c(f= c, unname(location_df[,c("lat","lon")])))
  location_df$end_point = paste0(location_df$city,location_df$district)
# result = mapply(getDirectionInfo,location_df$city,location_df$start_location,location_df$end_location,location_df$mall_name,location_df$end_point,SIMPLIFY = FALSE)
  pbar <- create_progress_bar('text')
  pbar$init(nrow(location_df))
  for(i in 1:nrow(location_df)){
    if(i %% 50 ==0){
    ptm <- proc.time()
    }
    temp_df = getDirectionInfo(location_df$city[i],location_df$start_location[[i]],
                               location_df$end_location[[i]],location_df$mall_name[i],location_df$end_point[i])
    result_df = rbind(result_df,temp_df)
    pbar$step()
    if(i %% 50 == 0){
    print(proc.time() - ptm)
    }
  }
  return(result_df)
}

makeFourDirection = function(shop_location){
  east = data.frame(district=paste0(shop_location$mall_name,"东"),mall_name = shop_location$mall_name,lat = shop_location$latitude,lon = shop_location$longitude+0.5)
  west = data.frame(district=paste0(shop_location$mall_name,"西"),mall_name = shop_location$mall_name,lat = shop_location$latitude,lon = shop_location$longitude-0.5)
  south = data.frame(district=paste0(shop_location$mall_name,"南"),mall_name = shop_location$mall_name,lat = shop_location$latitude-0.5,lon = shop_location$longitude)
  north = data.frame(district=paste0(shop_location$mall_name,"北"),mall_name = shop_location$mall_name,lat = shop_location$latitude+0.5,lon = shop_location$longitude)
  result = rbind(east,west,south,north)
  return(result)
}

makeFourDirectionFromDF = function(shop_location_df){
  nv = vector(mode = "numeric",length = 0L)
  cv = vector(mode = "character",length = 0L)
  result_df = data.frame(district = cv,mall_name = cv,lat = nv,lon = nv)
  for(i in 1:nrow(shop_location_df)){
    temp_df = makeFourDirection(shop_location_df[i,])
    result_df = rbind(result_df,temp_df)
  }
  return(result_df)
}
