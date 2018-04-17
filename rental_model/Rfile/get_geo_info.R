library(ggmap)
library(geosphere)
library(googleway)

place_key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
proxy_url <- "http://127.0.0.1:59327/"
Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)

# get the closest subway station from the coordination of the mall,without input for subway stations
find_closest_subways = function(coordinate_df,place_key){
  coordinate_df$lat_lon <- do.call(Map, c(f= c, unname(coordinate_df)))
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
#input should be data frame shops with city and location(shop names)
get_geo_info = function(shops) {
  #将设置改成全英文，容易理解和编码
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  #get coordination for all shops provided only the name of the mall
  if(nrow(shops)==1){
    shops_cord_ch = geocode(shops$location)
    shops_cord_ch_df = shops_cord_ch
  }
  else{
  shops_cord_ch = lapply(shops$location, geocode)
  shops_cord_ch_df = rbindlist(shops_cord_ch)
  }
  shops = cbind(shops,shops_cord_ch_df)
  #get the closest subway
  closest_subway = find_closest_subways(shops[, c("lat", "lon")], place_key)
  #get the subway distance 
  subway_distance = distHaversine(shops_cord_ch_df[, c('lon', 'lat')], closest_subway[, c('X2', 'X1')])
  #load the data set result_dt3 which is produced by dianping_scrapper2 in map session
  #save(result_dt5,file ="~/data/result_dt5.RData")
  #we store all info regarding region info including name and city and coord in result_dt5
  load(file = '~/data/result_dt5.RData')
  shop_commerce = merge(shops,result_dt5,by = "city",allow.cartesian = TRUE)
  obj_distance = distHaversine(shop_commerce[,c('lon.x','lat.x')],shop_commerce[,c('lon.y','lat.y')])
  shop_commerce = data.table(shop_commerce)
  shop_commerce[,distance := obj_distance]
  shop_commerce_count = shop_commerce[distance<2500,.N,by = location.x]
  shops = merge(shops,shop_commerce_count,by.x = "location",by.y = "location.x",all.x = TRUE)
  setnames(shops,"N","distance_commerce_in_2500")
  shop_commerce_distance = shop_commerce[,min(distance,na.rm = TRUE),by = location.x]  
  # false_row = shop_commerce[distance ==0,c("lon","lat","location.x","location.y")]
  shops = merge(shops,shop_commerce_distance,by.x = "location",by.y = "location.x",all.x = TRUE)
  setnames(shops,"V1","min_distance_commerce")
  shops = cbind(shops,subway_distance)
  #将设置还原
  Sys.setlocale(category = "LC_ALL", locale = "")
  }