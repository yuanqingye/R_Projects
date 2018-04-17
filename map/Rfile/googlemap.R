library(googleway)
# install.packages("https://cran.r-project.org/src/contrib/googleway_2.2.0.tar.gz",repos = NULL,type = "resource")
#key <- 'AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs' map api key
key <- "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
#AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs google map api
#  	9792023 		4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc 	服务端

#腾讯地图API key
tencent_key = "IESBZ-JKIRS-XVTOJ-6A2LQ-F2NAT-OABVW"

#高德地图API key
gaode_key = "de4e66af67591588df24da020bb3d3eb"

bjloc = geocode("beijing")

df_places <- google_places(search_string = "bank", 
                           location = c(bjloc$lat,bjloc$lon),   ## beijing, China
                           key = "AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs")

df_restaurant = google_places(search_string = "cruise",place_type = "food",key = key, location = c(-33.8670,151.1957))

res <- google_places(search_string = "Restaurants in Melbourne, Australia",
                     key = key)

df_places$results$name
url.exists("www.google.com")

proxy_url <- "http://127.0.0.1:50837/"
Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)

# https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-33.8670,151.1957&radius=500&types=food&name=cruise&key=AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034

find_closest_places = function(search_string,location,types,name,radius){
  library(RCurl)
  library(rjson)
  key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
  location = paste0(location,collapse = ",")
  web = getURL(
    paste(
      "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", location,
      "&key=", key,
      "&types=",types,
      "&name=",name,
      "&radius=",radius,
      sep = ""
    )
  )
  return(fromJSON(web))
}

library(RCurl)
library(rjson)

user = "blog125"
key = "21376174"

trans = function(word) {
  word = URLencode(word)
  web = getURL(
    paste(
      "http://fanyi.youdao.com/openapi.do?keyfrom=", user,
      "&key=", key,
      "&type=data&doctype=json&version=1.1&q=", word,
      sep = ""
    )
  )
  return(fromJSON(web)$translation)
}

library(RJSONIO)
library(RCurl)
getGeoData <- function(location){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=**[YOUR GOOGLE API KEY HERE]**", sep=""))
  raw_data_2 <- fromJSON(geo_data)
  return(raw_data_2)
}