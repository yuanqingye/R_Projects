library(rvest)
library(magrittr)

temp = list()
temp1 = list() 
temp2 = list()
for (i in 2001:4000){
  temp1[[i]] = google_geocode(pv[i],key = place_key)$results
}
for (i in 4001:length(pv)){
  temp2[[i]] = google_geocode(pv[i],key = place_key)$results
}
temp2[2001:4000] = temp[2001:4000]
temp = temp2[2001:length(pv)]
# temp_extract_col = lapply(temp,subset,select = c("geometry.location.lat","geometry.location.lng"))
l = list()
i = 1
for(element in temp){
  if(i>52){
    browser()
  }
  l[[i]] = element$geometry$location[,c("lat","lng")]
  if(is.null(l[[i]])){
    print(paste0("null elements:",i))
  }
  else if(nrow(l[[i]])>1){
    print(paste0("duplicate elements:",i))
  }
  i = i+1
  print(i)
}
i = 1
for(element in temp){
  if(length(element) == 0){
    print(i)
  }
  if(i>2600){
    break
  }
  i = i+1
}

geocode(commerce_cord_result[commerce_cord_result$pv =="常州曼哈顿广场",]$pv)
google_geocode(commerce_cord_result[commerce_cord_result$pv =="常州曼哈顿广场",]$pv,key = place_key)
getBaiduGeoData(commerce_cord_result[commerce_cord_result$pv =="常州曼哈顿广场",]$pv)

getDaodeGeoData(iconv("常州曼哈顿广场","gbk","utf-8"))
getDaodeGeoData("常州金坛商业区")
getDaodeGeoData("常州金坛市中心")


test_start = "39.983171,116.308479"
test_start = "39.8935055024,116.4526963234" #广泉小区
test_end = "39.996060,116.353455;39.949227,116.394310"
test_end = "39.9075017400,116.3912022800"
temp = getTencentRouteInfo(test_start,test_end)

google_directions(c(39.70221,106.8455),c(39.505925,106.726099),key = place_key)

temp3 = google_directions("beijing","shanghai",key = place_key)
temp3[[1]] = temp3
temp3[[2]] = temp3$routes$legs[[1]]$steps[[1]]
View(temp3[[2]])
temp3[[2]]$distance$value/temp3[[2]]$duration$value*3.6

temp = getDirectionInfoFromDF(redstar_district_mixed_df[1:5,])

a = makeFourDirection(redstar_location[1,])

options(java.parameters = "- Xmx1024m")
source('~/Rfile/R_hana.R', encoding = 'UTF-8')
mall_p_flow_sql = "select * from bigbi.dm_mall_pass_flow_day"
temp = read_data_from_hana(mall_p_flow_sql)

community_data_sample = community_data_focus[,.SD[1:10,],by = "city"]
community_data_sample = community_data_focus[pricesection > 0,.SD[sample(nrow(.SD),10),],by = "city"]
community_data_sample = community_data_sample[,c("city","name","pricesection")]
<<<<<<< HEAD
<<<<<<< HEAD

##This time we add some content for testing git
city_gdp_url = "https://wenku.baidu.com/view/b97475622f3f5727a5e9856a561252d380eb2021.html"
city_gdp_page = read_html(city_gdp_url)
city_gdp = city_gdp_page %>% html_nodes('div.reader-pic-item') %>% html_text()

city_gdp_url = "http://www.sohu.com/a/219596742_683734"
city_gdp_page = read_html(city_gdp_url)
city_gdp = city_gdp_page %>% html_nodes('article#mp-editor p') %>% html_text()
city_gdp = city_gdp[str_detect(city_gdp,"^[\\d]{1,2}\\.")]
gdp = str_extract(city_gdp,"\\d+(?=亿元)")
population = str_extract(city_gdp,"\\d+(?=万)")
increasing_rate = str_extract(city_gdp,"\\d+(\\.)?\\d*(?=%)")
city_name = str_extract(city_gdp,"(?<=\\d\\.)[^a-z]+?(?=(\\d){2})")
=======
>>>>>>> parent of 26dec8d... FirstUpdateCommit
=======
>>>>>>> parent of 26dec8d... FirstUpdateCommit
