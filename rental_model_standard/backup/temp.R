library(rvest)
library(magrittr)
library(tabulizer)
library(tesseract)
library(readxl)

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

city_gdp_pdf = extract_tables("D://downloads//gdp_ranking.pdf",encoding = "UTF-8",pages = 2)
gdp_pic1  <- ocr("D://downloads//gdp_ranking1.jpg")

#We will check if this can work
source('~/Rfile/R_hana.R', encoding = 'UTF-8')
shop_tables = read_data_from_hana("select * from BIGBI.DIM_ALL_MALL_V2")
shop_tables_self_manage = shop_tables[shop_tables$MALL_TYPE=="自营",c("MALL_NAME","MALL_CODE","GPS","COUNTRY_NAME","OPEN_DATE")]
shop_tables_self_manage[,c("lon","lat")] = str_split_fixed(shop_tables_self_manage$GPS,n = 2,";")
shop_tables_self_manage = shop_tables_self_manage[-1,]
shop_tables_self_manage$OPEN_DATE = str_sub(shop_tables_self_manage$OPEN_DATE,1,10)
shop_tables_self_manage$MALL_CODE = as.numeric(shop_tables_self_manage$MALL_CODE)
shop_tables_self_manage$lon = as.numeric(shop_tables_self_manage$lon)
shop_tables_self_manage$lat = as.numeric(shop_tables_self_manage$lat)
m_check_lon_lat = merge(redstar_points,shop_tables_self_manage,by.x = "商场代码",by.y = "MALL_CODE",all.x = TRUE)
m = m_check_lon_lat[abs(m_check_lon_lat$longitude-m_check_lon_lat$lon)>0.02|(m_check_lon_lat$latitude-m_check_lon_lat$lat)>0.02,]
m2 = m_check_lon_lat[abs(m_check_lon_lat$longitude-m_check_lon_lat$lon)>0.01|(m_check_lon_lat$latitude-m_check_lon_lat$lat)>0.01,]

# 39.4906288049,116.7004895210  廊坊龙河商场
# 28.1052031549,113.0133533478  长沙韶山商场
# 30.6197434307,114.1717457771  武汉额头湾竹叶海商场
# 22.5782826444,113.3909332752  中山港口商场
# 41.8920150908,123.4138655663  沈阳沈北欧丽洛雅
# 24.5086557356,118.0921548692  厦门宝象商场
# 29.5295259289,106.5633380413  重庆南坪商场
# 37.4649700000,121.3573100000  烟台建材商场
# 45.7203500000,126.5728900000  哈尔滨哈西商场


redstar_points[redstar_points$商场代码==10130,c("latitude","longitude","address")] = c(39.4906288049,116.7004895210,"廊坊市安次区南龙道33号")

city_rank = read.table("~/data/city_rank.txt",sep = " ",fileEncoding = "utf-8")
special_city_rank = read.table("~/data/city_rank2.txt",sep = " ",fileEncoding = "utf-8")
score = special_city_rank$V2
city_rank$score[1:19] = score
city_rank$score[20:49] = seq(from = 35,to = 25,length.out = 30)
city_rank$score[50:119] = seq(from = 22,to = 15,length.out = 70)
city_rank$score[120:209] = seq(from = 13,to = 8,length.out = 90)
city_rank$score[210:nrow(city_rank)] = seq(from = 6,to = 1,length.out = 129)
colnames(city_rank)[[1]] = "city_name"

for(i in extra_pv3){
  print(i)
  getDaodeGeoData(i)
}

some_function_that_may_fail <- function() {
  temp = runif(1)
  if( temp < 0.000001 ) stop()
  return(1)
}

r <- NULL
attempt <- 1
while( is.null(r) && attempt <= 3 ) {
  attempt <- attempt + 1
  print(attempt)
  try(
    r <- some_function_that_may_fail()
  )
} 

# redstar_result_mall_name = redstar_result$mall_name

library(plyr)
redstar_result$mall_name = as.character(redstar_result$mall_name)
redstar_result1 = join(redstar_result,redstar_location[,c("mall_name","mall_code")],by = "mall_name") 
# redstar_result = redstar_result1

redstar_result = setcolorder(redstar_result, as.character(colnames(redstar_result_update)))

city_revenue_avg = read_xlsx("C:/Users/qingye.yuan/Desktop/工作需要/租金定价/城市收入水平.xlsx")

colnames(city_revenue_avg) = c("city","revenue_all","increase_rate_all","revenue_town","increase_rate_town")

test = read.table(textConnection("54305 52530 48695 46667 咭6与肠 46116 44641 440的 43143 42757 41941 41902 41564 415伪 40154 40118 40012 39601 38529 3日弓肠 37193 37159 36915 35630 34074 34064 339的 32178 32070 30941 30此5 30296 30084 30045 29742 29462 28633 28259 28061 27608 26757 26745 25484 24759 24685 2吸唯63 2咭咯27 23821 23623 23194 22784 22762 22603 22348 22173 22142 21854 21602 21291 21230 21073 20905 20713 20644 20580 19978 195 13 18957 18859 18403 18061 17987 17937 17934 17654 17467 16607 16518 16077 15661"),header = F, sep = " ", stringsAsFactors = F)
test = t(test)
test = test[1:81,1]
test[5] = "46595"
test[8] = "44009"
test[14] = "41506"
test[20] = "38435"
test[27] = "33909"
test[31] = "30855"
test[46] = "24463"
test[47] = "24427"
test[67] = "19513"

test3 = c(test[1:67],test[69:81])
test3 = as.numeric(test3)
city_revenue_avg$revenue_all = test3

test2 = read.table(textConnection("57692 57275 48695 50941 别3吸l 52185 51560 49997 46254 48628 43120 43096 48423 50305 42537 48926 41613 47785 47之37 45058 45794 46554 47162 43598 37110 41794 弓3(52 41580 40152 39363 31场56 37684 392咭7 37833 38744 36436 35659 35828 33213 36014 35968 36188 33609 32364 34012 304今6 29557 33616 31818 30299 30726 30335 30583 28421 29677 30408 29987 3（旧5, 28340 27853 27818 28164 25121 27708 25907 25855 25281 24086 25267 23323 21787 23642 22389 24887 22944 22160 23277 21888 21817 22122"),header = F, sep = " ", stringsAsFactors = F)
test2 = t(test2)
test2 = test2[1:80,1]
test2[5] = "54341"
test2[19] = "47237"
test2[20] = "46058"
test2[27] = "43052"
test2[31] = "39656"
test2[33] = "39247"
test2[46] = "30496"
test2[58] = "30859"

test2 = as.numeric(test2)
city_revenue_avg$revenue_town = test2

library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rentModel <- train(rent~., data=train_set, method="rf", preProcess="scale", trControl=control,
                   importance = T)
Importance <- varImp(rentModel, scale=FALSE)
