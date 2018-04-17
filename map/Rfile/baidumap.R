library(ggmap)
library(baidumap)
options(baidumap.key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc')

#高德地图API key
gaode_key = "de4e66af67591588df24da020bb3d3eb"

#AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs google map api
#  	9792023 		4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc 	服务端 		设置 删除

bj_college = getPlace('大学','北京')
pku_map = getBaiduMap('北京大学', width=600, height=600, zoom=18, scale = 2, messaging=FALSE)
ggmap(pku_map)
home_coordinate = getCoordinate('上海市普陀区泸定路555号',output='xml',formatted = TRUE)

sh_mcdonald = getPlace('麦当劳', '上海')
hz_travel = getPlace('旅游景点','杭州')

business_center = getPlace('商业中心','中国')
bj_business_center = getPlace('商业中心','北京')
sh_business_center = getPlace('商业中心','上海')

bjMap = getBaiduMap('北京',color = 'bw')
df = getRoute('首都国际机场', '北京南苑机场')
ggmap(bjMap) + geom_path(data = df, aes(lon, lat), alpha = 0.5, col = 'red')

bjMap2 = getBaiduMap('北京市区',color = 'color')
df2 = getRoute('北京市海淀区北洼路46号','北京市第三十五中学')
ggmap(bjMap2) + geom_path(data = df2,aes(lon,lat),alpha = 0.5,col = 'blue')

ls_travel = getPlace('旅游景点','拉萨')

library(maps)
map('world', fill = TRUE, col = rainbow(200),ylim = c(-90, 90), mar = c(0, 2, 0, 0))

getCoordinate('红星·美凯龙',output = 'xml')
getLocation(location = c(121.39264,31.24583))

liyangmap = getBaiduMap("溧阳市")
ggmap(liyangmap)

sh_redstar = getPlace('美凯龙 商场', '上海')

changshamap = getBaiduMap("长沙政区")
ggmap(changshamap)

coordinate = getCoordinate(result_dt$address)
library(rjson)

rrcoordinate = sapply(coordinate,fromJSON)

h = basicJSONHandler()
fromJSON(co, h)

cosample = rcoordinate[1]
lng = cosample[[1]]$result$location$lng
cosample[[1]]$result$location$lat

library(data.table)
rrcoordinate = data.table(lng = vector(mode = 'numeric',length = 0),lat = vector(mode = 'numeric',length = 0))
for(i in 1:length(rcoordinate)){
  cosample = rcoordinate[i]
  lng = ifelse(is.null(cosample[[1]]$result$location$lng),NaN,cosample[[1]]$result$location$lng)
  lat = ifelse(is.null(cosample[[1]]$result$location$lat),NaN,cosample[[1]]$result$location$lat)
  tempdt = data.table(lng = lng,lat = lat)
  rrcoordinate = rbind(rrcoordinate,tempdt)
}
result = cbind(result_dt,rrcoordinate)

bj_subway = getPlace('地铁',"北京")
sh_subway = getPlace('地铁',"上海")

changsha_shops = read.table(text =
                            "NAME
我
你
他",header = FALSE,fileEncoding = "GBK" )
