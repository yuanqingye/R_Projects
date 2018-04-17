library(baidumap)
options(baidumap.key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc')

bj_trip = getPlace('旅游景点','北京')
bj_trip = iconv(bj_trip,'utf-8','gbk')

library(ggmap)
ggmap(bj_trip)

library(readxl)
library(data.table)
redstar_points = read_xlsx("~/data/selfmanaged_business.xlsx",sheet = '商场总名单')
redstar_points = data.table(redstar_points)
redstar_points = redstar_points[商场类型=='自营',]
redstar_location = redstar_points[,c('商场名称','longitude','latitude')]

library(maptools)
x=readShapePoly('~/data/map_data/bou2_4p/bou2_4p.shp')#下文中会继续用到x这个变量，
#如果你用的是其它的名称，
#请在下文的程序中也进行相应的改动。
par(mar=rep(0,4))
# plot(x)
plot(x,col=rainbow(924))
provincename = "江苏省"

plot(x, recs = getID(x, provincename), col = "green", ol = "white", xlab = "",
     ylab = "")

plot(x, col = getColor(x, provincename, rep("green", 6),
                       "white"), border = "white", xlab = "", ylab = "")

cqmap = get_map(location = "chongqing",zoom = 10,maptype = 'hybrid')

ggmap(cqmap)

chongqingmap = getBaiduMap("重庆江北区")
ggmap(chongqingmap)


ggmap(cqmap, extent = "normal") +
  geom_point(aes(x = lng, y = lat), data = result[city=='chongqing'], alpha = .5)


x[[1]]

points(redstar_location[,c(2,3)],pch = '+')

library(OasisR)
distance(x)

cal_object = result[,.(midlon = median(lng,na.rm = TRUE),midlat = median(lat,na.rm = TRUE)),by = c('city','region')]

library(rvest)
resultxml = getCoordinate('中关村',output = 'xml')
resultxml2 = read_xml(resultxml)
longitude = resultxml2%>%xml_node('lng')%>%xml_text()
latitude = resultxml2%>%xml_node('lat')%>%xml_text()

load('~/data/result_dt3.RData')
result_dt2 = result_dt3
library(stringr)
library(baidumap)
shops = malls[,.(city=str_sub(商场名称,1,2),name = 商场名称,lon = longitude,lat=latitude)]
result_dt3[,location := paste(city,region)]
result_dt3[,coordinates := getCoordinate(location,output = 'xml')]
xmls = lapply(result_dt3$coordinates,read_xml)
lonpart = lapply(xmls,xml_node,'lng')
latpart = lapply(xmls,xml_node,'lat')
xml_text_v = Vectorize(xml_text)
longitude1 = xml_text_v(lonpart)
latitude1 = sapply(latpart,xml_text)
result_dt3[,c('longitude','latitude'):=.(longitude1,latitude1)]
result_dt3[,c('longitude','latitude'):=.(as.numeric(longitude),as.numeric(latitude))]
df1 = data.frame(shops)
df2 = data.frame(result_dt3)
shop_business_conn = merge(df1,df2,by.x = 'city',by.y = 'city',allow.cartesian=TRUE)
shop_business_conn = data.table(shop_business_conn)
library(geosphere)
shop_business_conn = result_dt4[shops, on="city"]
# obj_distance = shop_business_conn[,distHaversine(lon,lat,longitude,latitude)]
obj_distance = distHaversine(shop_business_conn[,c('lon','lat')],shop_business_conn[,c('longitude','latitude')])
shop_business_conn[,distance := obj_distance]