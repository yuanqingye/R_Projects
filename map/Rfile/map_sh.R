library(maps)
library(mapdata)

library(maptools)
x=readShapePoly('~/data/map_data/bou2_4p/bou2_4p.shp')#下文中会继续用到x这个变量，
#如果你用的是其它的名称，
#请在下文的程序中也进行相应的改动。
par(mar=rep(0,4))
plot(x)
plot(x,col=rainbow(924))
x[[1]]

xx =readShapeSpatial('~/data/map_data/roa_4m/roa_4m.shp')
lines(xx,col = "yellow")

xxx = readShapeSpatial('~/data/map_data/rai_4m/rai_4m.shp')
lines(xxx,col = "red")

library(rgdal)
#new data from all_china
china_general = readShapeSpatial('~/data/map_data/all_china/国界线.shp')
plot(china_general)
china_road = readShapeSpatial('~/data/map_data/all_china/主要公路.shp')
lines(china_road,col = "blue")
china_river = readShapeSpatial('~/data/map_data/all_china/主要河流.shp')
# china_river = rgdal::readOGR(dsn = '~/data/map_data/all_china',layer = "主要河流")
plot(china_river)
china_railway = readShapeSpatial('~/data/map_data/all_china/主要铁路.shp')
lines(china_railway,col = "red")

# 上面的情况直接拿经纬度画图,有一定问题
# 首先，我们需要指定x本身的投影信息，下面一行代码指定x对应的投影为原始的地理坐标（经纬度）：  
proj4string(x) <- CRS("+proj=longlat +ellps=WGS84")  
# 然后我们指定新的投影方式，并将x投影到其上：  
projNew <- CRS("+proj=merc +lat_0=45n +lon_0=100e")  
xProj <- spTransform(x, projNew)  
# 现在我们可以画出它了：  
dev.new()  
plot(xProj)  

getColor = function(mapdata, provnames, provcol, othercol){
  f = function(x, y) ifelse(x %in% y, which(y == x), 0);
  colIndex = sapply(mapdata@data$NAME, f, provnames);
  fg = c(othercol, provcol)[colIndex + 1];
  return(fg);
}

provname = c("北京市", "天津市", "上海市", "重庆市",'江苏省');
provcol = c("red", "green", "blue", "purple",'yellow');
plot(x, col = getColor(x, provname, provcol, "grey"))

provname = c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
             "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
             "浙江省", "安徽省", "福建省", "江西省", "山东省",
             "河南省", "湖北省", "湖南省", "广东省",
             "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省",
             "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省",
             "宁夏回族自治区", "新疆维吾尔自治区", "台湾省",
             "香港特别行政区");
pop = c(1633, 1115, 6943, 3393, 2405, 4298, 2730, 3824, 1858, 7625,
        5060, 6118, 3581, 4368, 9367, 9360, 5699, 6355, 9449,
        4768, 845, 2816, 8127, 3762, 4514, 284, 3748, 2617,
        552, 610, 2095, 2296, 693);
provcol = rgb(red = 1 - pop/max(pop)/2, green = 1-pop/max(pop)/2, blue = 0);
plot(x, col = getColor(x, provname, provcol, "white"), xlab = "", ylab = "")

getID = function(mapdata, provnames){
  index = mapdata@data$NAME %in% provnames;
  ids = rownames(mapdata@data[index, ]);
  return(as.numeric(ids));
}
midchina = c("河南省", "山西省", "湖北省", "安徽省", "湖南省", "江西省");
plot(x, recs = getID(x, midchina), col = "green", ol = "white", xlab = "",
     ylab = "")

plot(x, col = getColor(x, midchina, rep("green", 6),
                       "white"), border = "white", xlab = "", ylab = "")


dat = read.csv(text = "城市,jd,wd
               北 京,116.4666667,39.9
               上 海,121.4833333,31.23333333
               天 津,117.1833333,39.15
               重 庆,106.5333333,29.53333333
               哈尔滨,126.6833333,45.75
               长 春,125.3166667,43.86666667
               沈 阳,123.4,41.83333333
               呼和浩特,111.8,40.81666667
               石家庄,114.4666667,38.03333333
               太 原,112.5666667,37.86666667
               济 南,117,36.63333333
               郑 州,113.7,34.8
               西 安,108.9,34.26666667
               兰 州,103.8166667,36.05
               银 川,106.2666667,38.33333333
               西 宁,101.75,36.63333333
               乌鲁木齐,87.6,43.8
               合 肥,117.3,31.85
               南 京,118.8333333,32.03333333
               杭 州,120.15,30.23333333
               长 沙,113,28.18333333
               南 昌,115.8666667,28.68333333
               武 汉,114.35,30.61666667
               成 都,104.0833333,30.65
               贵 阳,106.7,26.58333333
               福 州,119.3,26.08333333
               台 北,121.5166667,25.05
               广 州,113.25,23.13333333
               海 口,110.3333333,20.03333333
               南 宁,108.3333333,22.8
               昆 明,102.6833333,25
               拉 萨,91.16666667,29.66666667
               香 港,114.1666667,22.3
               澳门,113.5,22.2")
par(mar=rep(0,4))


library(maps)
library(mapdata)
map("china", col = "darkgray", ylim = c(18, 54), panel.first = grid())
points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                    0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                        4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)

library(sp)
library(spdep)
library(rgeos)

# this part may help in deciding distance
# Create toy SPDF
fn <- system.file("etc/shapes/eire.shp", package = "spdep")[1]
prj <- CRS("+proj=utm +zone=30 +units=km")
eire <- readShapeSpatial(fn, ID = "names", proj4string = prj)

# Create a point of interest
pt.coords <-readWKT("POINT(260 5900)")
eire.proj<-CRS(proj4string(eire))
proj4string(pt.coords) <- eire.proj

# check point is on the map
plot(eire)
plot(pt.coords, col="Red", add=TRUE)

centroid.ED <-SpatialPoints(coordinates(eire))
proj4string(centroid.ED) <- eire.proj
eire$distance <- gDistance(pt.coords,centroid.ED,byid=TRUE)
