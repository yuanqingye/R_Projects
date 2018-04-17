library(maps)
library(mapdata)

map("worldHires","Mexico",col = 'blue',fill = T)

library(maptools)
library(classInt)
library(foreign)
library(RColorBrewer)
pcontorta = readShapePoly("pinucont.shp")
testdbf = read.dbf('~/data/map_data/bou2_4p/bou2_4p.dbf')
china = readShapePoly('~/data/map_data/bou2_4p/bou2_4p.shp')
names(china)
cols = brewer.pal(5,"green")
brks = classIntervals(china$honds4,n=6,style = "fixed",fixedBreaks = c(0,5,10,15,20,600))
plot(china,col = cols[findInterval(china$good,brks,all.inside = T)],axes = F)

library(ggmap)
sport = readShapePoly("~/data/map_data/london_sport/london_sport.shp")
names(sport)

p = ggplot(sport@data,aes(Partic_Per,Pop_2001))
p+geom_point()
p+geom_point(aes(colour = Partic_Per,size = Pop_2001))+geom_text(size = 2,aes(label = name))

gpclibPermit()

sport_geom = fortify(sport,region = "ons_label")
sport_geom = merge(sport_geom,sport@data,by.x = 'id',by.y = 'ons_label')

ggplot(sport_geom,aes(long,lat,group = group,fill = Partic_Per))+geom_polygon()+coord_equal()

library(ggmap)
map = get_map(location = "china",zoom = 4)
ggmap(map)

bjmap <- get_map(location = 'Beijing', zoom = 10, maptype = 'roadmap')
ggmap(bjmap)

cqmap = get_map(location = "chongqing",zoom = 10,maptype = 'roadmap')
ggmap(cqmap)
library(raster)

china_map <- readShapeLines("http://www.epsg-registry.org/", 
                          proj4string=CRS("+init=epsg:4030 +proj=longlat +ellps=WGS84 +no_defs"))

class(cqmap)
mode(cqmap)

worldmap = get_map(location = "world")
ggmap(worldmap)

library(RCurl)
library(rjson)
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

library(ggmap)
library(ggplot2)
dd <- data.frame(lat=c(50.659631, 50.607213, 50.608129),
                lon=c(3.09319, 3.011473, 3.031529))

Lille <- get_map("Lille,France", zoom=12)
#1
p <- ggmap(Lille)
p <- p + geom_point(data=dd, aes(x=lon, y=lat),size=5)
p
#2
p <- ggmap(Lille)
p + geom_point(data=dd, aes(x=lon, y=lat), color="red", size=30, alpha=0.5)

