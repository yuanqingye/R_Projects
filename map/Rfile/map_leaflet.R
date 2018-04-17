install.packages("leafletCN")

# 数据使用范围
# From base R:
#   lng/lat matrix
# data frame with lng/lat columns
# From the sp package:
#   SpatialPoints[DataFrame]
# Line/Lines
# SpatialLines[DataFrame]
# Polygon/Polygons
# SpatialPolygons[DataFrame]
# From the maps package:
#   the data frame from returned from map()

library(XML)
library(leaflet)
library(leafletCN)
# 读取网页的表格
# Sorry for 爬了你家网站
table = readHTMLTable("http://www.pm25.in/rank",  
                      encoding = "UTF-8", stringsAsFactors = F)[[1]]

# 整理数据并命名
dat = table[ , 2:3]
names(dat) = c("city","AQI")
dat$AQI = as.numeric(dat$AQI)

# 调用geojsonMap进行绘制
geojsonMap(dat, "city",
           popup =  paste0(dat$city, ":", dat$AQI),
           palette = "Reds", legendTitle = "AQI")

regionNames("重庆")
demomap("江苏")

dat = data.frame(name = regionNames("world"),
                 value = runif(length(regionNames("world"))))
geojsonMap(dat,"world")

leaflet() %>%
  amap() %>%
  addMarkers(lng = 116.3125774825, lat = 39.9707249401,
             popup = "The birthplace of COS")

if(require(sp)){
  filePath = system.file("geojson/china.json",package = "leafletCN")
  map = read.geoShape(filePath)
  plot(map)
}

if(require(leaflet)){
  dat = data.frame(regionNames("china"),
                   runif(34))
  map = leafletGeo("china", dat)
  
  pal = colorNumeric(
    palette = "Blues",
    domain = map$value)
  
  leaflet(map) %>% addTiles() %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = ~htmltools::htmlEscape(popup)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "legendTitle",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)
}

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles()

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

help(package = 'sp')

library(sp)
chn_mapdata = readRDS("~/data/map_data/CHN_adm0.rds") 
spplot(chn_mapdata[1])
chn_mapdata_1 = readRDS("~/data/map_data/CHN_adm1.rds")
spplot(chn_mapdata_1[1])
chn_mapdata_2 = readRDS("~/data/map_data/CHN_adm2.rds")
spplot(chn_mapdata_2[1])
chn_mapdata_3 = readRDS("~/data/map_data/CHN_adm3.rds")
spplot(chn_mapdata_3[1])

Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

# topoData <- readLines("json/us-10m.json") %>% paste(collapse = "\n")

