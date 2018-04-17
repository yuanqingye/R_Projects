library(ggmap)
g=c(121.754252477584,24.7528993303431)
c=c(121.752751736839,24.7554214120371)
mapdist(g,c,mode=c("driving","walking","bicycling"),output=c("simple"))


wh <- as.numeric(geocode("the white house, dc"))
mapdist("waco, texas", "houston, texas")

mapdist("san diego,california",'los angeles,california')
mapdist("luding Rd,Shanghai",'nujiangbeilu 598,shanghai')
mapdist("beiwa Rd 46,beijing","No 4 highschool,beijing",mode = "walking")
mapdist("beiwa Rd 46,beijing","No 35 middleschool,beijing",mode = "bicycling")

google_results <- rbind.fill(apply(subset(sample, select=c("location", "locMall")), 1, function(x) mapdist(x[1], x[2], mode="driving")))

g = c(36.841287,-76.218922)
c = c(121.752751736839,24.7554214120371)
mapdist(g,c,output = c("simple"))

from <- c("houston", "houston", "dallas")
to <- c("waco, texas", "san antonio", "houston")
mapdist(from, to)

#计算多边形之间的距离
library(OasisR)
library(data.table)
province_dist = distance(chn_mapdata_1)
province_dist = data.table(province_dist)
rownames(province_dist) = chn_mapdata_1@data$NL_NAME_1
colnames(province_dist) = chn_mapdata_1@data$NL_NAME_1

# You could use the rgeos package and the gDistance method. 
# This will require you to prepare your geometries, 
# creating spgeom objects from the data you have (I assume it is a data.frame or something 
# similar). The rgeos documentation is very detailed 
# (see the PDF manual of the package from the CRAN page), 
# this is one relevant example from the gDistance documentation:

library(rgeos)
pt1 = readWKT("POINT(0.5 0.5)")
pt2 = readWKT("POINT(2 2)")
p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2 = readWKT("POLYGON((2 0,3 1,4 0,2 0))")
gDistance(pt1,pt2)
gDistance(p1,pt1)
gDistance(p1,pt2)
gDistance(p1,p2)

mapply(gDistance,c(p1,p2),c(pt1,pt2))

redstar_points = read_xlsx("~/data/selfmanaged_business.xlsx",sheet = '商场总名单')
redstar_points = data.table(redstar_points)
redstar_points = redstar_points[商场类型=='自营',]
redstar_location = redstar_points[,c('商场名称','longitude','latitude')]

mydf = as.data.frame(redstar_location)
xy <- mydf[,c(2,3)]

spdf <- SpatialPointsDataFrame(coords = xy, data = mydf,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# province.proj   <- spTransform(chn_mapdata_1,CRS(epsg.27700))
# shop.proj     <- spTransform(spdf,CRS(epsg.27700))


centroids=gCentroid(chn_mapdata_1, byid=TRUE, id = NULL)
plot(centroids)

gDistance(spdf,centroids)

# europeRG <- readOGR(dsn = patheurope, layer = "NUTS_RG_60M_2013", stringsAsFactors = FALSE)
# europewmercator <- spTransform(europeRG, CRS("+init=epsg:3035"))

proj4string(centroids) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
centroidsp <- spTransform(centroids, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
centroids1 <- spTransform(centroids[2,], CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
centroids2 <- spTransform(centroids[2,], CRS("+init=epsg:3035"))


# sp = SpatialPoints(coords)
proj4string(spdf) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sp <- spTransform(spdf, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
sp1 <- spTransform(spdf[57,], CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
sp2 <- spTransform(spdf[57,], CRS("+init=epsg:3035"))

gDistance(centroids2,sp2)
m = gDistance(sp,centroidsp,byid = TRUE)
library(data.table)
mdt = data.table(m)

map <- get_map(location = 'Beijing', zoom = 10, maptype = 'roadmap')


# S = example("SpatialPoints-class", ask=FALSE, echo=FALSE)
# S1 = example("SpatialLines-class", ask=FALSE, echo=FALSE)
# m <- gDistance(S, Sl, byid=TRUE)
# apply(m, 2, function(X) rownames(m)[order(X)][1])       ## Finds single closest

g1=readWKT("POINT(6 10)")
g2=readWKT("LINESTRING(3 4,10 50,20 25)")
g3=readWKT("POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2))")
g4=readWKT("MULTIPOINT((3.5 5.6),(4.8 10.5))")
g5=readWKT("MULTILINESTRING((3 4,10 50,20 25),(-5 -8,-10 -8,-15 -4))")
g6=readWKT("MULTIPOLYGON(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))")
try(readWKT("POINT EMPTY"))
try(readWKT("MULTIPOLYGON EMPTY"))
g9=readWKT("GEOMETRYCOLLECTION(POINT(4 6),LINESTRING(4 6,7 10))")

writeWKT(g1)
writeWKT(g2)
writeWKT(g3)
writeWKT(g4)
writeWKT(g5)
writeWKT(g6)
writeWKT(g9,byid=FALSE)
writeWKT(g9,byid=TRUE)

# Instead of gDistance (for planar coordinates) You can use dist2Line 
# (for angular coordinates), from the geosphere package. Although it is called dist2Line, 
# the function also works for (Spatial) Polygons*.
library(geosphere)
dist2Line(spdf[57,],chn_mapdata_1[2,])

#计算北京地理面积
areaPolygon(chn_mapdata_1[2,])
#计算北京周长
perimeter(chn_mapdata_1[2,])

raster::distance(spdf[57,],map)
raster_map = map
class(raster_map) = "raster"
raster::distance(spdf[57,],raster_map)


#create a new raster and set all its values to unity.
rt <- raster(nrows=18, ncols=36) 
rt <- setValues(rt,runif(ncell(rt),0,1))
#create a Transition object from the raster
trt <- transition(rt,function(x) 1/mean(x),8)

#asymmetric
ncf <- function(x) max(x) - x[1] + x[2]
trt2 <- transition(rt,ncf,8, symm=FALSE)

#create two sets of coordinates
sP1 <- cbind(c(65,5,-65),c(55,35,-35))
sP2 <- cbind(c(50,15,-40),c(80,20,-5))

#from and to identical
costDistance(trt,sP1)
costDistance(trt2,sP1)

#from and to different
costDistance(trt2,sP1,sP2)
costDistance(trt2,sP1,sP2)

mall_detail = read_xlsx("~/data/mall_detail.xlsx")
self_managed_business = read_xlsx("~/data/selfmanaged_business.xlsx")

library(geosphere)
distHaversine
