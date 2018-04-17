# this is all about sp package
xc = round(runif(10), 2)
yc = round(runif(10), 2)
xy = cbind(xc, yc)
xy

xy.sp = SpatialPoints(xy)
xy.sp
plot(xy.sp,pch = 2)

xy.cc = coordinates(xy.sp)
class(xy.cc)
bbox(xy.sp)
dimensions(xy.sp)
xy.sp[1:2]

xy.df = as.data.frame(xy.sp)
class(xy.df)
dim(xy.df)
summary(xy.sp)

df = data.frame(z1 = round(5+rnorm(10),2),z2 = 20:29)
xy.spdf = SpatialPointsDataFrame(xy.sp,df)
xy.spdf
summary(xy.spdf)
dimensions(xy.spdf)

xy.spdf[1]
xy.spdf[1,2]
xy.spdf[1:2,"z2"]

xy.newdf = as.data.frame(xy.spdf)
xy.newcc = coordinates(xy.spdf)
class(xy.newcc)

df_1 = data.frame(xy, df)
coordinates(df_1) = c("xc","yc")
df_1

df_2 = data.frame(xy,df)
coordinates(df_2) = ~xc+yc
df_2[1:2,]
as.data.frame(df_2)[1:2,]
df_2[["z2"]]
df_2$z2
df_2$z2[10] = 20
df_2[["z3"]] = 1:10

summary(df_2)

bubble(df_2, "z1", key.space = "bottom")
spplot(df_2,"z2",key.space = "bottom")

gt = GridTopology(cellcentre.offset = c(1,1,2), cellsize=c(1,1,1), cells.dim = c(3,4,6))
grd = SpatialGrid(gt)
summary(grd)

gridparameters(grd)

pts = expand.grid(x = 1:3, y = 1:4, z=2:7)
grd.pts = SpatialPixels(SpatialPoints(pts))
summary(grd.pts)

grd = as(grd.pts, "SpatialGrid")
summary(grd)

attr = expand.grid(xc = 1:3, yc = 1:3)
grd.attr = data.frame(attr, z1 = 1:9, z2 = 9:1)
coordinates(grd.attr) = ~xc+yc
gridded(grd.attr)

gridded(grd.attr) = TRUE #将其加入topology
summary(grd.attr)

fullgrid(grd.attr)
fullgrid(grd.pts)
fullgrid(grd)
spplot(grd.attr)

grd.attr$z1
grd.attr[["z2"]]
coordinates(grd.attr)
# as.array(grd.attr)

grd.attr[1:2, "z1"]

fullgrid(grd.attr) = TRUE
grd.attr[1:5, 1,c("z2","z1")]

# just 9 points on a grid:
xxx <- c(1,1,1,2,2,2,3,3,3)
yyy <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(xxx,yyy)
S <- SpatialPoints(xy)
class(S)
plot(S)
gridded(S) <- TRUE
gridded(S)
class(S)
summary(S)
plot(S)
gridded(S) <- FALSE
gridded(S)
class(S)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
plot(meuse.grid) # not much good
summary(meuse.grid)

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))
summary(Sl)

plot(Sl, col = c("red", "blue"))
df = data.frame(z = c(1,2), row.names=sapply(slot(Sl, "lines"), function(x) slot(x, "ID")))
Sldf = SpatialLinesDataFrame(Sl, data = df)
summary(Sldf)

Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")
# plot(SpP)

attr = data.frame(a=1:3, b=3:1, row.names=c("s3/4", "s2", "s1"))
SrDf = SpatialPolygonsDataFrame(SpP, attr)
as(SrDf, "data.frame")
spplot(SrDf)
SrDf = attr
polygons(SrDf) = SpP
