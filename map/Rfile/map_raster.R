library(raster)
x = raster()

r <- raster(ncol=10, nrow=10)
hasValues(r)
set.seed(0)
values(r) <- runif(ncell(r))
inMemory(r)

values(r)[1:10]
plot(r, main='Raster with 100 cells')

res(r)
dim(r)
xmax(r)

filename <- system.file("external/test.grd", package="raster")
r = raster(filename)
plot(r, main='RasterLayer from file')

r1 <- r2 <- r3 <- raster(nrow=10, ncol=10)
# Assign random cell values
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))

s = stack(r1,r2,r3)
plot(s)

b1 <- brick(r1, r2, r3)
b2 = brick(s)

filename <- system.file("external/rlogo.grd", package="raster")
b <- brick(filename)

plot(b)
rb <- raster(b, layer=2)
plot(rb)
rb2 <- raster(filename, band=2)
plot(rb2)

s <- r + 10
plot(s)

r[] <- runif(ncell(r))
r2 <- r/2
brks <- seq(0, 1, by=0.1) 
nb <- length(brks)-1 
cols <- rev(terrain.colors(nb))
par(mfrow=c(1,2))
plot(r, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), main='first') 
plot(r2, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), main='second') 


library(rgdal)
library(raster)

## Read in the ecoregion shapefile (located in R's current working directory)
teow <- readOGR(dsn = "official_teow/official", layer = "wwf_terr_ecos")

## Set up a raster "template" to use in rasterize()
ext <-  extent (-95, -50, 24, 63)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 5
r <- raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)

## Rasterize the shapefile
rr <-rasterize(teow, r)

## A couple of outputs
writeRaster(rr, "teow.asc")
plot(rr)

library(gdistance)
r <- raster(nrows=6, ncols=7, xmn=0, xmx=7, ymn=0, ymx=6, crs="+proj=utm +units=m")
r[] <- c(2, 2, 1, 1, 5, 5, 5,
         2, 2, 8, 8, 5, 2, 1,
         7, 1, 1, 8, 2, 2, 2,
         8, 7, 8, 8, 8, 8, 5,
         8, 8, 1, 1, 5, 3, 9,
         8, 1, 1, 2, 5, 3, 9)
T <- transition(r, function(x) 1/mean(x), 8)
T <- geoCorrection(T)
c1 <- c(5.5,1.5)
A <- accCost(T, c1)
plot(A)
plot(T)

transition

library(raster)
library(sp)
library(rgdal)
setwd("~/data/map_data/NEON-DS-Field-Site-Spatial-Data/SJER")

DEM <- raster("DigitalTerrainModel/SJER2013_DTM.tif")
DEM <- setMinMax(DEM)
hist(DEM, main="Distribution of elevation values", 
     col= "purple", 
     maxpixels=22000000)

plot(DEM, 
     main="Digital Elevation Model, SJER")

image(DEM)

col <- terrain.colors(10)
image(DEM, zlim=c(250,600), main="Digital Elevation Model (DEM)", col=col)

# add a color map with 5 colors
col=terrain.colors(5)

# add breaks to the colormap (6 breaks = 5 segments)
brk <- c(250, 300, 350, 400,450,500)

plot(DEM, col=col, breaks=brk, main="DEM with more breaks")

#plot the DEM
plot(DEM)
#Define the extent of the crop by clicking on the plot
cropbox1 <- drawExtent()
#crop the raster, then plot the new cropped raster
DEMcrop1 <- crop(DEM, cropbox1)

#plot the cropped extent
plot(DEMcrop1)

library(raster)
x <- raster()
x

# With other parameters
x <- raster(ncol=36, nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
# that can be changed
res(x)
res(x) = 100
ncol(x) = 18

projection(x) <- "+proj=utm +zone=48 +datum=WGS84"

r <- raster(ncol=10, nrow=10)
ncell(r)
hasValues(r)
# use the 'values' function
# e.g.,
values(r) <- 1:ncell(r)
# or
set.seed(0)
values(r) <- runif(ncell(r))
hasValues(r)
inMemory(r)
values(r)

plot(r, main='Raster with 100 cells')
xmax(r)
xmax(r) = 0
res(r)
ncol(r) = 6
hasValues(r)

filename <- system.file("external/test.grd", package="raster")
r <- raster(filename)
filename(r)
inMemory(r)
plot(r, main='RasterLayer from file')

# create three identical RasterLayer objects
r1 <- r2 <- r3 <- raster(nrow=10, ncol=10)
# Assign random cell values
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))
# combine three RasterLayer objects into a RasterStack
s <- stack(r1, r2, r3)
s
nlayers(s)

# combine three RasterLayer objects into a RasterBrick
b1 <- brick(r1, r2, r3)
# equivalent to:
b2 <- brick(s)
# create a RasterBrick from file
filename <- system.file("external/rlogo.grd", package="raster")
filename
b <- brick(filename)
nlayers(b)

# extract a single RasterLayer
r <- raster(b, layer=2)
# equivalent to creating it from disk
r <- raster(filename, band=2)

# create an empty RasterLayer
r <- raster(ncol=10, nrow=10)
# assign values to cells
values(r) <- 1:ncell(r)
s <- r + 10
s <- sqrt(s)
s <- s * r + 5
r[] <- runif(ncell(r))
r <- round(r)
r <- r == 1

s[r] <- -0.5
s[!r] <- 5
s[s == 5] <- 15

r <- raster(ncol=5, nrow=5)
r[] <- 1
s <- stack(r, r+1)
q <- stack(r, r+2, r+4, r+6)
x <- r + s + q

a <- mean(r,s,10)
b <- sum(r,s)
st <- stack(r, s, a, b)
sst <- sum(st)
sst

cellStats(st, 'sum')

r <- raster()
r[] <- 1:ncell(r)

ra <- aggregate(r, 10)
r1 <- crop(r, extent(-180,0,0,30))
r2 <- crop(r, extent(-10,180,-20,10))
m <- merge(r1, r2, filename='test.grd', overwrite=TRUE)
plot(MM)

rr = raster(nrow = 10, ncol = 10,xmn = -5,xmx = 5,ymn = -5,ymx = 5)
plot(rr)
extent1 = drawExtent()
rr1 = crop(rr,extent1)
extent2 = drawExtent()
rr2 = crop(rr,extent2)
MM = merge(rr1,rr2,filename = "ttt.grd",overwrite = TRUE)
rr3 = shift(rr1,y = 2,x = 0)
plot(MM)
plot(flip(t(merge(rr2,rr3)),direction = x))

r <- raster(nrow=3, ncol=3)
r[] <- 1:ncell(r)
s <- raster(nrow=10, ncol=10)
s <- resample(r, s, method='bilinear')
par(mfrow=c(1,2))
plot(r)
plot(s)

rr4 = overlay(rr1,rr2,fun = function(x,y){x*y})

r <- raster(ncol=3, nrow=2)
r[] <- 1:ncell(r)
getValues(r)
#so the raster matrix assign value by rows

s <- calc(r, fun=function(x){ x[x < 4] <- NA; return(x)} )
as.matrix(s)
#so the as.matrix will return the corresponding form in raster

t <- overlay(r, s, fun=function(x, y){ x / y} )
as.matrix(t)

u <- mask(r, t)
as.matrix(u)
as.matrix(mask(t,r))

w <- cover(t, r)
as.matrix(w)


x <- reclassify(w, c(0,1,5, 1,2,2, 2,3,3))
as.matrix(x)

y <- subs(x, data.frame(id=c(2,3), v=c(40,50)))
as.matrix(y)

?focal

#so here comes out the gdistance package to calculate high level of distance

r <- raster(ncol=36,nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 
#plot(dist / 1000)
#pointDistance calculate the distance between sp points
#distanceFromPoints()

xy = c(-150,-150)
distanceFromPoints(r,xy)

r <- raster(nrow=45, ncol=90)
r[] <- round(runif(ncell(r))*3)
a <- area(r)
zonal(a, r, 'sum')

r <- raster(nrow=18, ncol=36)
a <- area(r)

r <- raster(ncols=12, nrows=12)
set.seed(0)
r[] <- round(runif(ncell(r))*0.7 )
rc <- clump(r) 
freq(rc)
plot(rc)

r <- raster(nrow=18, ncol=36, xmn=0)
r[150:250] <- 1
r[251:450] <- 2
plot( boundaries(r, type='inner') )
plot( boundaries(r, type='outer') )
plot( boundaries(r, classes=TRUE) )

#prediction
#predict
#interpolate

#transfer between sps and rasters
?rasterize
?rasterToPoints
?rasterToPolygons

r <- raster(ncol=36, nrow=18)
r[] <- runif(ncell(r))
cellStats(r, 'mean')

s = r
s[] <- round(runif(ncell(r)) * 5)
zonal(r, s, 'mean')

freq(s)
freq(s, value=3)
crosstab(r*3, s)

image(s)
zoom()
xxxx = click()
library(rasterVis)
gplot(s)

b <- brick(system.file("external/rlogo.grd", package="raster"))
plot(b)
plotRGB(b, r=1, g=2, b=3)
#hist, persp, contour, and density

r1 <- raster(ncol=36, nrow=18)
r2 <- r1
r1[] <- runif(ncell(r1))
r2[] <- runif(ncell(r1))
s <- stack(r1, r2)
sgdf <- as(s, 'SpatialGridDataFrame')
newr2 <- raster(sgdf, 2)
news <- stack(sgdf)

library(raster)
r <- raster(ncol=36, nrow=18)
ncol(r)
nrow(r)
ncell(r)
rowFromCell(r, 100)
colFromCell(r, 100)
cellFromRowCol(r,5,5)
xyFromCell(r, 100)
cellFromXY(r, c(0,0))
colFromX(r, 0)
rowFromY(r, 0)

r <- raster(system.file("external/test.grd", package="raster"))
v <- getValues(r, 50)
v[35:39]
getValuesBlock(r, 50, 1, 35, 5)
