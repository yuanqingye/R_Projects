library(rgeos)
#gArea
gArea(readWKT("POINT(1 1)"))
gArea(readWKT("LINESTRING(0 0,1 1,2 2)"))
gArea(readWKT("LINEARRING(0 0,3 0,3 3,0 3,0 0)"))
p1 = readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0))")
p2 = readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0),(1 1,2 1,2 2,1 2,1 1))")
gArea(p1)
p1@polygons[[1]]@area
gArea(p2)
p2@polygons[[1]]@area

x = readWKT("POLYGON((0 0,10 0,10 10,0 10,0 0))")
b = gBoundary(x)
plot(x,col='black')
plot(b,col='red',lwd=3,add=TRUE)
# mod-2 rule example
x1 = readWKT("MULTILINESTRING((2 2,2 0),(2 2,0 2))")
x2 = readWKT("MULTILINESTRING((2 2,2 0),(2 2,0 2),(2 2,4 2))")
x3 = readWKT("MULTILINESTRING((2 2,2 0),(2 2,0 2),(2 2,4 2),(2 2,2 4))")
x4 = readWKT("MULTILINESTRING((2 2,2 0),(2 2,0 2),(2 2,4 2),(2 2,2 4),(2 2,4 4))")
b1 = gBoundary(x1)
b2 = gBoundary(x2)
b3 = gBoundary(x3)
b4 = gBoundary(x4)
par(mfrow=c(2,2))
plot(x1); plot(b1,pch=16,col='red',add=TRUE)
plot(x2); plot(b2,pch=16,col='red',add=TRUE)
plot(x3); plot(b3,pch=16,col='red',add=TRUE)
plot(x4); plot(b4,pch=16,col='red',add=TRUE)


#gBuffer
p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")
par(mfrow=c(2,3))
plot(gBuffer(p1,width=-0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p1,width=0),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p1,width=0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0.2")
plot(gBuffer(p2,width=-0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p2,width=0),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p2,width=0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0.2")
p3 <- readWKT(paste("GEOMETRYCOLLECTION(",
                    "POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1)),",
                    "POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1)))"))
par(mfrow=c(1,1))
plot(gBuffer(p3, byid=TRUE, width=c(-0.2, -0.1)),col='black',pbg='white',
     xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p3,border=c('blue', 'red'),lwd=2,add=TRUE);title("width: -0.2, -0.1")



library(sp)
p3df <- SpatialPolygonsDataFrame(p3, data=data.frame(i=1:length(p3),
                                                     row.names=row.names(p3)))
dim(p3df)
row.names(p3df)
dropEmpty = gBuffer(p3df, byid=TRUE, id=letters[1:nrow(p3df)], width=c(-1, 0))
dim(dropEmpty)
row.names(dropEmpty)
row.names(slot(dropEmpty, "data"))
plot(dropEmpty, col='black', pbg='white', xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p3df,border=c('blue'),lwd=2,add=TRUE);title("width: -1, 0")
par(mfrow=c(2,3))
#Style options
l1 = readWKT("LINESTRING(0 0,1 5,4 5,5 2,8 2,9 4,4 6.5)")
par(mfrow=c(2,3))
plot(gBuffer(l1,capStyle="ROUND"));plot(l1,col='blue',add=TRUE);title("capStyle: ROUND")
plot(gBuffer(l1,capStyle="FLAT"));plot(l1,col='blue',add=TRUE);title("capStyle: FLAT")
plot(gBuffer(l1,capStyle="SQUARE"));plot(l1,col='blue',add=TRUE);title("capStyle: SQUARE")
plot(gBuffer(l1,quadsegs=1));plot(l1,col='blue',add=TRUE);title("quadsegs: 1")
plot(gBuffer(l1,quadsegs=2));plot(l1,col='blue',add=TRUE);title("quadsegs: 2")
plot(gBuffer(l1,quadsegs=5));plot(l1,col='blue',add=TRUE);title("quadsegs: 5")
l2 = readWKT("LINESTRING(0 0,1 5,3 2)")
par(mfrow=c(2,3))
plot(gBuffer(l2,joinStyle="ROUND"));plot(l2,col='blue',add=TRUE);title("joinStyle: ROUND")
plot(gBuffer(l2,joinStyle="MITRE"));plot(l2,col='blue',add=TRUE);title("joinStyle: MITRE")
plot(gBuffer(l2,joinStyle="BEVEL"));plot(l2,col='blue',add=TRUE);title("joinStyle: BEVEL")
plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=0.5));plot(l2,col='blue',add=TRUE)
title("mitreLimit: 0.5")
plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=1));plot(l2,col='blue',add=TRUE)
title("mitreLimit: 1")
plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=3));plot(l2,col='blue',add=TRUE)
title("mitreLimit: 3")

#gCentroid
x = readWKT(paste("GEOMETRYCOLLECTION(POLYGON((0 0,10 0,10 10,0 10,0 0)),",
                  "POLYGON((15 0,25 15,35 0,15 0)))"))
# Centroids of both the square and circle independently
c1 = gCentroid(x,byid=TRUE)
# Centroid of square and circle together
c2 = gCentroid(x)
plot(x)
plot(c1,col='red',add=TRUE)
plot(c2,col='blue',add=TRUE)

#gContains
l1 = readWKT("LINESTRING(0 3,1 1,2 2,3 0)")
l2 = readWKT("LINESTRING(1 3.5,3 3,2 1)")
l3 = readWKT("LINESTRING(1 3.5,3 3,4 1)")
pt1 = readWKT("MULTIPOINT(1 1,3 0)")
pt2 = readWKT("MULTIPOINT(0 3,3 0)")
pt3 = readWKT("MULTIPOINT(1 1,2 2,3 1)")
p1 = readWKT("POLYGON((0 0,0 2,1 3.5,3 3,4 1,3 0,0 0))")
p2 = readWKT("POLYGON((1 1,1 2,2 2,2 1,1 1))")
par(mfrow=c(2,3))
plot(l1,col='blue');plot(pt1,add=TRUE,pch=16)
title(paste("Contains:",gContains(l1,pt1),
            "\nContainsProperly:",gContainsProperly(l1,pt1),
            "\nCovers:",gCovers(l1,pt1)))
plot(l1,col='blue');plot(pt2,add=TRUE,pch=16)
title(paste("Contains:",gContains(l1,pt2),
            "\nContainsProperly:",gContainsProperly(l1,pt2),
            "\nCovers:",gCovers(l1,pt2)))
plot(p1,col='blue',border='blue');plot(pt3,add=TRUE,pch=16)
title(paste("Contains:",gContains(p1,pt3),
            "\nContainsProperly:",gContainsProperly(p1,pt3),
            "\nCovers:",gCovers(p1,pt3)))
plot(p1,col='blue',border='blue');plot(l2,lwd=2,add=TRUE,pch=16)
title(paste("Contains:",gContains(p1,l2),
            "\nContainsProperly:",gContainsProperly(p1,l2),
            "\nCovers:",gCovers(p1,l2)))
plot(p1,col='blue',border='blue');plot(l3,lwd=2,add=TRUE,pch=16)
title(paste("Contains:",gContains(p1,l3),
            "\nContainsProperly:",gContainsProperly(p1,l3),
            "\nCovers:",gCovers(p1,l3)))
plot(p1,col='blue',border='blue');plot(p2,col='black',add=TRUE,pch=16)
title(paste("Contains:",gContains(p1,p2),
            "\nContainsProperly:",gContainsProperly(p1,p2),
            "\nCovers:",gCovers(p1,p2)))

#gConvexHull

x = readWKT(paste("POLYGON((0 40,10 50,0 60,40 60,40 100,50 90,60 100,60",
                  "60,100 60,90 50,100 40,60 40,60 0,50 10,40 0,40 40,0 40))"))
ch = gConvexHull(x)
plot(x,col='blue',border='blue')
plot(ch,add=TRUE)


#gCross
l1 = readWKT("LINESTRING(0 3,1 1,2 2,3 0)")
l2 = readWKT("LINESTRING(0 0.5,1 1,2 2,3 2.5)")
l3 = readWKT("LINESTRING(1 3,1.5 1,2.5 2)")
pt1 = readWKT("MULTIPOINT(1 1,3 0)")
pt2 = readWKT("MULTIPOINT(1 1,3 0,1 2)")
p1 = readWKT("POLYGON((0 0,0 2,1 3.5,3 3,4 1,3 0,0 0))")
p2 = readWKT("POLYGON((2 2,3 4,4 1,4 0,2 2))")
par(mfrow=c(2,3))
plot(l1,col='blue');plot(pt1,add=TRUE,pch=16)
title(paste("Crosses:",gCrosses(l1,pt1),
            "\nOverlaps:",gOverlaps(l1,pt1)))
plot(l1,col='blue');plot(pt2,add=TRUE,pch=16)
title(paste("Crosses:",gCrosses(l1,pt2),
            "\nOverlaps:",gOverlaps(l1,pt2)))
plot(l1,col='blue');plot(l2,add=TRUE)
title(paste("Crosses:",gCrosses(l1,l2),
            "\nOverlaps:",gOverlaps(l1,l2)))
plot(l1,col='blue');plot(l3,add=TRUE)
title(paste("Crosses:",gCrosses(l1,l3),
            "\nOverlaps:",gOverlaps(l1,l3)))
plot(p1,border='blue',col='blue');plot(l1,add=TRUE)
title(paste("Crosses:",gCrosses(p1,l1),
            "\nOverlaps:",gOverlaps(p1,l1)))
plot(p1,border='blue',col='blue');plot(p2,add=TRUE)
title(paste("Crosses:",gCrosses(p1,p2),
            "\nOverlaps:",gOverlaps(p1,p2)))

#gDelaunayTriangulation
if (version_GEOS0() > "3.4.0") {
  library(sp)
  data(meuse)
  coordinates(meuse) <- c("x", "y")
  plot(gDelaunayTriangulation(meuse))
  points(meuse)
  out <- gDelaunayTriangulation(meuse, onlyEdges=TRUE)
  lns <- slot(slot(out, "lines")[[1]], "Lines")
  out1 <- SpatialLines(lapply(seq(along=lns), function(i) Lines(list(lns[[i]]),
                                                                ID=as.character(i))))
  out2 <- lapply(1:length(out1), function(i) which(gTouches(meuse, out1[i],
                                                            byid=TRUE)))
  out3 <- do.call("rbind", out2)
  o <- order(out3[,1], out3[,2])
  out4 <- out3[o,]
  out5 <- data.frame(from=out4[,1], to=out4[,2], weight=1)
  head(out5)
  ## Not run:
  if (require(spdep)) {
    class(out5) <- c("spatial.neighbour", class(out5))
    attr(out5, "n") <- length(meuse)
    attr(out5, "region.id") <- as.character(1:length(meuse))
    nb1 <- sn2listw(out5)$neighbours
    nb2 <- make.sym.nb(nb1)
  }
  ## End(Not run)
}

#gDifference
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((3 3, 7 3, 7 7, 3 7, 3 3))")
d = gDifference(x,y)
plot(d,col='red',pbg='white')
# Empty geometry since y is completely contained with x
d2 = gDifference(y,x)

pt1 = readWKT("POINT(0.5 0.5)")
pt2 = readWKT("POINT(2 2)")
p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2 = readWKT("POLYGON((2 0,3 1,4 0,2 0))")
gDistance(pt1,pt2)
gDistance(p1,pt1)
gDistance(p1,pt2)
gDistance(p1,p2)
p3 = readWKT("POLYGON((0 0,2 0,2 2,0 2,0 0))")
p4 = readWKT("POLYGON((0 0,2 0,2 1.9,1.9 2,0 2,0 0))")
p5 = readWKT("POLYGON((0 0,2 0,2 1.5,1.5 2,0 2,0 0))")
p6 = readWKT("POLYGON((0 0,2 0,2 1,1 2,0 2,0 0))")
p7 = readWKT("POLYGON((0 0,2 0,0 2,0 0))")
gDistance(p3,hausdorff=TRUE)
gDistance(p3,p4,hausdorff=TRUE)
gDistance(p3,p5,hausdorff=TRUE)
gDistance(p3,p6,hausdorff=TRUE)
gDistance(p3,p7,hausdorff=TRUE)
