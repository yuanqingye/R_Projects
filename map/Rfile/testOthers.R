require(sp)
require(spdep)

# Create SpatialPolygonsDataFrame for 3 squares
poly1 <- Polygons(list(Polygon(matrix(c(15.7,42.3,16.7,42.3,16.7,41.6,15.7,41.6,15.7,42.3), 
                                      nrow=5, ncol=2, byrow=TRUE))),"1")     
poly2 <- Polygons(list(Polygon(matrix(c(15.7,42.3,16.7,42.3,16.7,41.6,15.7,41.6,15.7,42.3)+0.5, 
                                      nrow=5, ncol=2, byrow=TRUE))),"2")     
poly3 <- Polygons(list(Polygon(matrix(c(13.8, 45.4, 15.6, 45.4,15.6, 43.7,13.8, 43.7,13.8, 45.4), 
                                      nrow=5, ncol=2, byrow=TRUE))),"3")                      
spolys = SpatialPolygons(list(poly1,poly2,poly3),1:3)
spolys <- SpatialPolygonsDataFrame(spolys, data.frame(ID=sapply(slot(spolys, "polygons"), 
                                                                function(x) slot(x, "ID"))) )   
proj4string(spolys) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Centroid coordinates (not used but provided for example) 
coords <- coordinates(spolys)

# Create K Nearest Neighbor list
skNN.nb <- knn2nb(knearneigh(coordinates(spolys), longlat=TRUE), 
                  row.names=spolys@data$ID)

# Calculate maximum distance for all linkages 
maxDist <- max(unlist(nbdists(skNN.nb, coordinates(spolys), longlat=TRUE)))

# Create spdep distance object
sDist <- dnearneigh(coordinates(spolys), 0, maxDist^2, row.names=spolys@data$ID)
summary(sDist, coordinates(spolys), longlat=TRUE)

# Plot neighbor linkages                  
plot(spolys, border="grey") 
plot(sDist, coordinates(spolys), add=TRUE)  

# Create neighbor distance list 
( dist.list <- nbdists(sDist, coordinates(spolys), longlat=TRUE) )

# Minimum distance 
( dist.min <- lapply(dist.list, FUN=min) )

# Distance coefficient of variation    
( dist.cv <- lapply(dist.list, FUN=function(x) { sd(x) / mean(x) } ) )


 library(geosphere)
 Lon = c(1:9/1000, 1:9/100, 1:9/10, 1:90*2)
 Lat = c(1:9/1000, 1:9/100, 1:9/10, 1:90)
 dcos = distCosine(c(0,0), cbind(Lon, Lat))
 dhav = distHaversine(c(0,0), cbind(Lon, Lat))
 dvsp = distVincentySphere(c(0,0), cbind(Lon, Lat))
 par(mfrow=(c(1,2)))
 plot(log(dcos), dcos-dhav, col='red', ylim=c(-1e-05, 1e-05),
        xlab="Log 'Law of Cosines' distance (m)",
        ylab="Law of Cosines minus Haversine distance")
 plot(log(dhav), dhav-dvsp, col='blue',
        xlab="Log 'Haversine' distance (m)",
        ylab="Vincenty Sphere minus Haversine distance")
 
  dvse = distVincentyEllipsoid(c(0,0), cbind(Lon, Lat))
  plot(dvsp/1000, (dvsp-dvse)/1000, col='blue', xlab='Vincenty Sphere Distance (km)',
         ylab="Difference between 'Vincenty Sphere' and 'Vincenty Ellipsoid' methods (km)")
  
   LA <- c(-118.40, 33.95)
   NY <- c(-73.78, 40.63)
   data(wrld)
   plot(wrld, type='l')
   gc <- greatCircle(LA, NY)
   lines(gc, lwd=2, col='blue')
   gci <- gcIntermediate(LA, NY)
   lines(gci, lwd=4, col='green')
   points(rbind(LA, NY), col='red', pch=20, cex=2)
   mp <- midPoint(LA, NY)
   onGreatCircle(LA,NY, rbind(mp,c(0,0)))
   points(mp, pch='*', cex=3, col='orange')
   gcb = greatCircleBearing(LA, brng=270, n=10)
   lines(gcb,lwd = 3,col = "red")
   
    destPoint(LA, b=65, d=100000)
    circle=destPoint(c(0,80), b=1:365, d=1000000)
    circle2=destPoint(c(0,80), b=1:365, d=500000)
    circle3=destPoint(c(0,80), b=1:365, d=100000)
    plot(circle, type='l')
    polygon(circle, col='blue', border='black', lwd=4)
    polygon(circle2, col='red', lwd=4, border='orange')
    polygon(circle3, col='white', lwd=4, border='black')
    
     ml <- gcMaxLat(LA, NY)
     lat0 <- gcLat(LA, NY, lon=0)
     lon0 <- gcLon(LA, NY, lat=0)
     plot(wrld, type='l')
     lines(gc, lwd=2, col='blue')
     points(ml, col='red', pch=20, cex=2)
     points(cbind(0, lat0), pch=20, cex=2, col='yellow')
     points(t(rbind(lon0, 0)), pch=20, cex=2, col='green')
     f <- function(lon){gcLat(LA, NY, lon)}
     opt <- optimize(f, interval=c(-180, 180), maximum=TRUE)
     points(opt$maximum, opt$objective, pch=20, cex=2, col='dark green' )
     anti <- antipode(c(opt$maximum, opt$objective))
     points(anti, pch=20, cex=2, col='dark blue' )
     
      SF <- c(-122.44, 37.74)
      AM <- c(4.75, 52.31)
      gc2 <- greatCircle(AM, SF)
      plot(wrld, type='l')
      lines(gc, lwd=2, col='blue')
      lines(gc2, lwd=2, col='green')
      int <- gcIntersect(LA, NY, SF, AM)
      int
      antipodal(int[,1:2], int[,3:4])
     
      points(rbind(int[,1:2], int[,3:4]), col='red', pch=20, cex=2)
      bearing1 <- bearing(LA, NY)
      bearing2 <- bearing(SF, AM)
      bearing1
      gcIntersectBearing(LA, bearing1, SF, bearing2)
      
      MS <- c(-93.26, 44.98)
      gc1 <- greatCircleBearing(NY, 281)
      gc2 <- greatCircleBearing(MS, 195)
      gc3 <- greatCircleBearing(LA, 55)
      plot(wrld, type='l', xlim=c(-125, -70), ylim=c(20, 60)) #critical!!
      lines(gc1, col='green')
      lines(gc2, col='blue')
      lines(gc3, col='red')
      int <- gcIntersectBearing(rbind(NY, NY, MS),
                                   c(281, 281, 195), rbind(MS, LA, LA), c(195, 55, 55))
      int
      
      distm(rbind(int[,1:2], int[,3:4]))
      int <- int[,1:2]
      points(int)
      poly <- rbind(int, int[1,])
      centr <- centroid(poly)
      poly2 <- makePoly(int)
      polygon(poly2, col='yellow')
      points(centr, pch='*', col='dark red', cex=2)
      
      d = distm(LA,NY)
      b = bearing(LA,NY)
      destPoint(LA,b,d)
      finalBearing(LA,NY)
      
      atd <- alongTrackDistance(LA, NY, MS)
      p <- destPoint(LA, b, atd)
      plot(wrld, type='l', xlim=c(-130,-60), ylim=c(22,52))
      lines(gci, col='blue', lwd=2)
      points(rbind(LA, NY), col='red', pch=20, cex=2)
      points(MS[1], MS[2], pch=20, col='blue', cex=2)
      lines(gcIntermediate(LA, p), col='green', lwd=3)
      lines(gcIntermediate(MS, p), col='dark green', lwd=3)
      points(p, pch=20, col='red', cex=2)
      dist2gc(LA, NY, MS)
      
      line <- rbind(c(-180,-20), c(-150,-10), c(-140,55), c(10, 0), c(-140,-60))
      pnts <- rbind(c(-170,0), c(-75,0), c(-70,-10), c(-80,20), c(-100,-50),
                      + c(-100,-60), c(-100,-40), c(-100,-20), c(-100,-10), c(-100,0))
      d = dist2Line(pnts, line)
      plot( makeLine(line), type='l')
      points(line)
      points(pnts, col='blue', pch=20)
      points(d[,2], d[,3], col='red', pch='x', cex=2)
      for (i in 1:nrow(d)) lines(gcIntermediate(pnts[i,], d[i,2:3], 10), lwd=2, col='green')
      
      NP <- c(0, 85)
      bearing(SF, NP)
      b <- bearingRhumb(SF, NP)
      dc <- distCosine(SF, NP)
      dr = distRhumb(SF,NP)
      dc/dr
      
      pr <- destPointRhumb(SF, b, d=round(dr/100) * 1:100)
      pc <- rbind(SF, gcIntermediate(SF, NP), NP)
      par(mfrow=c(1,2))
      data(wrld)
      plot(wrld, type='l', xlim=c(-140,10), ylim=c(15,90), main='Equirectangular')
      lines(pr, col='blue')
      lines(pc, col='red')
      data(merc)
      plot(merc, type='l', xlim=c(-15584729, 1113195),
              ylim=c(2500000, 22500000), main='Mercator')
      lines(mercator(pr), col='blue')
      lines(mercator(pc), col='red')
      
      pol <- rbind(c(-120,-20), c(-80,5), c(0, -20), c(-40,-60), c(-120,-20))
      areaPolygon(pol)
      perimeter(pol)
      centroid(pol)
      span(pol, fun=max)
      nicepoly = makePoly(pol)
      plot(pol, xlab='longitude', ylab='latitude', cex=2, lwd=3, xlim=c(-140, 0))
      lines(wrld, col='grey')
      lines(pol, col='red', lwd=2)
      lines(nicepoly, col='blue', lwd=2)
      points(centroid(pol), pch='*', cex=3, col='dark green')
      text(centroid(pol)-c(0,2.5), 'centroid')
      legend(-140, -48, c('planar','spherical'), lty=1, lwd=2,
                col=c('red', 'blue'), title='polygon type')
      
      plot(wrld, type='l', col='grey')
      a = randomCoordinates(500)
      points(a, col='blue', pch=20, cex=0.5)
      b = regularCoordinates(3)
      points(b, col='red', pch='x')
      
      #leaflet plot 
      #http://blog.csdn.net/allenlu2008/article/details/52823492
      library(leaflet)
      leaflet()%>%setView(lng=116.38,lat=39.9,zoom=3)%>%
        addTiles()%>%addProviderTiles("Esri.WorldStreetMap")      
      leaflet()%>%setView(lng=116.38,lat=39.9,zoom=3)%>%
        addTiles()%>%addProviderTiles("Esri.WorldImagery")
      leaflet()%>%setView(lng=116.38,lat=39.9,zoom=3)%>%
        addTiles()%>%addProviderTiles("Esri.WorldTerrain")
      leaflet()%>%setView(lng=116.38,lat=39.9,zoom=3)%>%
        addTiles()%>%addProviderTiles("Esri.NatGeoWorldMap")
      leaflet()%>%setView(lng=116.38,lat=39.9,zoom=2)%>%
        addTiles()%>%addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
      
      leaflet() %>% 
        setView(lng = 110, lat = 30, zoom = 2) %>%
        addTiles() %>% 
        addProviderTiles("NASAGIBS.ModisTerraTrueColorCR",
                         options = providerTileOptions(
                           time = "2015-01-15", opacity = 1))
      
      leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
        addWMSTiles(
          "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
          layers = "nexrad-n0r-900913",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          attribution = "Weather data ? 2012 IEM Nexrad"
        )
      
      #general talking about how to implement maps
      #http://www.cdadata.com/8040
      library(maps)
      map("world", fill = TRUE, col = rainbow(200),ylim = c(-90, 90), mar = c(0, 2, 0, 0))
      
      library(sp)
      # con<-file("~/data/map_data/CHN_adm1.rds")
      # open(con)#打开链结
      # print(load(con))
      # close(con)#关闭链结
      # spplot(gadm[1])
      chn_mapdata = readRDS("~/data/map_data/CHN_adm0.rds") 
      spplot(chn_mapdata[1])
      
      library(ggmap);
      map <- get_map(location = 'Zhejiang', zoom = 10, maptype = 'roadmap')
      ggmap(map)
      
      #用googleVis包的另一种方法
      ##http://www.cdadata.com/8040
      library(googleVis)
      provname=c('CN-11','CN-12','CN-13','CN-14','CN-15',
                  'CN-21','CN-22','CN-23','CN-31','CN-32',
                  'CN-33','CN-34','CN-35','CN-36','CN-37',
                  'CN-41','CN-42','CN-43','CN-44','CN-45',
                  'CN-46','CN-50','CN-51','CN-52','CN-53',
                  'CN-54','CN-61','CN-62','CN-63','CN-64','CN-65')
      pop=c(110.56,112.51,113.43,112.52,108.45,112.83,111.23,109.71,110.64,116.51,
            113.86,127.85,117.93,114.74,112.17,118.46,128.18,126.16,130.30,
            125.55,135.64,115.13,116.01,107.03,108.71,102.73,122.10,114.82,
            110.35,108.79,106.12)
      ##
      library(googleVis)
      a<-data.frame(provname,pop)
      G2 <- gvisGeoChart(a, locationvar='provname', colorvar='pop',options=list(region='CN',displayMode='regions',resolution='provinces',colorAxis='{colors: ["yellow","red"]}' ))
      plot(G2)
      
      #Get the world map and select two countries [Colombia and Venezuela]
      library(maptools) #To get the polygon data 
      data(wrld_simpl)
      colven <- c("Colombia", "Venezuela")
      colven_map <- wrld_simpl[wrld_simpl$NAME %in% colven, ]
      
      # Raster data can be created with this code:
      library(raster)
      raster <- raster(colven_map, nrow=100, ncol=100)
      raster[] <- 1:length(raster)
      raster_colven <- mask(raster, colven_map)
      
      #example data
      library(maptools) #To get the polygon data 
      data(wrld_simpl)
      colven <- wrld_simpl[wrld_simpl$NAME %in% c("Colombia", "Venezuela"), ]
      
      library(raster)
      raster <- raster(colven, nrow=100, ncol=100)
      raster[] <- 1:length(raster)
      raster <- mask(raster, colven)
      # I create some random cells of interest
      
      set.seed(33)
      cells <- sample(ncell(raster), 10)
      xy <- xyFromCell(raster, cells)
      sp <- SpatialPoints(xy, proj4string=crs(colven))
      # And then use rgeos
      
      library(rgeos)
      # inside Colombia only
      col <- colven[colven$NAME == "Colombia", ]
      sp <- gIntersection(sp, col)
      
      # get the border between Venezuela and Colombia
      ven <- colven[colven$NAME == "Venezuela", ]
      border <- gIntersection(col, ven)
      
      # get the distance
      # this fails for me, that seems to be a bug in rgeos for these data
      gDistance(sp, border, byid=TRUE)  