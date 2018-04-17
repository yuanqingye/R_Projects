library(ggmap)
library(leaflet)
way1txt <- "forbidden city, beijing, china"
way2txt <- "Olympic village, beijing, china"

route_all <- route(way1txt, way2txt, structure = "route",
                   output = "all")

# Custom decode function
# Taken from http://s4rdd.blogspot.com/2012/12/google-maps-api-decoding-polylines-for.html

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}


route_df <- decodeLine( route_all$routes[[1]]$overview_polyline$points )


# Map using Leaflet R
m = leaflet() %>% addTiles()
m = m %>% addPolylines(route_df$lon, route_df$lat, fill = FALSE)
m = m %>% addPopups(route_df$lon[1], route_df$lat[1], 'Origin')
m = m %>% addPopups(route_df$lon[length(route_df$lon)], 
                    route_df$lat[length(route_df$lon)], 'Destination')

library("gdistance")
set.seed(123)
r <- raster(ncol=3,nrow=3)
r[] <- 1:ncell(r)
r

r[] <- 1
tr1 <- transition(r, mean, directions=8)
# T <- transition(r, function(x) 1/mean(x), 8)


r <- raster(nrows=18, ncols=36) 
r <- setValues(r, runif(ncell(r)))

#Create a Transition object from the raster
tr <- transition(r, transitionFunction= mean, directions=4)

r[] <- runif(9)
ncf <- function(x) max(x) - x[1] + x[2]
tr2 <- transition(r, ncf, 4, symm=FALSE)
tr2

library(raster)

# Create RasterLayer object
r <- raster('C:/temp/binary_nad83.tif')

# Define the Proj.4 spatial reference 
# http://spatialreference.org/ref/epsg/26915/proj4/
sr <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

# Project Raster
projected_raster <- projectRaster(r, crs = sr)

# Write the RasterLayer to disk (See datatype documentation for other formats)
writeRaster(projected_raster, filename="C:/temp/binary_utm15.tif", datatype='INT1U', overwrite=TRUE)

