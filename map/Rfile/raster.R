library(raster)
r <- raster("~/data/PopulationGrid_China2010/cnpop2010.tif")

library(foreign)
rat <- read.dbf("~/data/PopulationGrid_China2010/cnpop2010.tif.vat.dbf")

colnames(rat)[1] <- "ID"
levels(r) <- rat
