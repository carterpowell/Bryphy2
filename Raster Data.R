
# Libraries ---------------------------------------------------------------
require(raster)
require(rgdal)
library(maps)
library(mapdata)
library(dismo) 
library(rJava) 
library(maptools)
library(jsonlite)
require(wesanderson)
require(grDevices)
# Overlaying A World Clim Raster onto our Raster --------------------------

#Download data from World Clim
currentEnv=getData("worldclim", var="bio", res=2.5)

 #Crop Our Data to show only the new world
model.extent<-extent(min(-170),max(-20),min(-60),max(110))
modelEnv=crop(currentEnv,model.extent)

#Map mean annual temperature as a test (may take ahile)
plot(modelEnv[["bio1"]]/10, main="Annual Mean Temperature")
map('worldHires',xlim=c(min(-170),max(-20)), ylim=c(min(-60),max(100)), fill=FALSE, add=TRUE)

#Project latitude and longitude onto our raster
  #Create new gradient
colfunc <- colorRampPalette(c("dodgerblue", "darkgoldenrod1", "firebrick1"))
MossRichnessRasterNAll <- projectRaster(MossRichnessRasterNA, crs='+proj=longlat')
croppedMossRichnessRasterNAll = crop(MossRichnessRasterNAll, model.extent)
plot(croppedMossRichnessRasterNAll, col = colfunc(200))

#Resample to same grid:
r.new = resample(modelEnv, croppedMossRichnessRasterNAll, "bilinear")

#If required (for masking), set extents to match:
ex = extent(modelEnv)
r2 = crop(croppedMossRichnessRasterNAll, ex)

#Removed data which falls outside one of the rasters (if you need to):
r.new = mask(r.new, r2)


plot(r.new[["bio1"]]/10, main="Annual Mean Temperature", col =  colfunc(200))



