
# Libraries ---------------------------------------------------------------
require(raster)
require(rgdal)
library(maps)
library(mapdata)
library(dismo) 
library(rJava) 
library(maptools)
library(jsonlite)
require(grDevices)
require(ggplot2)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(factoextra)

#Download data from World Clim
currentEnv=getData("worldclim", var="bio", res=2.5)

#Crop Our Data to show only the new world
model.extent<-extent(min(-170),max(-20),min(-60),max(110))
modelEnv=crop(currentEnv,model.extent)

MossRichnessRasterNAll <- projectRaster(MossRichnessRasterNA, crs='+proj=longlat')
croppedMossRichnessRasterNAll = crop(MossRichnessRasterNAll, model.extent)

#Resample to same grid:
WC.new = resample(modelEnv, croppedMossRichnessRasterNAll, "bilinear")

#If required (for masking), set extents to match:
ex = extent(modelEnv)
moss.raster.projection.cropped = crop(MossRichnessRasterNAll, ex)

#Removed data which falls outside one of the rasters (if you need to):
WC.new = mask(WC.new, moss.raster.projection.cropped)

Moss.LL.data <- as.data.frame(moss.raster.projection.cropped, xy =TRUE)
names(Moss.LL.data)[names(Moss.LL.data) == 'blank_100km_raster'] <- 'Richness'
Moss.LL.data.new <- Moss.LL.data[complete.cases(Moss.LL.data),]
Moss.LL.data.new1 <- Moss.LL.data.new[,1:2]

data <- data.frame(coordinates(Moss.LL.data.new1),
                   extract(WC.new, Moss.LL.data.new1))
finaldataset <- merge(data, Moss.LL.data.new, by=c("x","y")) 

#Take NA's out
finalslimdataset<- finaldataset[complete.cases(finaldataset),]

#create a dataset with only bioclim variables and no NAs
nonadata <- data[complete.cases(data),]
pcadata <- nonadata[,3:21]

#Run PCA on this Data
pca <- prcomp(pcadata, center = TRUE, scale. = TRUE) 

#Predicting
pred <- predict(pca, newdata = finalslimdataset[,3:21])
nobioclim <- finalslimdataset[, -(3:21)]
library(gtools)

preddata <- as.data.frame(pred)

bigpca <- merge(nobioclim, preddata, by="row.names", all.x=TRUE)
