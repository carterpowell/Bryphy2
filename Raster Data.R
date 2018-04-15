
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
colfunc <- colorRampPalette(c("dodgerblue", "darkgreen","darkgoldenrod1", "firebrick1"))
MossRichnessRasterNAll <- projectRaster(MossRichnessRasterNA, crs='+proj=longlat')
croppedMossRichnessRasterNAll = crop(MossRichnessRasterNAll, model.extent)
plot(croppedMossRichnessRasterNAll, col = colfunc(200))

#Resample to same grid:
WC.new = resample(modelEnv, croppedMossRichnessRasterNAll, "bilinear")

#If required (for masking), set extents to match:
ex = extent(modelEnv)
moss.raster.projection.cropped = crop(MossRichnessRasterNAll, ex)

#Removed data which falls outside one of the rasters (if you need to):
WC.new = mask(WC.new, moss.raster.projection.cropped)

colfunc1 <- colorRampPalette(c("dodgerblue", "firebrick1"))

plot(WC.new[["bio1"]]/10, main="Annual Mean Temperature", col =  colfunc1(200))

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
#Loadings
head(pca$x)
pca$rotation

#Visuals
plot(pca, type = "l")

#Scatterplot
p <- ggbiplot(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)+
  geom_point(size = 0.01)
print(p)

#Circle Plot
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Plot of Variance for Each Variable
fviz_eig(pca)
