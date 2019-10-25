install.packages("caret")
install.packages("raster")
library(rgdal)
install.packages("snow")
setwd()

library(rgdal)
library(raster)
library(caret)
library(snow)

setwd("F:/Upper Lemhi_Beyeler_Tyler")
band_6 <- raster("UpperLemhi_Beyeler_Tyler-0-0.tif", band = 6)
band_5 <- raster("UpperLemhi_Beyeler_Tyler-0-0.tif", band = 5)
band_4 <- raster("UpperLemhi_Beyeler_Tyler-0-0.tif", band = 4)
img_stack <- stack(band_4, band_5,band_6)
img <- brick(img_stack)

names(img) <- paste0("B", c(4:6)) 


plotRGB(img, r = 1, g = 2, b = 3, scale = 100, stretch = "hist")


trainData <- shapefile("Training.shp")
responseCol <- "class"


dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}



nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]



modFit_rf <- train(as.factor(class) ~ B4 + B5 + B6, method = "rf", data = sdfAll)



beginCluster()
preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()


plot(preds_rf)












