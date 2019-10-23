# created MA 07-18-2019
rm(list = ls())
library(tidyverse)

# Large Woody Debris
wood = Wood_2_Hawley2019

##Calculate LWD Metrics##
Wood_Metrics <- Wood %>%
  select(Large.Wood.Number, Length..m., Diameter..m., 
         Wet., Channel.Forming., Ballasted., ParentGlobalID) %>%
  mutate(Wood_A = Diameter..m.*Length..m.) %>% ##Calculate individual piece area in m2
  mutate(Wood_V = pi * ((Diameter..m./2)^2) * Length..m.) %>%  ##Calculate individual piece Volume m3
  group_by(ParentGlobalID) %>% ##Calculate total wood volume, area, pieces, and decriptors
  mutate(LWD_VT = sum(Wood_V)) %>%
  mutate(LWD_AT = sum(Wood_A)) %>%
  mutate(LWD_Pieces = length(ParentGlobalID)) %>%
  mutate(LWD_Wet = sum(Wet. == "Yes")) %>%
  mutate(LWD_ChnFrm = sum(Channel.Forming. == "Yes")) %>%
  mutate(LWD_Ballast = sum(Ballasted. == "Yes")) %>%
  distinct(ParentGlobalID, .keep_all = TRUE) %>%
  select(LWD_VT, LWD_AT, 
         LWD_Pieces, LWD_Wet, LWD_ChnFrm, 
         LWD_Ballast, ParentGlobalID) 

##################################################################################

rm(list=ls())
##Set working directory housing all habitat files##
setwd("C:/Users/richiec/Dropbox/Sampling/HABITAT/Survey123_2019/Hawley_Survey123_2019")

##Read In Channel Unit File
Channel_Units <-read.csv("CU_1.csv", TRUE, ",") 
##Convert to tibble
Channel_Units <- tbl_df(Channel_Units)

####Read in habitat files####

##Large woody debris##
Wood <- read.csv('Wood_2.csv')
Wood <- tbl_df(Wood)

##Wood Jam##
Wood_Jam <- read.csv("Jam_3.csv")
Wood_Jam <- tbl_df(Wood_Jam)

##Undercut##
Undercut <- read.csv("Undercut_4.csv")
Undercut <- tbl_df(Undercut)

##Discharge##
Discharge_Location <- read.csv("Discharge_5.csv")
Discharge_Location <- tbl_df(Discharge_Location)
Discharge_Meas <- read.csv("DischargeMeasurements_6.csv")
Discharge_Meas <- tbl_df(Discharge_Meas)

####Start calculating metrics####

##Calculate Wood Jam Metrics## 
Jam_Metrics <- Wood_Jam %>%
  select(Length..m., Width..m., Height..m., 
         Estimated.Number.of.Pieces, ParentGlobalID) %>%
  mutate(Jam_Volume = Length..m. * Width..m. * Height..m.) %>%
  select(Estimated.Number.of.Pieces, Jam_Volume, ParentGlobalID,)
## DONE MA 7-19-2019

##Calculate LWD Metrics##
Wood_Metrics <- Wood %>%
  select(Large.Wood.Number, Length..m., Diameter..m., 
         Wet., Channel.Forming., Ballasted., ParentGlobalID) %>%
  mutate(Wood_A = Diameter..m.*Length..m.) %>% ##Calculate individual piece area in m2
  mutate(Wood_V = pi * ((Diameter..m./2)^2) * Length..m.) %>%  ##Calculate individual piece Volume m3
  group_by(ParentGlobalID) %>% ##Calculate total wood volume, area, pieces, and decriptors
  mutate(LWD_VT = sum(Wood_V)) %>%
  mutate(LWD_AT = sum(Wood_A)) %>%
  mutate(LWD_Pieces = length(ParentGlobalID)) %>%
  mutate(LWD_Wet = sum(Wet. == "Yes")) %>%
  mutate(LWD_ChnFrm = sum(Channel.Forming. == "Yes")) %>%
  mutate(LWD_Ballast = sum(Ballasted. == "Yes")) %>%
  distinct(ParentGlobalID, .keep_all = TRUE) %>%
  select(LWD_VT, LWD_AT, 
         LWD_Pieces, LWD_Wet, LWD_ChnFrm, 
         LWD_Ballast, ParentGlobalID)
## DONE MA 7-19-2019
  

##Merge LWD and Jam Metrics##
Wood_All <- full_join(Wood_Metrics, Jam_Metrics, by = "ParentGlobalID") %>%
  mutate_at(c("LWD_VT", "LWD_AT", "LWD_Pieces", 
              "LWD_Wet", "LWD_ChnFrm", "LWD_Ballast", 
              "Estimated.Number.of.Pieces","Jam_Volume"), 
            funs(replace(., is.na(.), 0))) %>%
  mutate(Jam_Pieces = Estimated.Number.of.Pieces) %>%
  select(-Estimated.Number.of.Pieces)
  
##Calculate undercut metrics##
Undercut_Metrics <- Undercut %>%
  select(Length..m., Width.25...m., Width.50...m., 
         Width.75...m., ParentGlobalID) %>%
  mutate(Undercut_A = Length..m.*((Width.25...m.*Width.50...m.*Width.75...m.)/3)) %>%
  group_by(ParentGlobalID) %>%
  mutate(Undercut_Lngt = sum(Length..m.)) %>%
  mutate(N_Undercuts = length(ParentGlobalID)) %>%
  mutate(Undercut_AT = sum (Undercut_A)) %>%
  distinct(ParentGlobalID, .keep_all = TRUE) %>%
  select(Undercut_AT, Undercut_Lngt, N_Undercuts,
         ParentGlobalID)

##Calculate Grain size d50, d84##
d50_d84 <- Channel_Units %>%
  select(Pebble.1..mm., Pebble.2..mm., Pebble.3..mm., 
         Pebble.4..mm., Pebble.5..mm., Pebble.6..mm., 
         Pebble.7..mm., Pebble.8..mm., Pebble.9..mm., 
         Pebble.10..mm., Pebble.11..mm.) %>%
  gather(key = "pebble", value = "sizeMM", Pebble.1..mm.:Pebble.11..mm.) %>%
  filter(!is.na(sizeMM)) %>%
  mutate(d50 = quantile(sizeMM, probs = 0.5)) %>%
  mutate(d84 = quantile(sizeMM, probs = 0.84)) %>%
  distinct(d50, d84, .keep_all = TRUE) %>%
  select(d50, d84)

##Calculate Ocular Substrate Estimates(replacing NA)##
Channel_Units <- Channel_Units %>%
  mutate_at(c("Sand.Fines.2mm", "Gravel.2.64mm", 
              "Cobble.64.256mm", "Boulder.256mm"), 
            funs(replace(., is.na(.), 0)))

##Calculate Cover Metrics (replacing NA)##
Channel_Units <- Channel_Units %>%
  mutate_at(c("Overhanging.Cover", "Aquatic.Vegetation", 
              "Woody.Debris.Cover", "Artificial.Cover"), 
            funs(replace(., is.na(.), 0)))

##Calculate residual depth (replacing NA)##
Channel_Units <- Channel_Units %>%
  mutate(Resid_Depth = Maximum.Depth..m. - Thalweg.Exit.Depth..m.)

##Calculate Avg SSC Width##
SSC_Width <- Channel_Units %>%
  select("Channel.Unit.Type", "Channel.Unit.Number",
         "Width.1", "Width.2", "Width.3",
         "Width.4", "Width.5") %>%
  mutate(Avg_wth = ((Width.1 + Width.2
         + Width.3 + Width.4 + Width.5)/5))

####Merging CU metrics together####working on this now
Channel_Unit_Metrics <- Channel_Units %>%
  select(-ObjectID, -ParentGlobalID, -CreationDate, 
         -Creator, -EditDate, -Editor) %>%
  left_join(Wood_All, by = c("GlobalID" = "ParentGlobalID")) %>%
  mutate_at(c("LWD_VT", "LWD_AT", "LWD_Pieces", 
            "LWD_Wet", "LWD_ChnFrm", "LWD_Ballast", 
            "Jam_Volume", "Jam_Pieces"), 
            funs(replace(., is.na(.), 0))) %>%
  left_join(Undercut_Metrics, by = c("GlobalID" = "ParentGlobalID")) %>%
  mutate_at(c("Undercut_AT", "Undercut_Lngt", "N_Undercuts"),
              funs(replace(., is.na(.), 0))) 

####Still need to figure out how to merge d50, d84, ssc width, and discharge####






####The code below here needs to be adapted and improved for site and/or reach level metric calculations###

##Removing NA where necessary          
Site_CU_Met <- Site_CU_Metrics %>%
  mutate_at(c(8:16, 19:22), funs(replace(., is.na(.), 0)))



##Read in spatial file generated from drone survey for main channel and large side channels##
Polys <- st_read("D:/MRA_Data/Area_Shapefiles/UpperSalmon_Shapefiles/US_Wetted_Length.shp")

Poly_Hab <- Polys %>%
  left_join(Site_CU_Met, by = c("Unit_Numbe" = "Unit_Number")) 



##Read in spatial file generated from drone survey for small side channels##
Lines <- st_read("D:/MRA_Data/Area_Shapefiles/UpperSalmon_Shapefiles/US_Side_Channels.shp")

Lines_Hab <- Lines %>%
  left_join(Site_CU_Met, by = c("Unit_Numbe" = "Unit_Number"))






##Calculate site level metrics###################################


##Site Length and area##

Site_length <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Segment_Number == 1) %>%
  select("Length") %>%
  sum()

Site_area <- Poly_Hab %>%
  st_drop_geometry() %>%
  select("SHAPE_Area") %>%
  sum()

##LSC total length and area ##

LSC_Length <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Segment_Number > 1) 

if(nrow(LSC_Length) == 0) {
  LSC_Length = 0
} else {
  
  LSC_Length = sum(LSC_Length$Length)
}



LSC_Area <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Segment_Number > 1) 

if(nrow(LSC_Area) == 0) {
  LSC_Area = 0
} else {
  
  LSC_Area = sum(LSC_Area$SHAPE_Area)
}



##SSC total Length##

SSC_Length <- Lines_Hab %>%
  st_drop_geometry() %>%
  select(SHAPE_Leng) %>%
  sum()


SSC_Area <- Lines_Hab %>%
  st_drop_geometry() %>%
  select(SHAPE_Area) %>%
  sum()

##Total side channel length and area##

Total_SC_lng <- LSC_Length + SSC_Length
Total_SC_Ar <- LSC_Area + SSC_Area

#Total site length and area##
Total_area <- sum(Site_area, SSC_Area)

Total_Length <- sum(Site_length, LSC_Length, SSC_Length)


##Total wood piece and volume and piece/volume per 100m##


Site_Wood_Cnt <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(Wood_Count) %>%
  sum(na.rm = TRUE)

Site_Wood_V <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(Wood_Volume) %>%
  sum(na.rm = TRUE)

Piece_100m <- (Site_Wood_Cnt / Site_length) * 100

Wood_V_100m <- (Site_Wood_V / Site_length) * 100 


##Total undercut site stuff and per 100m##


Site_UC_Cnt <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(Undercut_Count) %>%
  sum(na.rm = TRUE)

Site_UC_Lng <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(Undercut_L) %>%
  sum(na.rm = TRUE)

Site_UC_V <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(Undercut_Volume) %>%
  sum(na.rm = TRUE)


Site_UC_Cnt_100 <- (Site_UC_Cnt / Site_length) * 100

Site_UC_V_100 <- (Site_UC_V / Site_length) * 100


###Site level channel unit distribution metrics##

N_Pools <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Pool") %>%
  nrow()



N_Riffles <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Riffle") %>%
  nrow()


N_Runs <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Run") %>%
  nrow()


N_Rapids <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Rapid") %>%
  nrow()

CU_100m <- (nrow(Poly_Hab) + nrow(Lines_Hab)) / Site_length * 100

Pools_100m <- (N_Pools / Site_length) * 100

Pool_Area <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Pool") %>%
  select(SHAPE_Area) %>%
  sum()

Perc_Pool_Area <- Pool_Area / Site_area

Run_Area <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Run") %>%
  select(SHAPE_Area) %>%
  sum()

Perc_Run_Area <- Run_Area / Site_area


Riffle_Area <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Riffle") %>%
  select(SHAPE_Area) %>%
  sum()

Perc_Riffle_Area <- Riffle_Area / Site_area

Rapid_Area <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Rapid") 


if(nrow(Rapid_Area) == 0) {
  Rapid_Area = 0
} else {
  
  Rapid_Area = sum(Rapid_Area$SHAPE_Area)
}


Perc_Rapid_Area <- Rapid_Area / Site_area



Perc_Slow_Area <- (sum(Pool_Area, Run_Area)) / Site_area

Perc_Fast_Area <- (sum(Riffle_Area, Rapid_Area)) / Site_area

###Site level fish and aquatic vegetation cover##

Mean_Fish_Cover <- Poly_Hab %>%
  st_drop_geometry() %>%
  select(A_Veg_Cov, No_Cov, SiteName) %>%
  mutate(Tot_No_Cov = No_Cov - A_Veg_Cov) %>%
  mutate(Mean_No_Cov = mean(Tot_No_Cov)) %>%
  mutate(Fish_Cov = 100 - (No_Cov + A_Veg_Cov)) %>%
  mutate(Mean_Fish_Cov = mean(Fish_Cov)) %>%
  mutate(Mean_A_Veg = mean(A_Veg_Cov)) %>%
  select(SiteName, Mean_A_Veg , Mean_No_Cov , Mean_Fish_Cov) %>%
  distinct(.keep_all = TRUE) 


###Site level pool depths###

Site_Dpth_Avg <- Poly_Hab %>%
  st_drop_geometry() %>%
  filter(Unit_Type == "Pool") %>%
  select(Max_Depth) %>%
  filter(!is.na(Max_Depth)) %>%
  mutate(Site_Max_Dpth = mean(Max_Depth)) %>%
  select(Site_Max_Dpth) %>%
  distinct()




###Calculate site level D50 and d84

Site_Pebble <- PebbleTidy %>%
  select(SizeMM) %>%
  mutate(D50 = quantile(SizeMM, probs = 0.5)) %>%
  mutate(D84 = quantile(SizeMM, probs = 0.84)) %>%
  select(D50, D84) %>%
  distinct(.keep_all = TRUE)


###Calculate Site level ocular substrate estimates

Site_Level_Ocular <- Site_CU_Met %>%
  filter(Unit_Type == c("Run", "Pool")) %>%
  select(Perc_Fines, Perc_Gravels, Perc_Cobbles, Perc_Boulders) %>%
  mutate(Site_Fines = mean(Perc_Fines, na.rm = TRUE)) %>%
  mutate(Site_Gravel = mean(Perc_Gravels, na.rm = TRUE)) %>%
  mutate(Site_Cobble = mean(Perc_Cobbles, na.rm = TRUE)) %>%
  mutate(Site_Boulder = mean(Perc_Boulders, na.rm = TRUE)) %>%
  select(5:8) %>%
  distinct(.keep_all = TRUE)







####Join site level metrics to Fish cover data frame###

Site_Lvl_Metrics <- Mean_Fish_Cover %>%
  cbind(CU_100m) %>%
  cbind(LSC_Area) %>%
  cbind(LSC_Length) %>%
  cbind(Perc_Fast_Area) %>%
  cbind(Perc_Pool_Area) %>%
  cbind(Perc_Rapid_Area) %>%
  cbind(Perc_Riffle_Area) %>%
  cbind(Perc_Run_Area) %>%
  cbind(Perc_Slow_Area) %>%
  cbind(Piece_100m) %>%
  cbind(Wood_V_100m) %>%
  cbind(Pools_100m) %>%
  cbind(Site_UC_Cnt) %>%
  cbind(Site_UC_Cnt_100) %>%
  cbind(Site_UC_Lng) %>%
  cbind(Site_UC_V_100) %>%
  cbind(SSC_Area) %>%
  cbind(SSC_Length) %>%
  cbind(Total_SC_Ar) %>%
  cbind(Total_SC_lng) %>%
  cbind(Site_Pebble) %>%
  cbind(Site_Level_Ocular) %>%
  cbind(Site_Dpth_Avg)






###Write out newly created csv and shapefiles###

##Site level metrics##

write.csv(Site_Lvl_Metrics, "D:/MRA_Data/Metrics/US_Site_Lvl_Met.csv")

##Channel unit level with areas## 

st_write(Poly_Hab, "D:/MRA_Data/Metrics/US_Poly_Hab.shp")
st_write(Lines_Hab, "D:/MRA_Data/Metrics/US_Lines_Hab.shp")




####Code for generating classified drone imagery .tif####

library(snow)
library(rgdal)
library(raster)
library(caret)


setwd("F:/US_Imagery/Salmon_2018/Classify")

##Read in imagery .tif with all bands##
img <- brick("US_mask.tif")

##Rename Imagery Bands
names(img) <- paste0("B", 1:5)

##Read in shapefile with training polygons 
trainData <- shapefile("F:/US_Imagery/Salmon_2018/Classify/Train_Samples.shp")
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
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples),]



modFit_rf <- train(as.factor(class) ~ B1 + B2 + B3 + B4, method = "rf", data = sdfAll)
beginCluster()
pred_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()

plot(pred_rf)



writeRaster(pred_rf, "Classified_1.tif")





