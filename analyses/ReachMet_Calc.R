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