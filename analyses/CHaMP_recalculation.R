library(tidyverse)

setwd("C:/GIT/DASH/data/raw/champ")

##Read-in channel unit measurement data##

LWD_all <- read.csv("LargeWoodPiece.csv")
Undercut_all <- read.csv("UndercutBank.csv")
SubstrateCover <- read.csv("SubstrateCover.csv")
Channel_Unit_All <- read.csv("ChannelUnit.csv")
Side_Channel <- read.csv("SideChannel.csv")

##Selecting Channel units sampled after >=2014##

Channel_Unit_Select <- Channel_Unit_All %>%
  filter(VisitYear >= 2014) %>%
  select(1:5, 8, 11:13, 34, 36, 38, 39)

Unit_Type <- Channel_Unit_Select %>%
  select(8, 10, 12)


##Calculating channel unit wood area, volume, and total number of pieces with tier1 channel type##

LWD_Select <- LWD_all %>%
  select(1:5, 8, 11:13, 34:35, 38:40) %>%
  filter(Diameter >= 0.15, Length >= 1.5) %>%
  left_join(Unit_Type, by = c("VisitID", "ChannelUnitID"))

LWD_Metrics <- LWD_Select %>%
  mutate(Wood_A = Diameter * Length) %>%
  mutate(Wood_V = pi * ((Diameter/2)^2) * Length) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Wood_VT = sum(Wood_V)) %>%
  mutate(N_Wood = length(ChannelUnitID)) %>%
  select(8, 11, 15, 16, 18, 19) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE) 


##Creating Channel unit wood met with dry wet
LWD_Met_Dry_CU <- LWD_Select %>%
  filter(LargeWoodType == "Dry") %>%
  select(8, 11, 12:15) %>%
  mutate(Wood_A = Diameter * Length) %>%
  mutate(Wood_V = pi * ((Diameter/2)^2) * Length) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Dry_Wood_VT = sum(Wood_V)) %>%
  mutate(Dry_N_Wood = length(ChannelUnitID)) %>%
  select(1, 2, 6, 9, 10) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE) 
  
##Calculating channel unit "Wet" wood metrics, total volume and number of pieces##
LWD_Met_Wet_CU <- LWD_Select %>%
  filter(LargeWoodType == "Wet") %>%
  select(8, 11, 12:15) %>%
  mutate(Wood_A = Diameter * Length) %>%
  mutate(Wood_V = pi * ((Diameter/2)^2) * Length) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Wet_Wood_VT = sum(Wood_V)) %>%
  mutate(Wet_N_Wood = length(ChannelUnitID)) %>%
  select(1, 2, 6, 9, 10) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE) 

##Calculating site level "wet" wood metrics, total volume and number of pieces#
LWD_Met_Wet_Site <- LWD_Met_Wet_CU %>%
  group_by(VisitID) %>%
  mutate(N_Wet = sum(Wet_N_Wood)) %>%
  mutate(Wet_V_T = sum(Wet_Wood_VT)) %>%
  distinct(VisitID, .keep_all = TRUE) %>%
  select(-4, -5)


##Calculating site level "dry" wood metrics, total volume and number of pieces#
LWD_Met_Dry_Site <- LWD_Met_Dry_CU %>%
  group_by(VisitID) %>%
  mutate(N_Dry = sum(Dry_N_Wood)) %>%
  mutate(Dry_V_T = sum(Dry_Wood_VT)) %>%
  distinct(VisitID, .keep_all = TRUE) %>%
  select(-4, -5)



##Calculating wood volume metrics for Tier1 channel unit Type##
LWVol_WetFstNT <- LWD_Met_Wet_CU %>%
  filter(Tier1 == "Fast-NonTurbulent/Glide") %>%
  group_by(VisitID) %>%
  mutate(LWVol_WetFstNT = sum(Wet_Wood_VT)) %>%
  select(1, 6) %>%
  distinct(VisitID, .keep_all = TRUE)

LWVol_WetSlow <- LWD_Met_Wet_CU %>%
  filter(Tier1 == "Slow/Pool") %>%
  group_by(VisitID) %>%
  mutate(LWVol_WetSlow = sum(Wet_Wood_VT)) %>%
  select(1, 6) %>%
  distinct(VisitID, .keep_all = TRUE)


LWVol_WetFstTurb <- LWD_Met_Wet_CU %>%
  filter(Tier1 == "Fast-Turbulent") %>%
  group_by(VisitID) %>%
  mutate(LWVol_WetFstTurb = sum(Wet_Wood_VT)) %>%
  select(1, 6) %>%
  distinct(VisitID, .keep_all = TRUE)



##Calculating Channel unit undercuts
Undercut_Select <- Undercut_all %>%
  select(1:4, 8, 11:13, 34, 40, 45, 49) %>%
  filter(AverageWidth >= 0.2, VisitYear >= 2014) 

##Caclculate Channel unit undercut metrics##
Undercut_Met_CU <- Undercut_Select %>%
  select(7, 10:12) %>%
  mutate(Undercut_A = AverageWidth * EstimatedLength) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Undercut_A_CU = sum(Undercut_A)) %>%
  mutate(Undercut_lng_CU = sum(EstimatedLength)) %>%
  select(1, 4, 6, 7) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE)

##Calculating Channel Unit undercut areas and number of undercuts and length of undercuts at site##
Undercut_Metrics <- Undercut_Select %>%
  select(7, 10:12) %>%
  mutate(UnderCut_A = AverageWidth * EstimatedLength) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Undercut_Area_CU = sum(UnderCut_A)) %>%
  mutate(N_Undercuts_CU = length(ChannelUnitID)) %>%
  mutate(Undercut_L_CU = sum(EstimatedLength)) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  mutate(Undercut_A_Site = sum(Undercut_Area_CU)) %>%
  mutate(Undercut_Lng_Site = sum(EstimatedLength)) %>%
  mutate(N_Undercuts_Site = sum(N_Undercuts_CU)) %>%
  distinct(VisitID, .keep_all = TRUE) %>%
  select(1, 9:11)

##Calculating Substrate Cover metrics
SubstrateCover_Metrics <- SubstrateCover %>%
  select(8, 12, 34, 36:42) %>%
  filter(VisitYear >= 2014) %>%
  mutate(Perc_Gravel = CoarseGravel1764 + FineGravel316) %>%
  mutate(Perc_Fines = Sand0062 + FinesLT006) %>%
  rename(Perc_Boulder = BouldersGT256, 
         Perc_Cobble = Cobbles65255) %>%
  select(2, 3, 4:5, 11, 12)


##Joining channel unit metrics back to Channel_Unit_Select##
LWD_Metrics_Select <- LWD_Metrics %>%
  select(-3)

LWD_Met_Wet_CU_Select <- LWD_Met_Wet_CU %>%
  select(-3)

LWD_Met_Dry_CU_Select <- LWD_Met_Dry_CU %>%
  select(-3)

Champ_Dash_Met_CU <- Channel_Unit_All %>%
  filter(VisitYear >= 2014) %>%
  select(12, 34, 38) %>%
  left_join(LWD_Metrics_Select, by = c("VisitID", "ChannelUnitID")) %>%
  left_join(LWD_Met_Wet_CU_Select, by = c("VisitID", "ChannelUnitID")) %>%
  left_join(LWD_Met_Dry_CU_Select, by = c("VisitID", "ChannelUnitID")) %>%
  left_join(Undercut_Met_CU, by = c("VisitID", "ChannelUnitID")) %>%
  left_join(SubstrateCover_Metrics, by =c("VisitID", "ChannelUnitID")) %>%
  mutate_at(c(4:12), funs(replace(., is.na(.), 0))) 



  
  


CHaMP_CU_Metrics <- left_join(Channel_Unit_Select, LWD_Metrics, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(14:15), funs(replace(., is.na(.), 0))) %>%
  left_join(N_V_Wet, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(16:17), funs(replace(., is.na(.), 0))) %>%
  mutate(Dry_V_T = Wood_VT - Wet_V_T) %>%
  mutate(N_Dry = N_Wood - N_Wet) %>%
  left_join(Undercut_Metrics, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(20:22), funs(replace(., is.na(.), 0))) %>%
  left_join(SubstrateCover_Select, by = c("VisitID" , "ChannelUnitID")) 




##Calculating New Site Level metrics from CU metrics##

CHaMP_Site_Metrics <- CHaMP_CU_Metrics %>%
  group_by(VisitID) %>%
  mutate(Site_Wood_V = sum(Wood_VT)) %>%
  mutate(Site_N_Wood = sum(N_Wood)) %>%
  mutate(Site_N_Wet = sum(N_Wet)) %>%
  mutate(LWVol_Wet = sum(Wet_V_T)) %>%
  mutate(Site_Dry_VT = sum(Dry_V_T)) %>%
  mutate(Site_N_Dry = sum(N_Dry)) %>%
  mutate(Site_UC_Area = sum(Undercut_Area)) %>%
  mutate(Site_UC_Length = sum(Undercut_L)) %>%
  mutate(Site_N_UC = sum(N_Undercuts)) %>%
  mutate(Site_Bedrock = mean(Bedrock, na.rm = TRUE)) %>%
  mutate(SubEstBldr = mean(Perc_Boulder, na.rm = TRUE)) %>%
  mutate(SubEstCbl = mean(Perc_Cobble, na.rm = TRUE)) %>%
  mutate(SubEstGrvl = mean(Perc_Gravel, na.rm = TRUE)) %>%
  mutate(SubEstSandFines = mean(Perc_Fines, na.rm = TRUE)) %>%
  select(8, 10, 28:41) %>%
  filter(ChannelUnitID == 1) %>%
  left_join(LWVol_WetFstNT, by = "VisitID") %>%
  left_join(LWVol_WetFstTurb, by = "VisitID") %>%
  left_join(LWVol_WetSlow, by = "VisitID") %>%
  mutate_at(c(17:19), funs(replace(., is.na(.), 0)))
  


##Merging newly calculated CHaMP_Site_Metrics with CHaMP_Site_Data##
##This chunk of code can pull any desired metrics from the "CHaMP_Site_Data.csv' file##

CHaMP_Site_Data_Select <- read.csv("champ_site_2011_17.csv") %>%
  filter(VisitYear >= 2014) %>%
  select("FishCovNone", "FishCovLW", "FishCovTVeg",
         "FishCovArt", "FishCovNone", "FishCovAqVeg",
         "FishCovTotal", "Q", "SubD16", "SubD50", 
         "SubD84",
         "DpthThlwgExit", "DpthResid", "PolyArea",
         "Freq", "DpthThlwgMax_Avg", "DpthThlwgExit", 
         "WetWdth_Int", "Lgth_Wet", "Area_Wet", "SlowWater_Area",
         "SlowWater_Ct", "SlowWater_Freq", "SlowWater_Pct",
         "FstTurb_Area", "FstTurb_Pct", "FstTurb_Freq",
         "FstNT_Area", "FstNT_Ct", "FstNT_Pct", "WetWdth_CV",
         "Sin_CL", "WetBraid", "WetChnl_Ct", "WetChnl_Sin",
         "Wet_QIsland_Ct", "Lgth_WetChnl", "WetWMxDRat_CV",
         "WetWMxDRat_Avg", "WetMChnl_Area", "WetSCL_Area",
         "WetSCS_Area", "WetSC_Pct", "VisitYear", "ChannelType",
         "CU_Ct", "CU_Freq", "DistPrin1", "NatPrin1", "NatPrin2",
         "CUMDRAINAG", "Ppt", "MeanU") 

CHaMP_Winter_Metrics <- left_join(CHaMP_Site_Metrics, CHaMP_Site_Data_Select, by = "VisitID")





  



  

