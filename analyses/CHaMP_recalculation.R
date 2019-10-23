library(tidyverse)

setwd("D:/CHaMP_Measurements/Recalculation")

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


##Calculating channel unit wood area, volume, and total number of pieces##

LWD_Select <- LWD_all %>%
  select(1:5, 8, 11:13, 34:35, 38:40) %>%
  filter(Diameter >= 0.15, Length >= 1.5)

LWD_Metrics <- LWD_Select %>%
  mutate(Wood_A = Diameter * Length) %>%
  mutate(Wood_V = pi * ((Diameter/2)^2) * Length) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Wood_VT = sum(Wood_V)) %>%
  mutate(N_Wood = length(ChannelUnitID)) %>%
  select(1:9, 11, 17, 18) %>%
  ungroup() %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE) %>%
  select(8, 10:12)


  
##Calculating channel unit "Wet" wood metrics, total volume and number of pieces##
N_V_Wet <- LWD_Select %>%
  filter(LargeWoodType == "Wet") %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(N_Wet = length(ChannelUnitID)) %>%
  mutate(Wet_V = pi * ((Diameter/2)^2)* Length) %>%
  mutate(Wet_V_T = sum(Wet_V)) %>%
  ungroup() %>%
  select(8, 11, 15, 17) %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE)
  

##Calculating channel unit "Dry" wood metrics, total volume and number of pieces##
N_V_Dry <- LWD_Select %>%
  filter(LargeWoodTally == "Dry") %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(N_Dry = length(ChannelUnitID)) %>%
  mutate(Dry_V = pi * ((Diameter/2)^2)* Length) %>%
  mutate(Dry_V_T = sum(Dry_V)) %>%
  ungroup() %>%
  select(8, 11, 15, 17) %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE)

##Calculating Channel unit undercuts
Undercut_Select <- Undercut_all %>%
  select(1:4, 8, 11:13, 34, 40, 45, 49) %>%
  filter(AverageWidth >= 0.2, VisitYear >= 2014) 

##Calculating Channel Unit undercut areas and number of undercuts 
Undercut_Metrics <- Undercut_Select %>%
  mutate(UnderCut_A = AverageWidth * EstimatedLength) %>%
  group_by(VisitID, ChannelUnitID) %>%
  mutate(Undercut_Area = sum(UnderCut_A)) %>%
  mutate(N_Undercuts = length(ChannelUnitID)) %>%
  ungroup() %>%
  select(1:8, 12, 14, 15) %>%
  group_by(VisitID) %>%
  distinct(ChannelUnitID, .keep_all = TRUE) %>%
  select(7, 9:11)

##Calculating Substrate Cover metrics
SubstrateCover_Select <- SubstrateCover %>%
  select(1:5, 8, 12, 13, 34, 36:42) %>%
  filter(VisitYear >= 2014) %>%
  mutate(Perc_Gravel = CoarseGravel1764 + FineGravel316) %>%
  mutate(Perc_Fines = Sand0062 + FinesLT006) %>%
  select(1:12, 17, 18) %>%
  rename(Perc_Boulder = BouldersGT256, 
         Perc_Cobble = Cobbles65255) %>%
  select(7, 9:14)


##Joining channel unit metrics back to Channel_Unit_Select##

CHaMP_CU_Metrics <- left_join(Channel_Unit_Select, LWD_Metrics, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(14:15), funs(replace(., is.na(.), 0))) %>%
  left_join(N_V_Wet, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(16:17), funs(replace(., is.na(.), 0))) %>%
  mutate(Dry_V_T = Wood_VT - Wet_V_T) %>%
  mutate(N_Dry = N_Wood - N_Wet) %>%
  left_join(Undercut_Metrics, by = c("VisitID" , "ChannelUnitID")) %>%
  mutate_at(c(20:21), funs(replace(., is.na(.), 0))) %>%
  left_join(SubstrateCover_Select, by = c("VisitID" , "ChannelUnitID")) 




##Calculating New Site Level metrics from CU metrics##

CHaMP_Site_Metrics <- CHaMP_CU_Metrics %>%
  group_by(VisitID) %>%
  mutate(Site_Wood_V = sum(Wood_VT)) %>%
  mutate(Site_N_Wood = sum(N_Wood)) %>%
  mutate(Site_N_Wet = sum(N_Wet)) %>%
  mutate(Site_Wet_VT = sum(Wet_V_T)) %>%
  mutate(Site_Dry_VT = sum(Dry_V_T)) %>%
  mutate(Site_N_Dry = sum(N_Dry)) %>%
  mutate(Site_UC_Area = sum(Undercut_Area)) %>%
  mutate(Site_N_UC = sum(N_Undercuts)) %>%
  mutate(Site_Bedrock = mean(Bedrock, na.rm = TRUE)) %>%
  mutate(Site_Boulder = mean(Perc_Boulder, na.rm = TRUE)) %>%
  mutate(Site_Cobble = mean(Perc_Cobble, na.rm = TRUE)) %>%
  mutate(Site_Gravel = mean(Perc_Gravel, na.rm = TRUE)) %>%
  mutate(Site_Fines = mean(Perc_Fines, na.rm = TRUE)) %>%
  select(1:13, 27:39) %>%
  filter(ChannelUnitID == 1) 
  


##Merging newly calculated CHaMP_Site_Metrics with CHaMP_Site_Data##
##This chunk of code can pull any desired metrics from the "CHaMP_Site_Data.csv' file##

CHaMP_Site_Data_Select <- read.csv("CHaMP_Site_Data.csv") %>%
  filter(VisitYear >= 2014) %>%
  select("VisitID", "Q", "Sin", "FishCovLW", 
         "SubD50","SubD84", "SubEstGrvl", "SlowWater_Freq",
         "FstTurb_Freq", "FstNT_Freq", "PoolResidDpth",
         "SlowWater_Area", "WetSCL_Area", "WetSC_Pct", 
         "FishCovNone", "ChnlUnitTotal_Ct", "Lgth_WetChnl") %>%
  mutate(CU_Feq = (ChnlUnitTotal_Ct/Lgth_WetChnl)*100)

CHaMP_Winter_Metrics <- left_join(CHaMP_Site_Metrics, CHaMP_Site_Data_Select, by = "VisitID")





  
##Write out new channel unit metrics (optional) 
write.csv(CHaMP_Winter_Metrics, "CHaMP_Recalculated.csv")


  

