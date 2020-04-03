# Author: Mike Ackerman & Richie Carmichael
# 
# Purpose: Compile the 2018 DASH data from MRA sites to make QRF capacity predictions
#
# Created: Original version on 10/25/2019
# Last Modified: 04/02/2020

# Notes: 

#-----------------------------
# load necessary libraries
#-----------------------------
library(sf)
library(ggplot2)
library(tidyverse)

#-----------------------------
# read in 2018 MRA data stored as shapefiles
#-----------------------------
# note we store our 'default' coordinate reference system and set the us crs to be the same as the rest
# EPSG: 32612 = WGS 84/UTM zone 12N; 32611 = WGS 84/UTM zone 11N

# mainstem (ms) channel units, upper lemhi (ul), lower lemhi (ll), pahsimeroi (ph), and upper salmon (us)
ul_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Poly_Fish.shp')
mra_crs = st_crs(ul_ms_sf)
ll_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Poly_Fish.shp')
ph_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Poly_Fish.shp')
us_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Poly_Fish.shp') %>%
  st_transform(crs = mra_crs)

# side channels (sc)
ul_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Line_Fish.shp')
ll_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Line_Fish.shp')
ph_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Line_Fish.shp') 
us_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Line_Fish.shp') %>%
  st_transform(crs = mra_crs)

#-----------------------------
# clean and merge data into single sf object
#-----------------------------
# upper lemhi
ul_sf = ul_ms_sf %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(ul_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA))
rm(ul_ms_sf, ul_sc_sf)        

# lower lemhi  
ll_sf = ll_ms_sf %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(ll_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA,
                 Reach = NA) %>%
          select(-Reach))
rm(ll_ms_sf, ll_sc_sf)

# pahsimeroi
ph_sf = ph_ms_sf %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(ph_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA))
rm(ph_ms_sf, ph_sc_sf)

# upper salmon  
us_sf = us_ms_sf %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(us_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA,
                 sinusty = NA)) %>%
  rename(Hab_Roll = hr)
rm(us_ms_sf, us_sc_sf)

# merge all sites
dash2018_sf = rbind(ul_sf, ll_sf, ph_sf, us_sf) %>%
  mutate(Length = ifelse(is.na(Length), SHAPE_L, Length)) %>%
  select(-SHAPE_L, -sinusty, -Nhat, - Density) %>%
  rename(Sgmnt_ID = Sgmnt_N,
         CU_ID = Unt_Nmb,
         Hab_Rch = Hab_Roll,
         Fish_Rch = Reach_Nmb,
         CU_Typ = Unt_Typ,
         Aq_Veg_Cov = A_Vg_Cv,
         Off_Chnl_Typ = Off_C_T,
         Undrc_A = Undrc_V,
         Avg_Wdth = Avg_wdt) %>%
  select(SiteNam, Sgmnt_ID, CU_ID, Hab_Rch, 
         Fish_Rch, CU_Typ, Off_Chnl_Typ, Glbl_ID,            # site & unit info
         Length, SHAPE_A, Mx_Dpth, Avg_Wdth,                 # size
         No_Cov, Aq_Veg_Cov,                                  # cover
         Wod_Cnt, N_Bllst, N_ChFrm, N_Wet, Wod_Vlm, Jam_Vlm, # wood
         Undrc_C, Undrc_L, Undrc_A,                          # undercut
         d50, d84, Prc_Fns, Prc_Grv, Prc_Cbb, Prc_Bld,       # substrate
         Notes, everything()) %>%
  mutate(Aq_Veg_Cov = replace_na(Aq_Veg_Cov, 0),
         d50 = ifelse(CU_Typ == "Riffle", d50, NA),
         d84 = ifelse(CU_Typ == "Riffle", d84, NA),
         Prc_Fns = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Fns, NA),
         Prc_Grv = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Fns, NA),
         Prc_Cbb = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Fns, NA),
         Prc_Bld = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Fns, NA)) %>%
  arrange(SiteNam, Hab_Rch, Sgmnt_ID, CU_ID)

# clean up some datasets
#rm(ul_sf, ll_sf, ph_sf, us_sf)

# save channel unit scale data
save(dash2018_sf, file = "data/prepped/dash2018_channel_units.Rda")

#-----------------------------
# plot dash2018_sf for visualization
#-----------------------------
# I'll come back to this later; need to resolve facetting to zoom in on each map
ggplot() +
  geom_sf(data = dash2018_sf, 
          aes(fill = Unt_Typ)) +
  facet_wrap(~ SiteNam) +
  theme_bw() +
  labs(fill = 'Channel Unit Type',
       title = 'MRA Sub-reaches')

#-----------------------------
# begin to roll up the data using the Hab_Roll column
#--------------------------
CU_Typs = unique(dash2018_sf$CU_Typ)

# d50 and d84 should only be filled out if Chn_Typ = riffle
# Ocular substrate estimates should only be filled out if Chn_Typ = Pool or Run

dash2018_hr = dash2018_sf %>%
  group_by(SiteNam, Hab_Rch) %>%
  mutate(Mx_Mx_Dpth = max(Mx_Dpth),
         Avg_Mx_Dpth = mean(Mx_Dpth),
         CV_Mx_Dpth = sd(Mx_Dpth) / mean(Mx_Dpth),
         Ssc_Cnt = length(which(Off_Chnl_Typ == "Ssc")),
         HR_Tot_Cov = 1 - weighted.mean(No_Cov, SHAPE_A),
         HR_Aq_Veg_Cov = weighted.mean(Aq_Veg_Cov, SHAPE_A),

         
         HR_d50 = weighted.mean(d50, SHAPE_A),
         HR_d84 = weighted.mean(d84, SHAPE_A),
         HR_Prc_Fns = weighted.mean(Prc_Fns, SHAPE_A),
         HR_Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A),
         HR_Prc_Cbb = weighted.mean(Prc_Cbb, SHAPE_A),
         HR_Prc_Bld = weighted.mean(Prc_Bld, SHAPE_A)) %>%
  ungroup() %>%
  group_by(SiteNam, Hab_Rch) %>%
  summarise(Sgmnt_Cnt = n_distinct(Sgmnt_ID),
            CU_Cnt = n_distinct(CU_ID),
            Length = sum(Length),
            SHAPE_A = sum(SHAPE_A),
            Riffle_Cnt = length(which(CU_Typ == "Riffle")),
            Pool_Cnt = length(which(CU_Typ == "Pool")),
            Off_Channel_Cnt = length(which(CU_Typ == "Off Channel")),
            Run_Cnt = length(which(CU_Typ == "Run")),
            Rapid_Cnt = length(which(CU_Typ == "Rapid")),
            FstNT_Cnt = Run_Cnt,
            FstTurb_Cnt = sum(Riffle_Cnt, Rapid_Cnt),
            Wod_Cnt = sum(Wod_Cnt),
            N_Bllst = sum(N_Bllst),
            N_ChFrm = sum(N_ChFrm),
            N_Wet = sum(N_Wet),
            Wod_Vlm = sum(Wod_Vlm),
            Jam_Vlm = sum(Jam_Vlm),
            Undrc_C = sum(Undrc_C),
            Undrc_L = sum(Undrc_L),
            Undrc_A = sum(Undrc_A))

# To add:
Ssc_Avg_Wdth = weighted.mean(Avg_Wdth, Length) if(Off_chnl_Typ == "Ssc")


#-----------------------------------------------------
# Begin rolling up that data by fish reaches
#-----------------------------------------------------
dash2018_fr = dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(FishCovNone = weighted.mean(No_Cov, SHAPE_A),
            Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A),
            #FstTurb_Cnt = length(which(Unt_Typ == 'Riffle' | Unt_Typ == 'Rapid')),
            #FstNT_Cnt = length(which(Unt_Typ == 'Run')),
            #CU_Cnt = length(Unt_Nmb),
            #Length = sum(Length),
            FstTurb_Freq = (FstTurb_Cnt / Length) * 100,
            FstNT_Freq = (FstNT_Cnt / Length) * 100,
            CU_Freq = (CU_Cnt / Length) * 100,
            #SHAPE_A = sum(SHAPE_A),
            #Max_Dpth = max(Mx_Dpth),
            #Avg_Mx_Depth = mean(Mx_Dpth),
            #CV_Mx_Depth = sd(Mx_Dpth) / mean(Mx_Dpth),
            #Jam_Vlm = sum(Jam_Vlm),
            #Wod_Cnt = sum(Wod_Cnt),
            #Wod_Vlm = sum(Wod_Vlm),
            #N_Bllst = sum(N_Bllst),
            #N_ChFrm = sum(N_ChFrm),
            #N_Wet = sum(N_Wet),
            #Undrc_L = sum(Undrc_L),
            #Undrc_A = sum(Undrc_V)) %>%
  mutate(UcutArea_Pct = (Undrc_A / SHAPE_A) * 100)

# plot dash2018_fr
ggplot() +
  geom_sf(data = dash2018_fr,
          aes(fill = SiteNam)) +
  labs(title = "MRA Reaches",
       fill = "Site Name") +
  theme_bw()

  



# gaa, norwest, natdist, from Salmon basin only. Richie's NAS is mapped to Z:/...Mike's is S:/
gaa = st_read("Z:/habitat/full_join/SalmonBasin/SalmonBasin_fulljoin.shp")
gaa = st_read("S:/habitat/full_join/SalmonBasin/SalmonBasin_fulljoin.shp")
gaa_trans <- st_transform(gaa, crs = st_crs(pahs_line)) %>%
  st_zm()

