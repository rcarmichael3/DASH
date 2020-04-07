# Author: Mike Ackerman & Richie Carmichael
#   with assists from Kevin See
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
#rm(ul_ms_sf, ul_sc_sf)        

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
#rm(ll_ms_sf, ll_sc_sf)

# pahsimeroi
ph_sf = ph_ms_sf %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(ph_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA))
#rm(ph_ms_sf, ph_sc_sf)

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
#rm(us_ms_sf, us_sc_sf)

#-----------------------------
# fix a Lsc in the Pahsimeroi data
#-----------------------------
ph_sf = ph_sf %>%
  drop_na(Hab_Roll) %>%
  rbind(ph_sf %>%
          filter(is.na(Hab_Roll)) %>%
          mutate_at(vars(Off_C_T:sinusty),
                    list(~as.numeric(NA))) %>%
          mutate(Unt_Typ = "Off Channel",
                 Sgmnt_N = NA,
                 Hab_Roll = replace_na(Hab_Roll, 5)) )


#-----------------------------
# merge data together and do some cleaning
#-----------------------------
# merge all sites
dash2018_cu = rbind(ul_sf, ll_sf, ph_sf, us_sf) %>%
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
  mutate(ID = paste0(str_pad(Sgmnt_ID, 2, pad = "0"), "_", str_pad(CU_ID, 3, pad = "0")),
         Aq_Veg_Cov = replace_na(Aq_Veg_Cov, 0),
         Mx_Dpth = ifelse(CU_Typ == "Pool", Mx_Dpth, NA),
         d50 = ifelse(CU_Typ == "Riffle", d50, NA),
         d84 = ifelse(CU_Typ == "Riffle", d84, NA),
         Prc_Fns = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Fns, NA),
         Prc_Grv = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Grv, NA),
         Prc_Cbb = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Cbb, NA),
         Prc_Bld = ifelse(CU_Typ %in% c("Run", "Pool"), Prc_Bld, NA)) %>%
  mutate(CU_Typ = ifelse(is.na(Off_Chnl_Typ), as.character(CU_Typ),
                         ifelse(Off_Chnl_Typ == "Ssc", "Ssc",
                                ifelse(Off_Chnl_Typ == "Oca", "Oca",
                                       ifelse(Off_Chnl_Typ %in% c("Pool", "Riffle", "Run", "Rapid"), "Lsc", NA))))) %>%
  arrange(SiteNam, Hab_Rch, ID) %>%
  select(SiteNam, Hab_Rch, ID, everything())

# clean up some datasets
# rm(ul_sf, ll_sf, ph_sf, us_sf)

#-----------------------------
# calculate some hr scale metrics for the cus
#--------------------------
dash2018_cu_plus = dash2018_cu %>%
  group_by(SiteNam, Hab_Rch) %>%
  mutate(hr_Pool_Mx_Dpth = max(Mx_Dpth, na.rm = T),
         hr_Pool_Avg_Mx_Dpth = mean(Mx_Dpth, na.rm = T),
         hr_Pool_CV_Mx_Dpth = sd(Mx_Dpth, na.rm = T) / mean(Mx_Dpth, na.rm = T),
         hr_Tot_Cov = 100 - weighted.mean(No_Cov, SHAPE_A),
         hr_Aq_Veg_Cov = weighted.mean(Aq_Veg_Cov, SHAPE_A),
         hr_d50 = weighted.mean(d50, SHAPE_A, na.rm = T),
         hr_d84 = weighted.mean(d84, SHAPE_A, na.rm = T),
         hr_Prc_Fns = weighted.mean(Prc_Fns, SHAPE_A, na.rm = T),
         hr_Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A, na.rm = T),
         hr_Prc_Cbb = weighted.mean(Prc_Cbb, SHAPE_A, na.rm = T),
         hr_Prc_Bld = weighted.mean(Prc_Bld, SHAPE_A, na.rm = T),
         hr_CU_IDs = list(ID),
         hr_Pool_A = sum(SHAPE_A[CU_Typ == "Pool"]),
         hr_SC_A = sum(SHAPE_A[Sgmnt_ID > 1])) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("hr_")),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# save channel unit scale data
save(dash2018_cu, dash2018_cu_plus, file = "data/prepped/dash2018_cus.Rda")

# filter and save shapefile of mainstem units
# dash2018_cu_plus_ms = dash2018_cu_plus %>%
#   filter(st_geometry_type(.) == "POLYGON")
# 
# ...and side channels
# dash2018_cu_plus_sc = dash2018_cu_plus %>%
#   filter(st_geometry_type(.) == "LINESTRING")

#-----------------------------
# plot dash2018_cu for visualization
#-----------------------------
dash_cu_plotlist = purrr::map(unique(dash2018_cu_plus$SiteNam),
                              function (x) {
                                
                                # subset data
                                temp_sf = subset(dash2018_cu_plus, SiteNam == x)
                                
                                ggplot() +
                                  geom_sf(data = temp_sf, aes(fill = CU_Typ)) +
                                  guides(fill = FALSE) +
                                  ggtitle(x) +
                                  theme_bw()
                              })

dash_cu_plot = cowplot::plot_grid(plotlist = dash_cu_plotlist)
dash_cu_plot

#-----------------------------
# begin to summarize data using Hab_Roll column
#--------------------------
dash2018_hr = dash2018_cu_plus %>%
  group_by(SiteNam, Hab_Rch) %>%
  summarise(CU_IDs = unique(hr_CU_IDs),
            Sgmnt_Cnt = n_distinct(Sgmnt_ID),
            CU_Cnt = n_distinct(CU_ID),
            Pool_Cnt = length(which(CU_Typ == "Pool")),
            Run_Cnt = length(which(CU_Typ == "Run")),
            Riffle_Cnt = length(which(CU_Typ == "Riffle")),
            Rapid_Cnt = length(which(CU_Typ == "Rapid")),
            Lsc_Cnt = length(which(CU_Typ == "Lsc")),
            Ssc_Cnt = length(which(CU_Typ == "Ssc")),
            Oca_Cnt = length(which(CU_Typ == "Oca")),
            Length = sum(Length),
            SHAPE_A = sum(SHAPE_A),
            Wod_Cnt = sum(Wod_Cnt),
            N_Bllst = sum(N_Bllst),
            N_ChFrm = sum(N_ChFrm),
            N_Wet = sum(N_Wet),
            Wod_Vlm = sum(Wod_Vlm),
            Jam_Vlm = sum(Jam_Vlm),
            Undrc_C = sum(Undrc_C),
            Undrc_L = sum(Undrc_L),
            Undrc_A = sum(Undrc_A),
            Pool_Mx_Dpth = unique(hr_Pool_Mx_Dpth),
            Pool_Avg_Mx_Dpth = unique(hr_Pool_Avg_Mx_Dpth),
            Pool_CV_Mx_Dpth = unique(hr_Pool_CV_Mx_Dpth),
            Tot_Cov = round(unique(hr_Tot_Cov), 2),
            Aq_Veg_Cov = round(unique(hr_Aq_Veg_Cov), 2),
            d50 = unique(hr_d50),
            d84 = unique(hr_d84),
            Prc_Fns = unique(hr_Prc_Fns),
            Prc_Grv = unique(hr_Prc_Grv),
            Prc_Cbb = unique(hr_Prc_Cbb),
            Prc_Bld = unique(hr_Prc_Bld),
            Pool_A = unique(hr_Pool_A),
            SC_A = unique(hr_SC_A)) %>%
  mutate(FstNT_Cnt = Run_Cnt,
         FstTurb_Cnt = Riffle_Cnt + Rapid_Cnt,
         Slowwater_Pct = (Pool_A / SHAPE_A) * 100,
         SC_Pct = (SC_A / SHAPE_A) * 100,
         LWFreq_Wet = (N_Wet / Length) * 100)

# Others to consider?
#Ssc_Avg_Wdth = weighted.mean(Avg_Wdth, Length) if(Off_chnl_Typ == "Ssc")

#-----------------------------
# calculate sinuosity and braidedness metrics and add to dash2018_hr
#-----------------------------

# read in the centerlines with Hab_Roll column
ul_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Centerline_sin.shp")
ll_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Centerline_sin.shp")
ph_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Centerline_sin.shp")
us_cl <- st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Centerline_sin.shp") %>%
  st_transform(crs = st_crs(mra_crs))
  
# combine the above centerlines (still need to resolve the Pahsimeroi Hab_Roll 5)
mra_cl = rbind(ul_cl,
               ll_cl,
               ph_cl,
               us_cl) %>%
  # fix the Pahsimeroi NA Hab_Roll which belongs in Hab_Roll 5
  mutate(Hab_Roll = replace_na(Hab_Roll, 5)) %>%
  select("SiteNam", "Hab_Roll", "sinuosity") %>%
  mutate(Length = as.numeric(st_length(geometry))) %>%
  mutate(start = st_line_sample(geometry, sample = 0),
         end = st_line_sample(geometry, sample = 1)) %>%
  mutate(straight_line = mapply(st_distance, start, end)) %>%
  select(-start, -end) %>%
  mutate(sinuosity = straight_line / Length) %>%
  st_drop_geometry() 

# combine the side channels and calculate sc length by Hab_Roll
mra_sc = ul_sc_sf %>%
  mutate(A_Vg_Cv = NA,
         Length = NA) %>%
  rbind(ll_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA)) %>%
  rbind(ph_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA)) %>%
  rbind(us_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA,
                 sinusty = NA) %>%
          rename(Hab_Roll = hr)) %>%
  select("SiteNam", "Hab_Roll") %>%
  mutate(Length = as.numeric(st_length(geometry))) %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(sc_Length = sum(Length)) %>%
  st_drop_geometry()

# now calculate some sinuosity and braidedness metrics by Hab_Roll
hr_sin_wet_braid = mra_cl %>%
  left_join(mra_sc,
            by = c("SiteNam", "Hab_Roll")) %>%
  mutate_at("sc_Length", replace_na, 0) %>%
  mutate(Tot_Length = Length + sc_Length) %>%
  mutate(wet_braid = Tot_Length / Length) %>%
  mutate(wet_braid_sin = Tot_Length / straight_line) %>%
  rename(Hab_Rch = Hab_Roll)

# join to dash2018_hr
dash2018_hr = dash2018_hr %>%
  left_join(hr_sin_wet_braid, 
            by = c("SiteNam", "Hab_Rch"))

# save habitat reach scale data
save(dash2018_hr, file = "data/prepped/dash2018_hr.Rda")
# st_write(dash2018_hr, "dash2018_hr.shp")
  
# clean up some datasets
#rm(ul_cl, ll_cl, ph_cl, us_cl)

#-----------------------------------------------------
# read in Morgan's data, select attributes, and join to the dash2018_hr
#-----------------------------------------------------
# gaa, norwest, natdist, from Salmon basin only. Richie's NAS is mapped to Z:/...Mike's is S:/
load("Z:/habitat/full_join/SalmonBasin/salmon_full_join.Rda")
load("S:/habitat/full_join/SalmonBasin/salmon_full_join.Rda")

# transform to the same crs
salmon_full_join = salmon_full_join %>%
  st_transform(crs = st_crs(mra_crs))

# select attributes to join
salmon_gaa_trim = salmon_full_join %>%
  select("UniqueID",
         "S2_02_11.y",
         "NatPrin1",
         "NatPrin2",
         "DistPrin1",
         "MeanU_v1")

# plot salmon_gaa_trim
ggplot() +
  geom_sf(data = salmon_gaa_trim,
          aes(fill = UniqueID)) +
  labs(title = "GAA Data",
       fill = "Site ID") +
  theme_bw()

# what is the nearest gaa to each dash2018_fr?
st_nearest_feature(dash2018_hr,
                   salmon_gaa_trim)

# join nearest gaa to the dash2018_fr
dash2018_hr_gaa = dash2018_hr %>%
  st_join(salmon_gaa_trim,
          join = st_nearest_feature,
          left = TRUE)

# save habitat reach scale data
save(dash2018_hr, dash2018_hr_gaa, file = "data/prepped/dash2018_hr.Rda")
