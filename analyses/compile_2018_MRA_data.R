# Author: Mike Ackerman & Richie Carmichael
# Purpose: Compiling 2018 MRA DASH data
# Created: 10/25/2019
# Last Modified: 10/25/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
library(usethis)
library(rgeos)

#-----------------------------------------------------------------
# read in data
upper_lemhi_line  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Line_Fish.shp')
upper_lemhi_poly  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Poly_Fish.shp')
lower_lemhi_line  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Line_Fish.shp')
lower_lemhi_poly  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Poly_Fish.shp')
pahs_line         = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Line_Fish.shp')
pahs_poly         = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Poly_Fish.shp')
upper_salmon_line = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Line_Fish.shp')
upper_salmon_poly = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Poly_Fish.shp') 
# fixing upper salmon 
upper_salmon_poly <- st_transform(upper_salmon_poly, crs = st_crs(pahs_line))
upper_salmon_line <- st_transform(upper_salmon_line, crs = st_crs(pahs_line))
# gaa, norwest, natdist, from Salmon basin only
gaa               = st_read("C:/Processing/GIS/new_gaa/norwest_gaa_natdist_join/SalmonBasin/SalmonBasin_Join_All.shp")
gaa_trans <- st_transform(gaa, crs = st_crs(pahs_line)) 
gaa_trans <- st_zm(gaa_trans)
# Upper Lemhi
ggplot() +
  geom_sf(data = upper_lemhi_line) +
  geom_sf(data = upper_lemhi_poly, aes(fill = Unt_Typ)) +
  theme_classic() +
  labs(title = 'Upper Lemhi') 

# Lower Lemhi
ggplot() +
  geom_sf(data = lower_lemhi_line) +
  geom_sf(data = lower_lemhi_poly, aes(fill = Unt_Typ)) +
  theme_classic() +
  labs(title = 'Lower Lemhi') 

# Pahsimeroi
ggplot() +
  geom_sf(data = pahs_line) +
  geom_sf(data = pahs_poly, aes(fill = Unt_Typ)) +
  theme_classic() +
  labs(title = 'Pahsimeroi') 

# Upper Salmon
ggplot() +
  geom_sf(data = upper_salmon_line) +
  geom_sf(data = upper_salmon_poly, aes(fill = Unt_Typ)) +
  theme_classic() +
  labs(title = 'Upper Salmon') 

# merge upper lemhi data
upper_lemhi_line = upper_lemhi_line %>%
  mutate(A_Vg_Cv = NA, Length = NA)
upper_lemhi_poly = upper_lemhi_poly %>%
  mutate(Avg_wdt = NA, SHAPE_L = NA, sinusty = NA)
upper_lemhi = rbind(upper_lemhi_line, upper_lemhi_poly)
upper_lemhi = as.data.frame(upper_lemhi)

# # remove 'Reach' from lower lemhi data
# lower_lemhi_line = lower_lemhi_line %>%
#   select(-Reach)

# merge lower lemhi data
lower_lemhi_line = lower_lemhi_line %>%
  mutate(A_Vg_Cv = NA, Length = NA, Reach = NA) %>%
  select(-Reach)
lower_lemhi_poly = lower_lemhi_poly %>%
  mutate(Avg_wdt = NA, SHAPE_L = NA, sinusty = NA)
lower_lemhi = rbind(lower_lemhi_line, lower_lemhi_poly)
lower_lemhi = as.data.frame(lower_lemhi)

# merge pahsimeroi data
pahs_line = pahs_line %>%
  mutate(A_Vg_Cv = NA, Length = NA)
pahs_poly = pahs_poly %>%
  mutate(Avg_wdt = NA, SHAPE_L = NA, sinusty = NA)
pahs = rbind(pahs_line, pahs_poly)
pahs = as.data.frame(pahs)

# merge upper salmon data
upper_salmon_line = upper_salmon_line %>%
  mutate(A_Vg_Cv = NA, Length = NA, sinusty = NA) %>%
  rename(Hab_Roll = hr)
upper_salmon_poly = upper_salmon_poly %>%
  mutate(Avg_wdt = NA, SHAPE_L = NA, sinusty = NA) %>%
  rename(Hab_Roll = hr)
upper_salmon = rbind(upper_salmon_line, upper_salmon_poly)
upper_salmon = as.data.frame(upper_salmon)



# merge all sites
dash2018_cu = rbind(upper_lemhi, lower_lemhi, pahs, upper_salmon) %>%
  select(SiteNam, Reach_Nmb, everything()) %>%
  mutate(Len = ifelse(is.na(Length), SHAPE_L, Length)) %>%
  select(- Length, SHAPE_L) %>%
  rename(Length = Len) %>%
  select(-sinusty)


# begin rolling up summaries by fish reach
dash2018_fr = dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(FishCovNone = weighted.mean(No_Cov, SHAPE_A),
            Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A),
            FstTurb_Cnt = length(which(Unt_Typ == 'Riffle' | Unt_Typ == 'Rapid')),
            FstNT_Cnt = length(which(Unt_Typ == 'Run')),
            CU_Cnt = length(Unt_Nmb),
            Length = sum(Length),
            FstTurb_Freq = (FstTurb_Cnt / Length) * 100,
            FstNT_Freq = (FstNT_Cnt / Length) * 100,
            CU_Freq = (CU_Cnt / Length) * 100,
            SHAPE_A = sum(SHAPE_A),
            Max_Dpth = max(Mx_Dpth),
            Avg_Mx_Depth = mean(Mx_Dpth),
            CV_Mx_Depth = sd(Mx_Dpth) / mean(Mx_Dpth),
            Jam_Vlm = sum(Jam_Vlm),
            Wod_Cnt = sum(Wod_Cnt),
            Wod_Vlm = sum(Wod_Vlm),
            N_Bllst = sum(N_Bllst),
            N_ChFrm = sum(N_ChFrm),
            N_Wet = sum(N_Wet),
            Undrc_L = sum(Undrc_L),
            Undrc_A = sum(Undrc_V)) %>%
  mutate(UcutArea_Pct = (Undrc_A / SHAPE_A) * 100)

##Slowwater pct
Pool_A <- dash2018_cu %>%
  filter(Unt_Typ == "Pool") %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(Pool_A = sum(SHAPE_A)) %>%
  ungroup()

Slowwater_Pct <- dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(Total_A = sum(SHAPE_A)) %>%
  left_join(Pool_A, by = c("SiteNam", "Hab_Roll")) %>%
  ungroup() %>%
  mutate(Slowwat_Pct = (Pool_A/Total_A))

dash2018_fr = dash2018_fr %>%
  left_join(Slowwater_Pct, by = c("SiteNam", "Hab_Roll")) %>%
  select(-Total_A)


##Side channel percent
SC_A <- dash2018_cu %>%
  filter(Sgmnt_N > 1) %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(SC_A = sum(SHAPE_A)) %>%
  ungroup()

SC_Pct <- dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(Total_A = sum(SHAPE_A)) %>%
  left_join(SC_A, by = c("SiteNam", "Hab_Roll")) %>%
  ungroup() %>%
  mutate(SC_Pct = (SC_A/Total_A)) %>%
  select(-Total_A)

dash2018_fr = dash2018_fr %>%
  left_join(SC_Pct, by = c("SiteNam", "Hab_Roll"))

##large wood frequency
LW_count_wet <- dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(Wood_Ct = sum(N_Wet))

LWFreq_Wet <- dash2018_cu %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(Total_L = sum(Length)) %>%
  ungroup() %>%
  left_join(LW_count_wet, by = c("SiteNam", "Hab_Roll")) %>%
  mutate(LWFreq_Wet = ((Wood_Ct/Total_L)*100)) %>%
  select(-Total_L)

dash2018_fr = dash2018_fr %>%
  left_join(LWFreq_Wet, by = c("SiteNam","Hab_Roll"))

# NatPrin1: from gaa lines Salmon basin only
# DistPrin1: from gaa lines Salmon basin only
# avg_aug_temp: from gaa lines Salmon basin only. Scenario "02_20"

# Select attributes to join
gaa_select <- gaa_trans %>%
  select("S2_02_11", 
         "dstrb_1",
         "nt_ft_1",
         "nt_ft_2")
# join data to mra sites
all_cu <- rbind(upper_lemhi_poly, 
                lower_lemhi_poly, 
                pahs_poly, 
                upper_salmon_poly)

## Still creating new gaa data after issue with NatDist layer
gaa_join <- st_join(all_cu, gaa_select) %>%
  group_by(SiteNam) %>%
  distinct(Unt_Nmb, .keep_all = TRUE )

# Sin_CL: calculated from imagery for each fish reach
# read in cl with hab roll column

ll_cl <- st_read("C:/GIT/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Centerline_sin.shp")
ul_cl <- st_read("C:/GIT/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Centerline_sin.shp")
pah_cl <- st_read("C:/GIT/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Centerline_sin.shp")
us_cl <- st_read("C:/GIT/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Centerline_sin.shp")
us_cl <- st_transform(us_cl, crs = st_crs(pahs_line))
all_cl <- rbind(ll_cl,
                ul_cl,
                pah_cl,
                us_cl) %>%
  mutate_at("Hab_Roll", funs(replace(., is.na(.), 0))) %>%
  select("SiteNam", "Hab_Roll")

############ This calculated sinuosity of a single line as an example, 
############ but needs to be wrapped for each row of the "all_cl" above. 
LL_CL_sin <- st_read("C:/GIT/DASH/data/raw/dash2018/LowerLemhi/LL_Centerline_Dissolve.shp")
cl_length <- as.numeric(st_length((LL_CL_sin)))


cl_points <- (LL_CL_sin) %>%
  select("Hab_Roll") 
cl_points <- st_cast(cl_points, "MULTIPOINT")
start <- cl_points[1,] 
end <- cl_points[nrow(cl_points),]
line <- as.numeric(st_distance(start, end))
sin <- line/cl_length
# WetBraid
main_length <- all_cl %>%
  mutate(length = as.numeric(st_length(all_cl))) %>%
  st_drop_geometry()

sc_all <- rbind(lower_lemhi_line,
                   upper_lemhi_line,
                   upper_salmon_line,
                   pahs_line) %>%
                   select("SiteNam", "Hab_Roll") 


sc_all <- sc_all %>%
  mutate(length = as.numeric(st_length(sc_all)))

sc_length <- sc_all %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(sc_length = as.numeric(sum(length))) %>%
  st_drop_geometry()

wet_braid <- left_join(main_length, sc_length, 
                         by = c("SiteNam", "Hab_Roll")) %>%
                         mutate_at("sc_length", funs(replace(., is.na(.), 0))) %>%
  mutate(total_length = length + sc_length) %>%
  mutate(wet_braid = total_length/length) %>%
  select("SiteNam", "Hab_Roll", "wet_braid")
  
dash2018_fr = dash2018_fr %>%
  left_join(wet_braid, by = c("SiteNam","Hab_Roll"))
# write fish reach data to csv
write_csv(dash2018_fr, 'data/prepped/dash2018_fr.csv')
