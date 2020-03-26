# Author: Mike Ackerman & Richie Carmichael
# Purpose: Compiling 2018 MRA DASH data
# Created: 10/25/2019
# Last Modified: 10/25/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
library(usethis)
library(raster)

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

# remove 'Reach' from lower lemhi data
lower_lemhi_line = lower_lemhi_line %>%
  select(-Reach)

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
  group_by(SiteNam, Reach_Nmb) %>%
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
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(Pool_A = sum(SHAPE_A)) %>%
  ungroup()

Slowwater_Pct <- dash2018_cu %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(Total_A = sum(SHAPE_A)) %>%
  left_join(Pool_A, by = c("SiteNam", "Reach_Nmb")) %>%
  ungroup() %>%
  mutate(Slowwat_Pct = (Pool_A/Total_A))

dash2018_fr = dash2018_fr %>%
  left_join(Slowwater_Pct, by = c("SiteNam", "Reach_Nmb")) %>%
  select(-Total_A)
  

##Side channel percent
SC_A <- dash2018_cu %>%
  filter(Sgmnt_N > 1) %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(SC_A = sum(SHAPE_A)) %>%
  ungroup()

SC_Pct <- dash2018_cu %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(Total_A = sum(SHAPE_A)) %>%
  left_join(SC_A, by = c("SiteNam", "Reach_Nmb")) %>%
  ungroup() %>%
  mutate(SC_Pct = (SC_A/Total_A)) %>%
  select(-Total_A)

dash2018_fr = dash2018_fr %>%
  left_join(SC_Pct, by = c("SiteNam", "Reach_Nmb"))

##large wood frequency
LW_count_wet <- dash2018_cu %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(Wood_Ct = sum(N_Wet))

LWFreq_Wet <- dash2018_cu %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(Total_L = sum(Length)) %>%
  ungroup() %>%
  left_join(LW_count_wet, by = c("SiteNam", "Reach_Nmb")) %>%
  mutate(LWFreq_Wet = ((Wood_Ct/Total_L)*100)) %>%
  select(-Total_L)

dash2018_fr = dash2018_fr %>%
  left_join(LWFreq_Wet, by = c("SiteNam","Reach_Nmb"))

# NatPrin1: from master sample points data
# DistPrin1: from master sample points data
# avg_aug_temp: from Norwest data which Richie is joining to master sample points


# Sin_CL: calculated from imagery for each fish reach
LL_CL_sin <- st_read("C:/GIT/DASH/data/raw/dash2018/LowerLemhi/LL_Centerline_Dissolve.shp")
cl_length <- as.numeric(st_length((LL_CL_sin)))


cl_points <- (LL_CL_sin) %>%
  select("Hab_Roll") 
cl_points <- st_cast(cl_points, "Point")
start <- cl_points[1,] 
start_sp <- as(start, "Spatial")
1
end <- cl_points[nrow(cl_points),]
end_sp <- as(end, Class = "Spatial")
line <- pointDistance(start, end)

# WetBraid


# write fish reach data to csv
write_csv(dash2018_fr, 'data/prepped/dash2018_fr.csv')
