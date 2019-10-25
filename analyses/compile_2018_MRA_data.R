# Author: Mike Ackerman 
# Purpose: Compiling 2018 MRA DASH data
# Created: 10/25/2019
# Last Modified: 10/25/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(usethis)

#-----------------------------------------------------------------
# read in data
upper_lemhi_line  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/UL_Line_Fish.shp')
upper_lemhi_poly  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/UL_Poly_Fish.shp')
lower_lemhi_line  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/LL_Line_Fish.shp')
lower_lemhi_poly  = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/LL_Poly_Fish.shp')
pahs_line         = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/Pah_Line_Fish.shp')
pahs_poly         = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/Pah_Poly_Fish.shp')
upper_salmon_line = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/US_Line_Fish.shp')
upper_salmon_poly = st_read('C:/Git/DASH/data/raw/dash2018/ShapefilesWith_MetricsV3_reaches/US_Poly_Fish.shp')

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

# merge lower lemhi data
lower_lemhi_line = lower_lemhi_line %>%
  mutate(A_Vg_Cv = NA, Length = NA, Reach = NA)
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

# merge upper data
upper_salmon_line = upper_salmon_line %>%
  mutate(A_Vg_Cv = NA, Length = NA, sinusty = NA)
upper_salmon_poly = upper_salmon_poly %>%
  mutate(Avg_wdt = NA, SHAPE_L = NA, sinusty = NA)
upper_salmon = rbind(upper_salmon_line, upper_salmon_poly)
upper_salmon = as.data.frame(upper_salmon)

# remove 'Reach' from lower lemhi data
lower_lemhi = lower_lemhi %>%
  select(-Reach)

# merge all sites
dash2018_cu = rbind(upper_lemhi, lower_lemhi, pahs, upper_salmon) %>%
  select(SiteNam, Reach_Nmb, everything())

# begin rolling up summaries by fish reach
dash2018_fr = dash2018_cu %>%
  group_by(SiteNam, Reach_Nmb) %>%
  summarise(No_Cov = weighted.mean(No_Cov, SHAPE_A),
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
            Undrc_V = sum(Undrc_V))

# write fish reach data to csv
write_csv(dash2018_fr, 'data/prepped/dash2018_fr.csv')
