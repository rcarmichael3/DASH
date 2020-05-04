library(sf)
library(tidyverse)


setwd("D/MRA_Data/MRA_Fish_Capture")

##read in fish data##

Fish_Abundance = read.csv("C:/Users/richiec/Dropbox/MRA/Analyses/fish/outgoing/EstFishAbund.csv")


##read in spatial files with habitat and area##

LL_Poly <- st_read("D:/MRA_Data/Metrics/LL_Poly_Fish.shp")
LL_Poly <- LL_Poly %>%
  select(-28, -29)


LL_Lines <- st_read("D:/MRA_Data/Metrics/LL_Line_Fish.shp")
LL_Lines <- LL_Lines %>%
  select(-29, -30)


LL_Abundance <- Fish_Abundance %>%
  filter(SiteName == "LowerLemhi_2018" , Species == "Chinook") %>%
  select(HabitatUnit , Nhat)

LL_Poly_Fish <- left_join(LL_Poly, LL_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))
LL_Line_Fish <- left_join(LL_Lines, LL_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))

st_write(LL_Poly_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/LL_Poly_Fish.shp")
st_write(LL_Line_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/LL_Line_Fish.shp")






Pah_Poly <- st_read("D:/MRA_Data/Metrics/Pah_Poly_Fish.shp")
Pah_Poly <- Pah_Poly %>%
  select(-4,-29, -30)

Pah_Lines <- st_read("D:/MRA_Data/Metrics/Pah_Line_Fish.shp")
Pah_Lines <- Pah_Lines %>%
  select(-29, -30)

Pah_Abundance <- Fish_Abundance %>%
  filter(SiteName == "Pahsimeroi_2018" , Species == "Chinook") %>%
  select(HabitatUnit , Nhat)

Pah_Poly_Fish <- left_join(Pah_Poly, Pah_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))
Pah_Line_Fish <- left_join(Pah_Lines, Pah_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))

st_write(Pah_Poly_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/Pah_Poly_Fish.shp")
st_write(Pah_Line_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/Pah_Line_Fish.shp")



UL_Poly <- st_read("D:/MRA_Data/Metrics/UL_Poly_Fish.shp")
UL_Poly <- UL_Poly %>%
  select(-4, -29, -30)

UL_Lines <- st_read("D:/MRA_Data/Metrics/UL_Line_Fish.shp")
UL_Lines <- UL_Lines %>%
  select(-29, -30)

UL_Abundance <- Fish_Abundance %>%
  filter(SiteName == "UpperLemhi_2018" , Species == "Chinook") %>%
  select(HabitatUnit , Nhat)

UL_Poly_Fish <- left_join(UL_Poly, UL_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))
UL_Line_Fish <- left_join(UL_Lines, UL_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))


st_write(UL_Poly_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/UL_Poly_Fish.shp")
st_write(UL_Line_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/UL_Line_Fish.shp")



US_Poly <- st_read("D:/MRA_Data/Metrics/US_Poly_Fish.shp")
US_Poly <- US_Poly %>%
  select(-4, -29, -30)

US_Lines <- st_read("D:/MRA_Data/Metrics/US_Line_Fish.shp")
US_Lines <- US_Lines %>%
  select(-28, -29)

US_Abundance <- Fish_Abundance %>%
  filter(SiteName == "UpperSalmon_2018" , Species == "Chinook") %>%
  select(HabitatUnit , Nhat)

US_Poly_Fish <- left_join(US_Poly, US_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))
US_Line_Fish <- left_join(US_Lines, US_Abundance, by = c("Unt_Nmb" = "HabitatUnit"))


st_write(US_Poly_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/US_Poly_Fish.shp")
st_write(US_Line_Fish, "C:/Users/richiec/Dropbox/MRA/Analyses/fish_hab/MetricsV2/US_Line_Fish.shp")







