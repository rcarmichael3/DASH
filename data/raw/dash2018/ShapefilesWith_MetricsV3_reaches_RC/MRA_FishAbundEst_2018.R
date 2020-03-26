# Author: Kevin See
# Purpose: Estimate fish abundance
# Created: 1/16/2020
# Last Modified: 1/16/2020
# Notes: pulled out relevant sections from PrepFishData.R and Abund_RchCapProb.R scripts

#----------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(FSA) #citation: Ogle, D.H. 2017. FSA: Fisheries Stock Analysis. R package version 0.8.12.


setwd('D:/GIS_crap/MRA/ShapefilesWith_MetricsV3_reaches_RC')

#----------------------------------------
# read in fish data
#----------------------------------------
allData = read_csv('MRA_Fish_2018.csv') %>%
  filter(!is.na(SiteName)) %>%
  mutate(SiteName = recode(SiteName,
                           'UpperLemhi2_2018' = 'UpperLemhi_2018',
                           'UpperLemhi3_2018' = 'UpperLemhi_2018',
                           'LowerLemhi6_2018' = 'LowerLemhi_2018'))

#-----------------------------------
# pull out the relevant fish data
fishDf = allData %>%
  # filter out fish that were dead on the mark pass
  filter(!(dceType == 'Mark' & FishStatus == 'Dead'),
         FishStatus != 'Fry') %>%
  # filter out any spot that doesn't have an associated habitat unit
  filter(!is.na(HabitatReach)) %>%
  # pull out select species
  filter(Species %in% c('Chinook',
                        'Brook Trout',
                        'Steelhead')) %>%
  mutate_at(vars(dceType, 
                 SiteName,
                 FishStatus,
                 Species),
            list(fct_drop)) %>%
  mutate_at(vars(SurveyDateTime),
            list(dmy))

#--------------------------------------------
# which fish moved channel units?
#--------------------------------------------
movingFish = fishDf %>%
  filter(!is.na(PitTagID)) %>%
  select(Species, PitTagID, SiteName, HabitatReach) %>%
  distinct() %>%
  mutate(id = paste(SiteName, HabitatReach, sep = '_')) %>%
  group_by(Species, PitTagID) %>%
  summarise(nSites = n_distinct(SiteName),
            nHR = n_distinct(id)) %>%
  ungroup() %>%
  filter(nHR > 1 | nSites > 1) %>%
  inner_join(fishDf %>%
               group_by(PitTagID) %>%
               filter(SurveyDateTime == min(SurveyDateTime)) %>%
               select(PitTagID,
                      BegSite = SiteName,
                      BegHR = HabitatReach) %>%
               distinct()) %>%
  left_join(fishDf %>%
              group_by(PitTagID) %>%
              filter(SurveyDateTime == max(SurveyDateTime)) %>%
              select(PitTagID,
                     # EndSite = SiteName, 
                     EndHR = HabitatReach) %>%
              distinct())

#--------------------------------------------
# summarise capture histories by species, stream name and habitat unit
#--------------------------------------------
capHistSpp = fishDf %>%
  group_by(Species, SiteName, HabitatReach, dceType) %>%
  summarise(count = sum(FishCount)) %>%
  ungroup() %>%
  spread(dceType, count,
         fill = 0) %>%
  rename(M = Mark,
         C = Recapture) %>%
  left_join(fishDf %>%
              # drop fish recaptured in a different channel unit than they were marked in
              filter(!PitTagID %in% movingFish$PitTagID) %>%
              filter(dceType == 'Recapture',
                     Tag2RecaptureType == 'Non-Efficiency Recapture') %>%
              group_by(Species, SiteName, HabitatReach) %>%
              summarise(R = sum(FishCount))) %>%
  ungroup() %>%
  mutate_at(vars(M:R),
            list(~ ifelse(is.na(.), 0, .))) %>%
  # some channel units had more recaptures than marks. This is due to non-PIT tag marked fish moving in, so we're going to force those channel units to not have more recaptures than marks.
  mutate_at(vars(R),
            list(~ if_else(R > M, M, R))) %>%
  arrange(SiteName, HabitatReach)


#----------------------------------------
# redo capture histories, don't exclude fish who moved channel units, group by reach
capHistRch = fishDf %>%
  group_by(Species, SiteName, dceType) %>%
  summarise(count = sum(FishCount)) %>%
  ungroup() %>%
  spread(dceType, count,
         fill = 0) %>%
  rename(M = Mark,
         C = Recapture) %>%
  left_join(fishDf %>%
              # drop fish recaptured in a different channel unit than they were marked in
              filter(dceType == 'Recapture',
                     Tag2RecaptureType == 'Non-Efficiency Recapture') %>%
              group_by(Species, SiteName) %>%
              summarise(R = sum(FishCount))) %>%
  ungroup() %>%
  mutate_at(vars(M:R),
            list(~ ifelse(is.na(.), 0, .))) %>%
  # some channel units had more recaptures than marks. This is due to non-PIT tag marked fish moving in, so we're going to force those channel units to not have more recaptures than marks.
  mutate_at(vars(R),
            list(~ if_else(R > M, M, R))) %>%
  arrange(SiteName, Species)


#----------------------------------------
# fit a Chapman estimator to each species / site
Nmods1 = capHistRch %>% 
  split(list(.$Species, .$SiteName)) %>%
  map(.f = function(x) {
    with(x,
         mrClosed(M, C, R,
                  method = 'Chapman') )
  })

Nsumm1 = Nmods1 %>%
  map_df(.id = 'id',
         .f = function(x) {
           summary(x, incl.SE = T) %>%
             as_tibble()
         }) %>%
  mutate(Species = str_split(id, '\\.', simplify = T)[,1],
         SiteName = str_split(id, '\\.', simplify = T)[,2]) %>%
  select(-id) %>%
  full_join(capHistRch) %>%
  mutate(pHat = M / N,
         pSE = (M * SE) / (N^2)) %>%
  select(one_of(names(capHistRch)), everything()) 

CUsumm1 = capHistSpp %>%
  left_join(Nsumm1 %>%
              select(Species, SiteName, 
                     Ntot = N, 
                     SEtot = SE,
                     pHat, pSE)) %>%
  # mutate(Nhat = M / pHat,
  #        Nse = (M * pSE) / (Nhat^2)) %>%
  mutate(avgC = (M + C) / 2,
         Nhat = avgC / pHat,
         Nse = (avgC * pSE) / (pHat^2)) %>%
  mutate_at(vars(Nhat),
            list(round))

#----------------------------------------
# save as csv
#----------------------------------------
CUsumm1 %>%
  filter(Species != 'Brook Trout') %>%
  select(-Ntot, -SEtot) %>%
  write_csv('EstFishAbund.csv')

