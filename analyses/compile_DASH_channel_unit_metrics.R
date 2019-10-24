# Author: Kevin See
# Purpose: Calculate DASH metrics at channel unit scales
# Created: 10/23/2019
# Last Modified: 10/23/19
# Notes: started with 2019 DASH data

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(DASH)

top_folder = "data/raw/dash2019"
all_mets = compile_cu_metrics(top_folder)

