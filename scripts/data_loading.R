getwd()
library(tidyverse)

# importing and saving Hawley data
CU_1_Hawley2019 = read_delim('inst/extdata/Hawley_Survey123_2019/CU_1.csv', delim = "\t")
save(CU_1_Hawley2019, file = 'data/CU_1_Hawley2019.rda')

Wood_2_Hawley2019 = read_csv('inst/extdata/Hawley_Survey123_2019/Wood_2.csv')
save(Wood_2_Hawley2019, file = 'data/Wood_2_Hawley2019.rda')

Jam_3_Hawley2019 = read_csv('inst/extdata/Hawley_Survey123_2019/Jam_3.csv')
save(Jam_3_Hawley2019, file = 'data/Jam_3_Hawley2019.rda')

Undercut_4_Hawley2019 = read_csv('inst/extdata/Hawley_Survey123_2019/Undercut_4.csv')
save(Undercut_4_Hawley2019, file = 'data/Undercut_4_Hawley2019.rda')

DischargeLocations_5_Hawley2019 = read_csv('inst/extdata/Hawley_Survey123_2019/Discharge_5.csv')
save(DischargeLocations_5_Hawley2019, file = 'data/DischargeLocations_5_Hawley2019.rda')

DischargeMeasurements_6_Hawley2019 = read_csv('inst/extdata/Hawley_Survey123_2019/DischargeMeasurements_6.csv')
save(DischargeMeasurements_6_Hawley2019, file = 'data/DischargeMeasurements_6_Hawley2019.rda')

# importing and saving Upper Lemhi 1 data
CU_1_UpperLemhi1_2019 = read_delim('inst/extdata/UpperLemhi1_Survey123_2019/CU_1.csv', delim = "\t")
save(CU_1_UpperLemhi1_2019, file = 'data/CU_1_UpperLemhi1_2019.rda')

Wood_2_UpperLemhi1_2019 = read_csv('inst/extdata/UpperLemhi1_Survey123_2019/Wood_2.csv')
save(Wood_2_UpperLemhi1_2019, file = 'data/Wood_2_UpperLemhi1_2019.rda')

Jam_3_UpperLemhi1_2019 = read_csv('inst/extdata/UpperLemhi1_Survey123_2019/Jam_3.csv')
save(Jam_3_UpperLemhi1_2019, file = 'data/Jam_3_UpperLemhi1_2019.rda')

Undercut_4_UpperLemhi1_2019 = read_csv('inst/extdata/UpperLemhi1_Survey123_2019/Undercut_4.csv')
save(Undercut_4_UpperLemhi1_2019, file = 'data/Undercut_4_UpperLemhi1_2019.rda')

DischargeLocations_5_UpperLemhi1_2019 = read_csv('inst/extdata/UpperLemhi1_Survey123_2019/Discharge_5.csv')
save(DischargeLocations_5_UpperLemhi1_2019, file = 'data/DischargeLocations_5_UpperLemhi1_2019.rda')

DischargeMeasurements_6_UpperLemhi1_2019 = read_csv('inst/extdata/UpperLemhi1_Survey123_2019/DischargeMeasurements_6.csv')
save(DischargeMeasurements_6_UpperLemhi1_2019, file = 'data/DischargeMeasurements_6_UpperLemhi1_2019.rda')



