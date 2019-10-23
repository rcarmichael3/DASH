#' @title Channel Unit Metrics
#'
#' @description Combine all the channel unit measurements for a particular site and calculate channel unit metrics
#'
#' @author Kevin See
#'
#' @param folder_name character string of the folder name where Survey123 data has been saved
#'
#' @import dplyr
#' @return NULL
#' @export

calc_cu_metrics = function(folder_name = NULL) {
  
  cu_mets = read_csv(paste(folder_name, 'CU_1.csv', sep = '/')) %>%
    select(-starts_with('Pebble'))
  
  wood_mets = read_csv(paste(folder_name, 'Wood_2.csv', sep = '/')) %>%
    woodMetrics()
  
  jam_mets = read_csv(paste(folder_name, 'Jam_3.csv', sep = '/')) %>%
    jamMetrics() %>%
    group_by(parentGlobalId) %>%
    summarise_at(vars(estimatedNumberOfPieces, jamVolume),
                 list(sum))
  
  und_mets = read_csv(paste(folder_name, 'Undercut_4.csv', sep = '/')) %>%
    undercutMetrics() %>%
    group_by(parentGlobalId) %>%
    summarise_at(vars(undercutAT, undercutLength, nUndercuts),
                 list(sum))
    
  
  cu_mets %>%
    select(-starts_with('Width')) %>%
    rename(parentGlobalId = GlobalID) %>%
    full_join(wood_mets) %>%
    full_join(jam_mets) %>%
    full_join(und_mets)
  
  
  
  read_csv(paste(folder_name, 'Discharge_5.csv', sep = '/')) %>%
    as.data.frame()
  
  read_csv(paste(folder_name, 'DischargeMeasurements_6.csv', sep = '/'))
  
  cu_mets %>%
    select(GlobalID, starts_with("Pebble"))
  
}