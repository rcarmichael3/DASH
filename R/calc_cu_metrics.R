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
  
  cu_mets = read_csv(paste(folder_name, 'CU_1.csv', sep = '/'))
  
  wood_mets = read_csv(paste(folder_name, 'Wood_2.csv', sep = '/')) %>%
    woodMetrics()
  
  read_csv(paste(folder_name, 'Jam_3.csv', sep = '/')) %>%
    jamMetrics()
  
  read_csv(paste(folder_name, 'Undercut_4.csv', sep = '/')) %>%
    undercutMetrics()
  
}