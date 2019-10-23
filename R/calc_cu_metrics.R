#' @title Channel Unit Metrics
#'
#' @description Combine all the channel unit measurements for a particular site and calculate channel unit metrics
#'
#' @author Kevin See
#'
#' @param folder_name character string of the folder name where Survey123 data has been saved
#'
#' @import dplyr lubridate
#' @return NULL
#' @export

calc_cu_metrics = function(folder_name = NULL) {
  
  surv_pt = read_csv(paste(folder_name, 'surveyPoint_0.csv', sep = '/')) %>%
    clean_names() %>%
    select(site_id = global_id,
           site_name,
           survey_date,
           survey_crew) %>%
    mutate(survey_date = mdy_hms(survey_date),
           survey_date = floor_date(survey_date,
                                    unit = 'day'))
  
  cu_mets = read_csv(paste(folder_name, 'CU_1.csv', sep = '/')) %>%
    clean_names() %>%
    select(-starts_with('pebble'),
           -(creation_date:editor)) %>%
    rename(site_id = parent_global_id)
  
  # calculate avg and CV of small side channel widths
  ssc_width = cu_mets %>%
    filter(channel_unit_type == 'SSC') %>%
    select(parent_global_id = global_id, starts_with('width')) %>%
    gather(meas_num, width, starts_with('width')) %>%
    mutate_at(vars(width),
              list(as.numeric)) %>%
    group_by(parent_global_id) %>%
    summarise_at(vars(width),
                 list(avg_ssc_width = mean,
                      sd_ssc_width = sd)) %>%
    mutate(cv_ssc_width = sd_ssc_width / avg_ssc_width) %>%
    select(-sd_ssc_width)
  
  wood_mets = read_csv(paste(folder_name, 'Wood_2.csv', sep = '/')) %>%
    woodMetrics() %>%
    clean_names()
  
  jam_mets = read_csv(paste(folder_name, 'Jam_3.csv', sep = '/')) %>%
    jamMetrics() %>%
    clean_names() %>%
    group_by(parent_global_id) %>%
    summarise_at(vars(estimated_number_of_pieces, jam_volume),
                 list(sum))
  
  und_mets = read_csv(paste(folder_name, 'Undercut_4.csv', sep = '/')) %>%
    undercutMetrics() %>%
    clean_names() %>%
    group_by(parent_global_id) %>%
    summarise_at(vars(undercut_at, undercut_length, n_undercuts),
                 list(sum))
    
  # combine all of these together
  all_cu_mets = surv_pt %>%
    full_join(cu_mets %>%
                select(-starts_with('width')) %>%
                rename(parent_global_id = global_id)) %>%
    full_join(wood_mets) %>%
    full_join(jam_mets) %>%
    full_join(und_mets) %>%
    full_join(ssc_width) %>%
    rename(channel_unit_id = parent_global_id) %>%
    mutate_at(vars(overhanging_cover:boulder_256mm),
              list(~ if_else(is.na(.),
                             0, as.numeric(.))))
    
  
  disch_mets = read_csv(paste(folder_name, 'DischargeMeasurements_6.csv', sep = '/')) %>%
    clean_names() %>%
    mutate(station_discharge = station_width * station_depth * station_velocity) %>%
    group_by(parent_global_id) %>%
    summarise(Q = sum(station_discharge)) %>%
    left_join(read_csv(paste(folder_name, 'Discharge_5.csv', sep = '/')) %>%
                clean_names() %>%
                select(site_id = parent_global_id,
                       parent_global_id = global_id,
                       channel_unit_number = discharge_location_bos_tos_cu_number)) %>%
    select(-parent_global_id)
  
  all_cu_mets = all_cu_mets %>%
    left_join(disch_mets)
  
  return(all_cu_mets)
}