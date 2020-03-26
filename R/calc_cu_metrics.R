#' @title Channel Unit Metrics - Site Scale
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
  
  folder_files = list.files(folder_name)
  surv_pt_file = folder_files[grepl('surveyPoint', folder_files)]
  cu_mets_file = folder_files[grepl('CU', folder_files)]
  wood_mets_file = folder_files[grepl('Wood', folder_files)]
  jam_mets_file = folder_files[grepl('Jam', folder_files)]
  und_mets_file = folder_files[grepl('Undercut', folder_files)]
  disch_file = folder_files[grepl('Discharge_', folder_files)]
  disch_meas_file = folder_files[grepl('DischargeMeasurements', folder_files)]
  
  
  surv_pt = read_csv(paste(folder_name, surv_pt_file, sep = '/'),
                     col_types = cols()) %>%
    clean_names() %>%
    select(site_id = global_id,
           site_name,
           survey_date,
           survey_crew) %>%
    mutate(survey_date = str_split(survey_date, ' ', simplify = T)[,1]) %>%
    mutate(survey_date = mdy(survey_date))
  
  cu_mets = read_csv(paste(folder_name, cu_mets_file, sep = '/'),
                     col_types = cols()) %>%
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
  
  wood_mets = try(read_csv(paste(folder_name, wood_mets_file, sep = '/'),
                           col_types = cols()) %>%
                    woodMetrics() %>%
                    clean_names())
  
  if(class(wood_mets)[1] == 'try-error') {
    wood_mets = tibble(parent_global_id = cu_mets$global_id[1],
                      lwd_vt = NA,
                      lwd_at = NA,
                      lwd_pieces = NA,
                      lwd_wet = NA,
                      lwd_chn_frm = NA,
                      lwd_ballast = NA)
  }
  
  
  jam_mets = try(read_csv(paste(folder_name, jam_mets_file, sep = '/'),
                          col_types = cols()) %>%
                   jamMetrics() %>%
                   clean_names() %>%
                   group_by(parent_global_id) %>%
                   summarise_at(vars(estimated_number_of_pieces, jam_volume),
                                list(sum)))
  
  if(class(jam_mets)[1] == 'try-error') {
    jam_mets = tibble(parent_global_id = cu_mets$global_id[1],
                      estimated_number_of_pieces = NA,
                      jam_volume = NA)
  }
  
  und_mets = try(read_csv(paste(folder_name, und_mets_file, sep = '/'),
                          col_types = cols()) %>%
                   undercutMetrics() %>%
                   clean_names() %>%
                   group_by(parent_global_id) %>%
                   summarise_at(vars(undercut_at, undercut_length, n_undercuts),
                                list(sum)))
  
  if(class(und_mets)[1] == 'try-error') {
    und_mets = tibble(parent_global_id = cu_mets$global_id[1],
                      undercut_at = NA, 
                      undercut_length = NA, 
                      n_undercuts = NA)
  } 
  
   
  # combine all of these together
  all_cu_mets = surv_pt %>%
    full_join(cu_mets %>%
                select(-starts_with('width')) %>%
                rename(parent_global_id = global_id),
              by = 'site_id') %>%
    full_join(wood_mets,
              by = 'parent_global_id') %>%
    full_join(jam_mets,
              by = 'parent_global_id') %>%
    full_join(und_mets,
              by = 'parent_global_id') %>%
    full_join(ssc_width,
              by = 'parent_global_id') %>%
    rename(channel_unit_id = parent_global_id) %>%
    mutate_at(vars(overhanging_cover:boulder_256mm),
              list(~ if_else(is.na(.),
                             0, as.numeric(.)))) %>%
    tidyr::fill(site_id, site_name)
    

  # discharge is sometimes recorded at top or bottom of site (TOS, BOS), but this doesn't match
  # any particular channel_unit_id. Unclear whether channel units were labeled moving upstream or downstream.
  # until that is resolved, not including discharge
  
  # disch_mets = read_csv(paste(folder_name, disch_meas_file, sep = '/'),
  #                       col_types = cols()) %>%
  #   clean_names() %>%
  #   mutate(station_discharge = station_width * station_depth * station_velocity) %>%
  #   group_by(parent_global_id) %>%
  #   summarise(Q = sum(station_discharge)) %>%
  #   left_join(read_csv(paste(folder_name, disch_file, sep = '/'),
  #                      col_types = cols()) %>%
  #               clean_names() %>%
  #               select(site_id = parent_global_id,
  #                      parent_global_id = global_id,
  #                      channel_unit_number = discharge_location_bos_tos_cu_number)) %>%
  #   select(-parent_global_id)
  # 
  # all_cu_mets = all_cu_mets %>%
  #   left_join(disch_mets)
  
  return(all_cu_mets)
}