#' @title Channel Unit Metrics - All Sites
#'
#' @description Combine all the channel unit measurements for a particular site and calculate channel unit metrics
#'
#' @author Kevin See
#'
#' @param folder_name character string of the folder name where list of folders with Survey123 data
#'
#' @import dplyr lubridate purrr
#' @return NULL
#' @export

compile_cu_metrics = function(folder_name = NULL) {
  
  all_mets = list.files(folder_name) %>%
    as.list() %>%
    rlang::set_names() %>%
    map_df(.id = 'folder',
           .f = function(x) {
      res = try(calc_cu_metrics(paste(folder_name, x, sep = '/')))
      if(class(res)[1] == 'try-error') {
        cat(paste('Error with', x))
        return(NULL)
      } else {
        return(res)
      }
    })
  
  return(all_mets)
}