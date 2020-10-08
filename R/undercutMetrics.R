#' summarize the undercut bank metrics for a DASH reach
#' 
#' @param input the name of the tbl_df containing undercut bank data for a DASH reach.
#' 
#' @author Richie Carmichael
#' 
#' @import janitor dplyr
#' @export 
#' @return NULL

undercutMetrics = function(input = NULL)
{
  # import data into function
  if(is.character(input) == TRUE) { udcut_tbl = read_csv(input) } else { udcut_tbl = input }
  
  # calculate undercut metrics
  udcut_tbl = clean_names(udcut_tbl, case = "lower_camel") %>%
    select(lengthM, width25PercentM, width50PercentM, width75PercentM, parentGlobalId) %>%
    mutate(undercutArea = lengthM * ((width25PercentM+width50PercentM+width75PercentM)/3)) %>%
    group_by(parentGlobalId) %>%
    mutate(undercutLength = sum(lengthM),
           nUndercuts = length(parentGlobalId),
           undercutAT = sum(undercutArea)) %>%
    ungroup() %>%
    distinct(parentGlobalId, .keep_all = TRUE) %>%
    select(parentGlobalId, undercutAT, undercutLength, nUndercuts)

  return(udcut_tbl)
}  
