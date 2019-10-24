#' summarize the large woody debris metrics for a DASH reach
#' 
#' @param input the name of the tbl_df containing large woody debris data for a DASH reach. Changing comments
#' 
#' @author Richie Carmichael
#' 
#' @import tidyverse readr dplyr janitor
#' @export
#' @return NULL

woodMetrics = function(input = NULL)
{
  # import data into function
  if(is.character(input) == TRUE) { wd_tbl = read_csv(input) } else { wd_tbl = input }
  
  # calculate LWD metrics
  wd_tbl = clean_names(wd_tbl, case = "lower_camel") %>%
    select(largeWoodNumber, lengthM, diameterM, wet, channelForming, ballasted, parentGlobalId) %>%
    mutate(woodA = diameterM * lengthM, # calculate individual piece areas in m2
           woodV = pi * ((diameterM/2)^2) * lengthM) %>%
    group_by(parentGlobalId) %>% 
    mutate(lwdAT = sum(woodA),
           lwdVT = sum(woodV),
           lwdPieces = length(parentGlobalId),
           lwdWet = sum(wet == 'Yes'),
           lwdChnFrm = sum(channelForming == 'Yes'),
           lwdBallast = sum(ballasted == 'Yes')) %>%
    ungroup() %>%
    distinct(parentGlobalId, .keep_all = TRUE) %>%
    select(parentGlobalId, lwdVT, lwdAT, lwdPieces, lwdWet, lwdChnFrm, lwdBallast)
  
  return(wd_tbl)
}  

