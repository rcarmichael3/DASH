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
  udcut_tbl = clean_names(udcut_tbl, case = "lower_camel")
  udcut_tbl = select(udcut_tbl, lengthM, width25PercentM, width50PercentM, width75PercentM,
                     parentGlobalId)
  udcut_tbl = mutate(udcut_tbl, 
                     undercutArea = lengthM * ((width25PercentM*width50PercentM*width75PercentM)/3))
  udcut_tbl = group_by(udcut_tbl, parentGlobalId)
  udcut_tbl = mutate(udcut_tbl,
                     undercutLength = sum(lengthM),
                     nUndercuts = length(parentGlobalId),
                     undercutAT = sum(undercutArea))
  udcut_tbl = ungroup(udcut_tbl)
  udcut_tbl = distinct(udcut_tbl, parentGlobalId, .keep_all = TRUE)
  udcut_tbl = select(udcut_tbl, parentGlobalId, undercutAT, undercutLength, nUndercuts)
}  
