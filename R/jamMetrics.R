#' summarize the wood jam metrics for a DASH reach
#' 
#' @param input the name of the tbl_df containing wood jam data for a DASH reach.
#' 
#' @author Richie Carmichael
#' 
#' @import dplyr janitor
#' @export 
#' @return NULL

jamMetrics = function(input = NULL)
{
  # import data into function
  if(is.character(input) == TRUE) { jam_tbl = read_csv(input) } else { jam_tbl = input }
  
  # calculate jam metrics
  jam_tbl = clean_names(jam_tbl, case = "lower_camel")
  jam_tbl = select(jam_tbl, lengthM, widthM, heightM, estimatedNumberOfPieces, parentGlobalId)
  jam_tbl = mutate(jam_tbl, jamVolume = lengthM * widthM * heightM)
  jam_tbl = select(jam_tbl, estimatedNumberOfPieces, jamVolume, parentGlobalId)
  
  return(jam_tbl)
}  
