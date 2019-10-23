#' summarize the pebble count metrics for a DASH reach
#' 
#' @param input the name of the tbl_df containing pebble count data for a DASH reach.
#' 
#' @author Richie Carmichael
#' 
#' @import janitor dplyr tidyr
#' @importFrom stats quantile
#' @export 
#' @return NULL

pebbleCountMetrics = function(input = NULL)
{
  # import data into function
  if(is.character(input) == TRUE) { pbl_tbl = read_csv(input) } else { pbl_tbl = input }
  
  # calculate undercut metrics
  pbl_tbl = clean_names(pbl_tbl, case = "lower_camel")
  pbl_tbl = select(pbl_tbl, pebble1Mm, pebble2Mm, pebble3Mm, pebble4Mm, pebble5Mm, pebble6Mm,
                   pebble7Mm, pebble8Mm, pebble9Mm, pebble10Mm, pebble11Mm,)
  pbl_tbl = gather(pbl_tbl, key = "pebble", value = "sizeMM", pebble1Mm:pebble11Mm)
  pbl_tbl = filter(pbl_tbl, !is.na(sizeMM))
  pbl_tbl = mutate(pbl_tbl,
                   d50 = stats::quantile(sizeMM, probs = 0.50),
                   d84 = stats::quantile(sizeMM, probs = 0.84))
  pbl_tbl = distinct(pbl_tbl, d50, d84, .keep_all = TRUE)
  pbl_tbl = select(pbl_tbl, d50, d84)
  
  return(pbl_tbl)
}  
