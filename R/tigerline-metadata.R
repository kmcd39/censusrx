#' roads.tigerline.metadata
#'
#' Export metadata for roads tigerline features.
#'
#' Retruns a mapping for route type (`rttyp`) if `rttyp = T`; otherwise returns
#' descriptions for `MTFCC`
#'
#'
#' @export roads.tigerline.metadata
#'
roads.tigerline.metadata <- function(rttyp = F) {

  requireNamespace("dplyr")

  if(rttyp)
    return(
      tibble(
        rttyp = c("C", "I", "M", "O", "S", "U")
        ,desc = c("county", "interstate", "common.name", "other", "state", "US")
      )
    )

  tibble(
    mtfcc = c(
       "S1100"
      ,"S1200"
      ,"S1400"
      ,"S1630"
      ,"S1710"
      ,"S1730"
      ,"S1820"
    )
    ,desc = c(
      'Primary Road'
      ,'Secondary Road'
      ,'Local Street'
      ,'Ramp'
      ,'Walkway/Pedestrian Trail'
      ,'Alley'
      ,'Bike Path/Trail'
    )
  )

}
