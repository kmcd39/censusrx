#' roads.tigerline.metadata
#'
#' Export metadata for roads tigerline features
#'
#' @export roads.tigerline.metadata
roads.tigerline.metadata <- function(rttyp = F) {

  if(rttyp)
    return(tibble(
       rttyp = Hmisc::Cs(C, I, M, O, S, U)
      ,desc = Hmisc::Cs(county, interstate, common.name, other, state, US)
    ))

  tibble(
    mtfcc = Hmisc::Cs(
      S1100
      ,S1200
      ,S1400
      ,S1630
      ,S1710
      ,S1730
      ,S1820
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
