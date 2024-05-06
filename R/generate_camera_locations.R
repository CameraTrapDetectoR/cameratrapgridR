#' Generate Camera Locations
#'
#' @description Generate a set of possible camera locations across camera grid
#'
#'
#' @param poly.dat
#' @param area.threshold
#' @param num.per.poly
#'
#' @return
#'
#' @import sf
#' @import units
#' @import dplyr
#'
#' @export
#'
#' @examples
generate_cam_locations <- function(poly.dat, area.threshold, num.per.poly){

  gridn <- unique(poly.dat$grd_num)

  for(i in 1:length(gridn)){
    if(i==1){out <- list()}

    tmp <- poly.dat[poly.dat$grd_num==gridn[i],]

    tmp <- tmp %>%
      dplyr::group_by(grd_num) %>%
      dplyr::summarise() %>%
      sf::st_cast()

    tmp <- sf::st_cast(tmp, "POLYGON")

    tmp$area <- units::set_units(sf::st_area(tmp), "hectare")

    tmp <- tmp[tmp$area > units::set_units(area.threshold, "hectare"), ]

    if(nrow(tmp)!=0){
      ### could use st_sample here and might allow for stratified sample
      #pnt <- spsample(as(tmp,'Spatial') , n=num.per.poly, type="random", pretty=TRUE)
      pnt <- sf::st_sample(tmp, size=num.per.poly)
      pnt <- sf::st_as_sf(pnt)

      pnt <- cbind(pnt, sf::st_coordinates(pnt))

      pnt$grd_num <- gridn[i]

      out[[i]] <- pnt
    }
  }

  potential.pnts <- Reduce(rbind, out)

  return(potential.pnts)
}
