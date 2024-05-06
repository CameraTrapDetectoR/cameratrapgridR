#' GIS union
#'
#' @description perform a real GIS union operation, similar to QGIS or ArcGIS
#'
#' @param a the first sf object
#' @param b the second sf object
#'
#' @return an sf object that unions sf a and b
#'
#' @import sf
#' @import dplyr
#'
#' @export
#'
#' @examples
GIS_union <- function(a,b) {

  sf::st_agr(a) = "constant"
  sf::st_agr(b) = "constant"


  op1 <- sf::st_difference(a,sf::st_union(b))
  op2 <- sf::st_difference(b, sf::st_union(a))
  op3 <- sf::st_intersection(b, a)

  # union <- plyr::rbind.fill(op1, op2, op3)
  union <- dplyr::bind_rows(op1, op2, op3)
  sf_union <- sf::st_as_sf(union)

  return(sf_union)
}
