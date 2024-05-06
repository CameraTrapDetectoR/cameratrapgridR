#' Make Grid
#'
#' @description generate a grid over a polygon
#'
#' @param cellsize
#' @param org.x
#' @param org.y
#' @param num.cols
#' @param area.threshold
#'
#' @return
#'
#' @import sf
#'
#' @export
#'
#' @examples
make_grid <- function(dat, cellsize, org.x=NULL, org.y=NULL, num.cols=NULL,
                      area.threshold, plot.proportion=TRUE){

  # set scientific notation preferences
  options(scipen = 999)

  #--Make grid
  if(is.null(org.x)==FALSE){
    grd.dat <- sf::st_make_grid(dat, cellsize=cellsize, offset=c(org.x, org.y), square=TRUE, what="polygons")
  }
  if(is.null(org.x)==TRUE){
    grd.dat <- sf::st_make_grid(dat, cellsize=cellsize)
  }

  grd.dat <- sf::st_sf(grd.dat)

  #--Add grid number
  grd.dat$poly.id <- seq(1, nrow(grd.dat),1)

  #--Add grid centroid
  coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(grd.dat)))
  coords$poly.id <- seq(1, nrow(coords),1)

  coords$X <- round(coords$X,0)
  coords$Y <- round(coords$Y,0)

  coords <- coords[order(coords$X, -coords$Y),]

  coords$grd_num <- seq(1, nrow(coords),1)

  grd.dat <- merge(grd.dat, coords, by="poly.id", all=TRUE)

  grd.dat <- grd.dat[order(grd.dat$grd_num),]

  #--Restrict to user specified number of columns
  # if(is.null(num.cols)==FALSE){
  #   x <- grd.dat[grd.dat$grd_num==num.cols,"X"]
  #   st_geometry(x)<-NULL
  #
  #   grd.dat <- grd.dat[grd.dat$X <= x$X,]
  # }

  #--Restrict to full grid cells intersecting master polygons
  intr.dat <- sf::st_intersection(grd.dat, dat)

  intr.dat$area <- sf::st_area(intr.dat)
  intr.dat$proportion.area <- units::drop_units(intr.dat$area / cellsize)

  #--Return plot of cells with proportion in study area
  if(plot.proportion==TRUE){
    plot(intr.dat["proportion.area"])
  }

  #--Apply threshold for area of cell in study area
  intr.dat <- intr.dat[intr.dat$proportion.area > area.threshold,]

  grd.dat <- grd.dat[grd.dat$grd_num %in% intr.dat$grd_num,]

  return(grd.dat)
}


