#' Apply distance constraint
#'
#' @description select final camera locations based on distance constraints
#'
#' @param in.dat potential points to sample from
#' @param distance.threshold minimum distance between points
#'
#' @import units
#' @import dplyr
#' @import sf
#'
#' @return sf object with randomly sampled points
#' @export
#'
#' @examples
add_distance_constraint <- function(in.dat, distance.threshold){

  # get unique grids
  grids <- unique(in.dat$grd_num)

  # set up the final locations df with the first grid
  grd_dat <- in.dat %>%
    dplyr::filter(grd_num == grids[1])
  grid_samp <- dplyr::sample_n(grd_dat, size = 1)
  location_pts <- grid_samp

  # loop through grids and sample a point
  for(i in 2:length(grids)){

    # set a buffer around the current iterations of location points
    buffer <- sf::st_buffer(location_pts, dist=distance.threshold)

    # subset data to single grid
    grd_dat <- in.dat %>%
      dplyr::filter(grd_num == grids[i])

    # sample next point outside the buffer zone
    points_to_sample <- grd_dat[sf::st_within(grd_dat, buffer) %>% lengths == 0,]

    if(nrow(points_to_sample) > 0) {
      sample_pt <- points_to_sample %>%
        dplyr::sample_n(size=1)

      # update location points
      location_pts <- dplyr::bind_rows(location_pts, sample_pt)
    }
  }

  return(location_pts)

}
