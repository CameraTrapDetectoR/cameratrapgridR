#' Load Layers
#'
#' @description load shapefiles of the area you want to grid
#'
#' @param shape_path
#' @param string
#'
#' @return
#'
#' @import sf
#' @import stringr
#'
#' @export
#'
#' @examples
load_layers <- function(shape_path, string) {

  # list layers in data dir
  layers <- sf::st_layers(shape_path)

  # filter layer names by string of choice
  layer_list <- layers$name[stringr::str_detect(layers$name, regex(as.character(string, ignore.case=T)))]

  if(length(layer_list == 1)){
    loaded <- sf::st_read(path, layer = layer_list)
  }

  return(loaded)
}
