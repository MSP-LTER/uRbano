#' @title trim building footprints to a smaller extent and remove invalid geometries
#'
#' @description OPTIONAL BUT RECOMMENDED - reduce buildings to extent of a radius and filter out invalid geometries to avoid errors in calculating area
#'
#' @param blds (sfc) large simple features collection of polygons, output of get_bldgs
#' @param rad (polygon) output of get_city_rad or another extent to clip the building footprints to
#
#' @return large simple features object of polygons representing building footprints in invalid geometries omitted and trimmed to the extent of the second parameter, rad
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # fix invalid geometries and clip building footprints to city radius
#' blds<-uRbano::fix_n_trim_bldgs(blds, rad)
#' }


fix_n_trim_bldgs <- function(blds, rad) {
  cty_ex <- terra::ext(rad)
  bbox <- sf::st_bbox(cty_ex, crs = 4326)
  bbox_sf <- sf::st_as_sfc(bbox)

  old_s2 <- sf::sf_use_s2(FALSE)               # turns it off, saves the old value (TRUE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)   # turns it back on when the function exits

  blds <- sf::st_filter(blds, bbox_sf)
  gc()

  blds <- blds[which(sf::st_is_valid(blds)), ]

  return(blds)
}



