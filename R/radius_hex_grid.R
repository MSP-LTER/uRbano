#' @title create hex grid of user-specified resolution across city radius
#'
#' @description uses st_make_grid to produce an sf object of a hexagon mesh across the specified city radius. If given an sf object in WGS 84, it will approximate meters given for the width of the hexagons to decimal degrees
#'
#' @param radius (sf object) sf object, usually radius of a city from get_city_rad, but could be another sf object
#' @param hex_size (numeric) number of meters across that each hex cell should be
#
#' @return sf object of hexagon mesh with hexagons on the edge of the radius retained, not clipped 
#'
#' @export
#'
#' @examples
#' # create grid of 1km hexagons across 'rad' polygon
#' Rgrid<-uRbano::radius_hex_grid(rad, 1000)
#' 
#' 
radius_hex_grid<-function(radius, hex_size){
  if(st_crs(radius)$epsg==4326){
    gr <- st_make_grid(radius, square=FALSE, cellsize = (hex_size/111000)) %>% st_sf()
    warning("converting hex size to units of lat/lon, size is approximate conversion to meters")
  }else{gr <- st_make_grid(radius, square=FALSE, cellsize = hex_size) %>% st_sf()}
  #get predicate for where hexes intersect so they don't get cut off at the border
  ig = lengths(st_intersects(gr, radius)) > 0
  gr<-gr%>%mutate(pred=ig)
  # Filter the hex grid to keep only intersecting hexagons
  gr <- gr%>%filter(pred==TRUE)
  return(gr)
}