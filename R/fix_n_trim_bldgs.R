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
#' # fix invalid geometries and clip building footprints to city radius
#' blds<-uRbano::fix_n_trim_bldgs(blds, rad)


fix_n_trim_bldgs<-function(blds, rad){
  
  cty_ex<-terra::ext(rad)
  bbox <- st_bbox(cty_ex, crs = 4326) 
  bbox_sf <- st_as_sfc(bbox)
  sf_use_s2(FALSE) #turn off spherical geometry
  blds<-st_filter(blds, bbox_sf)
  gc()
  
  invalid_geometries <- blds[!st_is_valid(blds), ] 
  if(nrow(invalid_geometries)>0){
    x_blds<-as.numeric(rownames(invalid_geometries))
    blds<-blds[-c(x_blds),]
    sf_use_s2(TRUE)
  }else{
    sf_use_s2(TRUE)
    return(blds)
  }
}



