#' @title Get road features from OSM Overpass API for extent of city radius
#' 
#' @description Counts the number of occurrences of each element in both provided vectors and then calculates the difference in that count between the first and second input vector. Counting of NAs in addition to non-NA values is supported.
#' 
#' @param city_radius (sfc) can be any sf object from which an extent bounding box can be drawn
#' @return (LINESTRING sfc) sf object of line features as road segments of OSM key "highway" from within extent of given polygon argument
#' 
#' @importFrom magrittr %>%
#' @importFrom terra ext
#' @export
#' 
#' @examples
#' #result from get_city_radius
#' rad<-uRbano::get_city_radius("Minneapolis",30000)
#' # Count difference in all values between the two
#' uRbano::extract_osm_rds(city_radius=rad)
#' 
#function to get OSM roads from overpass API for the extent extracted from the city radius
extract_osm_rds<-function(city_radius){
  if(st_crs(city_radius)$epsg!=4326){city_radius<-st_transform(city_radius, crs=4326)}
  cty_ex<-terra::ext(city_radius)
  
  #get roads from OSM API 
  rds <- opq(bbox = c (cty_ex[1],cty_ex[3],cty_ex[2],cty_ex[4] )) %>%
    add_osm_feature(key = "highway", value=c("primary", "motorway_link", "unclassified","tertiary","residential",   
                                             "motorway","secondary","service","trunk","living_street", "trunk_link",
                                             "primary_link", "secondary_link","tertiary_link","road")) %>% osmdata_sf()
  wrds<-rds$osm_lines
  return(wrds)
}
