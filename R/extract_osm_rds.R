#' @title Get road features from OSM Overpass API for extent of city radius
#' 
#' @description Counts the number of occurrences of each element in both provided vectors and then calculates the difference in that count between the first and second input vector. Counting of NAs in addition to non-NA values is supported.
#' 
#' @param city_radius (sfc) can be any sf object from which an extent bounding box can be drawn
#' @return (LINESTRING sfc) sf object of line features as road segments of OSM key "highway" from within extent of given polygon argument
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' #result from get_city_rad
#' rad<-uRbano::get_city_rad("Minneapolis",30000)
#' # Count difference in all values between the two
#' uRbano::extract_osm_rds(city_radius=rad)
#' }
#function to get OSM roads from overpass API for the extent extracted from the city radius
extract_osm_rds<-function(city_radius){
  if(st_crs(city_radius)$epsg!=4326){city_radius<-st_transform(city_radius, crs=4326)}
  cty_ex<-terra::ext(city_radius)
  
  # Capture the user's existing agent settings
  old_agent <- getOption("HTTPUserAgent")
  
  # uRbano identifier to avoid getting booted from API
  pkg_agent <- "uRbano/1.0.0 (https://github.com/MSP-LTER/uRbano)"
  options(HTTPUserAgent = pkg_agent)
  
  # restore the user's original global setting when the function finishes running
  on.exit(options(HTTPUserAgent = old_agent), add = TRUE)
  
  # different server endpoint list to try if one fails
  overpass_servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://api.openstreetmap.fr/oapi/interpreter"
  )
  
  rds <- NULL
  
  # 4. Loop across servers with structured fallback rules
  for (server in overpass_servers) {
    message(paste("Attempting to query Overpass server:", server))
    
    # Safely apply valid string structure
    osmdata::set_overpass_url(server)
    
    tryCatch({
      rds <- osmdata::opq(bbox = c(cty_ex[1], cty_ex[3], cty_ex[2], cty_ex[4]), timeout = 120) %>%
        osmdata::add_osm_feature(
          key = "highway", 
          value = c("primary", "motorway_link", "unclassified", "tertiary", "residential",   
                    "motorway", "secondary", "service", "trunk", "living_street", "trunk_link",
                    "primary_link", "secondary_link", "tertiary_link", "road")
        ) %>% 
        osmdata::osmdata_sf()
      
      if (!is.null(rds$osm_lines)) {
        message("Download successful!")
        break
      }
    }, error = function(e) {
      warning(paste("Server failure encountered on:", server, "-", e$message))
    })
    
    Sys.sleep(1) # Grace interval to manage API limits politely
  }
  
  # 5. Fallback warning system for down servers
  if (is.null(rds) || is.null(rds$osm_lines)) {
    stop("All available OpenStreetMap Overpass servers are currently overloaded (HTTP 502/500). Please try again later.")
  }
  
  
  wrds<-rds$osm_lines
  return(wrds)
}

