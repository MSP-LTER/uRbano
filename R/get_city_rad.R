#' @title Get city and create radius in meters
#'
#' @description subsets user input city name from `world.cities` dataset in `maps` and creates a radius around the city center coordinates in specified meters
#'
#' @param city (character) string of city name for selection from `world.cities` dataset in `maps`
#' @param radius (numeric) radius in meters around city center coordinates
#'
#' @return (sfc) sf polygon simple feature collection of 1 feature with WGS 84 CRS
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # provide city name as a string and number of meters for radius drawn from coordinates of city center
#' ## 30km radius polygon for Minneapolis USA
#' uRbano::get_city_rad(city="Minneapolis", radius=30000)
#' }
#function to create a radius around user-specified city center
get_city_rad <- function(city, radius) {
  # Find matching cities
  cty <- subset(maps::world.cities, maps::world.cities$name == city)
  
  # Handle case where no cities are found
  if(nrow(cty) == 0) {
    stop("No cities found matching the criteria")
  }
  
  # Handle case where multiple cities are found
  if(nrow(cty) > 1) {
    cat("Multiple cities found:\n")
    for(i in 1:nrow(cty)) {
      cat(sprintf("%d: %s, %s (Pop: %s)\n", i, cty$name[i], cty$country.etc[i], 
                  format(cty$pop[i], big.mark = ",")))
    }
    
    # Get user selection, they give the number in the list of the city they choose
    selection <- as.numeric(readline(prompt = "Please select a city by number: "))
    
    # Validate selection
    if(is.na(selection) || selection < 1 || selection > nrow(cty)) {
      stop("Invalid selection")
    }
    
    # Use selected city
    cty <- cty[selection, ]
  }
  
  # Convert to sf object and create buffer
  cty_coords <- st_as_sf(cty, coords = c("long", "lat"), crs = 4326)
  cty_crc <- st_buffer(cty_coords, radius)
  
  return(cty_crc)
}