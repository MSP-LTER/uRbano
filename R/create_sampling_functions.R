#' @title create sampling functions
#'
#' @description generates a set of helper functions that construct different sampling scenarios of the hex grid: sample types: cardinal_transects, concentric_rings, density_stratified, simple_random, or random_transect
#'
#' @param hex_data (sf object) output hex built density grid with a density index (dns_dx_) column that shows numeric levels of road and building density
#' @param center_point (sf object) single point sf object that defaults to the centroid of the union of the grid radius
#' @param sample_size (number) number of samples to generate, defaults to 10
#' @param sample_type can specify 5 type of sampling scenarios, defaults to a list with all 5
#
#' @return line or polygon sf object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sampling_scenarios<-create_sampling_functions(grid)
#' # plot grid cells selected by concentric rings sampling
#' plot(sampling_scenarios[[2]])
#' }
# SAMPLING FUNCTIONS
create_sampling_functions <- function(hex_data, center_point=st_centroid(st_union(st_transform(hex_data, crs=4326))), sample_size=20, sample_type=NULL) {
  hex_data<-st_transform(hex_data, crs=4326)
  cat("Validating data...\n")
  # Check required columns - removed rarefied_richness
  required_columns <- c("rd_len", "bldg_ars", "dns_dx_")
  missing_columns <- required_columns[!required_columns %in% names(hex_data)]
  
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Remove NA values - removed rarefied_richness
  clean_data <- hex_data %>%
    filter(!is.na(rd_len), !is.na(bldg_ars), !is.na(dns_dx_))
  
  # Check if we have enough data
  if (nrow(clean_data) < 10) {
    stop("Not enough complete cases (minimum 10 required)")
  }
  
  cat("Data validation passed. Clean dataset has", nrow(clean_data), "grids\n")
  
  hex_data<-clean_data
  # Cardinal transects
  cardinal_transects <- function() {
    
    angles <- c(0, pi/2, pi, 3*pi/2)
    transect_lines <- map(angles, ~{
      end_coords <- st_coordinates(center_point) + c(cos(.x), sin(.x)) * 100000
      st_linestring(rbind(st_coordinates(center_point), end_coords))
    })
    transects_sf <- st_sf(geometry = st_sfc(transect_lines, crs = st_crs(hex_data)))
    transect_cells <- hex_data%>%st_filter(transects_sf)
    
    if (nrow(transect_cells) > 0) {
      if (nrow(transect_cells) >= sample_size) {
        return(sample_n(transect_cells, sample_size))
      }
      return(transect_cells)
    }
    return(NULL)
  }
  
  # Random transect
  random_transect <- function() {
    
    angle <- runif(1, 0, 2 * pi)
    end_coords <- st_coordinates(center_point) + c(cos(angle), sin(angle)) * 100000
    line_matrix <- rbind(st_coordinates(center_point), end_coords)
    line <- st_linestring(line_matrix)
    transect_sf <- st_sf(geometry = st_sfc(line, crs = st_crs(hex_data)))
    intersected <- hex_data%>%st_filter(transect_sf)
    
    if (nrow(intersected) > 0) {
      if (nrow(intersected) >= sample_size) {
        return(sample_n(intersected, sample_size))
      }
      return(intersected)
    }
    return(NULL)
  }
  
  # Simple random sampling
  simple_random <- function() {
    sample_n(hex_data, sample_size)
  }
  
  # Concentric rings
  concentric_rings <- function() {
    radii <- c(10000, 20000, 30000, 40000)
    samples_per_ring <- 5
    sampled_cells_list <- list()
    
    for (j in seq_along(radii)) {
      ring_geom <- if (j == 1) {
        st_buffer(center_point, radii[j])
      } else {
        st_difference(
          st_buffer(center_point, radii[j]),
          st_buffer(center_point, radii[j-1])
        )
      }
      
      ring_sf <- st_sf(geometry = st_sfc(ring_geom), crs = st_crs(hex_data))
      cells_in_ring <- st_intersection(hex_data, ring_sf)
      
      if (nrow(cells_in_ring) > 0) {
        n_to_sample <- min(nrow(cells_in_ring), samples_per_ring)
        sampled <- sample_n(cells_in_ring, n_to_sample)
        
        sampled$ring_id <- j
        sampled$inner_radius <- ifelse(j == 1, 0, radii[j-1])
        sampled$outer_radius <- radii[j]
        sampled_cells_list[[j]] <- sampled
      }
    }
    
    sampled_cells <- bind_rows(sampled_cells_list)
    if (nrow(sampled_cells) > sample_size) {
      sample_n(sampled_cells, sample_size)
    } else {
      sampled_cells
    }
  }
  
  # Density stratified sampling
  density_stratified <- function() {
    density_levels <- unique(hex_data$dns_dx_)
    samples_per_level <- ceiling(sample_size / length(density_levels))
    stratified_list <- purrr::map(density_levels, ~{
      subset <- filter(hex_data, dns_dx_ == .x)
      if (nrow(subset) > 0) {
        n_to_sample <- min(nrow(subset), samples_per_level)
        sample_n(subset, n_to_sample)
      }
    })
    
    sampled_cells <- bind_rows(stratified_list)
    if (nrow(sampled_cells) > sample_size) {
      sample_n(sampled_cells, sample_size)
    } else {
      sampled_cells
    }
  }
  if(is.null(sample_type)){
    warning("returning list of all sample types: cardinal_transects,concentric_rings,density_stratified,
  simple_random, or random_transect")
    return(list(
      cardinal_transects = cardinal_transects(),
      concentric_rings = concentric_rings(),
      density_stratified = density_stratified(),
      simple_random = simple_random(),
      random_transect = random_transect()
    ))
  }else if(sample_type=="cardinal_transects"){
    cardinal_transects()
  }else if(sample_type=="concentric_rings"){
    concentric_rings()
  }else if(sample_type=="density_stratified"){
    density_stratified()
  }else if(sample_type=="simple_random"){
    simple_random()
  }else if(sample_type=="random_transect"){
    random_transect()
  }else{
    warning("choose from sample_type cardinal_transects, concentric_rings, density_stratified,
  simple_random, or random_transect")
    
  }
  
  
}
