#' @title calculate total road length within each hexagon of the grid
#'
#' @description takes the result of radius_hex_grid and extract_osm_rds to sum the length of road segments in meters within each hexagon of the grid
#'
#' @param grid (sf object) output of radius_hex_grid
#' @param rds (sf object) osm roads sf object 
#
#' @return vector of total road length in meters that is the same length as the number of rows in the grid sf object
#'
#' @export
#'
#' @examples
#' # calculate the summed length of road segments per each 1km hexagon in the grid
#' rd_lens<-uRbano::calculate_rds_by_grid(Rgrid, rds)
#' # add column for road length to the hex grid
#' Rgrid<-Rgrid %>% mutate(rd_lens=rd_lens)
#' 
calculate_rds_by_grid <- function(grid, rds) {
  # Ensure parameters are sf objects
  if (!inherits(grid, "sf") || !inherits(rds, "sf")) {
    stop("Both grid and roads must be sf objects")
  }
  
  #ensure parameters have same CRS
  if ((crs(grid)) != (crs(rds))) {
    stop("Both grid and roads must be transformed to same UTM zone, use transform_US_utm()")
  }
  
  grid<-grid%>%mutate(ID=rownames(grid))
  
  
  rd_clips <-st_intersection(grid, rds)
  
  rd_clips <- rd_clips%>%mutate(rd_len=st_length(rd_clips))
  sumlen<-rd_clips%>%group_by(ID)%>%summarise(tlen=sum(rd_len))
  
  rd_jn<-left_join(grid, st_drop_geometry(sumlen), by="ID")
  
  # Return as a numeric vector
  return(drop_units(rd_jn$tlen))
}
