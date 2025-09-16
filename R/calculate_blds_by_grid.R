#' @title calculate total building footprint area within each hexagon of the grid
#'
#' @description takes the result of radius_hex_grid and get_bldgs to sum the area of building footprints (not including height info) in meters within each hexagon of the grid
#'
#' @param grid (sf object) output of radius_hex_grid
#' @param rds (sf object) building footprints sf object 
#
#' @return vector of total building area in meters that is the same length as the number of rows in the grid sf object
#'
#' @export
#'
#' @examples
#' # calculate the summed building footprint area per each 1km hexagon in the grid
#' bld_ars<-uRbano::calculate_blds_by_grid(Rgrid, blds)
#' # add column for building area to the hex grid
#' Rgrid<-Rgrid %>% mutate(bld_ars=bld_ars)
#' 
calculate_blds_by_grid<-function(grid, blds){
  if (!inherits(grid, "sf") || !inherits(blds, "sf")) {
    stop("Both grid and roads must be sf objects")
  }
  
  #ensure parameters have same CRS
  if ((crs(grid)) != (crs(blds))) {
    stop("Both grid and roads must be transformed to same UTM zone, use transform_US_utm()")
  }
  
  
  grid<-grid%>%mutate(ID=rownames(grid))
  
  
  bld_clips <-st_intersection(grid, blds)
  
  bld_clips <- bld_clips%>%mutate(area=st_area(bld_clips))
  sumars<-bld_clips%>%group_by(ID)%>%summarise(tarea=sum(area))
  
  bld_jn<-left_join(grid, st_drop_geometry(sumars), by="ID")
  
  
  # Return as a numeric vector
  return(drop_units(bld_jn$tarea))
}
