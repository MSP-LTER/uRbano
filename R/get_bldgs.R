#' @title Get building footprints for a region of interest
#'
#' @description takes user input as a string of the region or two regions - for border cities - to extract builing footprint data from Microsoft's ML Building Footprints released under ODbL license: https://minedbuildings.z5.web.core.windows.net. 
#'
#' @param region1 (string) character string of region to look up in online building footprint resealses
#' @param region2 (string) optional second string 
#
#' @return large simple features object of polygons representing building footprints. If region2 is in another country, a list containing two sf objects is returned
#'
#' @export
#'
#' @examples
#' # get building footprints for US state
#' blds<-uRbano::get_bldgs(region1="North Dakota")
#' #get building footprints for two US states
#' blds<-uRbano::get_bldgs(region1="North Dakota", region2="Minnesota")
#' # get building footprints for Canadian province
#' blds<-uRbano::getbldgs(region1="Manitoba")
#' # get buildings for country or other territories
#' blds<-uRbano::get_bldgs(region1="PuertoRico")
#' # two regions from different countries
#' blds<-uRbano::get_bldgs(region1 = "North Dakota", region2="Manitoba")
#' ND<-blds$region1
#' MB<-blds$region2

# if in US use this source, need to add user input to change the state file
get_bldgs<-function(region1, region2=NULL){
  if (region1 == "UnitedStates") {
    stop("You are going to want to use individual US states as inputs, trust me on this one :)")
  }
  if (region1 == "Canada") {
    stop("Use Canadian provinces as inputs, it will be easier on all of us")
  }
  USstates<-gsub("\\b([a-z])", "\\U\\1", c(unique(sub(":.*", "", maps::state.fips$polyname)),"alaska", "hawaii"), perl = TRUE)
  Ca<-c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon")
  all_regions<-read.table("https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv", header = TRUE, sep = ",")
  #use helper function get_blds_by_regions to run multiple times in the event there are two regions
  get_blds_by_regions<-function(region){
    if(region %in% USstates){
      region<-gsub(" ", "", region)
      ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/", region, ".geojson.zip")
      message(paste("getting building footprints from",ft_link))
      infile1 <- tempfile()
      try(download.file(ft_link,infile1,method="curl"))
      if (is.na(file.size(infile1))) download.file(ft_link,infile1,method="auto")
      foots<-st_read(unzip(infile1))
      unlink(infile1)
    }else if(region %in% Ca){
      region<-gsub(" ", "", region)
      ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/canadian-buildings-v2/", region, ".zip")
      message(paste("getting building footprints from",ft_link))
      infile1 <- tempfile()
      try(download.file(ft_link,infile1,method="curl"))
      if (is.na(file.size(infile1))) download.file(ft_link,infile1,method="auto")
      foots<-st_read(unzip(infile1))
      unlink(infile1)
    }else if(region %in% all_regions$Location){
      fts<-all_regions%>%filter(Location==region)
      foot_urls <- list()
      # Loop through the URLs and read each CSV
      for (url in fts$Url) {
        foot_urls[[url]] <- read_csv2(url, col_names = FALSE)
      }
      combined_foots <- do.call(rbind, foot_urls)
      cnty_fts<-geojson_sfc(combined_foots$X1)
      foots<-st_as_sf(cnty_fts, crs=4326)
    }else{warning("cannot find buildings for that region")}
    #ft_link<-paste0("https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/", state, ".geojson.zip")
    
    return(foots)
    
  }
  
  # Download first region
  # try catch to return separate results if rbind fails
  foots1 <- get_blds_by_regions(region1)
  
  # If second region is specified, download and bind
  if (!is.null(region2)) {
    if (region2 == "UnitedStates") {
      warning("You are going to want to use individual US states as inputs, trust me on this one :)")
      return(foots1)
    }
    if (region2 == "Canada") {
      warning("Use Canadian provinces as inputs, it will be easier on all of us")
      return(foots1)
    }
    foots2 <- get_blds_by_regions(region2)
    
    # Bind the two datasets together
    tryCatch({
      foots_combined <- rbind(foots1, foots2)
      message(paste("Combined building footprints from", region1, "and", region2))
      
      return(foots_combined)
    },
    error=function(e){
      # rbind failed, return the inputs separately
      warning(paste("rbind failed:", e$message, "\nReturning as list"))
      return(list(region1 = foots1, region2 = foots2))
    }
    
    )
    
  } else {
    return(foots1)
  }
}
