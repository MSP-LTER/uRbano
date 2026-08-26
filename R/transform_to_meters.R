#' @title transform coordinate reference system to meters datum
#'
#' @description Optional but recommended, transform buildings, roads and city radius features to meters datum with either web mercator EPSG 3857 or look up NAD 83 UTM zone to extract projection from the sf object parameter if spatial features are in North America
#'
#' @param sf_data (sf object) sf object to transform
#' @param method (string) string that represents CRS transformation to perform: "web_mercator" for EPSG 3857, "NAD" for NAD 83
#'
#' @return sf object with modified CRS
#'
#' @export
#'
#' @examples
#' # Create sample point and transform to meters
#' pt <- sf::st_sfc(sf::st_point(c(-93.2650, 44.9778)), crs = 4326)
#' pt_m <- uRbano::transform_to_meters(pt, method = "web_mercator")
#' 
transform_to_meters <- function(sf_data, method = "web_mercator") {
  if (method == "web_mercator") {
    return(sf::st_transform(sf_data, 3857))
    
  } else if (method == "NAD") {
    transform_NAD_utm<-function(rad){
      longitude <- sf::st_coordinates(sf::st_centroid(rad))[, 1]
      if(longitude>=(-126) && longitude<(-120)){return(sf::st_transform(rad, crs=26910))}
      else if(longitude>=(-120) && longitude<(-114)){return(sf::st_transform(rad,crs=26911))}
      else if(longitude>=(-114) && longitude<(-108)){return(sf::st_transform(rad,crs=26912))}
      else if(longitude>=(-108) && longitude<(-102)){return(sf::st_transform(rad,crs=26913))}
      else if(longitude>=(-102) && longitude<(-96)){return(sf::st_transform(rad,crs=26914))}
      else if(longitude>=(-96) && longitude<(-90)){return(sf::st_transform(rad,crs=26915))}
      else if(longitude>=(-90) && longitude<(-84)){return(sf::st_transform(rad,crs=26916))}
      else if(longitude>=(-84) && longitude<(-78)){return(sf::st_transform(rad,crs=26917))}
      else if(longitude>=(-78) && longitude<(-72)){return(sf::st_transform(rad,crs=26918))}
      else if(longitude>=(-72) && longitude<(-66)){return(sf::st_transform(rad,crs=26919))}
      else if(longitude>=(-180) && longitude<(-174)){return(sf::st_transform(rad,crs=26901))}
      else if(longitude>=(-174) && longitude<(-168)){return(sf::st_transform(rad,crs=26902))}
      else if(longitude>=(-168) && longitude<(-162)){return(sf::st_transform(rad,crs=26903))}
      else if(longitude>=(-162) && longitude<(-156)){return(sf::st_transform(rad,crs=26904))}
      else if(longitude>=(-156) && longitude<(-150)){return(sf::st_transform(rad,crs=26905))}
      else if(longitude>=(-150) && longitude<(-144)){return(sf::st_transform(rad,crs=26906))}
      else if(longitude>=(-144) && longitude<(-138)){return(sf::st_transform(rad,crs=26907))}
      else if(longitude>=(-138) && longitude<(-132)){return(sf::st_transform(rad,crs=26908))}
      else if(longitude>=(-132) && longitude<(-126)){return(sf::st_transform(rad,crs=26909))}
      else{warning("Coordinates outside North America UTM Zones")
        return(sf::st_transform(rad, crs = 4326))
      }
    }
    
    return(transform_NAD_utm(sf_data))
  }
}
