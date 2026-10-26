#' @title Get road features from OpenStreetMap for extent of city radius
#' 
#' @description Extracts road network line features (OSM key "highway") within the extent of a given study polygon.
#' Supports two extraction backends: "osmextract" (recommended, downloads and clips a regional .pbf dump from Geofabrik,
#' avoiding API quotas and timeouts) and "overpass" (queries the live Overpass API directly).
#' 
#' @details
#' When using \code{method = "osmextract"}, regional \code{.osm.pbf} extracts are automatically downloaded from
#' Geofabrik and cached in the persistent directory returned by \code{osmextract::oe_download_directory()}
#' (e.g. within the user's application support directory). Downloads are reused across calls for fast repeat queries.
#' To free up disk space, users can clear this download cache at any time using \code{osmextract::oe_clean()}.
#' 
#' For border cities or study extents intersecting multiple administrative areas, the function automatically
#' computes the minimal set of constituent sub-regions (e.g. states or provinces) to cover the extent without
#' downloading larger parent extracts (e.g. continent-level dumps).
#' 
#' @param city_radius (sfc / sf) Polygon or study extent from which roads will be extracted.
#' @param method (character) Extraction backend: `"osmextract"` (default) or `"overpass"`.
#' @param place (character) Optional region/state/country name(s) for `method = "osmextract"` (e.g., `"Minnesota"`, or a vector like `c("North Dakota", "Minnesota")` for border cities).
#' If `NULL` (default), the regional extract is automatically determined via spatial matching from `city_radius`.
#' @param highway_values (character vector) OSM highway values to extract.
#' @param timeout (numeric) Query timeout in seconds when `method = "overpass"`. Default is 120.
#' @param quiet (logical) Whether to suppress progress messages. Default is `FALSE`.
#' 
#' @return (LINESTRING sf) Simple features collection of line features matching the requested highway tags clipped to the extent of `city_radius`.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Get city radius for Minneapolis
#' rad <- uRbano::get_city_rad("Minneapolis", radius = 30000)
#' 
#' # Extract roads using regional PBF extracts (recommended for large radii)
#' rds <- uRbano::extract_osm_rds(city_radius = rad, method = "osmextract")
#' 
#' # Extract roads for a border city using multiple regional extracts:
#' # rds <- uRbano::extract_osm_rds(city_radius = rad, place = c("North Dakota", "Minnesota"))
#' 
#' # Extract roads using the live Overpass API
#' rds <- uRbano::extract_osm_rds(city_radius = rad, method = "overpass")
#' }
extract_osm_rds <- function(city_radius,
                            method = c("osmextract", "overpass"),
                            place = NULL,
                            highway_values = c("primary", "motorway_link", "unclassified", "tertiary", "residential",   
                                               "motorway", "secondary", "service", "trunk", "living_street", "trunk_link",
                                               "primary_link", "secondary_link", "tertiary_link", "road"),
                            timeout = 120,
                            quiet = FALSE) {
  method <- match.arg(method)
  
  if (sf::st_crs(city_radius) != sf::st_crs(4326)) {
    city_radius <- sf::st_transform(city_radius, crs = 4326)
  }
  
  if (method == "osmextract") {
    extract_roads_osmextract(
      city_radius = city_radius,
      place = place,
      highway_values = highway_values,
      quiet = quiet
    )
  } else if (method == "overpass") {
    extract_roads_overpass(
      city_radius = city_radius,
      highway_values = highway_values,
      timeout = timeout,
      quiet = quiet
    )
  }
}

# -------------------------------------------------------------------------
# Internal Helper Functions (Unexported)
# -------------------------------------------------------------------------

#' Find the minimal set of smallest Geofabrik zones covering a study polygon
#' @noRd
find_geofabrik_leaf_zones <- function(city_radius) {
  gz <- osmextract::geofabrik_zones
  inter_idx <- which(lengths(sf::st_intersects(gz, city_radius)) > 0)
  candidates <- gz[inter_idx, ]
  
  if (nrow(candidates) == 0) {
    return(NULL)
  }
  
  # Sort candidates by file size ascending (prefer smaller sub-regions like states/provinces)
  candidates <- candidates[order(candidates$pbf_file_size), ]
  target_geom <- sf::st_union(sf::st_geometry(city_radius))
  remaining <- target_geom
  orig_area <- sum(as.numeric(sf::st_area(target_geom)))
  
  selected <- character(0)
  for (i in seq_len(nrow(candidates))) {
    cand_geom <- sf::st_geometry(candidates[i, ])
    inter <- suppressWarnings(sf::st_intersection(cand_geom, remaining))
    inter_area <- sum(as.numeric(sf::st_area(inter)))
    
    # Include region if it covers more than 0.1% of the study extent.
    # This safely ignores coordinate-rounding boundary slivers (< 10^-4) while ensuring
    # that genuine border crossings (even narrow slices of adjacent states) are included.
    if (inter_area > 0.001 * orig_area) {
      selected <- c(selected, candidates$name[i])
      remaining <- suppressWarnings(sf::st_difference(remaining, cand_geom))
      rem_area <- sum(as.numeric(sf::st_area(remaining)))
      if (length(remaining) == 0 || rem_area / orig_area < 0.005) {
        break
      }
    }
  }
  
  selected
}

#' Extract roads using regional PBF files via osmextract + GDAL
#' @noRd
extract_roads_osmextract <- function(city_radius, place, highway_values, quiet) {
  if (!requireNamespace("osmextract", quietly = TRUE)) {
    stop("The 'osmextract' package is required for method = 'osmextract'. Install it with install.packages('osmextract').")
  }
  
  if (is.null(place)) {
    leaf_zones <- find_geofabrik_leaf_zones(city_radius)
    target_places <- if (length(leaf_zones) > 0) as.list(leaf_zones) else list(city_radius)
  } else {
    target_places <- as.list(place)
  }
  
  # Prepare boundary geometry and WKT string for GDAL -clipsrc
  boundary_geom <- sf::st_geometry(city_radius)
  if (length(boundary_geom) > 1) {
    boundary_geom <- sf::st_union(boundary_geom)
  }
  if (!sf::st_is_valid(boundary_geom)) {
    boundary_geom <- sf::st_make_valid(boundary_geom)
  }
  boundary_wkt <- sf::st_as_text(boundary_geom[[1]])
  
  query_vals <- paste(sprintf("'%s'", highway_values), collapse = ", ")
  where_clause <- paste0("highway IN (", query_vals, ")")
  
  if (!quiet) {
    message("Extracting roads using osmextract (regional PBF extract)...")
  }
  
  rds_list <- list()
  for (target in target_places) {
    if (!quiet && length(target_places) > 1 && is.character(target)) {
      message(paste("Processing region:", target))
    }
    
    # Use a unique process-local temporary GeoPackage for GDAL vectortranslate.
    # This ensures full thread/process safety during concurrent or parallel execution,
    # avoiding SQLite file lock contention on shared cache files.
    tmp_gpkg <- tempfile(pattern = "urbano_osm_", fileext = ".gpkg")
    
    res <- tryCatch({
      matched <- osmextract::oe_match(place = target, provider = "geofabrik", quiet = quiet)
      pbf_path <- osmextract::oe_download(
        file_url = matched$url,
        provider = "geofabrik",
        download_directory = osmextract::oe_download_directory(),
        file_size = matched$file_size,
        quiet = quiet
      )
      
      # We translate the .osm.pbf layer into an isolated temporary GeoPackage using sf::gdal_utils().
      # Rationale for direct vectortranslate to a tempfile:
      # 1. osmextract::oe_get() writes to a fixed file (<state>.gpkg) in the cache directory;
      #    running two cities or parallel workers collides on SQLite database locks.
      # 2. -clipsrc cuts roads exactly to the study polygon boundary WKT.
      # 3. -where filters the requested highway tags during linear scan to minimize disk I/O.
      # 4. -nlt PROMOTE_TO_MULTI prevents warnings when boundary clipping splits a line into pieces.
      # 5. The temporary .gpkg is deleted in the finally block as soon as sf::st_read() finishes.
      opts <- c(
        "-f", "GPKG",
        "-overwrite",
        "-clipsrc", boundary_wkt,
        "-where", where_clause,
        "-nlt", "PROMOTE_TO_MULTI",
        "-lco", "GEOMETRY_NAME=geometry",
        "lines"
      )
      
      sf::gdal_utils(
        util = "vectortranslate",
        source = normalizePath(pbf_path),
        destination = tmp_gpkg,
        options = opts,
        quiet = quiet
      )
      
      sf::st_read(tmp_gpkg, layer = "lines", quiet = quiet)
    }, error = function(e) {
      target_label <- if (is.character(target)) target else "study extent"
      warning(paste("Failed to extract roads for region:", target_label, "-", e$message))
      NULL
    }, finally = {
      if (file.exists(tmp_gpkg)) {
        unlink(tmp_gpkg)
      }
    })
    
    if (!is.null(res) && nrow(res) > 0) {
      rds_list[[length(rds_list) + 1]] <- res
    }
  }
  
  if (length(rds_list) == 0) {
    warning("No road features returned for the specified extent and highway values. ",
            "Consider manually specifying the region with 'place = ...' (e.g. place = 'Minnesota') ",
            "or setting method = 'overpass'.")
    return(NULL)
  } else if (length(rds_list) == 1) {
    rds <- rds_list[[1]]
  } else {
    rds <- do.call(rbind, rds_list)
    if ("osm_id" %in% names(rds)) {
      rds <- rds[!duplicated(rds$osm_id), ]
    }
  }
  
  return(rds)
}

#' Extract roads using the live Overpass API via osmdata
#' @noRd
extract_roads_overpass <- function(city_radius, highway_values, timeout, quiet) {
  cty_ex <- terra::ext(city_radius)
  
  # Capture the user's existing agent settings
  old_agent <- getOption("HTTPUserAgent")
  
  # uRbano identifier to avoid getting booted from API
  pkg_agent <- "uRbano/1.0.0 (https://github.com/MSP-LTER/uRbano)"
  options(HTTPUserAgent = pkg_agent)
  on.exit(options(HTTPUserAgent = old_agent), add = TRUE)
  
  overpass_servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.private.coffee/api/interpreter"
  )
  
  rds <- NULL
  for (server in overpass_servers) {
    if (!quiet) {
      message(paste("Attempting to query Overpass server:", server))
    }
    
    osmdata::set_overpass_url(server)
    
    tryCatch({
      rds <- osmdata::opq(bbox = c(cty_ex[1], cty_ex[3], cty_ex[2], cty_ex[4]), timeout = timeout) %>%
        osmdata::add_osm_feature(
          key = "highway", 
          value = highway_values
        ) %>% 
        osmdata::osmdata_sf()
      
      if (!is.null(rds$osm_lines) && nrow(rds$osm_lines) > 0) {
        if (!quiet) message("Download successful!")
        break
      }
    }, error = function(e) {
      warning(paste("Server failure encountered on:", server, "-", e$message))
    })
    
    Sys.sleep(1)
  }
  
  if (is.null(rds) || is.null(rds$osm_lines)) {
    stop("All available OpenStreetMap Overpass servers are currently overloaded (HTTP 502/500/504). Try again later or use method = 'osmextract'.")
  }
  
  return(rds$osm_lines)
}

