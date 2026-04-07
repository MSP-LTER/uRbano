# ============================================================
# MODULAR BOOTSTRAP ANALYSIS - ORIGINAL SAMPLING SCENARIOS
# ============================================================

library(tidyverse)
library(sf)
library(furrr)
library(purrr)
library(mgcv)
library(viridis)
library(patchwork)

# 1. Load and prepare your data with the correct name
hex_with_richness <- st_read("/Users/lsantiag/Library/CloudStorage/GoogleDrive-lsantiag@umn.edu/My Drive/Spatial Urban Project/Package uRbano/Extracted data with the package/Cairo30km.shp") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(hex_id = 1:nrow(.)) %>%
  # Add any necessary renaming here if columns don't match
  rename(bldg_ars = bldg_rs)  # Only if you need this rename

# ============================================================
# 1. DATA VALIDATION FUNCTION
# ============================================================

validate_and_prepare_data <- function(hex_data) {
  cat("Validating data...\n")
  
  # Check if required objects exist
  if (!exists("hex_with_richness")) {
    stop("hex_with_richness object not found. Please run your data preparation code first.")
  }
  
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
  
  return(clean_data)
}

# ============================================================
# 2. SAMPLING FUNCTIONS - EXACTLY AS IN YOUR ORIGINAL CODE
# ============================================================

create_sampling_functions <- function() {
  
  # Cardinal transects
  cardinal_transects <- function(hex_data, center_point, sample_size) {
    angles <- c(0, pi/2, pi, 3*pi/2)
    transect_lines <- map(angles, ~{
      end_coords <- st_coordinates(center_point) + c(cos(.x), sin(.x)) * 100000
      st_linestring(rbind(st_coordinates(center_point), end_coords))
    })
    transects_sf <- st_sf(geometry = st_sfc(transect_lines, crs = st_crs(hex_data)))
    transect_cells <- st_intersection(hex_data, transects_sf)
    
    if (nrow(transect_cells) > 0) {
      if (nrow(transect_cells) >= sample_size) {
        return(sample_n(transect_cells, sample_size))
      }
      return(transect_cells)
    }
    return(NULL)
  }
  
  # Random transect
  random_transect <- function(hex_data, center_point, sample_size) {
    angle <- runif(1, 0, 2 * pi)
    end_coords <- st_coordinates(center_point) + c(cos(angle), sin(angle)) * 100000
    line_matrix <- rbind(st_coordinates(center_point), end_coords)
    line <- st_linestring(line_matrix)
    transect_sf <- st_sf(geometry = st_sfc(line, crs = st_crs(hex_data)))
    intersected <- st_intersection(hex_data, transect_sf)
    
    if (nrow(intersected) > 0) {
      if (nrow(intersected) >= sample_size) {
        return(sample_n(intersected, sample_size))
      }
      return(intersected)
    }
    return(NULL)
  }
  
  # Simple random sampling
  simple_random <- function(hex_data, center_point, sample_size) {
    sample_n(hex_data, sample_size)
  }
  
  # Concentric rings
  concentric_rings <- function(hex_data, center_point, sample_size) {
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
  density_stratified <- function(hex_data, center_point, sample_size) {
    density_levels <- unique(hex_data$dens_dx)
    samples_per_level <- ceiling(sample_size / length(density_levels))
    stratified_list <- map(density_levels, ~{
      subset <- filter(hex_data, dens_dx == .x)
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
  
  return(list(
    cardinal_transects = cardinal_transects,
    concentric_rings = concentric_rings,
    density_stratified = density_stratified,
    simple_random = simple_random,
    random_transect = random_transect
  ))
}

# ============================================================
# 3. SINGLE BOOTSTRAP ITERATION FUNCTION
# ============================================================

run_single_bootstrap <- function(iter, scenario_id, sample_size, 
                                 hex_data, center_point, population_stats) {
  
  set.seed(iter * 100 + scenario_id * 10 + sample_size)
  
  sampling_functions <- create_sampling_functions()
  scenario_names <- names(sampling_functions)
  scenario_name <- scenario_names[scenario_id]
  scenario_func <- sampling_functions[[scenario_name]]
  
  tryCatch({
    sampled_cells <- scenario_func(hex_data, center_point, sample_size)
    
    if (is.null(sampled_cells) || nrow(sampled_cells) == 0) {
      return(NULL)
    }
    
    # Calculate statistics - removed rarefied_richness
    stats <- sampled_cells %>%
      st_drop_geometry() %>%
      summarise(
        mean_rd_len = mean(rd_len, na.rm = TRUE),
        sd_rd_len = sd(rd_len, na.rm = TRUE),
        mean_bldg_ars = mean(bldg_ars, na.rm = TRUE),
        sd_bldg_ars = sd(bldg_ars, na.rm = TRUE),
        n_cells = n()
      ) %>%
      mutate(
        iteration = iter,
        scenario = scenario_name,
        sample_size = sample_size,
        
        # Performance metrics - removed rarefied_richness
        bias_rd_len = mean_rd_len - population_stats$mean_rd_len,
        bias_bldg_ars = mean_bldg_ars - population_stats$mean_bldg_ars,
        
        rel_bias_rd_len = 100 * bias_rd_len / population_stats$mean_rd_len,
        rel_bias_bldg_ars = 100 * bias_bldg_ars / population_stats$mean_bldg_ars,
        
        cv_rd_len = sd_rd_len / mean_rd_len,
        cv_bldg_ars = sd_bldg_ars / mean_bldg_ars,
        
        sq_error_rd_len = bias_rd_len^2,
        sq_error_bldg_ars = bias_bldg_ars^2
      )
    
    return(stats)
    
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================
# 4. MAIN BOOTSTRAP FUNCTION
# ============================================================

run_bootstrap_for_sample_size <- function(sample_size, n_bootstrap, 
                                          hex_data, center_point, population_stats) {
  cat("Sample size:", sample_size, "\n")
  
  # Create all combinations
  all_combinations <- expand.grid(
    iteration = 1:n_bootstrap,
    scenario_id = 1:5
  )
  
  # Run bootstrap for this sample size
  results <- future_pmap_dfr(
    list(
      all_combinations$iteration,
      all_combinations$scenario_id,
      rep(sample_size, nrow(all_combinations))
    ),
    run_single_bootstrap,
    hex_data = hex_data,
    center_point = center_point,
    population_stats = population_stats,
    .options = furrr_options(seed = TRUE)
  )
  
  return(results)
}

# ============================================================
# 5. PERFORMANCE METRICS CALCULATION
# ============================================================

calculate_performance_metrics <- function(bootstrap_results) {
  cat("Calculating performance metrics...\n")
  
  performance_metrics <- bootstrap_results %>%
    group_by(scenario, sample_size) %>%
    summarise(
      # Road Length Metrics
      rd_len_cv_mean = mean(cv_rd_len, na.rm = TRUE),
      rd_len_cv_sd = sd(cv_rd_len, na.rm = TRUE),
      rd_len_rmse = sqrt(mean(sq_error_rd_len, na.rm = TRUE)),
      rd_len_rel_bias_mean = mean(rel_bias_rd_len, na.rm = TRUE),
      rd_len_rel_bias_sd = sd(rel_bias_rd_len, na.rm = TRUE),
      
      # Building Area Metrics
      bldg_ars_cv_mean = mean(cv_bldg_ars, na.rm = TRUE),
      bldg_ars_cv_sd = sd(cv_bldg_ars, na.rm = TRUE),
      bldg_ars_rmse = sqrt(mean(sq_error_bldg_ars, na.rm = TRUE)),
      bldg_ars_rel_bias_mean = mean(rel_bias_bldg_ars, na.rm = TRUE),
      bldg_ars_rel_bias_sd = sd(rel_bias_bldg_ars, na.rm = TRUE),
      
      n_iterations = n(),
      .groups = "drop"
    )
  
  return(performance_metrics)
}

# ============================================================
# 6. VISUALIZATION FUNCTIONS
# ============================================================

create_performance_plots <- function(performance_metrics) {
  cat("Creating performance plots...\n")
  
  scenario_colors <- c(
    "cardinal_transects" = "#BB3333",
    "concentric_rings" = "#F26357", 
    "density_stratified" = "#F5A994",
    "simple_random" = "#66A7D4",
    "random_transect" = "#355B8D"
  )
  
  # Function to create plot for one variable
  create_variable_plot <- function(metric_data, variable_name, variable_label) {
    
    plot_data <- metric_data %>%
      dplyr::select(scenario, sample_size,
                    cv_mean = paste0(variable_name, "_cv_mean"),
                    cv_sd = paste0(variable_name, "_cv_sd"),
                    rmse = paste0(variable_name, "_rmse"),
                    rel_bias_mean = paste0(variable_name, "_rel_bias_mean"),
                    rel_bias_sd = paste0(variable_name, "_rel_bias_sd")) %>%
      pivot_longer(
        cols = c(cv_mean, rmse, rel_bias_mean),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(metric,
                        levels = c("cv_mean", "rmse", "rel_bias_mean"),
                        labels = c("Coefficient of Variation", "RMSE", "Relative Bias (%)"))
      )
    
    ggplot(plot_data, aes(x = sample_size, y = value, color = scenario)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      geom_hline(data = data.frame(metric = "Relative Bias (%)", y = 0),
                 aes(yintercept = y), linetype = "dashed", color = "red") +
      facet_wrap(~metric, scales = "free_y", ncol = 1) +
      scale_color_manual(values = scenario_colors) +
      labs(
        title = paste("Performance Metrics:", variable_label),
        x = "Sample Size",
        y = "Metric Value",
        color = "Sampling Scenario"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Create plots for each variable - removed richness
  road_plot <- create_variable_plot(performance_metrics, "rd_len", "Road Length")
  building_plot <- create_variable_plot(performance_metrics, "bldg_ars", "Building Area")
  
  return(list(
    road_length = road_plot,
    building_area = building_plot
  ))
}

# ============================================================
# 7. MAIN EXECUTION FUNCTION
# ============================================================

run_complete_analysis <- function(n_bootstrap = 50, sample_sizes = seq(3, 15, by = 3)) {
  cat("=== STARTING COMPLETE BOOTSTRAP ANALYSIS ===\n")
  
  # Step 1: Validate and prepare data
  hex_data <- validate_and_prepare_data(hex_with_richness)
  
  # Step 2: Calculate population statistics - removed rarefied_richness
  population_stats <- list(
    mean_rd_len = mean(hex_data$rd_len, na.rm = TRUE),
    mean_bldg_ars = mean(hex_data$bldg_ars, na.rm = TRUE)
  )
  
  cat("Population means:\n")
  cat("  Road length:", round(population_stats$mean_rd_len, 2), "\n")
  cat("  Building area:", round(population_stats$mean_bldg_ars, 2), "\n")
  
  # Step 3: Calculate center point
  center_point <- st_centroid(st_union(hex_data))
  
  # Step 4: Set up parallel processing
  plan(multisession, workers = availableCores() - 1)
  
  # Step 5: Run bootstrap analysis
  cat("Running bootstrap analysis...\n")
  bootstrap_results <- map_dfr(
    sample_sizes,
    ~ run_bootstrap_for_sample_size(
      sample_size = .x,
      n_bootstrap = n_bootstrap,
      hex_data = hex_data,
      center_point = center_point,
      population_stats = population_stats
    )
  )
  
  # Clean up parallel workers
  plan(sequential)
  
  cat("Bootstrap completed. Total results:", nrow(bootstrap_results), "\n")
  
  # Step 6: Calculate performance metrics
  performance_metrics <- calculate_performance_metrics(bootstrap_results)
  
  # Step 7: Create plots - removed richness
  plots <- create_performance_plots(performance_metrics)
  
  # Step 8: Save results
  write.csv(bootstrap_results, "bootstrap_results_detailed.csv", row.names = FALSE)
  write.csv(performance_metrics, "performance_metrics_summary.csv", row.names = FALSE)
  
  # Save plots - removed richness plot
  ggsave("performance_road_length.png", plots$road_length, width = 10, height = 8, dpi = 300)
  ggsave("performance_building_area.png", plots$building_area, width = 10, height = 8, dpi = 300)
  
  cat("=== ANALYSIS COMPLETED ===\n")
  cat("Files saved:\n")
  cat("  - bootstrap_results_detailed.csv\n")
  cat("  - performance_metrics_summary.csv\n")
  cat("  - performance_*.png (2 plot files)\n")
  
  return(list(
    bootstrap_results = bootstrap_results,
    performance_metrics = performance_metrics,
    plots = plots,
    population_stats = population_stats
  ))
}

library(dplyr)

hex_with_richness <- hex_with_richness %>%
  rename(bldg_ars = bldg_rs)

head(hex_with_richness)

# ============================================================
# RUN THE ANALYSIS
# ============================================================

# First, run your original data preparation code to create hex_with_richness
# Then run this:

# Diagnostic check
if (exists("hex_with_richness")) {
  cat("✓ hex_with_richness exists\n")
  cat("Dimensions:", dim(hex_with_richness), "\n")
  
  # Check required columns - removed rarefied_richness
  required_cols <- c("rd_len", "bldg_ars", "dns_dx_")
  missing_cols <- required_cols[!required_cols %in% names(hex_with_richness)]
  
  if (length(missing_cols) > 0) {
    cat("✗ Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  } else {
    cat("✓ All required columns present\n")
    
    # Run the analysis with your original sampling scenarios
    results <- run_complete_analysis(
      n_bootstrap = 100, 
      sample_sizes = seq(5, 105, by = 10)
    )
    
    # Display one of the plots
    print(results$plots$road_length)
  }
} else {
  cat("✗ hex_with_richness not found - run your data prep code first\n")
}

print(results$plots$road_length)|print(results$plots$building_area)

####Figures made with GAM####
# ============================================================
# SIMPLIFIED GAM VISUALIZATION USING geom_smooth()
# ============================================================

create_gam_performance_plots <- function(bootstrap_results, performance_metrics) {
  cat("Creating GAM regression plots using geom_smooth()...\n")
  
  scenario_colors <- c(
    "cardinal_transects" = "#BB3333",
    "simple_random" = "#F26357", 
    "random_transect" = "#F5A994",
    "density_stratified" = "#66A7D4",
    "concentric_rings" = "#355B8D"
  )
  
  # 1. GAM REGRESSION PLOTS FOR BOOTSTRAP ITERATIONS
  create_gam_variable_plot <- function(bootstrap_data, variable_name, variable_label) {
    
    # Prepare data for the specific variable
    plot_data <- bootstrap_data %>%
      dplyr::select(scenario, sample_size, iteration,
                    cv = paste0("cv_", variable_name),
                    rel_bias = paste0("rel_bias_", variable_name),
                    sq_error = paste0("sq_error_", variable_name)) %>%
      # Calculate RMSE from squared error
      group_by(scenario, sample_size) %>%
      mutate(
        rmse = sqrt(mean(sq_error, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      dplyr::select(-sq_error) %>%
      pivot_longer(
        cols = c(cv, rmse, rel_bias),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(metric,
                        levels = c("cv", "rmse", "rel_bias"),
                        labels = c("Coefficient of Variation", "RMSE", "Relative Bias (%)"))
      )
    
    # Create the plot using geom_smooth()
    p <- ggplot(plot_data, aes(x = sample_size, y = value, color = scenario, fill = scenario)) +
      # Raw bootstrap points with transparency
      geom_point(alpha = 0.1, size = 0.8) +
      # GAM smooth with confidence intervals
      geom_smooth(method = "loess", se=T,alpha=0.3)+
      # Reference line for bias
      geom_hline(data = data.frame(metric = "Relative Bias (%)", y = 0), 
                 aes(yintercept = y), linetype = "dashed", color = "red", size = 0.8) +
      facet_grid(metric ~ scenario, scales = "free_y") +
      scale_color_manual(values = scenario_colors) +
      scale_fill_manual(values = scenario_colors) +
      labs(
        title = paste("Bootstrap Performance GAM Trends:", variable_label),
        subtitle = "GAM trends with 95% confidence intervals\nPoints show individual bootstrap iterations",
        x = "Sample Size", 
        y = "Performance Metric Value",
        color = "Sampling Design", 
        fill = "Sampling Design"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_rect(fill = "gray90", color = "gray80"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines")
      )
    
    return(p)
  }
  
  # 2. SD OF CV PLOTS USING geom_smooth()
  create_cv_sd_plot <- function(performance_data, variable_name, variable_label) {
    
    plot_data <- performance_data %>%
      dplyr::select(scenario, sample_size, cv_sd = paste0(variable_name, "_cv_sd"))
    
    p <- ggplot(plot_data, aes(x = sample_size, y = cv_sd, color = scenario, fill = scenario)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, k = 4),
                  se = TRUE,
                  alpha = 0.2) +
      scale_color_manual(values = scenario_colors) +
      scale_fill_manual(values = scenario_colors) +
      labs(
        title = paste("Standard Deviation of CV:", variable_label),
        subtitle = "GAM trends with 95% confidence intervals",
        x = "Sample Size", 
        y = "Standard Deviation of CV",
        color = "Sampling Design", 
        fill = "Sampling Design"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  # 3. CREATE ALL PLOTS
  cat("Creating GAM plots for road length...\n")
  road_gam_plot <- create_gam_variable_plot(bootstrap_results, "rd_len", "Road Length")
  road_cv_sd_plot <- create_cv_sd_plot(performance_metrics, "rd_len", "Road Length")
  
  cat("Creating GAM plots for building area...\n")
  building_gam_plot <- create_gam_variable_plot(bootstrap_results, "bldg_ars", "Building Area")
  building_cv_sd_plot <- create_cv_sd_plot(performance_metrics, "bldg_ars", "Building Area")
  
  # 4. COMBINED SD OF CV PLOT USING geom_smooth()
  combined_cv_sd_data <- performance_metrics %>%
    dplyr::select(scenario, sample_size,
                  road_cv_sd = rd_len_cv_sd,
                  building_cv_sd = bldg_ars_cv_sd) %>%
    pivot_longer(
      cols = c(road_cv_sd, building_cv_sd),
      names_to = "variable", 
      values_to = "cv_sd"
    ) %>%
    mutate(
      variable = factor(variable,
                        levels = c( "building_cv_sd","road_cv_sd"),
                        labels = c( "Building Area","Road Length"))
    )
  
  combined_cv_sd_plot <- ggplot(combined_cv_sd_data, 
                                aes(x = sample_size, y = cv_sd, color = scenario, fill = scenario)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "loess", alpha=0.4)+
    facet_wrap(~variable,  nrow = 1) +
    scale_color_manual(values = scenario_colors) +
    scale_fill_manual(values = scenario_colors) +
    labs(
      title = "Standard Deviation of Coefficient of Variation - All Variables",
      subtitle = "GAM trends with 95% confidence intervals",
      x = "Sample Size", 
      y = "Standard Deviation of CV",
      color = "Sampling Design", 
      fill = "Sampling Design"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Return all plots
  return(list(
    road_gam = road_gam_plot,
    building_gam = building_gam_plot,
    road_cv_sd = road_cv_sd_plot,
    building_cv_sd = building_cv_sd_plot,
    combined_cv_sd = combined_cv_sd_plot
  ))
}

# Run the simplified function
gam_plots <- create_gam_performance_plots(
  bootstrap_results = results$bootstrap_results,
  performance_metrics = results$performance_metrics
)

# View the plots
print(gam_plots$road_gam)
print(gam_plots$building_gam)
print(gam_plots$combined_cv_sd)

gam_plots$building_gam / gam_plots$road_gam