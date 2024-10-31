#' Install and Load Required Packages
#'
#' This function checks if each package in the provided list is installed. If a package is not installed, it installs the package and its dependencies. Then, it loads the package into the R session.
#'
#' @param packages A character vector of package names to be installed and loaded.
#' @return None. The function is used for its side effects of installing and loading packages.
#' @importFrom utils install.packages
#' @export
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
      }, error = function(e) {
        cat("Error installing package:", pkg, "\n", e$message, "\n")
      })
    }
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      cat("Error loading package:", pkg, "\n", e$message, "\n")
    })
  }
}

# List of required packages
required_packages <- c("RSAGA", "terra", "dplyr", "stringr", "future", "furrr")

# Install and load the packages
#install_and_load(required_packages)

#' Set Up the SAGA Environment
#'
#' Initializes the SAGA GIS environment with specified settings for parallel processing.
#'
#' @param cores Number of cores to use for processing. Default is 1.
#' @param parallel Logical indicating whether to enable parallel processing. Default is FALSE.
#' @importFrom RSAGA rsaga.env
#' @return A list representing the SAGA environment settings.
#' @export
setup_saga <- function(cores = 1, parallel = FALSE) {
  env <- rsaga.env()
  env$cores <- cores
  env$parallel <- parallel
  return(env)
}

#' Convert SAGA Files to TIFF
#'
#' Converts .sdat files in a specified directory to .tif format using the terra package.
#'
#' @param dir_path The directory path containing .sdat files.
#' @param outdir The output directory where .tif files will be saved.
#' @param prefix A prefix to add to the output file names.
#' @return None. The function is used for its side effects of converting files.
#' @importFrom terra rast
#' @importFrom terra writeRaster
#' @export
write_saga_to_tiff <- function(dir_path, outdir, prefix) {
  # List all .sdat files in the directory
  sdat_files <- list.files(path = dir_path, pattern = "\\.sdat$", full.names = TRUE, ignore.case = TRUE)

  # Check if there are any .sdat files to process
  if (length(sdat_files) == 0) {
    cat("No .sdat files found in the directory.\n")
    return()
  }

  # Process each .sdat file
  for (sdat in sdat_files) {
    # Load the raster from the SAGA file
    raster <- tryCatch({
      terra::rast(sdat)
    }, error = function(e) {
      cat("Error loading raster from file:", sdat, "\n", e$message, "\n")
      return(NULL)
    })

    if (is.null(raster)) next  # Skip to the next file if there was an error

    # Define the output .tif file name in the specified output directory
    file_name <- basename(sdat)
    file_name_no_ext <- sub("\\.sdat$", "", file_name)
    tiff_file <- file.path(outdir, paste0(prefix, file_name_no_ext, ".tif"))

    # Try to write the raster to a TIFF file with compression
    tryCatch({
      terra::writeRaster(raster, tiff_file, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
      cat("Converted ", sdat, " to ", tiff_file, "\n")
    }, error = function(e) {
      cat("Error occurred while processing file: ", sdat, "\n")
      cat("Error message: ", e$message, "\n")
    })
  }
}

#' Calculate Hillshade
#'
#' Computes hillshade from a Digital Elevation Model (DEM) using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the hillshade file.
#' @param azimuth The azimuth angle for the light source. Default is 315.
#' @param declination The declination angle for the light source. Default is 45.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating hillshade.
#' @export
calculate_hillshade <- function(dem, outdir, prefix, azimuth = 315, declination = 45, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "hillshade.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_lighting", module = 0,
                       list(ELEVATION = dem, SHADE = output_file, METHOD = 0, POSITION = 0,
                            AZIMUTH = azimuth, DECLINATION = declination, DATE = "2018-10-12", TIME = 12,
                            EXAGGERATION = 1, UNIT = 0, SHADOW = 0, NDIRS = 8, RADIUS = 10),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for hillshade:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, prefix)
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Analytical Hillshading Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Convergence Index
#'
#' Computes the convergence index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the convergence index file.
#' @param method The method to use for calculation. Default is "Aspect".
#' @param neighbours The number of neighbours to consider. Default is 1.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @importFrom furrr future_map2
#' @return None. The function is used for its side effects of generating the convergence index.
#' @export
calculate_convergence_index <- function(dem, outdir, prefix, method = "Aspect", neighbours = 1, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "convergence_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 1,
                       list(ELEVATION = dem, RESULT = output_file, METHOD = method, NEIGHBOURS = neighbours),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for convergence index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, prefix)
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Convergence Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Curvature Classification
#'
#' Computes curvature classification from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the curvature classification file.
#' @param threshold The threshold for curvature classification. Default is 0.05.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @importFrom furrr future_map2
#' @return None. The function is used for its side effects of generating curvature classification.
#' @export
calculate_curvature_classification <- function(dem, outdir, prefix, threshold = 0.05, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "curvature_classification.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 4,
                       list(DEM = dem, CLASSES = output_file, STRAIGHT = threshold),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for curvature classification:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir,prefix)
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  tt = round(difftime(end_time, start_time, units = "secs"), 2)
  cat('tempo', tt, '\n')
  if (verbose) cat("Curvature Classification Finished in", tt, "seconds\n")
}

#' Calculate Solar Radiation
#'
#' Computes potential incoming solar radiation from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the solar radiation file.
#' @param latitude The latitude for the location.
#' @param start_date The start date for the calculation period. Default is "01-01-2018".
#' @param end_date The end date for the calculation period. Default is "12-31-2018".
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @importFrom furrr future_map2
#' @return None. The function is used for its side effects of generating solar radiation data.
#' @export
calculate_solar_radiation <- function(dem, outdir, prefix, latitude, start_date = "01-01-2018", end_date = "12-31-2018", verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file_total <- file.path(temp_dir, "solar_total.sgrd")

  mde_rst <- dem
  mde_rst <- terra::project(dem, y = "EPSG:4326", method = "near")

  ext <- terra::ext(mde_rst) %>% as.vector()
  latitude <- as.numeric((ext[4] - ext[3]) / 2 + ext[3])

  tmp_inname <- sprintf("%s.tif", tempfile())
  terra::writeRaster(dem, tmp_inname,  overwrite = TRUE, gdal = c("COMPRESS=LZW"))

  r_base <- dem
  dem <- tmp_inname

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_lighting", module = 2,
                       list(GRD_DEM = dem, GRD_VAPOUR_DEFAULT = 10, GRD_LINKE_DEFAULT = 3,
                            SOLARCONST = 1367, LOCALSVF = 1, UNITS = 0, SHADOW = 0, LOCATION = 0,
                            LATITUDE = latitude, PERIOD = 2, DAY = start_date, DAY_STOP = end_date,
                            DAYS_STEP = 1, MOMENT = 12, HOUR_STEP = 0.5, METHOD = 2, ATMOSPHERE = 12000,
                            PRESSURE = 1013, WATER = 1.68, DUST = 100, LUMPED = 70,
                            GRD_TOTAL = output_file_total),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for solar radiation:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "solar_radiation_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Potential Incoming Solar Radiation Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Diurnal Anisotropic Heat
#'
#' Computes diurnal anisotropic heat from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the diurnal anisotropic heat file.
#' @param alpha_max The maximum alpha value for the calculation. Default is 202.5.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating diurnal anisotropic heat data.
#' @export
calculate_diurnal_anisotropic_heat <- function(dem, outdir, alpha_max = 202.5, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "diurnal_anisotropic_heat.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 12,
                       list(DEM = dem, DAH = output_file, ALPHA_MAX = alpha_max),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for diurnal anisotropic heat:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "diurnal_anisotropic_heat_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  tt =  round(difftime(end_time, start_time, units = "secs"), 2)
  if (verbose) cat("Diurnal Anisotropic Heat Finished in",tt, "seconds\n")
}

#' Calculate Downslope Distance Gradient
#'
#' Computes downslope distance gradient from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the downslope distance gradient files.
#' @param distance The distance parameter for the calculation. Default is 10.
#' @param output The output type for the calculation. Default is 2.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating downslope distance gradient data.
#' @export
calculate_downslope_distance_gradient <- function(dem, outdir, distance = 10, output = 2, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file_gradient <- file.path(temp_dir, "gradient.sgrd")
  output_file_difference <- file.path(temp_dir, "difference.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 9,
                       list(DEM = dem, GRADIENT = output_file_gradient, DIFFERENCE = output_file_difference,
                            DISTANCE = distance, OUTPUT = output),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for downslope distance gradient:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "downslope_distance_gradient_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Downslope Distance Gradient Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Effective Air Flow Heights
#'
#' Computes effective air flow heights from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the effective air flow heights file.
#' @param luv The LUV parameter for the calculation. Default is 1.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating effective air flow heights data.
#' @export
calculate_effective_air_flow_heights <- function(dem, outdir, luv = 1, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "effective_air_flow_heights.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 11,
                       list(DEM = dem, AFH = output_file, LUV = luv),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for effective air flow heights:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "effective_air_flow_heights_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Effective Air Flow Heights Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Mass Balance Index
#'
#' Computes mass balance index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the mass balance index file.
#' @param tslope The slope threshold for the calculation. Default is 15.
#' @param tcurve The curvature threshold for the calculation. Default is 0.01.
#' @param threl The relative threshold for the calculation. Default is 15.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating mass balance index data.
#' @export
calculate_mass_balance_index <- function(dem, outdir, tslope = 15, tcurve = 0.01, threl = 15, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "mass_balance_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 10,
                       list(DEM = dem, MBI = output_file, TSLOPE = tslope, TCURVE = tcurve, THREL = threl),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for mass balance index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "mass_balance_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Mass Balance Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Morphometric Protection Index
#'
#' Computes morphometric protection index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the morphometric protection index file.
#' @param radius The radius parameter for the calculation. Default is 2000.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating morphometric protection index data.
#' @export
calculate_morphometric_protection_index <- function(dem, outdir, radius = 2000, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "morphometric_protection_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 7,
                       list(DEM = dem, PROTECTION = output_file, RADIUS = radius),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for morphometric protection index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "morphometric_protection_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Morphometric Protection Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Real Surface Area
#'
#' Computes real surface area from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the real surface area file.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating real surface area data.
#' @export
calculate_real_surface_area <- function(dem, outdir, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "real_surface_area.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 6,
                       list(DEM = dem, AREA = output_file),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for real surface area:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "real_surface_area_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Real Surface Area Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Relative Heights and Slope Positions
#'
#' Computes relative heights and slope positions from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the relative heights and slope positions file.
#' @param w The weight parameter for the calculation. Default is 0.5.
#' @param t The threshold parameter for the calculation. Default is 10.
#' @param e The exponent parameter for the calculation. Default is 2.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating relative heights and slope positions data.
#' @export
calculate_relative_heights_and_slope_positions <- function(dem, outdir, w = 0.5, t = 10, e = 2, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "slope_height.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 14,
                       list(DEM = dem, HO = output_file, HU = file.path(temp_dir, "valley_depth.sgrd"),
                            NH = file.path(temp_dir, "normalized_height.sgrd"), SH = file.path(temp_dir, "standardized_height.sgrd"),
                            MS = file.path(temp_dir, "mid_slope_position.sgrd"), W = w, T = t, E = e),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for relative heights and slope positions:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "relative_heights_and_slope_positions_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Relative Heights and Slope Positions Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Ridge Level
#'
#' Computes ridge level from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the ridge level file.
#' @param threshold The threshold parameter for the calculation. Default is 1.
#' @param nounderground The no underground parameter for the calculation. Default is 1.
#' @param order The order parameter for the calculation. Default is 4.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating ridge level data.
#' @export
calculate_ridge_level <- function(dem, outdir, threshold = 1, nounderground = 1, order = 4, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "ridge_level.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_channels", module = 7,
                       list(ELEVATION = dem, RIDGE_LEVEL = output_file, THRESHOLD = threshold,
                            NOUNDERGROUND = nounderground, ORDER = order),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for ridge level:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "ridge_level_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Ridge Level Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Sky View Factor
#'
#' Computes sky view factor from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the sky view factor file.
#' @param radius The radius parameter for the calculation. Default is 10000.
#' @param method The method parameter for the calculation. Default is 1.
#' @param dlevel The detail level parameter for the calculation. Default is 3.
#' @param ndirs The number of directions parameter for the calculation. Default is 8.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating sky view factor data.
#' @export
calculate_sky_view_factor <- function(dem, outdir, radius = 10000, method = 1, dlevel = 3, ndirs = 8, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "sky_view_factor.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_lighting", module = 3,
                       list(DEM = dem, SVF = output_file, RADIUS = radius, METHOD = method,
                            DLEVEL = dlevel, NDIRS = ndirs),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for sky view factor:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "sky_view_factor_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Sky View Factor Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Slope, Aspect, and Curvatures
#'
#' Computes slope, aspect, and curvatures from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the slope, aspect, and curvatures files.
#' @param method The method parameter for the calculation. Default is 4.
#' @param unit_slope The unit slope parameter for the calculation. Default is 1.
#' @param unit_aspect The unit aspect parameter for the calculation. Default is 1.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating slope, aspect, and curvatures data.
#' @export
calculate_slope_aspect_curvatures <- function(dem, outdir, method = 4, unit_slope = 1, unit_aspect = 1, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file_slope <- file.path(temp_dir, "slope_degrees.sgrd")
  output_file_aspect <- file.path(temp_dir, "aspect.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 0,
                       list(ELEVATION = dem, METHOD = method, UNIT_SLOPE = unit_slope, UNIT_ASPECT = unit_aspect,
                            SLOPE = output_file_slope, ASPECT = output_file_aspect,
                            C_GENE = file.path(temp_dir, "curv_general.sgrd"), C_PROF = file.path(temp_dir, "curv_profile.sgrd"),
                            C_PLAN = file.path(temp_dir, "curv_plan.sgrd"), C_TANG = file.path(temp_dir, "curv_tangential.sgrd"),
                            C_LONG = file.path(temp_dir, "curv_longitudinal.sgrd"), C_CROS = file.path(temp_dir, "curv_cross_sectional.sgrd"),
                            C_MINI = file.path(temp_dir, "curv_minimal.sgrd"), C_MAXI = file.path(temp_dir, "curv_maximal.sgrd"),
                            C_TOTA = file.path(temp_dir, "curv_total.sgrd"), C_ROTO = file.path(temp_dir, "curv_flow_line.sgrd")),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for slope, aspect, and curvatures:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "slope_aspect_curvatures_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Slope, Aspect, and Curvatures Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Surface Specific Points
#'
#' Computes surface specific points from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the surface specific points file.
#' @param method The method parameter for the calculation. Default is 1.
#' @param threshold The threshold parameter for the calculation. Default is 2.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @return None. The function is used for its side effects of generating surface specific points data.
#' @export
calculate_surface_specific_points <- function(dem, outdir, method = 1, threshold = 2, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "surface_specific_points.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 3,
                       list(ELEVATION = dem, RESULT = output_file, METHOD = method, THRESHOLD = threshold),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for surface specific points:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "surface_specific_points_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Surface Specific Points Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Terrain Ruggedness Index
#'
#' Computes terrain ruggedness index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the terrain ruggedness index file.
#' @param mode The mode parameter for the calculation. Default is 1.
#' @param radius The radius parameter for the calculation. Default is 1.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating terrain ruggedness index data.
#' @export
calculate_terrain_ruggedness_index <- function(dem, outdir, mode = 1, radius = 1, dw_weighting = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "terrain_ruggedness_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 16,
                       list(DEM = dem, TRI = output_file, MODE = mode, RADIUS = radius,
                            DW_WEIGHTING = dw_weighting, DW_IDW_POWER = 2, DW_BANDWIDTH = 1),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for terrain ruggedness index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "terrain_ruggedness_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Terrain Ruggedness Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Terrain Surface Classification
#'
#' Computes terrain surface classification from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the terrain surface classification file.
#' @param type The type parameter for the calculation. Default is 2.
#' @param conv_scale The convolution scale parameter for the calculation. Default is 10.
#' @param text_scale The texture scale parameter for the calculation. Default is 2.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating terrain surface classification data.
#' @export
calculate_terrain_surface_classification <- function(dem, outdir, type = 2, conv_scale = 10, text_scale = 2, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "terrain_surface_classification_iwahashi.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 22,
                       list(DEM = dem, LANDFORMS = output_file, CONV_RECALC = 0, TEXTURE = 0,
                            TEXT_RECALC = 0, TYPE = type, CONV_SCALE = conv_scale, CONV_KERNEL = 0,
                            CONV_TYPE = 0, CONV_EPSILON = 0, TEXT_SCALE = text_scale, TEXT_EPSILON = 1),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for terrain surface classification:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "terrain_surface_classification_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Terrain Surface Classification (Iwahashi and Pike) Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Terrain Surface Convexity
#'
#' Computes terrain surface convexity from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the terrain surface convexity file.
#' @param scale The scale parameter for the calculation. Default is 3.
#' @param method The method parameter for the calculation. Default is 1.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 3.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating terrain surface convexity data.
#' @export
calculate_terrain_surface_convexity <- function(dem, outdir, scale = 3, method = 1, dw_weighting = 3, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "terrain_surface_convexity.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 21,
                       list(DEM = dem, CONVEXITY = output_file, KERNEL = 0, TYPE = 0, EPSILON = 0,
                            SCALE = scale, METHOD = method, DW_WEIGHTING = dw_weighting,
                            DW_IDW_POWER = 1, DW_BANDWIDTH = 0.7),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for terrain surface convexity:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "terrain_surface_convexity_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Terrain Surface Convexity Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Terrain Surface Texture
#'
#' Computes terrain surface texture from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the terrain surface texture file.
#' @param scale The scale parameter for the calculation. Default is 10.
#' @param method The method parameter for the calculation. Default is 1.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 3.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating terrain surface texture data.
#' @export
calculate_terrain_surface_texture <- function(dem, outdir, scale = 10, method = 1, dw_weighting = 3, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "terrain_surface_texture.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 20,
                       list(DEM = dem, TEXTURE = output_file, EPSILON = 1, SCALE = scale,
                            METHOD = method, DW_WEIGHTING = dw_weighting, DW_IDW_POWER = 1,
                            DW_BANDWIDTH = 0.7),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for terrain surface texture:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "terrain_surface_texture_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Terrain Surface Texture Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Topographic Openness
#'
#' Computes topographic openness from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the topographic openness file.
#' @param radius The radius parameter for the calculation. Default is 10000.
#' @param method The method parameter for the calculation. Default is 1.
#' @param dlevel The detail level parameter for the calculation. Default is 3.
#' @param ndirs The number of directions parameter for the calculation. Default is 8.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating topographic openness data.
#' @export
calculate_topographic_openness <- function(dem, outdir, radius = 10000, method = 1, dlevel = 3, ndirs = 8, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "topo_openness.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_lighting", module = 5,
                       list(DEM = dem, POS = output_file, RADIUS = radius, METHOD = method,
                            DLEVEL = dlevel, NDIRS = ndirs),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for topographic openness:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "topographic_openness_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Topographic Openness Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Topographic Position Index
#'
#' Computes topographic position index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the topographic position index file.
#' @param radius_min The minimum radius parameter for the calculation. Default is 0.
#' @param radius_max The maximum radius parameter for the calculation. Default is 100.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating topographic position index data.
#' @export
calculate_topographic_position_index <- function(dem, outdir, radius_min = 0, radius_max = 100, dw_weighting = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "topographic_position_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 18,
                       list(DEM = dem, TPI = output_file, STANDARD = 0, RADIUS_MIN = radius_min,
                            RADIUS_MAX = radius_max, DW_WEIGHTING = dw_weighting, DW_IDW_POWER = 1,
                            DW_BANDWIDTH = 75),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for topographic position index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "topographic_position_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Topographic Position Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate TPI Based Landform Classification
#'
#' Computes TPI based landform classification from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the TPI based landform classification file.
#' @param radius_a_min The minimum radius A parameter for the calculation. Default is 0.
#' @param radius_a_max The maximum radius A parameter for the calculation. Default is 100.
#' @param radius_b_min The minimum radius B parameter for the calculation. Default is 0.
#' @param radius_b_max The maximum radius B parameter for the calculation. Default is 1000.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating TPI based landform classification data.
#' @export
calculate_tpi_based_landform_classification <- function(dem, outdir, radius_a_min = 0, radius_a_max = 100, radius_b_min = 0, radius_b_max = 1000, dw_weighting = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "landforms_tpi_based.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 19,
                       list(DEM = dem, LANDFORMS = output_file, RADIUS_A_MIN = radius_a_min,
                            RADIUS_A_MAX = radius_a_max, RADIUS_B_MIN = radius_b_min, RADIUS_B_MAX = radius_b_max,
                            DW_WEIGHTING = dw_weighting, DW_IDW_POWER = 1, DW_BANDWIDTH = 75),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for TPI based landform classification:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "tpi_based_landform_classification_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("TPI Based Landform Classification Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Valley and Ridge Detection
#'
#' Computes valley and ridge detection from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the valley and ridge detection files.
#' @param radius_valley The radius for valley detection. Default is 1000.
#' @param radius_hill The radius for hill detection. Default is 1000.
#' @param threshold The threshold for detection. Default is 100.
#' @param method The method for detection. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating valley and ridge detection data.
#' @export
calculate_valley_and_ridge_detection <- function(dem, outdir, radius_valley = 1000, radius_hill = 1000, threshold = 100, method = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file_valley <- file.path(temp_dir, "valley.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 24,
                       list(DEM = dem, VALLEY = output_file_valley, HILL = file.path(temp_dir, "hill.sgrd"),
                            VALLEY_IDX = file.path(temp_dir, "valley_idx.sgrd"), HILL_IDX = file.path(temp_dir, "hill_idx.sgrd"),
                            SLOPE_IDX = file.path(temp_dir, "slope_idx.sgrd"), RADIUS_VALLEY = radius_valley,
                            RADIUS_HILL = radius_hill, THRESHOLD = threshold, METHOD = method),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for valley and ridge detection:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "valley_and_ridge_detection_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Valley and Ridge Detection Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Vector Ruggedness Measure
#'
#' Computes vector ruggedness measure from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the vector ruggedness measure file.
#' @param mode The mode parameter for the calculation. Default is 1.
#' @param radius The radius parameter for the calculation. Default is 1.
#' @param dw_weighting The distance weighting parameter for the calculation. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating vector ruggedness measure data.
#' @export
calculate_vector_ruggedness_measure <- function(dem, outdir, mode = 1, radius = 1, dw_weighting = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "vector_ruggedness_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 17,
                       list(DEM = dem, VRM = output_file, MODE = mode, RADIUS = radius,
                            DW_WEIGHTING = dw_weighting, DW_IDW_POWER = 1, DW_BANDWIDTH = 1),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for vector ruggedness measure:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "vector_ruggedness_measure_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Vector Ruggedness Measure Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Wetness Index
#'
#' Computes wetness index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the wetness index file.
#' @param suction The suction parameter for the calculation. Default is 10.
#' @param area_type The area type parameter for the calculation. Default is 1.
#' @param slope_type The slope type parameter for the calculation. Default is 1.
#' @param slope_min The minimum slope parameter for the calculation. Default is 0.
#' @param slope_off The slope offset parameter for the calculation. Default is 0.1.
#' @param slope_weight The slope weight parameter for the calculation. Default is 1.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating wetness index data.
#' @export
calculate_wetness_index <- function(dem, outdir, suction = 10, area_type = 1, slope_type = 1, slope_min = 0, slope_off = 0.1, slope_weight = 1, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "wetness_index.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_hydrology", module = 15,
                       list(DEM = dem, AREA = file.path(temp_dir, "area_twi.sgrd"), SLOPE = file.path(temp_dir, "slope_twi.sgrd"),
                            AREA_MOD = file.path(temp_dir, "area_mod_twi.sgrd"), TWI = output_file, SUCTION = suction,
                            AREA_TYPE = area_type, SLOPE_TYPE = slope_type, SLOPE_MIN = slope_min,
                            SLOPE_OFF = slope_off, SLOPE_WEIGHT = slope_weight),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for wetness index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "wetness_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Wetness Index Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Calculate Wind Exposition Index
#'
#' Computes wind exposition index from a DEM using SAGA GIS.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the wind exposition index file.
#' @param maxdist The maximum distance parameter for the calculation. Default is 300.
#' @param step The step parameter for the calculation. Default is 15.
#' @param oldver The old version parameter for the calculation. Default is 0.
#' @param accel The acceleration parameter for the calculation. Default is 1.5.
#' @param pyramids The pyramids parameter for the calculation. Default is 0.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param env The SAGA environment settings.
#' @importFrom RSAGA rsaga.geoprocessor
#' @return None. The function is used for its side effects of generating wind exposition index data.
#' @export
calculate_wind_exposition_index <- function(dem, outdir, maxdist = 300, step = 15, oldver = 0, accel = 1.5, pyramids = 0, verbose = TRUE, env) {
  start_time <- Sys.time()
  temp_dir <- tempfile()
  dir.create(temp_dir)
  output_file <- file.path(temp_dir, "wind_exposition.sgrd")

  if (!file.exists(dem)) {
    stop("The DEM file ", dem, " does not exist.")
  }

  tryCatch({
    rsaga.geoprocessor("ta_morphometry", module = 27,
                       list(DEM = dem, EXPOSITION = output_file, MAXDIST = maxdist, STEP = step,
                            OLDVER = oldver, ACCEL = accel, PYRAMIDS = pyramids),
                       flags = "s", env = env)
  }, error = function(e) {
    cat("Error in SAGA geoprocessor for wind exposition index:", e$message, "\n")
  })

  write_saga_to_tiff(temp_dir, outdir, "wind_exposition_index_")
  unlink(temp_dir, recursive = TRUE)
  end_time <- Sys.time()
  if (verbose) cat("Wind Exposition Finished in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
}

#' Main Processing Function with Parallel Execution
#'
#' Processes a DEM using multiple terrain analysis functions in parallel.
#'
#' @param dem The file path to the DEM.
#' @param outdir The output directory for the processed files.
#' @param prefix string to be add at the begging of all file name
#' @param cores The number of cores to use for parallel processing. Default is 2.
#' @param parallel Logical indicating whether to enable parallel processing. Default is TRUE.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @return None. The function is used for its side effects of processing the DEM.
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map
#'
#' @examples
#' # Define the path to the DEM file and the output directory
#' dem_path <- system.file("exdata", "mde_utm.tif", package = "yourPackageName")
#' output_directory <- tempdir()  # Use a temporary directory for output
#'
#' # Process the DEM with default parameters
#' process_dem(dem = dem_path, outdir = output_directory, prefix = "morpho")
#'
#' # Check the output files in the output directory
#' list.files(output_directory, pattern = "morpho", full.names = TRUE)
#' unlink(output_directory)
#' @export
process_dem <- function(dem, outdir, prefix = 'morpho', cores = 1,
                        parallel = FALSE, verbose = TRUE) {
  env <- setup_saga(1, FALSE)
  start_time <- Sys.time()

  prefixo <<- prefix  # variável global
unlink()
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  if (parallel) {
    plan(multisession, workers = cores)
  } else {
    plan(sequential)
  }

  # Lista de funções a serem executadas
  functions_to_run <- list(
    calculate_hillshade,
    calculate_convergence_index,
    calculate_curvature_classification,
    # calculate_solar_radiation,
    calculate_diurnal_anisotropic_heat,
    calculate_downslope_distance_gradient,
    calculate_effective_air_flow_heights,
    calculate_mass_balance_index,
    calculate_morphometric_protection_index,
    calculate_real_surface_area,
    calculate_relative_heights_and_slope_positions,
    calculate_ridge_level,
    calculate_sky_view_factor,
    calculate_slope_aspect_curvatures,
    calculate_surface_specific_points,
    calculate_terrain_ruggedness_index,
    calculate_terrain_surface_classification,
    calculate_terrain_surface_convexity,
    calculate_terrain_surface_texture,
    calculate_topographic_openness,
    calculate_topographic_position_index,
    calculate_tpi_based_landform_classification,
    calculate_valley_and_ridge_detection,
    calculate_vector_ruggedness_measure,
    calculate_wetness_index,
    calculate_wind_exposition_index
  )

  # Inform the user about the start of processing
  if (verbose) cat("Starting processing of DEM...\n")

  # Use future_map to run functions in parallel
  future_map(functions_to_run, function(f) {
    tryCatch({
      f(dem, outdir, prefixo = prefix, verbose = FALSE, env = env)
    }, error = function(e) {
      cat("Error processing function:", e$message, "\n")
    })
  })

  end_time <- Sys.time()
  if (verbose) {
    cat("Total processing time:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
  }
}
