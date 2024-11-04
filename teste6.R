# Definir o diretório onde os rasters estão localizados
library(demMorpho)
path_remoto = '//200.235.173.229/backup_antigo/cluster'
raster_dir = file.path(path_remoto, 'tile_mg_utm')
dem_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
# Chamar a função process_dem
# Lista de funções a serem executadas

functions_to_run <- list(
  #calculate_hillshade,
  #calculate_convergence_index,
  #calculate_curvature_classification,
  ## calculate_solar_radiation,
  #calculate_diurnal_anisotropic_heat,
  #calculate_downslope_distance_gradient,
  #calculate_effective_air_flow_heights,
  #calculate_mass_balance_index,
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


process_dem_area2(dem_files = dem_files, outdir = "./morfo_tile_area",
                  prefixo =  "morpho_", functions_to_run = functions_to_run)

dem_files[1]



extract_number_from_filename(filename = "./tile_mg_utm/tile_com_buffer100.tif")
