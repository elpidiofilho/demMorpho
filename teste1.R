library(demMorpho)
#Define the path to the DEM file and the output directory
dem_path <- system.file("extdata","mde_utm.tif", package = "demMorpho")
dem_path
file.exists(dem_path)
output_directory <- 'morpho1'  # Use a temporary directory for output

# Process the DEM with default parameters
process_dem(dem = dem_path, outdir = output_directory, prefixo = 'morpho_', cores = 6, parallel = FALSE)
# Check the output files in the output directory
list.files(output_directory, pattern = "morpho", full.names = TRUE)
unlink(output_directory, recursive = TRUE)
