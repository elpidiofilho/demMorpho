write_saga_to_tiff <- function(dir_path, outdir, prefix = 'morpho') {
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
