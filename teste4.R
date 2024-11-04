library(demMorpho)

path_remoto = '//200.235.173.229/backup_antigo/cluster'

# Define o caminho para a pasta que contém os arquivos DEM
dem_directory <- file.path(path_remoto, './tile_mg_utm2')

# Lista todos os arquivos .tif na pasta
tile_path =
dem_files <- list.files(dem_directory, pattern = "tile_com_buffer\\d+\\.tif$", full.names = TRUE)

# Processa cada arquivo DEM
dem_path = dem_files[1]
for (dem_path in dem_files) {
  # Extrai o número do nome do arquivo
  start_time <- Sys.time()
  file_name <- basename(dem_path)  # Obtém o nome do arquivo sem o caminho
  number <- sub("tile_com_buffer(\\d+)\\.tif", "\\1", file_name)  # Captura o número
  padded_number <- sprintf("%04d", as.numeric(number))  # Adiciona zeros à esquerda

  # Define o caminho de saída
  output_path <- file.path(path_remoto,'morpho_tile', paste0('tile_', padded_number))
  print(output_path)

  # Cria o diretório de saída se não existir
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Processa o DEM com os parâmetros padrão
  process_dem(dem = dem_path, outdir = output_path, prefixo = 'morpho_', cores = 7, parallel = TRUE)
  endtime = Sys.time()
  tt = difftime(endtime, Sys.time(), units = "mins")
  cat(dem_path,' tempo ', tt, 'minutos \n' )
  gc()
}
