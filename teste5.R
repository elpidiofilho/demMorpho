library(terra)
library(sf)
library(demMorpho)

path_remoto = '//200.235.173.229/backup_antigo/cluster'


# Função para reprojetar um raster
reproj_raster <- function(caminho_raster) {
  raster_original <- rast(caminho_raster)
  crs(raster_original) <- "EPSG:4326"  # WGS84, equivalente ao SIRGAS2000

  epsg_utm <- determinar_epsg_utm(raster_original)
  raster_reprojetado <- project(raster_original, y = epsg_utm)
  plot(raster_reprojetado)
  return(raster_reprojetado)
}

# Diretórios de entrada e saída
diretorio_entrada <- file.path(path_remoto, "tile_mg2")
diretorio_saida <- file.path(path_remoto, "tile_mg_utm2")


# Criar o diretório de saída se não existir
if (!dir.exists(diretorio_saida)) {
  dir.create(diretorio_saida)
}

repro
# Mensagem de conclusão
cat("Todos os rasters foram reprojetados e salvos com sucesso!\n")
