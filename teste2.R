
# Carregar o pacote terra
library(terra)
path_remoto = '//200.235.173.229/backup_antigo/cluster'
# Obter o caminho do arquivo mde_utm.tif
caminho_raster <- './copernicusDem/copernicusDem_Mg.tif'

# Ler o arquivo raster
raster_mde <- rast(caminho_raster)

# Verificar as dimensões do raster
print(dim(raster_mde))  # Deve retornar 701 linhas e 894 colunas

# Definir o número de linhas e colunas para os tiles
num_tiles <- c(2500, 1500)  # 7 linhas e 9 colunas para cobrir o raster


# Criar tiles com buffer (exemplo: buffer de 10 pixels)
buffer_size <- 5
tiles_com_buffer <- makeTiles(raster_mde, y = num_tiles,
                              filename = file.path (path_remoto, "./tile_mg2/tile_com_buffer.tif"),
                              buffer = buffer_size, na.rm = TRUE)

# Visualizar os tiles com buffer
#plot(rast(tiles_com_buffer[1]))
#plot(rast(tiles_com_buffer[3]), add = TRUE)
terra::free_RAM()
# Salvar os tiles em arquivos separados (opcional)
# writeRaster(tiles_sem_buffer, filename = "tiles_sem_buffer.tif", bylayer = TRUE, suffix = "names")
# writeRaster(tiles_com_buffer, filename = "tiles_com_buffer.tif", bylayer = TRUE, suffix = "names")
