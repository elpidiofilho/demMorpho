library(demMorpho)
library(sf)
library(terra)
library(ggplot2)

# Carregar o pacote terra
path_remoto = '//200.235.173.229/backup_antigo/cluster'
# Obter o caminho do arquivo mde_utm.tif
caminho_raster <- './copernicusDem/copernicusDem_Mg.tif'
caminho_limite = './limite_area/mg_buf.shp'

nt = calcular_numero_tiles(caminho_raster = caminho_raster,
                           caminho_limite = caminho_limite,
                           numcol = 3600, numrow = 3600)

cat("numero de tiles", nt, '\n')


grid1 = criar_grid_tiles(path_raster = './copernicusDem/copernicusDem_Mg.tif',
                 shape_limite = './limite_area/mg_buf.shp',
                 numcol = 3600, numrow = 3600,
                 path_saida =  './limite_area/grid_tiles.shp')


#####

centros <- st_centroid(grid1)
centros_coords <- st_coordinates(centros)
centros_df <- data.frame(centros_coords, tile_id = grid1$tile_id)

# Plotar usando ggplot2
ggplot(data = grid1) +
  geom_sf(fill = 'lightblue', color = 'blue') +
  geom_text(data = centros_df, aes(x = X, y = Y, label = tile_id), color = 'red',
            size = 3, vjust = 0.5, hjust = 0.5) +
  ggtitle("Grid de Polígonos com Tile IDs") +
  theme_minimal()


######
# Criar raster tiles

raster_mde = rast(caminho_raster)
grid2 = vect('./limite_area/grid_tiles.shp')
buffer_size = 5

fn =  file.path (path_remoto, "tile_mg2/tile_com_buffer.tif")
tiles_com_buffer <- makeTiles(raster_mde, y = grid2,
                              filename = file.path (path_remoto, "tile_mg2/tile_com_buffer.tif"),
                              buffer = buffer_size, na.rm = TRUE)

reproj = reprojetar_rasters(path_entrada = file.path(path_remoto, "tile_mg2"),
                            path_saida = file.path(path_remoto, "tile_mg2_utm"))

