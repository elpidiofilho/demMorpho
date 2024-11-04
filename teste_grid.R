#exemplo de uso


# Carregue os pacotes
library(geobr)
library(sf)
library(basinPhysiographics)
#remotes::install_github('hydroversebr/basinPhysiographics')
# Baixe o shapefile do estado de Santa catarina
MG_shape <- read_state(code_state = "MG", year = 2020)
MG_shape_buf = sf::st_buffer(x = MG_shape, dist = 150)
st_write(obj = MG_shape_buf, dsn = './limite_area/mg_buf.shp', append = FALSE)
# Verifique o shapefile
plot(MG_shape_buf$geom, col = "lightblue")

# Chame a função downloadCopernicusDem com todos os parâmetros
dem_MG <- downloadCopernicusDem(
  aoi = MG_shape_buf,
  outputDir = "./copernicusDem",
  outputFileName = "copernicusDem_MG.tif",
  res = 30,  # ou 30, dependendo da resolução desejada
  type = "DGED",  # ou "DTED", dependendo do tipo desejado
  outputDirTempFile = "./copernicusDem/tempDirDem",
  keepInvidualTiles = FALSE,
  timeout = 600,
  ncores = 6,  # Ajuste o número de núcleos conforme necessário
  saveAsInteger = TRUE,
  multiplier = 1,
  show = TRUE,
  retry = 3
)

# Verifique o DEM baixado
print(dem_MG)
