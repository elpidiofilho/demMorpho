library(sf)

#' Determina o EPSG UTM a partir das coordenadas geográficas de um shapefile.
#'
#' Esta função calcula a zona UTM correspondente às coordenadas geográficas
#' de um shapefile e retorna o código EPSG da zona que contém a maior área
#' do shapefile, considerando as zonas adjacentes se necessário.
#'
#' @param shapefile Um objeto do tipo sf (simple features) representando o shapefile.
#'
#' @return Um caractere representando o código EPSG da zona UTM com a maior área.
#'
#' @examples
#'
#' # Supondo que 'meu_shapefile' é um objeto sf válido
#' epsg <- determinar_epsg_utm(meu_shapefile)
#' print(epsg)
#'
#' @importFrom sf st_bbox st_transform st_crs
#' @importFrom sf st_intersection
#' @importFrom sf st_as_sfc
#' @importFrom sf st_area
#' @export
determinar_epsg_utm <- function(tile) {
  # Verificação de erro: checar se o shapefile é um objeto sf
  bbox <- ext(tile)

  # Criar um polígono a partir da bounding box
  polygon <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[3], xmax = bbox[2], ymax = bbox[4])), crs = st_crs(4326))

  # Transformar o polígono em um objeto sf
  shapefile <- st_sf(geometry = polygon)
  plot(shapefile)
  if (!inherits(shapefile, "sf")) {
    stop("O argumento 'shapefile' deve ser um objeto do tipo sf.")
  }

  # Verificação de erro: checar se o shapefile está em coordenadas geográficas (EPSG:4326 ou EPSG:4674)
  crs <- st_crs(shapefile)
  if (crs != 4326 && crs != 4674) {
    stop("O shapefile deve estar em coordenadas geográficas (EPSG:4326 ou EPSG:4674).")
  }

  # Obter a extensão do shapefile
  ext <- st_bbox(shapefile)
  xmin <- ext[1]
  xmax <- ext[3]

  # Verificação de erro: checar se a extensão é válida
  if (is.na(xmin) || is.na(xmax) || xmin >= xmax) {
    stop("A extensão do shapefile é inválida. Verifique os dados de entrada.")
  }

  # Determinar zonas UTM para xmin e xmax
  zona_utm_xmin <- floor((xmin + 180) / 6) + 1
  zona_utm_xmax <- floor((xmax + 180) / 6) + 1

  hemisferio <- ifelse(ext[2] < 0, "S", "N")

  # Calcular EPSG para cada zona
  epsg_xmin <- if (hemisferio == "N") {
    paste0("EPSG:", 31999 + zona_utm_xmin)  # SIRGAS2000 UTM Norte
  } else {
    paste0("EPSG:", 32700 + zona_utm_xmin)  # SIRGAS2000 UTM Sul
  }

  epsg_xmax <- if (hemisferio == "N") {
    paste0("EPSG:", 31999 + zona_utm_xmax)  # SIRGAS2000 UTM Norte
  } else {
    paste0("EPSG:", 32700 + zona_utm_xmax)  # SIRGAS2000 UTM Sul
  }

  # Se as zonas forem diferentes, calcular a extensão em cada zona
  if (zona_utm_xmin != zona_utm_xmax) {
    return(calcular_extensao(shapefile, epsg_xmin))
  }

  # Se as zonas forem iguais, retornar a EPSG correspondente
  return(epsg_xmin)
}

#' Calcula a extensão em km² para uma zona UTM.
#'
#' Esta função transforma um shapefile para uma zona UTM específica e calcula
#' a área do shapefile na nova projeção.
#'
#' @param shapefile Um objeto do tipo sf (simple features) representando o shapefile.
#' @param epsg Um caractere representando o código EPSG da zona UTM.
#'
#' @return Um número representando a área em km² do shapefile na zona UTM especificada.
#'
#' @importFrom sf st_transform st_area
calcular_extensao <- function(shapefile, epsg) {
  # Transformar o shapefile para a zona UTM correspondente
  shapefile_utm <- st_transform(shapefile, epsg)

  # Obter os limites da zona UTM
  limites <- st_bbox(shapefile_utm)

  # Criar um polígono de limite para a zona UTM
  poligono <- st_as_sfc(st_bbox(limites))

  # Cortar o shapefile nos limites da zona
  parte <- st_intersection(shapefile_utm, poligono)

  # Calcular a área em km²
  area_km2 <- st_area(parte) / 1e6  # Convertendo de m² para km²

  return(sum(area_km2))  # Retornar a soma das áreas
}


#' Reprojeta um raster de coordenadas geográficas para UTM.
#'
#' Esta função lê um arquivo raster, reprojeta-o de coordenadas geográficas
#' (EPSG:4326 ou EPSG:4674) para a zona UTM correspondente e retorna o raster reprojetado.
#'
#' @param caminho_raster Um caractere representando o caminho do arquivo raster a ser reprojetado.
#'
#' @return Um objeto raster reprojetado.
#'
#' @examples
#' # Reprojetar um raster específico
#' raster_reprojetado <- reproj_raster("caminho/para/raster.tif")
#'
#' @importFrom terra rast crs project
reproj_raster <- function(caminho_raster) {
  # Ler o raster original
  raster_original <- rast(caminho_raster)

  # Definir o CRS do raster original como WGS84
  crs(raster_original) <- "EPSG:4326"  # WGS84, equivalente ao SIRGAS2000

  # Determinar a EPSG UTM correspondente
  epsg_utm <- determinar_epsg_utm(raster_original)

  # Reprojetar o raster
  raster_reprojetado <- project(raster_original, y = epsg_utm)

  # Plotar o raster reprojetado
  plot(raster_reprojetado)

  return(raster_reprojetado)
}

#' Reprojeta todos os rasters em uma pasta de coordenadas geográficas para UTM.
#'
#' Esta função lê todos os arquivos raster em uma pasta especificada, reprojeta
#' cada um deles de coordenadas geográficas (EPSG:4326 ou EPSG:4674) para a
#' zona UTM correspondente e salva os rasters reprojetados em uma nova pasta.
#'
#' @param path_entrada Um caractere representando o caminho da pasta contendo os rasters
#' em coordenadas geográficas.
#' @param path_saida Um caractere representando o caminho da pasta onde os rasters
#' reprojetados serão salvos.
#'
#' @return Um vetor de caracteres com os nomes dos arquivos reprojetados.
#'
#' @examples
#' # Reprojetar rasters de uma pasta para UTM
#' reprojetar_rasters("caminho/para/pasta/input", "caminho/para/pasta/output")
#'
#' @importFrom terra rast writeRaster
#' @export
reprojetar_rasters <- function(path_entrada, path_saida) {
  # Verificação de erro: checar se o diretório de entrada existe
  if (!dir.exists(path_entrada)) {
    stop("O diretório de entrada não existe.")
  }

  # Criar o diretório de saída se não existir
  if (!dir.exists(path_saida)) {
    dir.create(path_saida, recursive = TRUE)
  }

  # Listar todos os arquivos raster na pasta de entrada
  arquivos_raster <- list.files(path_entrada, pattern = "\\.tif$", full.names = TRUE)

  # Verificação de erro: checar se existem arquivos raster na pasta
  if (length(arquivos_raster) == 0) {
    stop("Nenhum arquivo raster encontrado no diretório de entrada.")
  }

  # Vetor para armazenar os nomes dos arquivos reprojetados
  reprojetados <- character(length(arquivos_raster))

  # Loop através de cada arquivo raster
  for (caminho_raster in arquivos_raster) {
    # Reprojetar o raster
    raster_final <- reproj_raster(caminho_raster)

    # Criar o nome do arquivo de saída
    nome_arquivo_saida <- file.path(path_saida, basename(caminho_raster))

    # Salvar o raster reprojetado
    writeRaster(raster_final, nome_arquivo_saida, overwrite = TRUE, gdal = c("COMPRESS=LZW"))

    # Mensagem de progresso
    cat("Raster reprojetado e salvo:", nome_arquivo_saida, "\n")

    # Armazenar o nome do arquivo reprojetado
    reprojetados <- c(reprojetados, nome_arquivo_saida)
  }

  return(reprojetados)
}

#' Criar grid de Tiles
#'
#' Esta função cria uma grade de tiles a partir de um raster e corta os tiles
#' pelo limite de uma área definida em um shapefile.
#'
#' @param caminho_raster O caminho para o arquivo raster (ex: .tif).Raster deve estar
#' projeção geografica com datum SIRGAS2000 ou WGS84
#' @param caminho_limite O caminho para o shapefile que define o limite da área.
#' @param caminho_saida O caminho onde o shapefile dos tiles cortados será salvo.
#' @param numrow número de colunas do tile
#' @param numcol numero de linhas do tile
#'
#' @return Um objeto sf contendo os tiles cortados.

#' @examples
#' \dontrun {
#' criar_grid_tiles('./copernicusDem/copernicusDem_Mg.tif',
#'                  './limite_area/mg_buf.shp',
#'                  './saida/tiles_cortados.shp')}
#'
#'
#' @importFrom sf st_read st_bbox st_polygon st_sfc st_sf st_intersection st_write
#' @importFrom terra rast res
#'
#' @export
criar_grid_tiles <- function(path_raster, shape_limite, numcol = 3600,
                             numrow = 3600, path_saida) {
  # Carregar o raster e o limite

  # Verificar se os arquivos existem
  if (!file.exists(caminho_raster)) {
    stop("Erro: O arquivo raster não existe no caminho especificado: ", caminho_raster)
  }

  if (!file.exists(caminho_limite)) {
    stop("Erro: O shapefile de limite não existe no caminho especificado: ", caminho_limite)
  }

  # Tentar carregar o raster e o limite com tratamento de erro
  raster <- tryCatch({
    rast(caminho_raster)
  }, error = function(e) {
    stop("Erro ao carregar o raster: ", e$message)
  })

  limite <- tryCatch({
    st_read(caminho_limite)
  }, error = function(e) {
    stop("Erro ao carregar o shapefile de limite: ", e$message)
  })

  # Verificar se o CRS do raster e do limite estão em SIRGAS 2000 ou WGS84
  crs_raster <- st_crs(raster)
  crs_limite <- st_crs(limite)

  if (!isTRUE(crs_raster$epsg %in% c(4326, 4674))) {
    stop("Erro: O raster não está em um sistema de coordenadas geográficas (SIRGAS 2000 ou WGS84).")
  }

  if (!isTRUE(crs_limite$epsg %in% c(4326, 4674))) {
    stop("Erro: O shapefile de limite não está em um sistema de coordenadas geográficas (SIRGAS 2000 ou WGS84).")
  }

  #Obter resolução do raster
  resolucao <- res(raster)

  # Calcular tamanho do tile em metros
  tamanho_tile_x <- resolucao[1] * numcol # Tamanho do tile em graus de longitude
  tamanho_tile_y <- resolucao[2] * numrow  # Tamanho do tile em graus de latitude

  # Criar grid de tiles com coordenadas centrais ajustadas
  x_seq <- seq(from = floor(min(st_bbox(limite)["xmin"])),
               to = ceiling(max(st_bbox(limite)["xmax"])),
               by = tamanho_tile_x)

  y_seq <- seq(from = floor(min(st_bbox(limite)["ymin"])),
               to = ceiling(max(st_bbox(limite)["ymax"])),
               by = tamanho_tile_y)

  # Criar grid de tiles
  grid_tiles <- expand.grid(x = x_seq, y = y_seq)

  # Criar polígonos de tiles
  tiles <- lapply(1:nrow(grid_tiles), function(i) {
    x_min <- grid_tiles$x[i]
    x_max <- x_min + tamanho_tile_x
    y_min <- grid_tiles$y[i]
    y_max <- y_min + tamanho_tile_y

    # Criar um polígono para cada tile
    polygon <- st_polygon(list(rbind(c(x_min, y_min),
                                     c(x_max, y_min),
                                     c(x_max, y_max),
                                     c(x_min, y_max),
                                     c(x_min, y_min))))

    # Criar a numeração do tile
    num_tile <- sprintf("%02d%02d", as.integer(abs(grid_tiles$y[i])), as.integer(abs(grid_tiles$x[i])))  # Sem sinais e com 4 dígitos

    # Retornar o polígono e a numeração como um data frame
    st_sf(geometry = st_sfc(polygon, crs = st_crs(limite)), tile_id = num_tile)
  })

  # Combinar todos os tiles em um único objeto sf
  tiles_sf <- do.call(rbind, tiles)

  # Cortar o grid pelo limite da área
  tiles_cortados <- st_intersection(tiles_sf, limite)

  # Salvar shapefile de tiles cortados
  st_write(tiles_cortados, path_saida, append = FALSE)

  # Retornar os tiles cortados
  return(tiles_cortados)
}



#' Calcular Número de Tiles
#'
#' Esta função calcula o número de tiles que podem ser criados com base no limite do polígono
#' e nas dimensões de cada tile (número de colunas e linhas).
#'
#' @param caminho_raster O caminho para o arquivo raster (ex: .tif).
#' @param caminho_limite O caminho para o shapefile que define o limite da área.
#' @param numcol O número de colunas de cada tile.
#' @param numrow O número de linhas de cada tile.
#'
#' @return O número total de tiles que podem ser criados dentro do limite do polígono.
#'
#' @examples
#' \dontrun{
#' num_tiles <- calcular_numero_tiles('./copernicusDem/copernicusDem_Mg.tif',
#'                                     './limite_area/mg_buf.shp',
#'                                     numcol = 3600, numrow = 3600)
#' print(num_tiles)}
#' @export

calcular_numero_tiles <- function(caminho_raster, caminho_limite, numcol, numrow, overwrite) {
  # Verificar se os arquivos existem
  if (!file.exists(caminho_raster)) {
    stop("Erro: O arquivo raster não existe no caminho especificado: ", caminho_raster)
  }

  if (!file.exists(caminho_limite)) {
    stop("Erro: O shapefile de limite não existe no caminho especificado: ", caminho_limite)
  }

  # Tentar carregar o raster e o limite com tratamento de erro
  raster <- tryCatch({
    rast(caminho_raster)
  }, error = function(e) {
    stop("Erro ao carregar o raster: ", e$message)
  })

  limite <- tryCatch({
    st_read(caminho_limite)
  }, error = function(e) {
    stop("Erro ao carregar o shapefile de limite: ", e$message)
  })

  # Verificar se o CRS do raster e do limite estão em SIRGAS 2000 ou WGS84
  crs_raster <- st_crs(raster)
  crs_limite <- st_crs(limite)

  if (!isTRUE(crs_raster$epsg %in% c(4326, 4674))) {
    stop("Erro: O raster não está em um sistema de coordenadas geográficas (SIRGAS 2000 ou WGS84).")
  }

  if (!isTRUE(crs_limite$epsg %in% c(4326, 4674))) {
    stop("Erro: O shapefile de limite não está em um sistema de coordenadas geográficas (SIRGAS 2000 ou WGS84).")
  }

  # Obter resolução do raster
  resolucao <- res(raster)

  # Calcular tamanho do tile em graus
  tamanho_tile_x <- resolucao[1] * numcol  # Largura do tile em graus
  tamanho_tile_y <- resolucao[2] * numrow  # Altura do tile em graus

  # Criar grid de tiles com coordenadas centrais ajustadas
  x_seq <- seq(from = floor(min(st_bbox(limite)["xmin"])),
               to = ceiling(max(st_bbox(limite)["xmax"])),
               by = tamanho_tile_x)

  y_seq <- seq(from = floor(min(st_bbox(limite)["ymin"])),
               to = ceiling(max(st_bbox(limite)["ymax"])),
               by = tamanho_tile_y)

  # Criar grid de tiles
  grid_tiles <- expand.grid(x = x_seq, y = y_seq)

  # Criar polígonos de tiles
  tiles <- lapply(1:nrow(grid_tiles), function(i) {
    x_min <- grid_tiles$x[i]
    x_max <- x_min + tamanho_tile_x
    y_min <- grid_tiles$y[i]
    y_max <- y_min + tamanho_tile_y

    # Criar um polígono para cada tile
    polygon <- st_polygon(list(rbind(c(x_min, y_min),
                                     c(x_max, y_min),
                                     c(x_max, y_max),
                                     c(x_min, y_max),
                                     c(x_min, y_min))))

    # Retornar o polígono como um data frame
    st_sf(geometry = st_sfc(polygon, crs = st_crs(limite)))
  })

  # Combinar todos os tiles em um único objeto sf
  tiles_sf <- do.call(rbind, tiles)

  # Cortar o grid pelo limite da área
  tiles_cortados <- st_intersection(tiles_sf, limite)

  # Contar o número de tiles que estão dentro do limite
  num_tiles <- nrow(tiles_cortados)

  return(num_tiles)
}

# Exemplo de uso (descomente para testar)
# num_tiles <- calcular_numero_tiles('./copernicusDem/copernicusDem_Mg.tif',
#                                     './limite_area/mg_buf.shp',
#                                     numcol = 3, numrow = 3)
# print(num_tiles)
