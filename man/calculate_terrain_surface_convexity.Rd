% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcoes_rsaga.R
\name{calculate_terrain_surface_convexity}
\alias{calculate_terrain_surface_convexity}
\title{Calculate Terrain Surface Convexity}
\usage{
calculate_terrain_surface_convexity(
  dem,
  outdir,
  prefix,
  num_tile,
  scale = 3,
  method = 1,
  dw_weighting = 3,
  verbose = TRUE,
  env
)
}
\arguments{
\item{dem}{The file path to the DEM.}

\item{outdir}{The output directory for the terrain surface convexity file.}

\item{scale}{The scale parameter for the calculation. Default is 3.}

\item{method}{The method parameter for the calculation. Default is 1.}

\item{dw_weighting}{The distance weighting parameter for the calculation. Default is 3.}

\item{verbose}{Logical indicating whether to print progress messages. Default is TRUE.}

\item{env}{The SAGA environment settings.}
}
\value{
None. The function is used for its side effects of generating terrain surface convexity data.
}
\description{
Computes terrain surface convexity from a DEM using SAGA GIS.
}
