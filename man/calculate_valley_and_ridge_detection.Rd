% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcoes_rsaga.R
\name{calculate_valley_and_ridge_detection}
\alias{calculate_valley_and_ridge_detection}
\title{Calculate Valley and Ridge Detection}
\usage{
calculate_valley_and_ridge_detection(
  dem,
  outdir,
  prefix,
  num_tile,
  radius_valley = 1000,
  radius_hill = 1000,
  threshold = 100,
  method = 0,
  verbose = TRUE,
  env
)
}
\arguments{
\item{dem}{The file path to the DEM.}

\item{outdir}{The output directory for the valley and ridge detection files.}

\item{radius_valley}{The radius for valley detection. Default is 1000.}

\item{radius_hill}{The radius for hill detection. Default is 1000.}

\item{threshold}{The threshold for detection. Default is 100.}

\item{method}{The method for detection. Default is 0.}

\item{verbose}{Logical indicating whether to print progress messages. Default is TRUE.}

\item{env}{The SAGA environment settings.}
}
\value{
None. The function is used for its side effects of generating valley and ridge detection data.
}
\description{
Computes valley and ridge detection from a DEM using SAGA GIS.
}
