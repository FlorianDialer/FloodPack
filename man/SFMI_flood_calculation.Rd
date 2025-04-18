% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SFMI_flood_calculation.R
\name{SFMI_flood_calculation}
\alias{SFMI_flood_calculation}
\title{Calculate Flood Areas based on Sentinel-2 Bands; previous S2_data_download and S2_data_processing function runs are necessary for this function to work}
\usage{
SFMI_flood_calculation(
  aoi,
  path_to_rasters = file.path(getwd(), "processed_data")
)
}
\arguments{
\item{aoi}{Area of Interest as the Path to a Vector File .shp or .gpkg; Warning: Always the first Feature gets selected --> If you have multiple Geometries unionize them beforehand}

\item{path_to_rasters}{Optional: If you changed the processed Files Location or want to use your own Data specify the Directory as a String}
}
\value{
Returns Flood Areas inside the AOI in Raster and Vector format named ascending in the Working Directory in the folder "flood_data" ; also returns an Elevation Folder in the Working Directory necessary for processing which can be ignored/deleted
}
\description{
Calculate Flood Areas based on Sentinel-2 Bands; previous S2_data_download and S2_data_processing function runs are necessary for this function to work
}
\examples{

aoi <- "path/to/file.shp"
aoi <- "path/to/file.gpkg"

path_to_rasters <- "path/to/folder/containing/processed-data"


}
