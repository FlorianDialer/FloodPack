#' Create a Map displaying calculated Flood Areas
#'
#' @param aoi Area of Interest as the Path to a Vector File .shp or .gpkg; Warning: Always the first Feature gets selected --> If you have multiple Geometries unionize them beforehand
#' @param title Define a Title for the Map as a String
#' @param map_file_name Optional: Define File Name for the Map as a String; Default: "my_flood_map"
#' @param name_of_aoi_in_legend Optional: Define how your AOI will be named in the Map as a String; Default: "Area of Interest"
#' @param name_of_flood_areas Optional: Define how your Flood Areas will be named in the Map as a String; Default: "Flood Areas"
#' @param x_axis Optional: Define how your x-Axis will be named in the Map as a String; Default: "Longitude"
#' @param y_axis Optional: Define how your y-Axis will be named in the Map as a String; Default: "Latitude"
#' @param caption_text Optional: Define a Caption for your Map as a String; A new Line can be created using "BACKSLASHn"; Default: empty
#' @param sort_by_flood_size Optional: If you cannot see the Flood Areas clearly, this Option sorts them by size to increase visibility, as a Boolean
#'
#' @returns A Map in the Working Directory in the folder "flood_map" in different Formats for Publication
#' @export
#'
#' @examples
#'
#'aoi <- "path/to/file.shp"
#'aoi <- "path/to/file.gpkg"
#'
#'title <- "Floods in Bavaria, Germany 2024"
#'
#'map_file_name <- "flood_map_germany"
#'
#'name_of_aoi_in_legend <- "Bavaria"
#'
#'name_of_flood_areas <- "Inundation Areas"
#'
#'x_axis <- "X"
#'
#'y_axis <- "Y"
#'
#'sort_by_flood_size <- TRUE
#'
#' @importFrom sf st_read st_area st_transform st_crs st_bbox
#' @importFrom dplyr bind_rows
#' @importFrom basemaps set_defaults basemap_terra flush_cache
#' @importFrom terra project
#' @importFrom ggplot2 ggplot coord_sf geom_sf aes scale_fill_viridis_d scale_color_manual labs theme element_text ggsave
#' @importFrom tidyterra geom_spatraster_rgb
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orienteering


FloodMap <- function(aoi, title = "", map_file_name = "my_flood_map", name_of_aoi_in_legend = "Area of Interest", name_of_flood_areas = "Flood Areas", x_axis = "Longitude", y_axis = "Latitude", caption_text = "", sort_by_flood_size = FALSE) {

  #Reading in flood area vector files
  flooded_areas_path <- list.files(path = file.path(getwd(), "flood_data"), pattern = "\\.gpkg$", full.names = TRUE, recursive = FALSE)

  flood_areas_list <- list()

  #Calculating Areas of Flood Areas
  for (i in seq_along(flooded_areas_path)) {
    flood_areas_list[[i]] <- sf::st_read(flooded_areas_path[i], quiet = TRUE)
    flood_areas_list[[i]]$flooded_area <- sf::st_area(flood_areas_list[[i]])
  }

  #Sort by date
  flood_areas_list <- flood_areas_list[order(sapply(flood_areas_list, function(X) X$date[1]), decreasing = FALSE)]

  #Order by flooded area descending for seeing most dates accurately (smaller areas get plotted on larger ones to be visible)
  if (sort_by_flood_size == TRUE) {
    flood_areas_list <- flood_areas_list[order(sapply(flood_areas_list, function(X) X$flooded_area), decreasing = TRUE)]
  }

  #Defining Elements for Map

  #Reading in AOI
  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi_crs_of_rasters <- sf::st_transform(aoi[1,], crs = sf::st_crs(flood_areas_list[[1]]))
  aoi_area <- sf::st_area(aoi_crs_of_rasters)

  #Flooded Area Percent Calculation in Relation to AOI Area and Label Creation for ggplot
  for (i in seq_along(flood_areas_list)) {
    flood_areas_list[[i]]$flood_percent <- (flood_areas_list[[i]]$flooded_area) / (aoi_area) * 100

    label <- paste(round(as.numeric(flood_areas_list[[i]]$flood_percent), 2), "% flooded on", flood_areas_list[[i]]$date)
    flood_areas_list[[i]]$label <- label
  }

  #Combining all flood_areas for easier plotting in ggplot, using dplyr due to name issues in column names which circumvents/ignores them automatically
  combined_flood_areas <- dplyr::bind_rows(flood_areas_list)

  #Basemap Preparation
  message("Preparing Basemap...")

  basemap_temp_directory <- file.path(getwd(), "basemap_temp")

  if (!dir.exists(basemap_temp_directory)) {
    dir.create(basemap_temp_directory)
  }

  bbox <- sf::st_bbox(aoi_crs_of_rasters)

  #Downloading Basemap based on Bounding Box of AOI and converting/projectiong to SpatRaster and Flood Area CRS
  basemaps::set_defaults(map_service = "carto", map_type = "light", map_res = 1, map_dir = basemap_temp_directory)

  basemap <- suppressWarnings(basemaps::basemap_terra(bbox))
  basemap_reprojected <- terra::project(basemap, flood_areas_list[[1]])


  #Defining Colors and Dates for Flood Areas
  message("Plotting Map...")


  #CRS extraction for displaying on Map
  crs_for_map <- sf::st_crs(flood_areas_list[[1]])$input

  #Creating ggplot2 map-frame with all elements
  mapframe <- ggplot2::ggplot() +

    #Display Basemap
    tidyterra::geom_spatraster_rgb(data = basemap_reprojected) +
    ggplot2::coord_sf() +

    #Plotting and Labeling Flood Areas, automatically coloring areas based on viridis color scale implemented in ggplot
    ggplot2::geom_sf(data = combined_flood_areas, ggplot2::aes(fill = date), lwd = 0)+
    ggplot2::scale_fill_viridis_d(labels = combined_flood_areas$label)+

    #Display AOI boundary
    ggplot2::geom_sf(data = aoi_crs_of_rasters, ggplot2::aes(color = " "), fill = NA, lwd = 0.8) +

    #Display AOI boundary in legend, color definition
    ggplot2::scale_color_manual(values = c(" " = "darkgrey")) +

    #Display scale bar and north arrow
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering)+

    #Display all remaining labels
    ggplot2::labs(title = title, x = x_axis, y = y_axis, fill = name_of_flood_areas, color = name_of_aoi_in_legend,
                  caption = paste0("Coordinate Reference System: ", crs_for_map, "\n", caption_text))+

    #Adjust text element positions and sizes
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5), axis.title = ggplot2::element_text(size = 14), legend.title = ggplot2::element_text(size = 14),
                   plot.caption = ggplot2::element_text(hjust = 0))

  #Plotting the Map
  base::plot(mapframe)

  #Creating folder for exporting Map
  flood_map_directory <- file.path(getwd(), "flood_map")

  if (!dir.exists(flood_map_directory)) {
    dir.create(flood_map_directory)
  }

  message("Saving Map in: ", flood_map_directory)

  #Exporting Map in Tiff, PNG and PDF formats for different use cases
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_file_name, ".TIF" ),  dpi = 320, units = "px", limitsize = FALSE))
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_file_name, ".png" ),  dpi = 320, units = "px", limitsize = FALSE))
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_file_name, ".pdf" ),  dpi = 320, units = "px", limitsize = FALSE))

  #Flushing basemap cache and deleting the temporary basemap folder because of issues that would occur otherwise (function would only run once without local drive issues otherwise)
  basemaps::flush_cache()
  unlink(basemap_temp_directory, recursive = TRUE)

  message("Function Complete!")
}


