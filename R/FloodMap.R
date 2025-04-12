#https://rpubs.com/paul4forest/ggplot_lapply
#https://stackoverflow.com/questions/77533388/using-lapply-to-create-multiple-graphs-in-ggplot2-based-on-group-and-output-the
#https://stackoverflow.com/questions/34936994/how-to-use-lapply-with-ggplot2-while-indexing-variables

FloodMap <- function(aoi, title = "", map_name = "my_flood_map", name_of_aoi_in_legend = "Area of Interest", name_of_flood_areas = "Flood Areas", x_axis = "Longitude", y_axis = "Latitude", sort_by_flood_size = FALSE, path_to_SFMI_data = file.path(getwd(), "SFMI_data")) {

  #Reading in flood area vector files
  flooded_areas_path <- list.files(path = file.path(getwd(), "SFMI_data"), pattern = "\\.gpkg$", full.names = TRUE, recursive = FALSE)

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

  #Flooded Area Percent Calculation in Relation to AOI Area
  flood_area_percent <- list()

  for (i in seq_along(flood_areas_list)) {
    flood_area_percent[[i]] <- (flood_areas_list[[i]]$flooded_area) / (aoi_area) * 100
  }


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

  #Colors extracted from viridis with as large as possible spread for distinct color visualization
  colors <- viridis::viridis(n = length(flood_areas_list), begin = 0, end = 1)

  #Extracting dates from flood area files
  dates <- lapply(seq_along(flood_areas_list), function(i) {
    flood_areas_list[[i]]$date
  })

  #CRS extraction for displaying on Map
  crs_for_map <- sf::st_crs(flood_areas_list[[1]])$input

  #Creating ggplot2 map-frame with all elements
  mapframe <- ggplot2::ggplot() +

    #Display Basemap
    tidyterra::geom_spatraster_rgb(data = basemap_reprojected) +
    ggplot2::coord_sf() +

    #Display flood areas
    lapply(seq_along(flood_areas_list), function(i) {
      ggplot2::geom_sf(data = flood_areas_list[[i]], ggplot2::aes(fill = colors[i]), alpha = 1, lwd = 0)
    }) +

    #Display flood areas color and label definitions
    ggplot2::scale_fill_manual(values = colors, labels = paste(round(as.numeric(flood_area_percent), 2), "% flooded on", dates)) +

    #Display AOI boundary
    ggplot2::geom_sf(data = aoi_crs_of_rasters, ggplot2::aes(color = " "), fill = NA, lwd = 0.8) +

    #Display AOI boundary color definition
    ggplot2::scale_color_manual(values = c(" " = "darkgrey")) +

    #Display scale bar and north arrow
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering)+

    #Display all remaining labels
    ggplot2::labs(title = title, x = x_axis, y = y_axis, fill = name_of_flood_areas, color = name_of_aoi_in_legend,
                  caption = paste("Coordinate Reference System:", crs_for_map, "\nData Providers: Copernicus Sentinel-2 (processed by ESA) (2021). DOI: https://doi.org/10.5270/S2_-6eb6imz; Carto (2025). URL: https://carto.com/basemaps \nFlood Index: Farhadi et al. (2025). DOI: https://doi.org/10.1016/j.cageo.2024.105742"))+

    #Adjust text element positions and sizes
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5), axis.title = ggplot2::element_text(size = 14), legend.title = ggplot2::element_text(size = 14),
                   plot.caption = ggplot2::element_text(hjust = 0))

  #Plotting the Map
  base::plot(mapframe)

  #Creating folder for exporting Map
  flood_map_directory <- file.path(getwd(), "Flood_Map")

  if (!dir.exists(flood_map_directory)) {
    dir.create(flood_map_directory)
  }

  message("Saving Map in: ", flood_map_directory)

  #Exporting Map in Tiff, PNG and PDF formats for different use cases
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_name, ".TIF" ),  dpi = 320, units = "px", limitsize = FALSE))
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_name, ".png" ),  dpi = 320, units = "px", limitsize = FALSE))
  suppressMessages(ggplot2::ggsave(mapframe, scale = 1.5, filename = paste0(flood_map_directory, "/", map_name, ".pdf" ),  dpi = 320, units = "px", limitsize = FALSE))

  #Flushing basemap cache and deleting the temporary basemap folder because of issues that would occur otherwise (function would only run once without local drive issues otherwise)
  basemaps::flush_cache()
  unlink(basemap_temp_directory, recursive = TRUE)

  message("Function Complete!")
}


