# https://www.etiennebacher.com/posts/2023-05-09-making-post-requests-with-r/#making-post-requests-from-r
# https://dataspace.copernicus.eu/news/2023-9-28-accessing-sentinel-mission-data-new-copernicus-data-space-ecosystem-apis
# https://documentation.dataspace.copernicus.eu/APIs/OData.html#list-of-odata-query-attributes-by-collection
# https://cran.r-project.org/web/packages/httr/httr.pdf
# https://www.dataquest.io/blog/apply-functions-in-r-sapply-lapply-tapply/
# https://gis.stackexchange.com/questions/233670/sentinel2-get-jpeg200-bands-only
# https://stackoverflow.com/questions/4216753/folder-management-with-r-check-existence-of-directory-and-create-it-if-it-does
# https://stackoverflow.com/questions/50479535/cant-suppress-messages-in-blogdown-knitr
# https://stackoverflow.com/questions/22109774/r-raster-mosaic-from-list-of-rasters
# https://www.statology.org/r-add-leading-zeros/



S2_data_download <- function(username, password, start_date, end_date, aoi, condition, cloud_cover_percent = 50, number_of_results = 12) {

  # Retrieve Access Token from Copernicus Hub for later download
  access_token_retrival <- list(client_id = "cdse-public",
                                username = username,
                                password = password,
                                grant_type = "password")

  request_token <- httr::POST(url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token",
            body = access_token_retrival,
            encode = "form")


  access_token <- httr::content(request_token, "parsed")$access_token

  #print(access_token)

  print("Querying results from Copernicus...")

  # From aoi.shp/gpkg create Bounding Box and convert to WKT format for the query
  if(tools::file_ext(aoi) != "shp" & tools::file_ext(aoi) != "gpkg") warning("The provided AOI file needs to be in format .shp or .gpkg!")

  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi <- sf::st_transform(aoi, crs = "EPSG:4326")

  bbox <- sf::st_as_sfc(sf::st_bbox(aoi[1,])) # always the first feature gets selected!

  # Reformatting because of API request format deviations
  bbox_WKT <- sf::st_as_text(bbox)
  bbox_WKT_formatted <- gsub("POLYGON ", "POLYGON", bbox_WKT)
  bbox_WKT_formatted <- paste0(bbox_WKT_formatted, "'")


  # Defining missing attributes for the query for retrieving Sentinel-2 BOA data
  data_collection <- "SENTINEL-2"
  product_type <- "S2MSI2A"

  # Creating request string for getting query results
  request_string <- glue::glue("https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=Collection/Name eq '{data_collection}' and OData.CSC.Intersects(area=geography'SRID=4326;{bbox_WKT_formatted}) and Attributes/OData.CSC.DoubleAttribute/any(att:att/Name eq 'cloudCover' and att/OData.CSC.DoubleAttribute/Value le {cloud_cover_percent}.00) and Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType' and att/OData.CSC.StringAttribute/Value eq '{product_type}') and ContentDate/Start gt {start_date}T00:00:00.000Z and ContentDate/Start lt {end_date}T00:00:00.000Z&$top={number_of_results}&$orderby=PublicationDate asc")
  request_string_no_spaces <- toString(gsub(" ", "%20", request_string))

  granule_json_data <- httr::GET(request_string_no_spaces)
  granule_json_data_content <- httr::content(granule_json_data, "parsed")



  #create temp folder if it doesn't exist yet for upcoming operations
  temp_directory <- file.path(getwd(), "temp")

  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory)
  }

  # Let user choose tiles based on result
  granule_IDs <- sapply(X = granule_json_data_content$value, function(X) X$Id)

  #check if there is valid data

  if(length(granule_IDs)==0) warning("There are no Sentinel-2 tiles with your selected options!") #needs rework!!

  footprints <- sapply(X = granule_json_data_content$value, function(X) X$Footprint)

  time_stamps <- sapply(X = granule_json_data_content$value, function(X) X$OriginDate)
  time_stamps <- sapply(time_stamps, function(X) substr(X, 1, 10))

  # loop to extract the data extent of each granule for user to chose intersecting tiles to cover AOI fully
  for (footprint_ID in seq_along(footprints)) {
    footprints_conversion_1 <- gsub("geography'", "", footprints[footprint_ID])
    footprints_conversion_2 <- gsub("'", "", footprints_conversion_1)

    footprint_wkt <- sf::st_as_sfc(footprints_conversion_2)

    footprint_sf <- sf::st_sf(geometry = footprint_wkt)

    sf::st_write(footprint_sf, paste0(temp_directory, "/", stringr::str_pad(footprint_ID, width = 2, side = "left", pad = "0"),"_", granule_IDs[footprint_ID], "_footprint.gpkg"), delete_layer = TRUE, quiet = TRUE)
  }


  #creating function selecting tiles

  tile_footprint_list <- list.files(temp_directory, pattern = "\\.gpkg$", full.names = TRUE)

  tile_footprint_sf <- sapply(tile_footprint_list, sf::st_read, quiet = TRUE)

  graphics::par(mfrow = c(2, 2))

  for (i in seq_along(tile_footprint_sf)) {
    plot(tile_footprint_sf[[i]], main = paste0("Tile Nr. ", i, ", Date: ", time_stamps[i]))
    plot(aoi[1,]$geometry, add = TRUE, col = "red")
  }

  backup_granule_IDs <- granule_IDs


  tile_answer <- readline(prompt = "Select S2 tile(s) for your AOI (e.g. 1 or 3,4,6):")
  tile_answer <- unlist(strsplit((tile_answer), split = ","))

  grDevices::graphics.off()
  unlink(temp_directory, recursive=TRUE)

  selected_granules <- vector()

  if (length(tile_answer)==0) {
      warning("No tiles selected! Function will fail!")
    } else if (length(tile_answer)==1) {
      selected_granules <- backup_granule_IDs[as.numeric(tile_answer)]
    } else {
      for (selected_tiles in tile_answer) {
        selected_granules <- backup_granule_IDs[as.numeric(tile_answer)]
      }
    }

  # selected_granules <- stats::na.omit(selected_granules)
  selected_granules <- as.vector(selected_granules)

  print("Downloading selected tile(s)...")

  headers <- httr::add_headers(Authorization = glue::glue("Bearer {access_token}"))


  #create data folder if it doesn't exist yet
  temp_data_directory <- file.path(getwd(), "temp_data_directory")

  if (!dir.exists(temp_data_directory)) {
    dir.create(temp_data_directory)
  }

  #create condition subdirectory

  temp_data_condition_directory <- file.path(temp_data_directory, condition)

  if (!dir.exists(temp_data_condition_directory)) {
    dir.create(temp_data_condition_directory)
  }


  # get bands based on user choosing AOI
  for (granule_ID in selected_granules) {

    nodes_base_url <- glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes")

    nodes_1 <- httr::GET(url = nodes_base_url, headers)
    nodes_1_content <- httr::content(nodes_1)$result[[1]]$Name[1]

    nodes_2 <- httr::GET(glue::glue("{nodes_base_url}({nodes_1_content})/Nodes(GRANULE)/Nodes"))
    nodes_2_content <- httr::content(nodes_2)$result[[1]]$Name[1]

    nodes_3 <- httr::GET(glue::glue("{nodes_base_url}({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes"))
    nodes_3_content <- httr::content(nodes_3)$result[[2]]$Name[1]

    nodes_4 <- httr::GET(glue::glue("{nodes_base_url}({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes({nodes_3_content})/Nodes"))
    nodes_4_content <- httr::content(nodes_4)$result[[1]]$Name[1]

    nodes_5 <- httr::GET(glue::glue("{nodes_base_url}({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes({nodes_3_content})/Nodes({nodes_4_content})/Nodes"))
    nodes_5_content_B02 <- httr::content(nodes_5)$result[[2]]$Name
    nodes_5_content_B03 <- httr::content(nodes_5)$result[[3]]$Name
    nodes_5_content_B04 <- httr::content(nodes_5)$result[[4]]$Name
    nodes_5_content_B08 <- httr::content(nodes_5)$result[[5]]$Name

    nodes_6 <- httr::GET(glue::glue("{nodes_base_url}({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes({nodes_3_content})/Nodes(R20m)/Nodes"))
    nodes_6_content_SCL <- httr::content(nodes_6)$result[[12]]$Name

    granule_directory <- file.path(temp_data_condition_directory, granule_ID)

    if (!dir.exists(granule_directory)) {
      dir.create(granule_directory)
    }

    meta_data <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(MTD_MSIL2A.xml)/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_meta_data.xml")), overwrite = T))
    B02 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B02})/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_B02.jp2")), overwrite = T))
    B03 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B03})/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_B03.jp2")), overwrite = T))
    B04 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B04})/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_B04.jp2")), overwrite = T))
    B08 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B08})/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_B08.jp2")), overwrite = T))
    SCL <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R20m)/Nodes({nodes_6_content_SCL})/$value"), headers, httr::write_disk(paste0(granule_directory, glue::glue("/{granule_ID}_SCL.jp2")), overwrite = T))

  }
  print("Download Complete!")
}




