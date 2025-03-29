# https://www.etiennebacher.com/posts/2023-05-09-making-post-requests-with-r/#making-post-requests-from-r
# https://dataspace.copernicus.eu/news/2023-9-28-accessing-sentinel-mission-data-new-copernicus-data-space-ecosystem-apis
# https://documentation.dataspace.copernicus.eu/APIs/OData.html#list-of-odata-query-attributes-by-collection
# https://cran.r-project.org/web/packages/httr/httr.pdf
# https://www.dataquest.io/blog/apply-functions-in-r-sapply-lapply-tapply/
# https://gis.stackexchange.com/questions/233670/sentinel2-get-jpeg200-bands-only



S2_data_download <- function(username, password, start_date, end_date, aoi, cloud_cover_percent = 30, number_of_results = 2) {

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


  # From aoi.shp/gpkg create Bounding Box and convert to WKT format for the query
  if(tools::file_ext(aoi) != "shp" & tools::file_ext(aoi) != "gpkg") warning("The provided AOI file needs to be in format .shp or .gpkg!")

  aoi <- sf::st_read(aoi)
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

  granule_IDs <- sapply(X = granule_json_data_content$value, function(X) X$Id)

  #print(granule_IDs)
  #print(request_string_no_spaces)

  headers <- add_headers(Authorization = glue("Bearer {access_token}"))


  # get bands for letting user choose granules based on AOI
  for (granule_ID in granule_IDs) {

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


    B02 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B02})/$value"), headers, write_disk(paste0(getwd(), glue::glue("/{granule_ID}_B02.jp2")), overwrite = T))
    B03 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B03})/$value"), headers, write_disk(paste0(getwd(), glue::glue("/{granule_ID}_B03.jp2")), overwrite = T))
    B04 <- httr::GET(url = glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_ID})/Nodes({nodes_1_content})/Nodes(GRANULE)/Nodes({nodes_2_content})/Nodes(IMG_DATA)/Nodes(R10m)/Nodes({nodes_5_content_B04})/$value"), headers, write_disk(paste0(getwd(), glue::glue("/{granule_ID}_B04.jp2")), overwrite = T))

  }


  granule_urls <- matrix(glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({granule_IDs})"))

  print(granule_urls)



}






