#' @title Compute standard deviation
#' @description Given a grid system and environmental data this function compute the standard deviation for each feature of the grid system.
#'
#' @param x ee$Image or ee$ImageCollection objects with a single band.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
#' @param by Numerical input. Numbers of features.
#' @param scale A nominal scale in meters of the Image projection to work in. By default 1000.
#'
#' @return a sf object.
#' @export
#' @import dplyr rgdal rgee rmapshaper
#'
#' @examples
#' \dontrun{
#' # Read a base map and transform in sf object
#' base_map <- raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp) |>
#' sf::st_as_sf()
#'
#' # Read a grid system transform in sf object
#' gridID_26 <- raster::shapefile("data/South_Africa/grid_id_26/v2_grid_id_26.shp") |>
#' sf::st_as_sf()
#'
#' # Select a region of interest
#' South_Africa <- base_map |>
#' dplyr::filter(ADMIN=="South Africa") |>
#' sf::st_intersection(gridID_26) |>
#' dplyr::select(ADMIN, geometry)
#'
#' # Simplify the geometry
#' South_Africa_s <-  South_Africa |>
#' rmapshaper::ms_simplify(keep = 0.001,keep_shapes = TRUE)
#'
#' # Initilize rgee
#' rgee::ee_Initialize()
#'
#' # sf object to featurecollection
#' SA_grid26_ee <- rgee::sf_as_ee(South_Africa_s)
#'
#' # Load image of interest
#' nasadem<- rgee::ee$Image('NASA/NASADEM_HGT/001')$select('elevation')
#'
#' # Compute stdDev
#' stdDev <-  ee_stdDev(x = nasadem,y = SA_grid26_ee)
#' }
ee_stdDev = function(x, y, by = 1000,scale = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$stdDev(),
        y = ee_value_layer,
        scale = scale,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$stdDev(),
        scale = scale,
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
