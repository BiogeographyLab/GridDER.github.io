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
#' base_map <- raster::shapefile("./data/0_basemap/ne_10m_admin_0_countries.shp) |>
#' sf::st_as_sf()
#'
#' }
#'
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
