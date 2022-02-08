#' @title Compute standard deviation
#' @description Given a grid system and environmental data this function compute the standard deviation for each feature of the grid system.
#'
#' @param x ee$Image or ee$ImageCollection objects with a single band.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
#' @param by Numerical input. Numbers of features.
#' @param scale A nominal scale in meters of the Image projection to work in. By default 1000.
#'
#' @return The function returns a sf object with column called "elevation" with standard deviation for each feature.
#' @note The functions with prefix "ee_" is based on rgee, a package for interacting with Google Earth Engine (GEE). To run this functions or anything relates to GEE, users must have:
#'  \itemize{
#' \item{Google account with Earth Engine activated}
#' \item{Python >= v3.5}
#' \item{EarthEngine Python API (Python package)}
#' }
#' If the strict dependencies are not installed, rgee just will not work. It highly recommended seeing the installations and activation instructions in \href{https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html}{rgee} documentation
#' @seealso \href{https://csaybar.github.io/rgee-examples/}{rgee examples}
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the grid system of interest
#' gridID_26 <- raster::shapefile("./data/South_Africa/grid_id_26/v2_grid_id_26.shp") |>
#' sf::st_as_sf()
#'
#' # Load ImageCollection of interest
#' nasadem<- rgee::ee$Image('NASA/NASADEM_HGT/001')$select('elevation')
#'
#' # Countries Map Base
#' base_map <- raster::shapefile("./data/0_basemap/ne_10m_admin_0_countries.shp") |>
#' sf::st_as_sf()
#'
#' #Select a region of interest and simplify the geometries
#' ZA_gridID_26 <- base_map |>
#' dplyr::filter(ADMIN=="South Africa") |>
#' sf::st_intersection(gridID_26) |>
#' dplyr::select(ADMIN, geometry) |>
#' rmapshaper::ms_simplify(keep = 0.001,keep_shapes = TRUE) |>
#' rgee::sf_as_ee()
#'
#' # Compute standard deviation
#' stdDev_stat <-  ee_stdDev_stat(x = nasadem,y = ZA_gridID_26)
#' }
ee_stdDev <- function(x, y, by = 1000,scale = 1000) {
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
