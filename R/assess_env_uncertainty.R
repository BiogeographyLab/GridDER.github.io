#' @title Compute variation of environmental conditions.
#' @description Given a grid system and environmental data this function compute the standard deviation for each feature of the grid system.
#' @author Tainá Rocha
#'
#' @param x ee$Image or ee$ImageCollection objects with a single band.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
#' @param by Numerical input. Numbers of features.
#' @param scale A nominal scale in meters of the Image projection to work in. By default 1000.
#'
#' @return The function returns a sf object with column called "elevation" with standard deviation for each feature.
#' @note The functions with prefix "ee_" is based on rgee package for interacting with Google Earth Engine (GEE). To run these functions or anything related to rgee/GEE, users must have:
#'  \itemize{
#' \item{Google account with Earth Engine activated}
#' \item{Python >= v3.5}
#' \item{EarthEngine Python API (Python package)}
#' }
#' If the strict dependencies are not installed, rgee just will not work. It highly recommended seeing the installations and activation instructions in \href{https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html#6_Installation}{rgee} documentation
#' @seealso \href{https://csaybar.github.io/rgee-examples/}{rgee examples} and \href{https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html}{rgee cran documentation}.
#' @export
#'
#' @import rgee rmapshaper
#' @import raster sf
#'
#'
#' @examples
#' \dontrun{
#' # As this function is based on rgee package, the following commands must be executed:
#'
#' # Create virtual environmental at local machine to install the necessary dependencies
#' rgee::ee_install()
#'
#' # Initialize
#' rgee::ee_Initialize()
#'
#' # Load ImageCollection of interest i.e, an environmental layer
#'
#' nasadem<- rgee::ee$Image('NASA/NASADEM_HGT/001')$select('elevation')
#'
#' # Hypothetical grid system
#'lat_lon_grid <- structure(list(ID = 758432:758443,
#'                               lat = c(-14.875, -14.875, -14.625, -14.625, -14.875, -14.875, -14.625, -14.625, -14.375, -14.375, -14.125, -14.125),
#'                               lon = c(-42.875, -42.625, -42.625, -42.875, -42.375, -42.125, -42.125, -42.375, -42.375, -42.125, -42.125, -42.375)),
#'                          class = "data.frame", row.names = c(NA, -12L))
#'
#' grid_to_raster <- raster::rasterFromXYZ(lat_lon_grid [, c('lon', 'lat', 'ID')], crs = '+proj=longlat +datum=WGS84 +no_defs')
#'
#' grid <- raster::rasterToPolygons(grid_to_raster, fun=NULL, na.rm=TRUE, dissolve=FALSE) |>
#'  sf::st_as_sf() |>
#'  rgee::sf_as_ee()
#'
#' # Plot
#' rgee::Map$addLayer(grid)
#'
#' # Compute standard deviation
#' std_dev <- gridder::assess_env_uncertainty(x= nasadem, y= grid)
#' }
#'
assess_env_uncertainty <- function(x, y, by = 1000, scale = 1000) {
  
  #add by xf
  if( "ee.image.Image" %in% class(x)  ){

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
      
     
    # add by xf
    }# end of for loop
  }# end of gee processing 
   else  {
    #do something locally    
    for (i in seq(1, y_len, 1)) { # loop through every grid polygon
      temp_extractedValue = raster::extract(x,y[i,])
      temp_sd = sd (temp_extractedValue)
      temp_sd = data.frame(id=i,sd=temp_sd)
      if(i==1){
        dataset = temp_sd
      } else{
        dataset = rbind(dataset, temp_sd)
      }
    }# end of for loop
  } # end of local processing
    
    
    
    
    
  return(dataset)
}
