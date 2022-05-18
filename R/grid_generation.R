#' @title  Generate a grid system
#' @description This function is used to generate (simulate) a grid system based on metadata. We have used our metadata to simulate some grid systems (2_simulate_grid.Rmd).
#' @author Xiao Feng
#'
#' @param res_x A numeric input of longitude resolution.
#' @param res_y A numeric input of latitude resolution.
#' @param unit Character vector of unite in arc-minute ("minute") or kilometre ("km").
#' @param flag_crs Logical. Flag the CRS.
#' @param extent_unit Character vector. It can be "crs_countryPolygon", "crs_web_extent", "empirical_occ_extent", "shapefile_extent", "shifted_shapefile_extent".
#' @param input_extent
#' @param country Character vector of country name where the grid system is from, e.g. "Germany", it can be used to extract spatial extent and the final grid could be masked by country polygons.
#' @param crs_num Character, which is a crs number for the grid system, e.g. "4326".
#' @param crs_obj An crs object for the grid system.
#' @param flag_offset Do any adjustment of the grid system, a vector of two numbers, represent adjustment of the origin along x,y.
#' @param flag_maskByCountry Logical. Mask the grid system by country polygon.
#' @param flag_buffer Draw an additional buffer (e.g. 2 additional grids) along the simulated grid system, this can help cover the areas along the coastal line.
#' @param flag_loadCountryPolygon Load Natural Earth 10meter country polygon ne_10m_admin_0_countries.shp).
#'
#' @return The function returns a shapefile.
#' @export
#' @import raster sp
#' @import gsheet data.table
#'
#' @examples
#'  \dontrun{
#' # Will change. Not run!
#' # Load a shapefile of countries
#' country_shp <- raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
#'
#' #Load metadata information
#' grid_metadata <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1mSad3lUL5eMgtwH311Bo8PUWIqJkvgxUfG99NiuRLjo/edit#gid=166945453') |>
#' as.data.frame() |>
#' subset(!grepl("del_", grid_ID))
#'
#' # Select the Netherlands data
#' iii= which(grid_metadata$grid_ID==24)
#' grid_metadata$country_name[iii]
#' crs_num = grid_metadata$crs_code[iii]
#' crs_num
#'
#' if(grid_metadata$extent_unit[iii]=="empirical_occ_extent"){
#' empirical_occ_extent = gsub('"',"",grid_metadata$extent[[iii]])
#' empirical_occ_extent = as.numeric(strsplit(empirical_occ_extent,",")[[1]])
#' }
#'
#' # Simulate grid system
#' NL_grid <- occMagnet::generateGRID(res_x = as.numeric(grid_metadata$resolution_x[iii]),
#'                 res_y = as.numeric(grid_metadata$resolution_y[iii]),
#'                 unit = grid_metadata$resolution_unit[iii],
#'                 flag_crs=TRUE,
#'                 extent_unit=grid_metadata$extent_unit[iii],
#'                 input_extent=empirical_occ_extent,
#'                 country = grid_metadata$country_name[iii],
#'                 crs_num = grid_metadata$crs_code[iii],
#'                 flag_maskByCountry = T,
#'                 flag_buffer=10
#'                 )
#'
#' # Load occurrence data
#' gbif_occ_proj <- load_occ("data-raw/Netherlands/gridID_24/0047643-210914110416597.csv") |>
#' spTransform(CRS(paste0("+init=epsg:",crs_num)))
#'
#' # Plot
#' plot(NL_grid)
#' plot(NL_grid,
#' xlim=c(extent(gbif_occ_proj)[1],
#'        extent(gbif_occ_proj)[1]+3),
#'        ylim=c(extent(gbif_occ_proj)[3],
#'               extent(gbif_occ_proj)[3]+3))
#'plot(gbif_occ_proj,add=T,col="red")
#'
#'
#'  }
grid_generation = function(res_x = 10,
                        res_y = 10,
                        unit = "km",
                        flag_crs=TRUE,
                        extent_unit= NULL,
                        input_extent = NULL,
                        country = NULL,
                        crs_num = NULL,
                        crs_obj = NULL,
                        flag_offset=NULL,
                        flag_maskByCountry=FALSE,
                        flag_buffer=2,
                        flag_loadCountryPolygon = TRUE
){

  if(grepl(",",country)){
    country = gsub('"',"",country)
    country = strsplit(country,",")[[1]]
  }



  if( !is.null(crs_num) ){
    crs_grid = sp::CRS(paste0("+init=epsg:",crs_num))
  } else{
    crs_grid = crs_obj
  }
  if (unit == "km"){
    sizex =  res_x*1000
    sizey =  res_y*1000
  }
  if (unit == "meter" | unit == "m"){
    sizex =  res_x
    sizey =  res_y
  }
  if (unit == "minute"){
    sizex =  res_x/60
    sizey =  res_y/60
  }
  if (unit == "second"){
    sizex =  res_x
    sizey =  res_y
  }
  if (unit == "degree"){
    sizex =  res_x/3600
    sizey =  res_y/3600
  }
  print(sizex)
  print(sizey)
  print(unit)

  if(extent_unit=="crs_countryPolygon"){
    flag_loadCountryPolygon= TRUE
  }
  if(flag_maskByCountry){
    flag_loadCountryPolygon= TRUE
  }
  if(flag_loadCountryPolygon){

    data(ne_10m_admin_0_countries, package = "gridder", envir=environment() )
    ne_10m_admin_0_countries

    country_shp = ne_10m_admin_0_countries
    #country_shp = load("data/ne_10m_admin_0_countries.rda", envir=system.file(package = "gridder"))
    #country_shp = load("data/ne_10m_admin_0_countries.rda", envir=system.file(package = "gridder"))
    #country_shp = load(country_shp)
    #country_shp = raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
    #country_shp = data("data/ne_10m_admin_0_countries.rda", envir=environment())
    one_country = subset(country_shp, ADMIN %in% country)

    one_country = spTransform(one_country,crs_grid)

  }

  if(flag_crs){
    if( extent_unit=="crs_countryPolygon"   ){
      print("crs_countryPolygon" )
      ext_temp = raster::extent(one_country)

      crs_ext_full = rep(NA,4)
      crs_ext_full[1] = ext_temp[1]
      crs_ext_full[2] = ext_temp[3]
      crs_ext_full[3] = ext_temp[2]
      crs_ext_full[4] = ext_temp[4]

      crs_ext_full[1] = crs_ext_full[1] - crs_ext_full[1]%%sizex
      crs_ext_full[2] = crs_ext_full[2] - crs_ext_full[2]%%sizey
      crs_ext_full[3] = crs_ext_full[3] - crs_ext_full[3]%%sizex
      crs_ext_full[4] = crs_ext_full[4] - crs_ext_full[4]%%sizey

      print(crs_ext_full )
    }

    if(!is.null(crs_num)) {
      if (crs_num == "4326" & !exists("crs_ext_full") ){
        crs_ext_full = c(-180,-90,180,90)
      }
    }

    if(extent_unit=="crs_web_extent"){
      crs_ext = find_crs_extent(crs_num)
      crs_ext_full = crs_ext
      crs_ext_full[1] = crs_ext[1] - crs_ext[1]%%sizex
      crs_ext_full[2] = crs_ext[2] - crs_ext[2]%%sizey
      crs_ext_full[3] = crs_ext[3] - crs_ext[3]%%sizex
      crs_ext_full[4] = crs_ext[4] - crs_ext[4]%%sizey
    }
    if(extent_unit=="empirical_occ_extent"){
      if(!is.null(input_extent) ){
        crs_ext_full = input_extent
      }
    }
  }
  if(!is.null(flag_offset)){
    crs_ext_full[1] = crs_ext_full[1] - flag_offset[1]
    crs_ext_full[2] = crs_ext_full[2] - flag_offset[2]
  }
  print(crs_ext_full)
  gradient_x =   seq(from=crs_ext_full[1], to= crs_ext_full[3],by = sizex)
  gradient_y =   seq(from=crs_ext_full[2], to= crs_ext_full[4],by = sizey)
  #}

  if(TRUE){
    gradient_x = gradient_x + sizex/2
    gradient_y = gradient_y + sizey/2
  }
  grid_xy = expand.grid(gradient_x,gradient_y)
  grid_xy = data.frame(grid_xy)
  names(grid_xy) = c("x","y")
  sp::coordinates(grid_xy) <- ~ x +y
  raster::crs(grid_xy) = crs_grid


  sp::gridded(grid_xy) = TRUE
  one_grid_ly <- raster::raster(grid_xy)
  one_grid_polygon = raster::rasterToPolygons(one_grid_ly, dissolve = F)
  raster::crs(one_grid_polygon) = raster::crs(grid_xy)


  if(flag_maskByCountry){
      if(!is.null(flag_buffer) ){
      one_country = raster::buffer(one_country,width = max(sizex,sizey)*flag_buffer )
    } else{
      one_country = raster::buffer(one_country,width = 0 )
    }
    temp_over = sp::over(one_grid_polygon,one_country)
    length(temp_over)
    one_grid_polygon_masked = one_grid_polygon[!is.na(temp_over),]
    one_grid_polygon = one_grid_polygon_masked
    }
  return(one_grid_polygon)
}

