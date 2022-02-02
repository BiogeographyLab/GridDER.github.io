
# this function is used to generate(simulate) a grid system based on metadata
# we have used our metadata to simulate some grid systems (2_simulate_grid.Rmd).

#' Create grid
#'
#' @param res_x A numeric input of longitude resolution
#' @param res_y A numeric input of latitude resolution
#' @param unit Character vector of unite in minute ("minute") or kilometre ("km")
#' @param flag_crs Logical. Flag the CRS
#' @param extent_unit Character vector. It can be "crs_web_extent" for... or "crs_countryPolygon" for..
#' @param input_extent
#' @param country Character vector of countries. Ex.: "Germany"
#' @param crs_num Character of CRS code . Ex.: "4326"
#' @param crs_obj
#' @param flag_offset
#' @param flag_maskByCountry
#' @param flag_buffer
#' @param flag_loadCountryPolygon
#'
#' @return
#' @export
#' @import raster sp
#'
#' @examples
#'
#'
generateGRID = function(res_x = 10,
                        res_y = 10,
                        unit = "km",# minute
                        #num_x = 10,
                        #num_y = 5,
                        flag_crs=TRUE,
                        extent_unit= NULL,#"crs_web_extent",# "crs_countryPolygon"
                        input_extent = NULL,
                        country = NULL,#"Germany",
                        crs_num = NULL,
                        crs_obj = NULL,
                        #flag_maskToCountry=NULL,
                        flag_offset=NULL,
                        flag_maskByCountry=FALSE,
                        flag_buffer=2,
                        #flag_center=FALSE
                        flag_loadCountryPolygon = TRUE
){
  # res_x = 10
  # res_y = 6
  # unit = "minute"
  # crs_num = "4326"
  # flag_crs=TRUE
  # extent_unit= "crs_countryPolygon"
  # country = "Germany"

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
  if (unit == "minute"){
    sizex =  res_x/60
    sizey =  res_y/60
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
    country_shp = raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
    #one_country = subset(country_shp,ADMIN==country)
    one_country = subset(country_shp,ADMIN %in% country) # this would consider multiple countries

    one_country = spTransform(one_country,crs_grid)
    #grid_xy = crop(grid_xy,extent(one_country) )
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
      crs_ext = find_CRS_extent(crs_num)
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

  # turn the corner to center
  if(TRUE){
    gradient_x = gradient_x + sizex/2
    gradient_y = gradient_y + sizey/2
  }
  grid_xy = expand.grid(gradient_x,gradient_y)
  grid_xy = data.frame(grid_xy)
  names(grid_xy) = c("x","y")
  sp::coordinates(grid_xy) <- ~ x +y
  raster::crs(grid_xy) = crs_grid


  # here turn the grid corners into polygons
  sp::gridded(grid_xy) = TRUE
  one_grid_ly <- raster::raster(grid_xy)
  one_grid_polygon = raster::rasterToPolygons(one_grid_ly, dissolve = F)
  raster::crs(one_grid_polygon) = raster::crs(grid_xy)


  if(flag_maskByCountry){
    #country_shp = shapefile("D:/Google Drive/1_osu_lab/GISDATA/naturalEarth/country/ne_50m_admin_0_countries.shp")
    #one_country = subset(country_shp,ADMIN==country)
    #one_country = spTransform(one_country,crs(crs_grid))
    #grid_xy = crop(grid_xy,extent(one_country) )
    raster::crs(one_country) = raster::crs(grid_xy)

    #one_country = subset(country_shp,ADMIN==country)
    #one_country = spTransform(one_country,crs(crs_grid))

    # default add a buff of 2 res
    if(!is.null(flag_buffer) ){
      #one_country_copy = one_country
      one_country = raster::buffer(one_country,width = max(sizex,sizey)*flag_buffer )
      #one_country@data = one_country_copy@data
    } else{
      one_country = raster::buffer(one_country,width = 0 )
    }
    temp_over = sp::over(one_grid_polygon,one_country)
    length(temp_over)
    one_grid_polygon_masked = one_grid_polygon[!is.na(temp_over),]
    #shapefile(one_grid_polygon_masked,file="data/2_clean_grid/grid_ID_2_polygon_masked.shp")

    one_grid_polygon = one_grid_polygon_masked
    # turn polygon into center points
    #one_grid_center_masked= rgeos::gCentroid(one_grid_polygon_masked, byid=T)
    #shapefile(one_grid_center_masked,file="data/2_clean_grid/grid_ID_2_center_masked.shp")
  }
  #plot(grid_xy)
  return(one_grid_polygon)
}

