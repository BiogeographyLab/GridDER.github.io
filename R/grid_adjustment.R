#' @title Adjust the Grid System
#' @description Given a grid system and occurrences, this function try to shift the grid system, in order to minimize the distance between the grid system and input occurrences i.e. doing some small adjustment of the grid system.
#' @author Xiao Feng
#'
#' @param input_grid Character vector. A string with path of input grid system.
#' @param input_occ_grid Character vector. A string with path of occurrences data.
#'
#' @return The function returns the grid adjusted (shapefile).
#' @export
#' @import raster sp
#'
#' @examples
grid_adjustment = function(input_grid,
                           input_occ_grid
                           ){
  #grid_country=NA,
  #country_base_map = "data/0_basemap/ne_10m_admin_0_countries.shp",
  #country_shp = shapefile(country_base_map)
  if( class(input_grid) == "character"){
    input_grid = raster::shapefile(input_grid)
  }

  if(grepl("csv",input_occ_grid)  ){
    one_occ = load_occ(input_occ_grid)
  }
  if(grepl("shp",input_occ_grid)  ){
    one_occ = shapefile(input_occ_grid)
  }
  input_occ_grid = spTransform(one_occ,crs(input_grid))

  temp_over = over(input_grid,input_occ_grid)
  if(is.null( dim(temp_over))){
    input_grid_subset = input_grid[!is.na(temp_over),]
  } else {
    input_grid_subset = input_grid[!is.na(temp_over[,1]),]
  }

  input_grid_subset_center = rgeos::gCentroid(input_grid_subset, byid=T)
  input_occ_grid = input_occ_grid[input_grid_subset,]


  #find nearest grid center
  d_grid_occ = raster::pointDistance(p1=input_occ_grid,
                                     p2=input_grid_subset_center,
                                     lonlat = F,allpairs = T)
  d_grid_occ_minI = apply(d_grid_occ,1,FUN=which.min)

  plot(input_occ_grid[1:2,])
  plot(input_grid_subset_center[d_grid_occ_minI[1:2],],add=T,col="red")


  empirical_shift = input_occ_grid@coords - input_grid_subset_center@coords[d_grid_occ_minI,]
  empirical_shift_mean = apply(empirical_shift,2,FUN=mean)
  #decimalLongitude  decimalLatitude
  #-456.4099         141.9952

  # input_grid_subset_center_NEW = input_grid_subset_center
  # input_grid_subset_center_NEW@coords[,1] = input_grid_subset_center@coords[,1]+empirical_shift_mean[1]
  # input_grid_subset_center_NEW@coords[,2] = input_grid_subset_center@coords[,2]+empirical_shift_mean[2]
  #

  # test if the diff is smaller
  #plot(input_occ_grid[1:2,])
  #plot(input_grid_subset_center_NEW[d_grid_occ_minI[1:2],],add=T,col="red")
  #temp_empirical_shift = input_occ_grid@coords - input_grid_subset_center_NEW@coords[d_grid_occ_minI,]
  #temp_empirical_shift_mean = apply(temp_empirical_shift,2,FUN=mean)
  # decimalLongitude  decimalLatitude
  # -5.67598e-12      3.56946e-10
  #temp_diff =  temp_empirical_shift - empirical_shift

  output_grid = raster::shift(input_grid,
                              dx=empirical_shift_mean[1],
                              dy=empirical_shift_mean[2])
  #shapefile(output_grid,"xin/Shapefiles/grid_ID_36_corrected.shp")
  # plot(output_grid[1,])
  # plot(input_grid[1,],add=T,col="red")
  return(output_grid)
}
