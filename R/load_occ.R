#' @title Load Occurrence Records
#' @description This function read in a GBIF dataset, then return the unique coordinates in spatial format.
#' @author Xiao Feng
#'
#' @param path Character vector. Path of occurrences data, should have decimalLongitude & decimalLatitude.
#' @param crs_num Character. Crs of the output spatial data points.
#'
#' @return Unique records.
#' @export
#'
#' @import raster sp
#' @import data.table
#' @examples
#' \dontrun{
#' # Will change. Not run!
#' unique_records <- load_occ("data-raw/Netherlands/gridID_24/0047643-210914110416597.csv")
#' }
load_occ = function(path,crs_num=NULL){
  library(raster)
  library(sp)
  library(data.table)
  # temp_occ = data.table::fread(path,sep="\t",header = T)
  temp_occ = data.table::fread(path,header = T)

  sel_col = c("decimalLatitude","decimalLongitude")
  temp_occ_narrow = unique(temp_occ[,..sel_col])
  temp_occ_narrow = temp_occ_narrow[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
  sp::coordinates(temp_occ_narrow) <- ~ decimalLongitude + decimalLatitude
  crs(temp_occ_narrow) = crs("+init=epsg:4326")
  if (! is.null(crs_num) ){
    tempcrs = CRS(paste0("+init=epsg:",crs_num))
    temp_occ_narrow = spTransform(temp_occ_narrow,tempcrs)
  }
  rm(temp_occ)
  return(temp_occ_narrow)
}
