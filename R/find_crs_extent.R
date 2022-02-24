#' @title  Find extent given a coordinate reference system.
#' @description This function extracting the extent of coordinate reference systems (crs) from \href{https://epsg.io/}{epsg website}. The extent is required for generateGRID function.
#' @author Xiao Feng
#'
#' @param crs_num Character vector of crs number, e.g. "4326"
#'
#' @return The function returns the extent bounds (xmin,ymin,xmax,ymax)
#' @export
#' @import rvest
#'
#' @examples
#' \dontrun{
#' crs <- find_CRS_extent(crs_num = "2154")
#' }
 find_crs_extent = function(crs_num = "2154"){
  library(rvest)
  simple <- read_html(paste0("https://epsg.io/",crs_num))
  temp1 = simple |>
    html_nodes(css ="p") |>
    html_nodes(css = "[class=caption]")  |>  html_text()
  which(temp1=="Projected bounds:")
  #html_nodes(css = "br")#%>%
  #html_text()

  temp1= simple |>
    html_nodes(css ='[class="col3 minimap-pad"]') |>
    html_nodes("p") |>  html_text()

  temp2 = temp1[grep("Projected bounds",temp1)]
  temp3 = strsplit(temp2,"\n")[[1]]
  xmin = as.numeric(  strsplit(trim(temp3[3])," ")[[1]][1] )
  ymin = as.numeric(  strsplit(trim(temp3[3])," ")[[1]][2] )
  xmax = as.numeric(  strsplit(trim(temp3[4])," ")[[1]][1] )
  ymax = as.numeric(  strsplit(trim(temp3[4])," ")[[1]][2] )

  return(  c(xmin,ymin,xmax,ymax ))
}
