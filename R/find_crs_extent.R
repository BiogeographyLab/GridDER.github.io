#' @title  Find extent given a coordinate reference system.
#' @description This function extracting the extent of coordinate reference systems (crs) from \href{https://epsg.io/}{epsg website}. The extent is required for grid_generation function.
#' @author Xiao Feng
#'
#' @param crs_num Character vector of crs number, e.g. "4326"
#'
#' @return The function returns the extent bounds (xmin,ymin,xmax,ymax)
#' @export
#' @import rvest data.table
#'
#' @examples
#' \dontrun{
#' crs <- find_CRS_extent(crs_num = "2154")
#' }

find_crs_extent <- function(crs_num = "2154") {
  library(rvest)
  simple <- read_html(paste0("https://epsg.io/", crs_num))
  temp1 <- html_text(html_nodes(html_nodes(simple, css = "p"),
    css = "[class=caption]"
  ))

  if (any(grepl("Projected bounds:", temp1))) {
    # which(temp1 == "Projected bounds:")
    temp1 <- html_text(html_nodes(
      html_nodes(simple, css = "[class=\"col3 minimap-pad\"]"),
      "p"
    ))
    temp2 <- temp1[grep("Projected bounds", temp1)]

    temp3 <- strsplit(temp2, "\n")[[1]]
    xmin <- as.numeric(strsplit(trimws(temp3[3]), " ")[[1]][1])
    ymin <- as.numeric(strsplit(trimws(temp3[3]), " ")[[1]][2])
    xmax <- as.numeric(strsplit(trimws(temp3[4]), " ")[[1]][1])
    ymax <- as.numeric(strsplit(trimws(temp3[4]), " ")[[1]][2])
  } else if (any(grepl("WGS84 bounds:", temp1))) {
    temp1 <- html_text(html_nodes(
      html_nodes(simple, css = "[class=\"col3 minimap-pad\"]"),
      "p"
    ))
    temp2 <- temp1[grep("WGS84 bounds:", temp1)]

    temp3 <- strsplit(temp2, "\n")[[1]]
    xmin <- as.numeric(strsplit(trimws(temp3[6]), " ")[[1]][1])
    ymin <- as.numeric(strsplit(trimws(temp3[6]), " ")[[1]][2])
    xmax <- as.numeric(strsplit(trimws(temp3[7]), " ")[[1]][1])
    ymax <- as.numeric(strsplit(trimws(temp3[7]), " ")[[1]][2])
  }


  # temp3 = strsplit(temp2, "\n")[[1]]
  # xmin = as.numeric(strsplit(glue::trim(temp3[3]), " ")[[1]][1])
  # ymin = as.numeric(strsplit(glue::trim(temp3[3]), " ")[[1]][2])
  # xmax = as.numeric(strsplit(glue::trim(temp3[4]), " ")[[1]][1])
  # ymax = as.numeric(strsplit(glue::trim(temp3[4]), " ")[[1]][2])
  return(c(xmin, ymin, xmax, ymax))
}
