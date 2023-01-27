#' @title Infer extent
#' @description Return an extent in the order of xmin, xmax, ymin, ymax.
#' @author Xiao Feng
#'
#' @param method Character. A string, country_extent, crs_extent, occ_extent
#' @param country Character. A string of country
#' @param occ Spatial points
#' @param crs_gridCharacter EPSG code for a coordinate reference system
#' @param flag_adjust_by_res Logical. True or False to adjust by resolution
#' @param res_x Numeric. X resolution
#' @param res_y Numeric. Y resolution
#' @param flag_adjust_origin Boolean parameter for adjust the origin of the grid
#' @param flag_unit Character. A a string; default value is meter "meter". It also can "degree","minute","second"
#
#'
#'
#' @import raster rgdal
#' @import sp
#'
infer_extent <- function(method = "",
                         country = NULL,
                         occ = NULL,
                         crs_grid = NULL,
                         flag_adjust_by_res = FALSE,
                         res_x = NULL,
                         res_y = NULL,
                         flag_adjust_origin = (method == "occ_extent"),
                         flag_unit = "meter") {
  if (method == "country_extent") {
    if (!is.null(country) & !is.null(crs_grid)) {
      if (grepl(",", country)) {
        country <- gsub('"', "", country)
        country <- strsplit(country, ",")[[1]]
      }
      data(ne_10m_admin_0_countries, package = "GridDER", envir = environment())
      country_shp <- ne_10m_admin_0_countries
      # country_shp = load("data/ne_10m_admin_0_countries.rda")
      one_country <- subset(country_shp, ADMIN %in% country)
      one_country <- spTransform(
        one_country,
        crs(paste0(
          "+init=epsg:",
          crs_grid
        ))
      )
      ext_temp <- raster::extent(one_country)
      crs_ext_full <- rep(NA, 4)
      crs_ext_full[1] <- ext_temp[1]
      crs_ext_full[2] <- ext_temp[3]
      crs_ext_full[3] <- ext_temp[2]
      crs_ext_full[4] <- ext_temp[4]

      if (flag_adjust_by_res) {
        crs_ext_full[1] <- crs_ext_full[1] - crs_ext_full[1] %% res_x
        crs_ext_full[2] <- crs_ext_full[2] - crs_ext_full[2] %% res_y
        crs_ext_full[3] <- crs_ext_full[3] - crs_ext_full[3] %% res_x
        crs_ext_full[4] <- crs_ext_full[4] - crs_ext_full[4] %% res_y
      }
    } else {
      print("Please provide country name/s, and crs of the grid system")
    }
  }

  if (method == "crs_extent") {
    crs_ext_full <- find_crs_extent(crs_grid)

    if (flag_adjust_by_res) {
      crs_ext_full[1] <- crs_ext_full[1] - crs_ext_full[1] %% res_x
      crs_ext_full[2] <- crs_ext_full[2] - crs_ext_full[2] %% res_y
      crs_ext_full[3] <- crs_ext_full[3] - crs_ext_full[3] %% res_x
      crs_ext_full[4] <- crs_ext_full[4] - crs_ext_full[4] %% res_y
    }
  }

  if (method == "occ_extent") {
    if (!is.null(occ)) {
      occ_prj <- spTransform(occ, crs_grid)
      ext_temp <- raster::extent(occ_prj)
      crs_ext_full <- rep(NA, 4)
      crs_ext_full[1] <- ext_temp[1]
      crs_ext_full[2] <- ext_temp[3]
      crs_ext_full[3] <- ext_temp[2]
      crs_ext_full[4] <- ext_temp[4]


      if (flag_adjust_origin) {
        if (!is.null(res_x) & !is.null(res_y)) {
          filters <- c(res_x, res_y)
        } else {
          filters <- sort(c(10^(2:10), 10^(3:10) / 2))
        }
        temp_origin <- infer_origin(
          input_occ = occ_prj,
          fun = min,
          flag_unit = flag_unit,
          filters = filters
        )
        crs_ext_full[1] <- temp_origin[1]
        crs_ext_full[2] <- temp_origin[2]
      }
    } else {
      print("Please provide occurrences as a spatial object")
    }
  }

  return(crs_ext_full)
}
