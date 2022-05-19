#' @title Adjust the Grid System
#' @description Given a grid system and occurrences, this function tries to shift the grid system, in order to minimize the distance between the grid system and input occurrences.
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
grid_adjustment <- function(input_grid,
                            input_occ_grid) {
  if (class(input_grid) == "character") {
    input_grid <- raster::shapefile(input_grid)
  }

  if (grepl("csv", input_occ_grid)) {
    one_occ <- load_occ(input_occ_grid)
  }
  if (grepl("shp", input_occ_grid)) {
    one_occ <- shapefile(input_occ_grid)
  }
  input_occ_grid <- spTransform(one_occ, crs(input_grid))

  temp_over <- over(input_grid, input_occ_grid)
  if (is.null(dim(temp_over))) {
    input_grid_subset <- input_grid[!is.na(temp_over), ]
  } else {
    input_grid_subset <- input_grid[!is.na(temp_over[, 1]), ]
  }

  input_grid_subset_center <- rgeos::gCentroid(input_grid_subset, byid = T)
  input_occ_grid <- input_occ_grid[input_grid_subset, ]


  d_grid_occ <- raster::pointDistance(
    p1 = input_occ_grid,
    p2 = input_grid_subset_center,
    lonlat = F, allpairs = T
  )
  d_grid_occ_minI <- apply(d_grid_occ, 1, FUN = which.min)

  plot(input_occ_grid[1:2, ])
  plot(input_grid_subset_center[d_grid_occ_minI[1:2], ], add = T, col = "red")


  empirical_shift <- input_occ_grid@coords - input_grid_subset_center@coords[d_grid_occ_minI, ]
  empirical_shift_mean <- apply(empirical_shift, 2, FUN = mean)


  output_grid <- raster::shift(input_grid,
    dx = empirical_shift_mean[1],
    dy = empirical_shift_mean[2]
  )
  return(output_grid)
}
