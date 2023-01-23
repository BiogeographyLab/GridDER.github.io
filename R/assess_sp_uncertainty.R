#' @title Assess the Spatial Uncertainty
#' @description The function computes spatial uncertainty of a grid system by calculating the distance between grid centroids toward gridded coordinates and non-gridded coordinates (e.g. iNaturalist data).

#' @author Xiao Feng
#'
#' @param input_grid Character vector. Path of a grid system (shapefile).
#' @param input_occ_grid Character vector. Path of gridded occurrences (shapefile).
#' @param input_occ_random Character vector. Path of non-gridded occurrences (inaturalist is used by default).
#' @param flag_degree Logical. Parameter passed to raster::pointDistance.
#' @param flag_plot Logical. If TRUE, coordinates (based on the crs of the grid system) should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters). Default is FALSE.
#'
#'
#' @import ggplot2 sp
#' @import raster
#'
#' @examples
#' \dontrun{
#'
#' distance <- cal_emp_G_distance(input_grid = "data/2_clean_grid/grid_ID_2_polygon_masked.shp",
#' input_occ_grid = "data/france_datasetID10/0068190-210914110416597.csv",
#' input_occ_random = "data/iNaturalist_20211205/0071565-210914110416597.csv")
#' }
assess_sp_uncertainty <- function(input_grid,
                                  input_occ_grid,
                                  input_occ_random,
                                  flag_degree = FALSE,
                                  flag_plot = FALSE) {
  if (grepl(".shp", input_occ_grid)) {
    one_occ <- shapefile(input_occ_grid)
  } else {
    one_occ <- load_occ(input_occ_grid)
  }
  if (nrow(one_occ@coords) > 10000) {
    set.seed(1)
    one_occ <- one_occ[sample(nrow(one_occ@coords), 10000), ]
  }


  input_grid <- shapefile(input_grid)
  input_occ_grid <- spTransform(one_occ, crs(input_grid))

  temp_over <- over(input_grid, input_occ_grid)
  if (is.null(dim(temp_over))) {
    input_grid_subset <- input_grid[!is.na(temp_over), ]
  } else {
    input_grid_subset <- input_grid[!is.na(temp_over[, 1]), ]
  }
  input_grid_subset_center <- rgeos::gCentroid(input_grid_subset, byid = T)


  input_occ_random_rds <- gsub(".csv", "_latlon.rds", input_occ_random)
  if (file.exists(input_occ_random_rds)) {
    iNaturalist <- readRDS("data/iNaturalist_20211205/0071565-210914110416597_latlon.rds")
  } else {
    temp_occ <- fread(input_occ_random)
    nrow(temp_occ) # 36,167,804
    sel_col <- c("decimalLatitude", "decimalLongitude")
    temp_occ_narrow <- unique(temp_occ[, ..sel_col])
    iNaturalist <- temp_occ_narrow[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
    nrow(iNaturalist) # 28,542,360

    coordinates(iNaturalist) <- ~ decimalLongitude + decimalLatitude
    crs(iNaturalist) <- crs("+init=epsg:4326")
    nrow(iNaturalist@coords) # 28,542,360
    saveRDS(iNaturalist, input_occ_random_rds)
  }

  if (identical(crs(iNaturalist), crs(one_occ))) {
    iNaturalist_subset <- crop(iNaturalist, extent(one_occ))
  } else {
    iNaturalist_subset <- crop(
      iNaturalist,
      extent(spTransform(one_occ, crs(iNaturalist)))
    )
  }
  iNaturalist_subset_prj <- spTransform(iNaturalist_subset, crs(input_grid))
  iNaturalist_subset_prj <- iNaturalist_subset_prj[input_grid_subset, ]

  if (nrow(iNaturalist_subset_prj@coords) > 10000) {
    set.seed(1)
    iNaturalist_subset_prj <- iNaturalist_subset_prj[sample(nrow(iNaturalist_subset_prj@coords), 10000), ]
  }

  d_grid_occ <- raster::pointDistance(
    p1 = input_occ_grid,
    p2 = input_grid_subset_center,
    lonlat = flag_degree, allpairs = T
  )
  d_random_occ <- raster::pointDistance(
    p1 = iNaturalist_subset_prj,
    p2 = input_grid_subset_center,
    lonlat = flag_degree, allpairs = T
  )
  d_grid_occ_min <- apply(d_grid_occ, 1, FUN = min)
  hist(d_grid_occ_min) # can I write the values to Google sheet?
  mean_d_grid_occ <- mean(d_grid_occ_min) # 4.348756
  sd(d_grid_occ_min) # 0.2358579
  d_random_occ_min <- apply(d_random_occ, 1, FUN = min)
  hist(d_random_occ_min) # can I write the values to Google sheet?
  mean_d_random_occ <- mean(d_random_occ_min) # 3745.848
  sd(d_random_occ_min) # 1409.333

  if (flag_plot) {
    plot(input_grid_subset, col = "red")
    plot(iNaturalist_subset_prj, add = T, col = "gray")
    plot(input_occ_grid, add = T, col = "blue")

    library(ggplot2)
    d1 <- data.frame(d_grid_occ_min)
    d2 <- data.frame(d_random_occ_min)
    names(d1) <- "distance"
    names(d2) <- "distance"
    ggplot2::ggplot(data = d1) +
      ggplot2::geom_density(ggplot2::aes(distance), col = "blue") +
      ggplot2::geom_density(data = d2, ggplot2::aes(distance), col = "red") +
      ggplot2::scale_x_log10()
  }

  output <- list(
    mean_d_grid_occ,
    mean_d_random_occ,
    d_grid_occ_min,
    d_random_occ_min
  )
  return(output)
}
