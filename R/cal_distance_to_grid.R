#' @title Compute nearest distance
#' @description This function compute  nearest distance  between a set of occurrences and a set of grid systems.
#'
#' @param input_occ Input occurrences (shapefile or .txt/.csv data with decimalLongitude & decimalLatitude).
#' @param country Character vector. Country name; can be used to subset known grid systems (G sheet, or a dataframe from user, in which case it should have a similar structure as G sheet).
#' @param gridID Character vector. Grid ID to be used in the calculation (gridID from G sheet or user provided dataframe).
#' @param grid_metadata A dataframe. If null, G sheet is loaded; otherwise, user should provide a table that looks like G sheet.
#' @param flag_over_XPer Numerical input. If a relative distance is >0.1 or 10% of max distance (half of the diagonal line), then make it NA (meaning not from a grid).
#' @param flag_rm_Large_outlier Logical. Give a pool of distances, the remove outliers (based on R boxplot algorithm)
#' @param flag_user_threshold Numerical input. A number between 0 and 100; representing a threshold of relative distance (actual distance/(half of the diagonal line), beyond of which an occurrence is excluded.
#'
#' @return The funtion returns the most close grid system for each occurrence, based on abs distance, relative distance, or both distance.
#' @export
#'
#' @import raster sp
#' @import gsheet
#'
#' @examples
#' \dontrun{
#' # Add example
#' }
cal_distance_to_grid <- function(input_occ = NULL,
                                 country = NULL,
                                 gridID = NULL,
                                 grid_metadata = NULL,
                                 flag_over_XPer = 10,
                                 flag_rm_Large_outlier = TRUE,
                                 flag_user_threshold = NULL) {
  printf <- function(...) cat(sprintf(...))

  if (class(input_occ) == "character") {
    if (grepl(".shp", input_occ)) {
      input_occ <- shapefile(input_occ)
    } else {
      input_occ <- load_occ(input_occ)
    }
  }
  if (nrow(input_occ@coords) > 10000) {
    set.seed(1)
    input_occ <- input_occ[sample(1:nrow(input_occ@coords), 10000), ]
  }
  # input_occ@data = data.frame(id = 1:nrow(input_occ@coords))

  input_occ$id <- 1:nrow(input_occ@coords)

  printf("input %d occ \n", nrow(input_occ@coords))
  # first subselect grids
  if (is.null(country)) {
    # overlap occ with country polygon, and find country names
    country_shp <- raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
    if (!identicalCRS(input_occ, country_shp)) {
      input_occ_temp1 <- spTransform(input_occ, crs(country_shp))
    } else {
      input_occ_temp1 <- input_occ
    }

    temp_o <- sp::over(input_occ_temp1, country_shp)
    temp_country <- unique(temp_o$ADMIN)
    country <- temp_country
  } else {
  }

  # read google doc
  # library(gsheet)
  if (is.null(grid_metadata)) {
    grid_metadata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1mSad3lUL5eMgtwH311Bo8PUWIqJkvgxUfG99NiuRLjo/edit#gid=166945453")
    grid_metadata <- data.frame(grid_metadata)
    grid_metadata <- subset(grid_metadata, !grepl("del_", grid_metadata$grid_ID))
  }


  sel_metadata <- grid_metadata
  if (!is.null(country)) {


    # sel_metadata = subset(sel_metadata,country_name %in% country)

    # this is used to handle one cell (from the metadata table) has multiple country names
    text_c_index <- lapply(country, FUN = function(x) {
      which(grepl(x, sel_metadata$country_name))
    })
    text_c_index <- unlist(text_c_index)
    text_c_index <- sort(unique(text_c_index))
    sel_metadata <- sel_metadata[text_c_index, ]
  }
  if (!is.null(gridID)) {
    sel_metadata <- subset(sel_metadata, grid_ID %in% gridID)
  }

  print(sel_metadata$grid_ID)

  # diff grid has to be used independently, because of diff crs, diff unit

  output <- data.frame(input_occ@coords)
  output$id <- 1:nrow(output)
  output$latlon <- paste0(
    as.character(input_occ@coords[, 1]),
    as.character(input_occ@coords[, 2])
  )
  N_grid <- nrow(sel_metadata)
  if (N_grid >= 1) {
    i <- 1
    for (i in 1:N_grid) {
      printf("checking %d grid out of %d", i, N_grid)
      print(i)

      path_grid_shp <- list.files("data/2_clean_grid/", pattern = paste0(
        "grid_ID_",
        sel_metadata$grid_ID[i], "_", ".*.shp"
      ), full.names = T)
      if (length(path_grid_shp) == 0) {
        path_grid_shp <- list.files("data/2_clean_grid/",
          pattern = paste0(
            "grid_ID_",
            sel_metadata$grid_ID[i], "", ".*.shp"
          ),
          full.names = T
        )
      }

      print(path_grid_shp)
      one_grid <- raster::shapefile(path_grid_shp)


      # this step is added to avoid error during projection(Inf values of coordinates)
      temp_ext <- as(extent(one_grid), "SpatialPolygons")
      crs(temp_ext) <- crs(one_grid)
      temp_ext_prj <- spTransform(temp_ext, crs(input_occ))
      input_occ_temp <- input_occ[temp_ext_prj, ]


      if (nrow(input_occ_temp@coords) == 0) {
        next
      }

      occ_prj <- sp::spTransform(input_occ_temp, crs(one_grid))
      occ_prj <- occ_prj[one_grid, ] # only keep the ones that are inside the grid polygons

      temp_over <- sp::over(one_grid, occ_prj, returnList = F)

      if (is.null(dim(temp_over))) {
        one_grid <- one_grid[!is.na(temp_over), ]
      } else {
        # if temp_over is a data frame, then find the NA column
        one_grid <- one_grid[!is.na(temp_over[, 1]), ]
      }

      if (nrow(one_grid) == 0) {
        next
      }
      one_grid_center <- rgeos::gCentroid(one_grid, byid = T)



      #
      # plot(one_grid,xlim=c(131604.8-4000, 131604.8 +4000),
      #      ylim=c(180611.1-4000,180611.1+4000))
      # plot(one_grid_center,add=T,col="red")
      # plot(occ_prj[409,],add=T,col="blue")


      if (sel_metadata$resolution_unit[i] == "km" |
        sel_metadata$resolution_unit[i] == "m") {
        flag_degree <- FALSE
      }
      if (sel_metadata$resolution_unit[i] == "minute" |
        sel_metadata$resolution_unit[i] == "degree") {
        flag_degree <- TRUE
      }


      d_matrix <- raster::pointDistance(
        p1 = occ_prj,
        p2 = one_grid_center,
        lonlat = flag_degree,
        allpairs = T
      )
      # dim(d_matrix_inaturalist)
      # nrow(d_matrix_inaturalist)
      # if(length(d_matrix)==1){
      if (is.vector(d_matrix)) {
        if (length(d_matrix) == 1) {
          d_min <- min(d_matrix)
        } else {
          d_min <- d_matrix
        }
      } else {
        d_min <- apply(d_matrix, 1, FUN = min)
      }
      # hist(d_min_inaturalist)
      # mean(d_min_inaturalist)#3848.058
      # sd(d_min_inaturalist)  #1419.272
      # summary(d_min_inaturalist)

      res_x <- as.numeric(sel_metadata$resolution_x[i])
      res_y <- as.numeric(sel_metadata$resolution_y[i])
      unit <- sel_metadata$resolution_unit[i]
      if (unit == "km") {
        sizex <- res_x * 1000
        sizey <- res_y * 1000
      }
      if (unit == "minute") {
        sizex <- 100000 * res_x / 60
        sizey <- 100000 * res_y / 60
      }
      one_unit_D <- (sizex^2 + sizey^2)^0.5

      if (!is.null(flag_over_XPer)) { # remove points over 1/10 of the diag distance
        d_min[d_min > (one_unit_D / flag_over_XPer)] <- NA
      }

      if (!is.null(flag_user_threshold)) { # remove points a certain distance
        d_min[d_min > flag_user_threshold] <- NA
      }
      if (flag_rm_Large_outlier) { # remove outliers
        temp_outlier <- boxplot.stats(d_min)$out
        if (length(temp_outlier) > 0) {
          i_big <- which(temp_outlier > mean(d_min, na.rm = T))
          if (length(length(i_big)) > 0) {
            thd_big <- min(temp_outlier[i_big])
            d_min[d_min >= thd_big] <- NA
          }
        }
      }

      temp_out <- data.frame(
        id = occ_prj$id,
        d_min = d_min
      )
      output <- merge(output, temp_out, by = "id", all.x = T)
      # ~~~~~~~~~~ here is a bug, have to merge back by id
      # output = cbind(output,d_min)

      names(output)[ncol(output)] <- paste0("absD_grid_ID_", sel_metadata$grid_ID[i])



      d_min_relative <- d_min / one_unit_D

      temp_out2 <- data.frame(
        id = occ_prj$id,
        d_min = d_min_relative
      )
      output <- merge(output, temp_out2,
        by = "id", all.x = T
      )
      # output = cbind(output,d_min_relative)

      names(output)[ncol(output)] <- paste0("relD_grid_ID_", sel_metadata$grid_ID[i])
    }
  }

  # output will be a matrix, rows-> occ records,  columns -> grid systems

  # to add a probability of each grid?
  # to add best grid?
  # add sp & env uncertainty [this shall be precalculated for each grid?]

  find_min <- function(temp_v) {
    if (all(is.na(temp_v))) {
      return(NA)
    } else {
      return(which.min(temp_v))
    }
  }

  temp_col_i <- grep("relD_grid_ID_", names(output))

  if (length(temp_col_i) == 1) {
    temp_which_min <- rep(1, nrow(output))
    temp_which_min[is.na(output[, temp_col_i])] <- NA
  } else {
    temp_which_min <- apply(output[, temp_col_i], 1, FUN = find_min)
  }
  output$grid_closest_relative <- names(output)[temp_col_i][temp_which_min]


  temp_col_i <- grep("absD_grid_ID_", names(output))
  if (length(temp_col_i) == 1) {
    temp_which_min <- rep(1, nrow(output))
    temp_which_min[is.na(output[, temp_col_i])] <- NA
  } else {
    temp_which_min <- apply(output[, temp_col_i], 1, FUN = find_min)
  }
  output$grid_closest_absolute <- names(output)[temp_col_i][temp_which_min]


  output$grid_closest_relative <- gsub("relD_", "", output$grid_closest_relative)
  output$grid_closest_absolute <- gsub("absD_", "", output$grid_closest_absolute)

  flag1 <- ifelse((output$grid_closest_relative == output$grid_closest_absolute) &
    (!is.na(output$grid_closest_relative) &
      !is.na(output$grid_closest_absolute)), TRUE, FALSE)
  output$grid_closest_both <- NA
  output$grid_closest_both[flag1] <- output$grid_closest_absolute[flag1]


  # join grid metadata to the results
  # sel_metadata$grid_ID_string = paste0("grid_ID_",sel_metadata$grid_ID)
  # output= merge(output,
  #               sel_metadata[c("resolution_x",
  #                              "resolution_y",
  #                              "resolution_unit",
  #                              "grid_ID_string")],
  #               by.x="grid_closest",
  #               by.y="grid_ID_string",
  #               all.x=T)

  output <- output[order(output$id), ]

  output_occ <- sp::SpatialPointsDataFrame(
    coords = input_occ, data = output,
    match.ID = FALSE
  )
  # plot(output_occ[output_occ$resolution_x==1,])
  # plot(output_occ[output_occ$resolution_x==4,],add=T,col="green")

  return(output_occ)
}
