### This code covert shapefiles to Rdata (all data examples of packages in /data directory should be .rda extention (See instructions at: https://r-pkgs.org/data.html)

### For while I do not put the raw data at repo package (.csv or .shp), because is a large file. Also the data that will use as examples is provided by the package in /data directory in .rds extention

## Creating data for the package as Internal (see some infos at: https://coolbutuseless.github.io/2018/12/10/r-packages-internal-and-external-data/ and at : https://r-pkgs.org/data.html#data-sysdata)

## Internal data is data within the package, but not (generally) made available to the user. Setting as  "Internal" all functions within the package can freely access the dataset, but the user won’t see them.

## I Thus first I create the dataset in internal mood in order to test the package functions

grid_ID_2_polygon_masked <- raster::shapefile("inst/extdata/grid_id_2/grid_ID_2_polygon_masked.shp", )
usethis::use_data(grid_ID_2_polygon_masked, internal = TRUE, overwrite = TRUE)

ne_10m_admin_0_countries <- raster::shapefile("inst/extdata/0_basemap/ne_10m_admin_0_countries.shp")
usethis::use_data(ne_10m_admin_0_countries, internal = TRUE, overwrite = TRUE)


## Grid_id_9 (Flora of Vladimir- Russia)

grid_ID_9 <- sf::st_read("inst/extdata/grid_id_9/grid_ID_9.kml")
usethis::use_data(grid_ID_9, internal = TRUE, overwrite = TRUE)

## GBIF occurrences of gird_id_9 system

occs_grid_id_9 <- readr::read_csv("inst/extdata/grid_id_9/0060040-210914110416597.csv")
usethis::use_data(occs_grid_id_9, internal = TRUE, overwrite = TRUE)


## CRS List object

crs_list_prj <- readRDS("data-raw/crs_list_prj.rds")

usethis::use_data(crs_list_prj, internal = TRUE, overwrite = TRUE)


## External data
