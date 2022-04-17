### This code covert shapefiles to Rdata (all data examples of packages in /data directory should be a .Rdata)

## For while I do not put the raw data at repo package (.csv or .shp), once data that will use as examples is provided by the package in .RData format


grid_ID_2_polygon_masked <- raster::shapefile("inst/extdata/grid_id_2/grid_ID_2_polygon_masked.shp")
usethis::use_data(grid_ID_2_polygon_masked)

ne_10m_admin_0_countries <- raster::shapefile("inst/extdata/0_basemap/ne_10m_admin_0_countries.shp")
usethis::use_data(ne_10m_admin_0_countries)


## Grid_id_9 (Flora of Vladimir- Russia)

grid_ID_9 <- sf::st_read("inst/extdata/grid_id_9/grid_ID_9.kml")
usethis::use_data(grid_ID_9)

## GBIF occurrences of gird_id_9 system

occs_grid_id_9 <- readr::read_csv("inst/extdata/grid_id_9/0060040-210914110416597.csv")
usethis::use_data(occs_grid_id_9)


## CRS List object

crs_list_prj <- readRDS("data/crs_list_prj.rds")

usethis::use_data(crs_list_prj)

