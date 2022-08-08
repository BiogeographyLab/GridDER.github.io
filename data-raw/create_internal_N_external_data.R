### This code covert shapefiles to Rdata (all data examples of packages in /data directory should be .rda extention (See instructions at: https://r-pkgs.org/data.html)

### For while I do not put the raw data at repo package (.csv or .shp), because is a large file. Also the data that will use as examples is provided by the package in /data directory in .rds extention

## Creating data for the package as Internal (see some infos at: https://coolbutuseless.github.io/2018/12/10/r-packages-internal-and-external-data/ and at : https://r-pkgs.org/data.html#data-sysdata)

## Internal data is data within the package, but not (generally) made available to the user. Setting as  "Internal" all functions within the package can freely access the dataset, but the user won’t see them.

## I Thus first I create the dataset in internal mood in order to test the package functions

## Description of dataset
grid_ID_2_polygon_masked <- raster::shapefile("inst/extdata/grid_id_2/grid_ID_2_polygon_masked.shp", )

## Description of dataset
ne_10m_admin_0_countries <- raster::shapefile("inst/extdata/0_basemap/ne_10m_admin_0_countries.shp")


## Grid_id_9 (Flora of Vladimir- Russia)

grid_ID_9 <- sf::st_read("inst/extdata/grid_id_9/grid_ID_9.kml")

## Unique occurences

occs_unique <- readr::read_csv("inst/extdata/occs_unique/occs_unique.csv")


## GBIF occurrences of gird_id_9 system # Not work

### occs_grid_id_9 <- readr::read_csv("inst/extdata/grid_id_9/0060040-210914110416597.csv")

## CRS List object

crs_list_prj <- readRDS("data-raw/crs_list_prj.rds")


## Take all DATA and put as INTERNAL data into a gridder package

usethis::use_data(occs_unique, internal = TRUE, overwrite = TRUE) # update occ_unique (reduce row number) 8/8/2022

usethis::use_data(grid_ID_2_polygon_masked, ne_10m_admin_0_countries, grid_ID_9,occs_unique,crs_list_prj, internal = TRUE, overwrite = TRUE)

## To acess the data afetr installation use

# ```gridder:::occs_unique```

## External data : External data is data contained in the package that is made available to the user, but is not (generally) available to the functions within the package. Once the package is loaded, the dataset will be made available to the user. To create external use ``` usethis::use_data```  and set  internal= FALSE


## Take all DATA and put as EXTERNAL data into a gridder package

usethis::use_data(occs_unique, internal = FALSE, overwrite = TRUE) # update occ_unique (reduce row number) 8/8/2022

usethis::use_data(grid_ID_2_polygon_masked, ne_10m_admin_0_countries, grid_ID_9,occs_unique,crs_list_prj, internal = FALSE, overwrite = TRUE)

## to access the data use

# ``str (occs_unique)`` or gridder::occs_unique


