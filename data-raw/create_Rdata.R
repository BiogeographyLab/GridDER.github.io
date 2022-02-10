### This code covert shapefiles to Rdata (all data examples of packages in /data directory should be a .Rdata)

## For while I do not put the raw data at repo package (.csv or .shp), once data that will use as examples is provided by the package in .RData format


grid_ID_2_polygon_masked <- raster::shapefile("inst/extdata/grid_id_2/grid_ID_2_polygon_masked.shp")
usethis::use_data(grid_ID_2_polygon_masked, compress = "bzip2")

ne_10m_admin_0_countries <- raster::shapefile("inst/extdata/0_basemap/ne_10m_admin_0_countries.shp")
usethis::use_data(ne_10m_admin_0_countries, compress = "bzip2")




