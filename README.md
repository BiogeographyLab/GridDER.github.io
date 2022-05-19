# GridDER  <img src='man/figures/logo.png' align="right" height="250" />

The package is a tool for identifying biodiversity records that have been designated locations on widely used grid systems. Our tool also estimates the degree of environmental heterogeneity associated with grid systems, allowing users to make informed decisions about how to use such occurrence data in global change studies. We show that a significant proportion (~13.5%; 261 million) of records on GBIF, largest aggregator of natural history collection data, are potentially gridded data, and demonstrate that our tool can reliably identify such records and quantify the associated uncertainties.



### Citation



### Installing

Currently **gridder** can be installed from GitHub:

``` r

# Without vignette

remotes::install_github("BiogeographyLab/gridder",
                        auth_token = "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")


```


### To see the internal data use the fution `data()` or `str()`

`str(crs_list_prj)` : lisf of crs <br>

`str(grid_ID_9)` : sf dataframe of grid system <br>

`str(ne_10m_admin_0_countries)` : SpatialPolygonsDataFrame of all countries <br>

`str(occs_unique)` : dataframe of occurrences 



### The workflow overview


<img src='inst/workflow.png' align="center" height="450" />
<br />
<br />

- `assess_env_uncertainty()` compute variation of environmental conditions for each feature of the grid system. The functions has followed parameters :
``` r
assess_env_uncertainty(x, y, by = 1000, scale = 1000)	
```

`x` ee$Image or ee$ImageCollection objects with a single band.

`y` ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.

`by` Numerical input. Numbers of features. Default set to 1000.

`scale` A nominal scale in meters of the Image projection to work in. Default set to 1000.

<br />
<br />

- `assess_sp_uncertainty()` assess the environmental uncertainty of a grid system, by calculating the variation of environmental conditions (e.g. 30m elevation) within each grid. The functions has followed parameters :
``` r
assess_sp_uncertainty(input_grid, input_occ_grid, input_occ_random, flag_degree = FALSE, flag_plot = FALSE)
```
`input_grid` Character vector. Path of a grid system (shapefile).

`input_occ_grid` Character vector. Path of gridded occurrences (shapefile).

`input_occ_random` Character vector. Path of non-gridded occurrences (inaturalist is used by default).

`flag_degree` Logical. Parameter passed to raster::pointDistance.

`flag_plot` Logical. If TRUE, coordinates (based on the crs of the grid system) should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters). Default is FALSE.

<br />
<br />

- `cal_angle()` :
``` r
cal_angle(M, N)
```
`M` 

`N` 

<br />
<br />

- `assess_sp_uncertainty()` This functions load from Zenodo all grid systems compiled

``` r
download_demoGrid(downloadNew = F)

```

<br />
<br />

- `find_crs_extent()` The function extracts the extent of coordinate reference systems (crs) from epsg website. The extent is required for generateGRID function. 
``` r
find_crs_extent(crs_num = "2154")
```
`crs_num` Character vector of crs number, e.g. "4326"

<br />
<br />

- `get_dist_freq` The function finds the distance of x(or y), then return a frequency table
``` r
get_dist_freq(input_v, round_num = 0)
```
`input_v` Vector? X or Y coordinates

`round_num` Numeric

<br />
<br />

- `grid_adjustment` Given a grid system and occurrences, this function try to shift the grid system, in order to minimize the distance between the grid system and input occurrences i.e. doing some small adjustment of the grid system.
```r
grid_adjustment(input_grid, input_occ_grid)
```
`input_grid` Character vector. A string with path of input grid system.

`input_occ_grid` Character vector. A string with path of occurrences data.

<br />
<br />

- `grid_generation` 

