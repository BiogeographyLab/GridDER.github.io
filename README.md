# gridder  <img src='man/figures/logo.png' align="right" height="250" />

in development

gridder identifies collection records that have been designated locations on widely used gridding systems. Our tool also estimates the degree of environmental heterogeneity associated with grid systems, allowing users to make informed decisions about how to use occurrence data in global change studies.  


# Citation

citation here 

# Installing

Currently **gridder** can be installed from GitHub:

``` r

# Without vignette

remotes::install_github("BiogeographyLab/gridder",
                        auth_token = "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")


```
## The workflow

The workflow consists of mainly N functions that should be ....


<img src='inst/workflow.png' align="center" height="450" />


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


