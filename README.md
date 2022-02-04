# occMagnet  <img src='man/figures/logo.png' align="right" height="250" />

ps.: in development

occMagnet can serve as a tool to not only screen for gridded points, but to quantify the geographic and environmental uncertainties associated with these records, which can be used to inform models and analyses that utilize these data, including those pertaining to global change.

# Citation

# Installing

Currently **occMagnet** can be installed from GitHub:

``` r

# Without vignette

if(!require(devtools)){
    install.packages("devtools")
}

if(!require(occMagnet)){
    devtools::install_github("Tai-Rocha/occMagnet")
}

```
## The workflow

The workflow consists of mainly N functions that should be ....

<img src='inst/workflow.png' align="center" height="450" />




## The example dataset


## Simulating a grind system: `generateGRID()`

The first step of the workflow is to setup the data, that is, to
partition it according to each project needs, to sample background
pseudoabsences and to apply some data cleaning procedures, as well as
some filters. This is done by function `setup_sdmdata()`

`generateGRID()` has a large number of parameters:

``` r
args(generateGRID)
#> function (species_name, occurrences, predictors, lon = "lon", 
#>     lat = "lat", models_dir = "./models", real_absences = NULL, 
#>     buffer_type = NULL, dist_buf = NULL, env_filter = FALSE, 
#>     env_distance = "centroid", buffer_shape = NULL, min_env_dist = NULL, 
#>     min_geog_dist = NULL, write_buffer = FALSE, seed = NULL, 
#>     clean_dupl = FALSE, clean_nas = FALSE, clean_uni = FALSE, 
#>     geo_filt = FALSE, geo_filt_dist = NULL, select_variables = FALSE, 
#>     cutoff = 0.8, sample_proportion = 0.8, png_sdmdata = TRUE, 
#>     n_back = 1000, partition_type = c("bootstrap"), boot_n = 1, 
#>     boot_proportion = 0.7, cv_n = NULL, cv_partitions = NULL) 
#> NULL
```
