# gridder  <img src='man/figures/logo.png' align="right" height="250" />

PS 1: in development

PS 2: I guess this logo may be the final version. Suggestions are welcome :blush:  

gridder identifies collection records that have been designated locations on widely used gridding systems. Our tool also estimates the degree of environmental heterogeneity associated with grid systems, allowing users to make informed decisions about how to use occurrence data in global change studies.  


# Citation

put citation here

# Installing

PS.: We need to decide the final host

Currently **occMagnet** can be installed from GitHub:

``` r

# Without vignette

remotes::install_github("BiogeographyLab/occMagnet",
                        auth_token = "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")


```
## The workflow

The workflow consists of mainly N functions that should be ....


<img src='inst/workflow.png' align="center" height="450" />


`generateGRID()` has the following parameters:

``` r
generateGRID(res_x = 10,
                        res_y = 10,
                        unit = "km",# minute
                        #num_x = 10,
                        #num_y = 5,
                        flag_crs=TRUE,
                        extent_unit= NULL,#"crs_web_extent",# "crs_countryPolygon"
                        input_extent = NULL,
                        country = NULL,#"Germany",
                        crs_num = NULL,
                        crs_obj = NULL,
                        #flag_maskToCountry=NULL,
                        flag_offset=NULL,
                        flag_maskByCountry=FALSE,
                        flag_buffer=2,
                        #flag_center=FALSE
                        flag_loadCountryPolygon = TRUE
)
```


