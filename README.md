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


`generateGRID()` has a large number of parameters:

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


