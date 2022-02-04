# occMagnet  <img src='man/figures/logo.png' align="right" height="250" />

PS 1: in development

PS 2: this logo definitively is not the final logo :neutral_face: . Suggestions are welcome :blush:  

occMagnet can serve as a tool to not only screen for gridded points, but to quantify the geographic and environmental uncertainties associated with these records, which can be used to inform models and analyses that utilize these data, including those pertaining to global change.

# Citation

put citation here

# Installing

PS.: We need to decide the final host

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


