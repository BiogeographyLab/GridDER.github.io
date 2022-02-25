# gridder  <img src='man/figures/logo.png' align="right" height="250" />

PS 1: in development

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


`assess_env_uncertainty()` compute variation of environmental conditions for each feature of the grid system. The functions has followed parameters :
``` r
assess_env_uncertainty (x, y, by = 1000, scale = 1000)	
```

- `x` ee$Image or ee$ImageCollection objects with a single band.
- `y` ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
- `by` Numerical input. Numbers of features. Default set to 1000.
- `scale` A nominal scale in meters of the Image projection to work in. Default set to 1000.



