# occMagnet  <img src='man/figures/logo.png' align="right" height="250" />

occMagnet can serve as a tool to not only screen for gridded points, but to quantify the geographic and environmental uncertainties associated with these records, which can be used to inform models and analyses that utilize these data, including those pertaining to global change.


# Installing

Currently **modleR** can be installed from GitHub:

``` r

# Without vignette

if(!require(devtools)){
    install.packages("devtools")
}

if(!require(occMagnet)){
    devtools::install_github("Tai-Rocha/occMagnet")
}

```
