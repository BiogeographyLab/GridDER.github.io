# occMagnet
The package find a grid system 

occMagnet can serve as a tool to not only screen for gridded points, but to quantify the geographic and environmental uncertainties associated with these records, which can be used to inform models and analyses that utilize these data, including those pertaining to global change.


<img src="https://user-images.githubusercontent.com/11633554/152386786-cda0b538-d13d-475c-a535-c6b8ee323391.png"
     alt="temp logo"
     width="100"
     style="float: right;" />
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
