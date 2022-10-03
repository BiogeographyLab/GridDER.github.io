# GridDER  <img src='man/figures/logo.png' align="right" height="250" />

The package is a tool for identifying biodiversity records that have been designated locations on widely used grid systems. Our tool also estimates the degree of environmental heterogeneity associated with grid systems, allowing users to make informed decisions about how to use such occurrence data in global change studies. We show that a significant proportion (~13.5%; 261 million) of records on GBIF, largest aggregator of natural history collection data, are potentially gridded data, and demonstrate that our tool can reliably identify such records and quantify the associated uncertainties.



### Citation
Xiao Feng, Tainá Rocha, Hanna TThammavong,Rima Tulaiha, Xin Chen,Yingying Xie, Daniel Park (2022) GridDER: Grid Detection and Evaluation in R. https://doi.org/10.32942/osf.io/6qy5u



### Installing

Currently **gridder** can be installed from GitHub:

``` r

# Without vignette

remotes::install_github("BiogeographyLab/gridder")


```

### vignette 
A short guide of using gridder can be found here: https://biogeographylab.github.io/gridder.github.io/


### To see the internal data use the fution `data()` or `str()`

`str(crs_list_prj)` : lisf of crs <br>

`str(grid_ID_9)` : sf dataframe of grid system <br>

`str(ne_10m_admin_0_countries)` : SpatialPolygonsDataFrame of all countries <br>

`str(occs_unique)` : dataframe of occurrences 



### The workflow overview


<img src='inst/workflow.png' align="center" height="450" />
<br />
<br />




