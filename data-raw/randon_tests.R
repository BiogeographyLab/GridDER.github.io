remotes::install_github("BiogeographyLab/gridder",
                       auth_token = "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")


library(gridder)

dir(system.file(package = "gridder"))
system.file(package = "gridder")

## To read internal data after installation

gridder:::occs_grid_id_9

gridder:::grid_ID_2_polygon_masked

gridder:::grid_ID_9

gridder:::crs_list_prj

gridder:::ne_10m_admin_0_countries



tet <- gridder::infer_crs(
  occ_path = "/home/tai-rocha/0068200-210914110416597_unique.csv",
  truth_crs_num = 4326,
  flag_saveTemp = T,
  temp_path = "/home/tai-rocha/infer_crs_test-1/",
  #cpu_num = 2,
  flag_debug = -1
)

test <- load("data/crs_list_prj.rda")
