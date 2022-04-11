download_demoGrid = function(){
  share_url = "https://zenodo.org/record/6447902/files/2_clean_grid.zip?download=1"

  local_path = system.file(package="gridder")
  local_path = system.file(package="dismo")
  
  zip_path = paste0(local_path,"/data/demoGrid.zip")
  zip_to_path = paste0(local_path,"/data")
  dir.create(zip_to_path)
  
  utils::download.file(url=share_url,mode = "wb",
                       destfile=zip_path
  )
  
  utils::unzip(zip_path,exdir=zip_to_path) 
}
