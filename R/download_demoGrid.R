#' @title Download grid systems
#' @description This functions load from Zenodo all grid systems compiled
#' @author  Xiao Feng
#'
#' @param downloadNew Logical. TRUE to download the zip file containing all grid systems
#'
#' @return
#' @export
#'
#' @examples
download_demoGrid = function(downloadNew=F){
  share_url = "https://zenodo.org/record/6447902/files/2_clean_grid.zip?download=1"

  local_path = system.file(package="gridder")
  #local_path = system.file(package="dismo")

  zip_path = paste0(local_path,"/data/demoGrid.zip")

  if(  !file.exists(zip_path)   ){

    if(downloadNew){
      zip_to_path = paste0(local_path,"/data")
      dir.create(zip_to_path)

      utils::download.file(url=share_url,mode = "wb",
                           destfile=zip_path
      )
      utils::unzip(zip_path,exdir=zip_to_path)
    }
  }
}
