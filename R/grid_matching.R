#' @title Compute nearest distance
#' @description This function calculate nearest distance between a set of occurrences and a set of grid systems.
#' @author Xiao Feng
#'
#' @param input_occ Input occurrences (shapefile or .txt/.csv data with decimalLongitude & decimalLatitude).
#' @param input_grid A list of spatial polygons, each represent one grid system flag_degree, res_x,res_y,res_unit, input_metadata=,~~~~ to fix.
#' @param country Character vector. A string or a vector of strings of countries names; can be used to subset known grid systems (G sheet, or a dataframe from user, in which case it should have a similar structure as G sheet).
#' @param gridID Character vector. A string or a vector of strings; grid ID to be used in the calculation (gridID from G sheet or user provied dataframe).
#' @param grid_metadata  A dataframe, if null, G sheet is loaded; otherwise, user should provide a table that looks like G sheet.
#' @param flag_relativeTHD Numerical. 0.1 if a relative distance is >0.1 or 10% of max distance (half of the diagonal line), then make it NA (meaning not from a grid).
#' @param flag_rm_Large_outlier Give a pool of distances, the remove outliers (based on R boxplot algorithm)
# flag_user_threshold=NULL a number between 0 and 100; representing a threshold of relative distance (actual distance/(alf of the diagonal line), beyond of which an occurrence is excluded.
#' @param flag_absoluteTHD Numerical. An absolute distance, beyond of which an occurrence is excluded.
#'
#' @return
#' @export
#' @import raster gsheet
#'
#' @examples
grid_matching = function(input_occ,
                         input_grid=NULL,
                         country=NULL,
                         grid_metadata=NULL,
                         flag_relativeTHD=NULL,
                         flag_rm_Large_outlier=TRUE,
                         flag_absoluteTHD=NULL
                         ){
  printf <- function(...) cat(sprintf(...))

  if(class(input_occ) %in%  c("SpatialPoints","SpatialPointsDataFrame")  ){
  }else if(class(input_occ) == "character"){
    if( grepl(".shp",input_occ) ){
      input_occ = shapefile(input_occ)
    } else {
      input_occ = load_occ(input_occ)
    }
  }
  if(nrow(input_occ@coords)>10000){
    set.seed(1)
    input_occ= input_occ[sample(1:nrow(input_occ@coords), 10000),]
  }


  input_occ$id = 1:nrow(input_occ@coords)
  printf("input %d occ \n", nrow(input_occ@coords) )


  if(class(input_grid)=="list"){
    N_grid = length(input_grid)
    flag_input_shapefile=TRUE
    sel_metadata = grid_metadata
  } else if(   class(input_grid)  %in% c("SpatialPolygons","SpatialPolygonsDataFrame")   )  {
    N_grid=1
    flag_input_shapefile=TRUE
    sel_metadata = grid_metadata
  } else{
    flag_input_shapefile=FALSE
    if(is.null(country)){
      # overlap occ with country polygon, and find country names
      country_shp = raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
      if(!identicalCRS(input_occ,country_shp)){
        input_occ_temp1 = spTransform(input_occ,crs(country_shp))
      } else{
        input_occ_temp1=input_occ
      }

      temp_o = sp::over(input_occ_temp1,country_shp)
      temp_country = unique(temp_o$ADMIN)
      country = temp_country
    } else {}


    if(is.null(grid_metadata) ){
      grid_metadata = gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Qp5KOpLSVnF2t16uIbNwKwK5ZBXt4k5unONp7eopPzI/edit?usp=sharing')
      grid_metadata = data.frame(grid_metadata)
      grid_metadata = subset(grid_metadata,!grepl("del_",grid_metadata$grid_ID) )
    }

    sel_metadata = grid_metadata
    if(!is.null(country) ){
      text_c_index = lapply(country, FUN=function(x){ which(grepl(x,sel_metadata$country_name))})
      text_c_index = unlist(text_c_index)
      text_c_index = sort(unique(text_c_index) )
      sel_metadata = sel_metadata[text_c_index,]
    }

    if(!is.null(gridID) ){
      sel_metadata = subset(sel_metadata,grid_ID %in% gridID)
    }

    print(sel_metadata$grid_ID )


    N_grid = nrow(sel_metadata)
  }

  output = data.frame(input_occ@coords)
  output$id = 1: nrow(output)
  output$latlon = paste0(as.character(input_occ@coords[,1]),
                         as.character(input_occ@coords[,2]))


  if(N_grid>=1){
    i=1
    for(i in 1:N_grid){
      printf("checking %d grid out of %d", i, N_grid)
      print(i)


      if(!flag_input_shapefile){
        
        download_demoGrid()  # download the previously generated grid shapefiles        
        
        path_grid_shp = list.files(   paste0(system.file(package="gridder"), "/data/2_clean_grid/"  )  ,
                                   pattern=paste0("grid_ID_",
                                                                       sel_metadata$grid_ID[i],"_",".*.shp"),full.names = T)
        if(length(path_grid_shp)==0){
          path_grid_shp = list.files( paste0(system.file(package="gridder"), "/data/2_clean_grid/"  )  ,
                                     pattern=paste0("grid_ID_",
                                                    sel_metadata$grid_ID[i],"",".*.shp"),
                                     full.names = T)
        }

        one_grid = raster::shapefile(path_grid_shp)
      } else if( N_grid == 1){
        one_grid= input_grid
      } else if( N_grid > 1){
        one_grid = input_grid[[i]]
      }


      temp_ext = as(extent(one_grid),"SpatialPolygons")
      crs(temp_ext) = crs(one_grid)
      temp_ext_prj = spTransform(temp_ext,   crs(input_occ))
      input_occ_temp = input_occ[temp_ext_prj,]


      if(nrow(input_occ_temp@coords)==0){
        next
      }

      occ_prj = sp::spTransform(input_occ_temp,crs(one_grid))
      occ_prj = occ_prj[one_grid,]

      temp_over = sp::over(one_grid,occ_prj,returnList = F)

      if(  is.null( dim(temp_over)) ){
        one_grid = one_grid[!is.na(temp_over),]
      } else{
        one_grid = one_grid[!is.na(temp_over[,1]),]
      }

      if(nrow(one_grid)==0){
        next
      }
      one_grid_center = rgeos::gCentroid(one_grid, byid=T)

      res_x = as.numeric(sel_metadata$resolution_x[i])
      res_y = as.numeric(sel_metadata$resolution_y[i])
      res_unit = sel_metadata$resolution_unit[i]
      grid_ID = sel_metadata$grid_ID[i]


      if(res_unit %in% c("km","m")) {
        flag_degree = FALSE
      }
      if(res_unit %in% c("second","minute","degree")){
        flag_degree = TRUE
      }


      d_matrix = raster::pointDistance(p1=occ_prj,
                                       p2=one_grid_center,
                                       lonlat = flag_degree,
                                       allpairs = T)

      if(is.vector(d_matrix)){
        if(length(d_matrix)==1){d_min = min(d_matrix)} else{d_min = d_matrix}

      } else{
        d_min = apply(d_matrix,1,FUN=min)
      }


      if (res_unit == "m"){
        sizex =  res_x
        sizey =  res_y
      }

      if (res_unit == "km"){
        sizex =  res_x*1000
        sizey =  res_y*1000
      }
      if (res_unit == "minute"){
        sizex =  100000 * res_x/60
        sizey =  100000 * res_y/60
      }
      one_unit_D = (sizex^2+sizey^2)^0.5




      temp_out = data.frame(id=occ_prj$id,
                            d_min=d_min )
      output = merge(output,temp_out,by="id",all.x=T)


      names(output)[ncol(output)] = paste0("absD_grid_ID_",grid_ID)



      d_min_relative = d_min/one_unit_D

      temp_out2 = data.frame(id=occ_prj$id,
                             d_min=d_min_relative )
      output = merge(output,temp_out2,
                     by="id",all.x=T)

      names(output)[ncol(output)] = paste0("relD_grid_ID_",grid_ID)
    }
  }

  find_min = function(temp_v){
    if(  all(is.na(temp_v))  ){
      return (NA)
    } else {
      return (  which.min(temp_v)  )
    }
  }

  temp_col_i = grep("relD_grid_ID_",names(output) )


  temp_which_min = apply(output[ ,temp_col_i,drop=F],1,FUN=find_min)


  output$grid_closest_relative_id = names(output)[temp_col_i][ temp_which_min ]

  output$grid_closest_relative_value = apply(output[ ,temp_col_i,drop=F],1,FUN=min)


  temp_col_i = grep("absD_grid_ID_",names(output) )

  temp_which_min = apply(output[ ,temp_col_i,drop=F],1,FUN=find_min)

  output$grid_closest_absolute_id = names(output)[temp_col_i][ temp_which_min ]
  output$grid_closest_absolute_value =  apply(output[ ,temp_col_i,drop=F],1,FUN=min)


  output$grid_closest_relative_id = gsub("relD_","",output$grid_closest_relative_id)
  output$grid_closest_absolute_id = gsub("absD_","",output$grid_closest_absolute_id)




  output$flag=""
  output$flag[is.na(output$grid_closest_relative_id )] = "out"

  if( !is.null(flag_relativeTHD) ){
    bad_index = (output$grid_closest_relative_value>flag_relativeTHD)  &
      (!is.na(output$grid_closest_relative_value))
    output$flag[bad_index] = paste(output$flag[bad_index],"aboveRelativeTHD",sep = ",")
  }

  if( !is.null(flag_absoluteTHD)){
    bad_index = (output$grid_closest_absolute_value>flag_absoluteTHD)  &
      (!is.na(output$grid_closest_absolute_value))
    output$flag[bad_index] = paste(output$flag[bad_index],"aboveAbsoluteTHD",sep = ",")
  }
  if(flag_rm_Large_outlier){

    temp_values = output$grid_closest_absolute_value

    temp_outlier =boxplot.stats(temp_values)$out
    if(length(temp_outlier)>0 ){
      i_big =which(temp_outlier>mean(temp_values,na.rm=T))
      if(length(length(i_big))>0){
        thd_big = min(temp_outlier[i_big])
        index_outlier = which(temp_values>=thd_big)

        output$flag[index_outlier] = paste(output$flag[index_outlier],
                                           "outlierAbsolute",sep = ",")
      }
    }

    temp_values = output$grid_closest_relative_value
    temp_outlier =boxplot.stats(temp_values)$out
    if(length(temp_outlier)>0 ){
      i_big =which(temp_outlier>mean(temp_values,na.rm=T))
      if(length(length(i_big))>0){
        thd_big = min(temp_outlier[i_big])
        index_outlier = which(temp_values>=thd_big)
        output$flag[index_outlier] = paste(output$flag[index_outlier],
                                           "outlierRelative",sep = ",")
      }
    }

  }




  flag1 = ifelse( (   output$grid_closest_relative_id==output$grid_closest_absolute_id )&
                    (!is.na(output$grid_closest_relative_id) &
                       !is.na(output$grid_closest_absolute_id) &
                       output$flag ==""
                    ),TRUE,FALSE)
  output$grid_closest_both_id = "notFound" #
  output$grid_closest_both_id[flag1] = output$grid_closest_absolute_id[flag1]



  output = output[order(output$id),]

  output_occ = sp::SpatialPointsDataFrame(coords=input_occ,data = output,
                                          match.ID=FALSE)

  return(output_occ)
}
