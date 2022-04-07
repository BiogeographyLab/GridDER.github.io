# a collection of functions used to calculate distance or angle between occurrences



# find the distance of x(or y), then return a frequency table
get_dist_freq = function(input_v,
                         round_num=0){
  distance_on_x = as.numeric(dist(input_v))
  if(round_num >=0){
    distance_on_x_fixed = round(distance_on_x,digits = round_num)
  } else{
    distance_on_x_fixed = distance_on_x
  }
  distance_on_x_freq = table(distance_on_x_fixed)
  distance_on_x_freq = data.frame(distance_on_x_freq)
  distance_on_x_freq = distance_on_x_freq[order(-distance_on_x_freq$Freq),]
  distance_on_x_freq$distance_on_x_fixed = as.numeric(as.character(distance_on_x_freq$distance_on_x_fixed))
  return(distance_on_x_freq)
}

# get_freq_table = function(input_v,round_num=0){
#   #print(round_num)
#   distance_on_x = as.numeric(input_v)
#   if(round_num >=0){
#     distance_on_x_fixed = round(distance_on_x,digits = round_num)
#   } else{
#     distance_on_x_fixed = distance_on_x
#   }
#   distance_on_x_freq = table(distance_on_x_fixed)
#   distance_on_x_freq = data.frame(distance_on_x_freq)
#   distance_on_x_freq = distance_on_x_freq[order(-distance_on_x_freq$Freq),]
#   distance_on_x_freq$distance_on_x_fixed = as.numeric(as.character(distance_on_x_freq$distance_on_x_fixed))
#   return(distance_on_x_freq)
# }



infer_resolution = function(input_coord,digits=1,
                            flag_unit="meter",#"meter","degree","minute","second"
                            # should be a string; default value is m
                            ...){
  demo = input_coord #  using x & y
  myD = dist( demo )
  myD = as.matrix(myD)
  myD[myD==0] = NA
  #myD_near= apply(myD, 1, FUN = min,na.rm = TRUE)
  #myD_near_which= apply(myD, 1, FUN = which.min)

  #also consider the top four nearest points from up/down/left/right
  #... to do...
  #
  occ_i = 1
  distance_4 = matrix(NA,nrow = 4,ncol = nrow(input_coord) )
  angle_4 = matrix(NA,nrow = 4,ncol = nrow(input_coord) )
  for(occ_i in 1: nrow(input_coord)  ){
    # first find the near 10 points
    top_i = kit::topn(myD[,occ_i],n=12,decreasing=F)
    top_angle = rep(NA,length(top_i))
    j=1
    # get the angle of the 10 points
    for(j in 1:length(top_i)){
      top_angle[j]= cal_angle(input_coord[occ_i,],
                              input_coord[top_i[j],])
    }
    # seperate them into 4 directions
    s_angle = 1
    up_i = which(   top_angle>= (315+s_angle) |
                      ( top_angle>=0 & top_angle <=(45-s_angle)   ) |
                      top_angle<=0   & top_angle >=(-45+s_angle)
    ) # up
    right_i = which(top_angle>=(45+s_angle) & top_angle <=(135-s_angle)  ) # right
    bottom_i = which(top_angle>=(135+s_angle) & top_angle <=(225-s_angle) ) # bottom
    left_i = which(
      (top_angle>=(225+s_angle) & top_angle <= (315-s_angle) ) |
        (top_angle>=(-135+s_angle) & top_angle <= (-45-s_angle)  )
    ) # left
    distance_4[,occ_i] = c(ifelse(length(up_i)==0, NA, min( myD[top_i[up_i],occ_i]     )),
                           ifelse(length(right_i)==0, NA, min( myD[top_i[right_i],occ_i]  )),
                           ifelse(length(bottom_i)==0, NA, min( myD[top_i[bottom_i],occ_i] )),
                           ifelse(length(left_i)==0, NA, min( myD[top_i[left_i],occ_i]   ))   )

    angle_4[,occ_i] = c(ifelse(length(up_i)==0, NA,top_angle [ up_i[ which.min(myD[top_i[up_i],occ_i]) ] ]   ),
                        ifelse(length(right_i)==0, NA,top_angle [ right_i[ which.min(myD[top_i[right_i],occ_i]) ] ]  ) ,
                        ifelse(length(bottom_i)==0, NA,top_angle [ bottom_i[ which.min(myD[top_i[bottom_i],occ_i]) ] ]  ) ,
                        ifelse(length(left_i)==0, NA,top_angle [ left_i[ which.min(myD[top_i[left_i],occ_i]) ] ] )  )

  }
  #distance_on_y_freq = get_freq_table(input_v = distance_4[1,] )
  #distance_on_x_freq = get_freq_table(distance_4[2,] )

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  #temp_v = signif (distance_4[1,],digits  = 1)
  if(flag_unit=="meter"){
    y_s = signif (distance_4[1,],digits  = 1)
    x_s = signif (distance_4[2,],digits  = 1)
    distance_on_y_freq = getmode( y_s )
    distance_on_x_freq = getmode( x_s )

    } else if (flag_unit=="degree") {
    y_s = signif (distance_4[1,],digits  = 1)
    x_s = signif (distance_4[2,],digits  = 1)
    distance_on_y_freq = getmode(y_s )
    distance_on_x_freq = getmode(x_s )   
    
  } else if (flag_unit=="minute") {
    y_s = signif (distance_4[1,]*60,digits  = 1)
    x_s = signif (distance_4[2,]*60,digits  = 1)
    distance_on_y_freq = getmode(y_s )/60
    distance_on_x_freq = getmode(x_s )/60
  } else if(flag_unit=="second"){
    y_s = signif (distance_4[1,]*3600,digits  = 1)
    x_s = signif (distance_4[2,]*3600,digits  = 1)
    distance_on_y_freq = getmode(y_s )/3600
    distance_on_x_freq = getmode(x_s  )/3600
  }

  freq_table_x = table(x_s)
  freq_table_x = data.frame(freq_table_x)
  freq_table_x = freq_table_x[order(-freq_table_x$Freq),]
  freq_table_x$x_s = as.numeric(as.character(freq_table_x$x_s))
  names(freq_table_x)[1] = "res_x"

  freq_table_y = table(y_s)
  freq_table_y = data.frame(freq_table_y)
  freq_table_y = freq_table_y[order(-freq_table_y$Freq),]
  freq_table_y$y_s = as.numeric(as.character(freq_table_y$y_s))
  names(freq_table_y)[1] = "res_y"


  if(flag_unit=="meter"){
  } else if (flag_unit=="minute") {
    freq_table_x$res_x = freq_table_x$res_x/60
    freq_table_y$res_y = freq_table_y$res_y/60
  } else if(flag_unit=="second"){
    freq_table_x$res_x = freq_table_x$res_x/3600
    freq_table_y$res_y = freq_table_y$res_y/3600
  }



  return( list(res_x=distance_on_x_freq,
               res_y=distance_on_y_freq,
               all_x=freq_table_x,
               all_y=freq_table_y
  ) )
}



# get_dist_freq = function(input_v,round_num=0){
#
#
#
# }



# this function seems unnecessary
# find_occ_distance = function(input_occ){
#   #input_occ is spatial points, its crs shall be the crs used during field work
#   #input_occ$x = input_occ@coords[,1]
#   #input_occ$y = input_occ@coords[,2]
#   freq_x = get_dist_freq(input_occ@coords[,1])
#   freq_y = get_dist_freq(input_occ@coords[,2])
#   return(list(freq_x,freq_y))
# }

# find the min/max of occ, here the min/max should match the resolution of the grid
infer_origin = function(input_occ,
                        fun=min,
                        flag_unit="meter",#"meter","degree","minute","second"
                        # should be a string; default value is meter
                        filters = sort(c (10^(2:10), 10^(3:10)/2  ))
                        #c(1000000,100000,10000,5000, 1000, 100, 10)
){
  if (flag_unit=="minute") {
    input_occ@coords = input_occ@coords * 60
  } else if(flag_unit=="second"){
    input_occ@coords = input_occ@coords * 3600
  }

  if(length(filters)== 2){
    filters_x=filters[1]
    filters_y=filters[2]
  } else{
    filters_x=filters
    filters_y=filters
  }

  M_y = fun(input_occ@coords[,2])
  M_y_group = unique( c ( round( M_y),
                          ceiling( M_y),
                          floor( M_y) ) )
  #here the logic is that, 5000 is better than 5500, better than 5550, better than 5555
  # the filters shall be modified in the future
  for (p in filters_y){
    temp_reminder = M_y_group %% p == 0
    if(  any( temp_reminder )   ){
      print(p)
      sel_M_y = M_y_group[ which(temp_reminder)]
      break
    }
    sel_M_y = min(M_y_group)
  }
  M_x = fun(input_occ@coords[,1])
  M_x_group = unique( c ( round( M_x),
                          ceiling( M_x),
                          floor( M_x) ) )
  for (p in filters_x){
    temp_reminder = M_x_group %% p == 0
    if(  any( temp_reminder )   ){
      print(p)
      sel_M_x = M_x_group[ which(temp_reminder)]
      break
    }
    sel_M_x = min(M_x_group)
  }

  out_put = c  (sel_M_x,sel_M_y)

  if (flag_unit=="minute") {
    out_put= out_put/ 60
  } else if(flag_unit=="second"){
    out_put = out_put/ 3600
  }

  return ( out_put)
}



# return an extent in the order of xmin, xmax, ymin, ymax
infer_extent = function(method= "",   #a string, country_extent, crs_extent, occ_extent
                        country = NULL,
                        occ=NULL,
                        crs_grid=NULL,

                        flag_adjust_by_res=FALSE,
                        res_x=NULL,
                        res_y=NULL,

                        flag_adjust_origin= (method=="occ_extent"),
                        flag_unit="meter"#"meter","degree","minute","second"
                        # should be a string; default value is meter
){
  if(method=="country_extent"){
    if(!is.null(country)  & !is.null(crs_grid)){
      if(grepl(",",country)){
        country = gsub('"',"",country)
        country = strsplit(country,",")[[1]]
      }
      # load country polygon
      country_shp = raster::shapefile("data/0_basemap/ne_10m_admin_0_countries.shp")
      one_country = subset(country_shp,ADMIN %in% country)
      one_country = spTransform(one_country,
                                crs(paste0("+init=epsg:",
                                           crs_grid))
      )
      ext_temp = raster::extent(one_country)
      crs_ext_full = rep(NA,4)
      crs_ext_full[1] = ext_temp[1]
      crs_ext_full[2] = ext_temp[3]
      crs_ext_full[3] = ext_temp[2]
      crs_ext_full[4] = ext_temp[4]

      if(flag_adjust_by_res){
        crs_ext_full[1] = crs_ext_full[1] - crs_ext_full[1]%%res_x
        crs_ext_full[2] = crs_ext_full[2] - crs_ext_full[2]%%res_y
        crs_ext_full[3] = crs_ext_full[3] - crs_ext_full[3]%%res_x
        crs_ext_full[4] = crs_ext_full[4] - crs_ext_full[4]%%res_y
      }
    }else{
      print("Please provide country name/s, and crs of the grid system")
    }
  }

  if(method=="crs_extent"){
    crs_ext_full = find_crs_extent(crs_grid)

    if(flag_adjust_by_res){
      crs_ext_full[1] = crs_ext_full[1] - crs_ext_full[1]%%res_x
      crs_ext_full[2] = crs_ext_full[2] - crs_ext_full[2]%%res_y
      crs_ext_full[3] = crs_ext_full[3] - crs_ext_full[3]%%res_x
      crs_ext_full[4] = crs_ext_full[4] - crs_ext_full[4]%%res_y
    }
  }

  if(method=="occ_extent" ){
    if( !is.null(occ) ){
      occ_prj = spTransform(occ,crs_grid)
      ext_temp = raster::extent(occ_prj)
      crs_ext_full = rep(NA,4)
      crs_ext_full[1] = ext_temp[1]
      crs_ext_full[2] = ext_temp[3]
      crs_ext_full[3] = ext_temp[2]
      crs_ext_full[4] = ext_temp[4]


      ### further refine the origin coordinate
      if(flag_adjust_origin){

        if( !is.null(res_x)  & !is.null(res_y) ){
          filters= c(res_x,res_y)
        } else{
          filters= sort(c (10^(2:10), 10^(3:10)/2  ))
        }
        temp_origin = infer_origin(input_occ=occ_prj,
                                   fun=min,
                                   flag_unit=flag_unit,
                                   filters = filters)
        crs_ext_full[1] = temp_origin[1]
        crs_ext_full[2] = temp_origin[2]
      }
    } else{
      print("Please provide occurrences as a spatial object")
    }
  }

  return(crs_ext_full)
}




cal_angle <- function(M,
                      N){
  N=c(N[1]-M[1],N[2]-M[2])
  M=c(0,100)
  temp <- atan2(N[2],N[1]) - atan2(M[2],M[1])
  -1* temp*180/pi
  #  so.....   0 is north, 180 is south, 3 clock is 90
  # angle3(c(1,1),
  #        c(2,2)  )
  # angle3(c(1,1),
  #        c(1,2)  )
  # angle3(c(1,1),
  #        c(1,1)  )
  # angle3(c(1,1),
  #        c(2,1)  )
  # angle3(c(1,1),
  #        c(-2,1)  )
}
