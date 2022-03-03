#' @title Infer Resolution
#' @description
#'
#' @author Xiao Feng
#'
#' @param input_coord
#' @param digits
#' @param flag_unit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
