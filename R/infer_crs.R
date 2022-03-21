#' @title Infer Crs
#' @description This function will prj occ to a list of ~6600 crs, find  nearest 4 points, find the angle to the 4 points, then take reminder %%90, the smaller the better also find the distance to the 4 points, then all the distance is standardized to 0 mean and 1sd  then do a histogram, and calculate the freq of each bin,  pick the bin with highest freq ideally, the true crs will lead to the highest freq in that bin.
#' @author Xiao Feng
#'
#' @param occ_path Character. A string, path of input occurrences, should have decimalLongitude & decimalLatitude.
#' @param truth_crs_num Character. A string, epsg code of the true crs if known.
#' @param flag_saveTemp Logical. If "True", save the middle output.
#' @param flag_newCal Logical. If "True" do the distance calculation again.
#' @param temp_path Character. Path to save.
#' @param cup_num
#' @param flag_debug
#'
#' @return
#' @export
#' @note Using 15 cores, it took ~10-20 minutes to finish this workflow.
#'
#' @import data.table
#' @import raster rgdal
#' @examples
infer_crs = function(occ_path,  # a string, path of input occurrences, should have decimalLongitude & decimalLatitude
                     truth_crs_num = NA,  # a string, epsg code of the true crs if known
                     flag_saveTemp=TRUE,  # save the middle output
                     flag_newCal=TRUE,    # do the distance calculation again
                     temp_path = "data/3_infer_grid_crs/prj_occ_temp_v2/",
                     cup_num = 10,  # adjust it before use
                     flag_debug=-1
){
  library(data.table)
  library(raster)
  library(rgdal)

  source("inst/1_source_seek_occ_pattern.R")
  source("inst/grid_generation.R")
  catf <- function(..., file="temp/temp.log", append=TRUE){
    cat(..., file=file, append=append)
  }



  dir.create(dirname(temp_path))
  dir.create(temp_path)

  #1 load occ, project to a focal crs
  occ1 = load_occ(occ_path)
  crs_list = readRDS("data/3_infer_grid_crs/crs_list_prj.rds")
  if(any(flag_debug>0) )crs_list = crs_list[flag_debug,]

  if(!is.na(truth_crs_num)){
    to_add = crs_list[1,]
    to_add[] = NA
    to_add$code = truth_crs_num
    to_add$note = "truth"
    crs_list = rbind(to_add,crs_list)
  }

  #crs_list$maxHist_x = NA
  #crs_list$maxHist_y = NA

  # i= 1
  # i=6101
  # i=169
  # i=3
  # i=4964
  # i=6610
  NNN = nrow(crs_list)
  #NNN=1417

  # if( user_bin >= nrow(occ1@coords)/10 ){
  #   user_bin = round( nrow(occ1@coords)/10)
  # }


  #for(i in c(1523:1525,1:10) ) {

  library(foreach)
  library(doParallel)
  cl<-makeCluster(cup_num)
  registerDoParallel(cl)
  out_foreach = foreach(i = 1:NNN,#1400:1417,
                        .combine=rbind,
                        .errorhandling = 'pass',
                        .export=c("catf","cal_angle"),
                        .packages=c("raster","sp","rgdal"))%dopar%{



                          print(i)
                          catf(i,"\n",file=paste0(temp_path,"_log.txt"))
                          one_crs_code =  crs_list$code[i]
                          one_isGeo = crs_list$is_Geo[i]
                          crs_focal = crs(paste0("+init=epsg:",one_crs_code )   )
                          maxHist_x = NA ;maxHist_y =NA
                          if(is.na(crs_focal) | is.null(crs_focal)  ){
                            catf(i,"skip crs","\n",file=paste0(temp_path,"_log.txt"))
                            return ( c(i,maxHist_x,maxHist_y) )
                            next # looks like this does not work
                          }


                          #basename(occ_path)
                          occ_name  = paste0(temp_path,basename(occ_path),"_prj_",one_crs_code)
                          # occ_dist_nameX  = paste0(temp_path,basename(occ_path),
                          #                          "_prj_",one_crs_code,"dist_x")
                          # occ_dist_nameY  = paste0(temp_path,basename(occ_path),
                          #                          "_prj_",one_crs_code,"dist_y")
                          # occ_dist_nameXY  = paste0(temp_path,basename(occ_path),
                          #                          "_prj_",one_crs_code,"dist_xy")
                          occ_dist4  = paste0(temp_path,basename(occ_path),
                                              "_prj_",one_crs_code,"dist4")
                          occ_angle4  = paste0(temp_path,basename(occ_path),
                                               "_prj_",one_crs_code,"angle4")

                          if(#file.exists(occ_dist_nameX) &
                            #file.exists(occ_dist_nameY) &
                            #file.exists(occ_dist_nameXY) &
                            file.exists(occ_dist4) &
                            file.exists(occ_angle4) &
                            (!flag_newCal)
                          ){
                            catf(i,"load x,y ","\n",file=paste0(temp_path,"_log.txt"))

                            # myD_near_x_std = readRDS(occ_dist_nameX)
                            # myD_near_y_std = readRDS(occ_dist_nameY)
                            # myD_near_xy_std = readRDS(occ_dist_nameXY)
                            distance_4 = readRDS(occ_dist4)
                            angle_4 = readRDS(occ_angle4)
                          } else{
                            if (file.exists(occ_name) ){
                              occ_prj = readRDS(occ_name)
                              catf(i,"load occ ","\n",file=paste0(temp_path,"_log.txt"))

                            } else {
                              occ_prj = spTransform(occ1,crs_focal)
                              catf(i,"prj occ ","\n",file=paste0(temp_path,"_log.txt"))

                              if(flag_saveTemp){
                                saveRDS(occ_prj,file=occ_name)
                              }
                            }
                            catf(i,"cal dist ","\n",file=paste0(temp_path,"_log.txt"))

                            # demo = occ_prj@coords[,1] # only using x
                            # myD = dist( demo )
                            # myD = as.matrix(myD)
                            # myD[myD==0] = NA
                            # myD_near= apply(myD, 1, FUN = min,na.rm = TRUE)
                            # myD_near_x_std <- (myD_near- mean(myD_near))/sd(myD_near)
                            #myD_near_x_std = myD_near

                            # demo = occ_prj@coords[,2] # only using y
                            # myD = dist( demo )
                            # myD = as.matrix(myD)
                            # myD[myD==0] = NA
                            # myD_near= apply(myD, 1, FUN = min,na.rm = TRUE)
                            # myD_near_y_std <- (myD_near- mean(myD_near))/sd(myD_near)
                            #myD_near_y_std = myD_near

                            demo = occ_prj@coords #  using x & y
                            myD = dist( demo )
                            myD = as.matrix(myD)
                            myD[myD==0] = NA
                            #myD_near= apply(myD, 1, FUN = min,na.rm = TRUE)
                            #myD_near_which= apply(myD, 1, FUN = which.min)

                            #also consider the top four nearest points from up/down/left/right
                            #... to do...
                            #
                            occ_i = 1
                            distance_4 = matrix(NA,nrow = 4,ncol = nrow(occ_prj@coords) )
                            angle_4 = matrix(NA,nrow = 4,ncol = nrow(occ_prj@coords) )
                            for(occ_i in 1: nrow(occ_prj@coords)  ){
                              # first find the near 10 points
                              top_i = kit::topn(myD[,occ_i],n=12,decreasing=F)
                              top_angle = rep(NA,length(top_i))
                              j=1
                              # get the angle of the 10 points
                              for(j in 1:length(top_i)){
                                top_angle[j]= cal_angle(occ_prj@coords[occ_i,],
                                                        occ_prj@coords[top_i[j],])
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


                              if(F){
                                plot(occ_prj)
                                #occ_i = 13
                                plot(occ_prj[occ_i,],add=T,col="red")
                                plot(occ_prj[top_i,],add=T,col="blue")
                                plot(occ_prj[top_i[up_i],],add=T,col="green")
                                plot(occ_prj[top_i[left_i],],add=T,col="yellow")
                                plot(occ_prj[top_i[right_i],],add=T,col="purple")
                                plot(occ_prj[top_i[bottom_i],],add=T,col="orange")


                              }

                              # find the nearest distance of each direction, then get its angle
                              ####as.numeric(names(which.min( myD[top_i[up_i],occ_i] )))
                              # distance_4[,occ_i] = c(min( myD[top_i[up_i],occ_i] ),
                              #                        min( myD[top_i[right_i],occ_i] ),
                              #                        min( myD[top_i[bottom_i],occ_i] ),
                              #                        min( myD[top_i[left_i],occ_i] )   )
                              #
                              # angle_4[,occ_i] = c(top_angle [ up_i[ which.min(myD[top_i[up_i],occ_i]) ] ]   ,
                              #                     top_angle [ right_i[ which.min(myD[top_i[right_i],occ_i]) ] ]   ,
                              #                     top_angle [ bottom_i[ which.min(myD[top_i[bottom_i],occ_i]) ] ]   ,
                              #                     top_angle [ left_i[ which.min(myD[top_i[left_i],occ_i]) ] ]   )

                              distance_4[,occ_i] = c(ifelse(length(up_i)==0, NA, min( myD[top_i[up_i],occ_i]     )),
                                                     ifelse(length(right_i)==0, NA, min( myD[top_i[right_i],occ_i]  )),
                                                     ifelse(length(bottom_i)==0, NA, min( myD[top_i[bottom_i],occ_i] )),
                                                     ifelse(length(left_i)==0, NA, min( myD[top_i[left_i],occ_i]   ))   )

                              angle_4[,occ_i] = c(ifelse(length(up_i)==0, NA,top_angle [ up_i[ which.min(myD[top_i[up_i],occ_i]) ] ]   ),
                                                  ifelse(length(right_i)==0, NA,top_angle [ right_i[ which.min(myD[top_i[right_i],occ_i]) ] ]  ) ,
                                                  ifelse(length(bottom_i)==0, NA,top_angle [ bottom_i[ which.min(myD[top_i[bottom_i],occ_i]) ] ]  ) ,
                                                  ifelse(length(left_i)==0, NA,top_angle [ left_i[ which.min(myD[top_i[left_i],occ_i]) ] ] )  )

                            }
                            #myD_near_xy_std <- (myD_near- mean(myD_near))/sd(myD_near)
                            #myD_near_xy_std = myD_near
                            if(flag_saveTemp){
                              saveRDS(distance_4,file=occ_dist4)
                              saveRDS(angle_4,file=occ_angle4)
                              #saveRDS(myD_near_x_std,file=occ_dist_nameX)
                              #saveRDS(myD_near_y_std,file=occ_dist_nameY)
                              #saveRDS(myD_near_xy_std,file=occ_dist_nameXY)
                            }
                          }
                          #mean( distance_4[1,],na.rm=T)
                          mean_d_4 = apply(distance_4,1,mean,na.rm=T)
                          #mean_d_y = mean(distance_4[c(1,3),],na.rm=T)
                          #mean_d_x = mean(distance_4[c(2,4),],na.rm=T)

                          sd_d_4 = apply(distance_4,1,sd,na.rm=T)
                          #sd_d_y = sd(distance_4[c(1,3),],na.rm=T)
                          #sd_d_x = sd(distance_4[c(2,4),],na.rm=T)


                          temp_angle2 = array(dim = c(dim(angle_4),2) )
                          temp_angle2[,,1] = abs(angle_4 %% 90)
                          temp_angle2[,,2] = abs( (90-angle_4) %% 90)
                          temp_angle3 = apply(temp_angle2,c(1,2),min)
                          mean_a_4 = apply(temp_angle3,1,mean,na.rm=T)
                          #mean_a_y = mean(temp_angle3[c(1,3),],na.rm=T)
                          #mean_a_x = mean(temp_angle3[c(2,4),],na.rm=T)
                          #mean_a_all = mean(temp_angle3,na.rm=T)
                          mean_a_all = mean(temp_angle3[c(1,2),],na.rm=T)

                          sd_a_4 = apply(temp_angle3,1,sd,na.rm=T)
                          #sd_a_y = sd(temp_angle3[c(1,3),],na.rm=T)
                          #sd_a_x = sd(temp_angle3[c(2,4),],na.rm=T)
                          #sd_a_all = sd(temp_angle3,na.rm=T)
                          sd_a_all = sd(temp_angle3[c(1,2),],na.rm=T)

                          #sd_a_= apply(temp_angle3,1,sd,na.rm=T)
                          #user_bin = length(myD_near_xy_std)
                          #one_hist_xy = hist(myD_near_xy_std,user_bin,plot =FALSE)
                          #maxHist_xy = max(one_hist_xy$counts)
                          #maxDen_xy = max(one_hist_xy$density)
                          #temp_angle2 = rbind( abs(temp_angle %% 90) ,
                          #                    abs( (90-temp_angle) %% 90)  )
                          #temp_angle3 = apply(temp_angle2, 2, FUN = min)
                          #mean_angle = mean( temp_angle3 )

                          getMaxHist = function(a_vector,flag_std=T){
                            a_vector = as.vector(a_vector)
                            a_vector = a_vector[!is.na(a_vector)]

                            if(flag_std){
                              a_vector = (a_vector- mean(a_vector))/sd(a_vector)
                            }
                            a_vector_hist = hist(a_vector,length(a_vector),plot =FALSE)
                            return( max(a_vector_hist$counts) )
                          }

                          max_histSTD_4 = c(
                            ifelse(all(is.na(distance_4[1,])),NA , getMaxHist(distance_4[1,],flag_std=T))   ,
                            ifelse(all(is.na(distance_4[2,])),NA , getMaxHist(distance_4[2,],flag_std=T))   ,
                            ifelse(all(is.na(distance_4[3,])),NA , getMaxHist(distance_4[3,],flag_std=T))   ,
                            ifelse(all(is.na(distance_4[4,])),NA , getMaxHist(distance_4[4,],flag_std=T))
                            # getMaxHist(distance_4[2,],flag_std=T),
                            # getMaxHist(distance_4[3,],flag_std=T),
                            # getMaxHist(distance_4[4,],flag_std=T)#,
                            #getMaxHist(distance_4[c(1,3),],flag_std=T),
                            #getMaxHist(distance_4[c(2,4),],flag_std=T)
                          )

                          #max_hist_4 = c(
                          #  getMaxHist(distance_4[1,],flag_std=F),
                          #  getMaxHist(distance_4[2,],flag_std=F),
                          #  getMaxHist(distance_4[3,],flag_std=F),
                          #  getMaxHist(distance_4[4,],flag_std=F),
                          #  getMaxHist(distance_4[c(1,3),],flag_std=F),
                          #  getMaxHist(distance_4[c(2,4),],flag_std=F)  )


                          catf(i,"done ","\n",file=paste0(temp_path,"_log.txt"))
                          gc()
                          c(i,
                            mean_d_4,#mean_d_y,mean_d_x,
                            sd_d_4,#sd_d_y,sd_d_x,
                            mean_a_4,mean_a_all,#mean_a_y,mean_a_x,
                            sd_a_4,sd_a_all,#sd_a_y,sd_a_x,
                            max_histSTD_4,
                            #max_hist_4
                            sum(max_histSTD_4,na.rm=T)
                          )


                        } # end of crs loop
  stopCluster(cl)

  #crs_list$maxHist_xy = crs_list$maxHist_x + crs_list$maxHist_y

  out_foreach = data.frame(out_foreach)
  names(out_foreach) = c("i",
                         "mean_d_up","mean_d_right","mean_d_bottom","mean_d_left",#"mean_d_up_bottom","mean_d_right_lfet",
                         "sd_d_up","sd_d_right","sd_d_bottom","sd_d_left",#"sd_d_up_bottom","sd_d_right_lfet",            # smaller, better

                         "mean_a_up","mean_a_right","mean_a_bottom","mean_a_left","mean_a_all",#"mean_a_up_bottom","mean_a_right_lfet",# close to 0, better
                         "sd_a_up","sd_a_right","sd_a_bottom","sd_a_left", "sd_a_all",#"sd_a_up_bottom","sd_a_right_lfet",           # smaller, better

                         "max_histSTD_up","max_histSTD_right","max_histSTD_bottom","max_histSTD_left",#"max_histSTD_up_bottom","max_histSTD_right_lfet" ,# larger, better
                         #"max_hist_up","max_hist_right","max_hist_bottom","max_hist_left","max_hist_up_bottom","max_hist_right_lfet"# larger, better
                         "max_histSTD_sum"
  )
  out_foreach= out_foreach[order(out_foreach$i),]
  crs_list_update = cbind(crs_list[1:NNN,],out_foreach)

  # do some sorting here:
  crs_list_update$max_histSTD_all = crs_list_update$max_histSTD_up+
    crs_list_update$max_histSTD_right+
    crs_list_update$max_histSTD_bottom+
    crs_list_update$max_histSTD_left

  good_angle = kit::topn(crs_list_update$mean_a_all,n=10,decreasing=F)
  temp1 = crs_list_update[good_angle,]
  good_dist = kit::topn(temp1$max_histSTD_all,n=10,decreasing=T)
  temp2 = temp1[good_dist,]

  print("The top 10 CRS are:")
  print( temp2[,c("code","note")])  # the true crs is ranked no1


  saveRDS(crs_list_update,paste0(occ_path,"_inferredCRS_v2.rds"))
  write.csv(crs_list_update,paste0(occ_path,"_inferredCRS_v2.csv"))

  saveRDS(temp2,paste0(occ_path,"_inferredCRS_v2_selected.rds"))
  write.csv(temp2,paste0(occ_path,"_inferredCRS_v2_selected.csv"))
  #in crs_list_update, the best crs should have the highest score

  outout = list(selected = temp2, all=crs_list_update)
  return(outout)
}
