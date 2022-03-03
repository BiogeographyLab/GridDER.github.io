#' @title Find the Distance
#' @description Find the distance of x(or y), then return a frequency table
#'
#' @author Xiao Feng
#'
#' @param input_v Vector? X or Y coordinates
#' @param round_num Numeric
#'
#' @return
#' @export
#'
#' @examples
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
