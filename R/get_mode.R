#' @title
#' @description
#' @author Xiao Feng
#'
#' @param v
#'
#' @return
#' @export
#'
#' @examples
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
