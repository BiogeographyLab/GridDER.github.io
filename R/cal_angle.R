#' @title Cal Angle function
#' @description This function calculates the angle between two points. It uses one point as the reference center. 0 degree means top, 90 degree means right, 180 degree means below, and 270 degree means left.
#' @author Xiao Feng
#'
#' @param M Coordinate of the reference point
#' @param N Coordinate of the reference point
#'
cal_angle <- function(M,
                      N) {
  N <- c(N[1] - M[1], N[2] - M[2])
  M <- c(0, 100)
  temp <- atan2(N[2], N[1]) - atan2(M[2], M[1])
  -1 * temp * 180 / pi
}
