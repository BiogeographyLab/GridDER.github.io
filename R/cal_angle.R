#' @title Cal Angle function
#' @description
#' @author Xiao Feng
#'
#' @param M
#' @param N
#'
cal_angle <- function(M,
                      N) {
  N <- c(N[1] - M[1], N[2] - M[2])
  M <- c(0, 100)
  temp <- atan2(N[2], N[1]) - atan2(M[2], M[1])
  -1 * temp * 180 / pi
}
