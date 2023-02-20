#res_x, resolution of a grid system along horizontal axis
#res_y, resolution of a grid system along vertical axis
# this function assesses the spatial uncertainty of a grid system by calculating half of the diagonal of a grid cell
assess_sp_uncertainty <-  function(res_x,res_y){
  output = (res_x^2+res_y^2)^0.5/2
  return (output)
}
