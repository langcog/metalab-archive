
# calculate variance on d, assumes variance in (imaginery) control group same as experimental
d_var<- function(n, d){
  vd = ((2*n)/(n^2)) + ((d^2)/(4*n))
  return(vd)
}