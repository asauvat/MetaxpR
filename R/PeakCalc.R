#' Better than mean for non-Gaussian distribution scaling
#'
#' Get peak value of data distribution
#'
#' @param x A numerical vector 
#' @param use.log Shall a log10 transformation be applied ?
#'
#' @return A double which corresponds to the abciss of maximum density
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
peak.calc = function(x,use.log=F){
  
  if(use.log){
  dens = density(x)
  y = dens$x[which.max(dens$y)]
  }else{
  dens = density(log10(na.omit(x)+1))
  y = 10^(dens$x[which.max(dens$y)])-1
  }
  return(y)
}
