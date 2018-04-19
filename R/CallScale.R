#' Sigmoidal data normalization
#'
#' Flatten Z-score by sigmoidal transformation
#'
#' @param x array of double 
#' @param lambda smoothness of transformation
#' @param z0 center of sigmoidal transformation
#' @param scaled are data scaled ?
#'
#' @return Normalized data
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

call.scale = function(x, lambda = 2, z0 = 1.5, scaled = F){

  if(!scaled){
      x = scale(x)
  }
  
  return(1/(1+exp(-lambda*(x-z0))))
}