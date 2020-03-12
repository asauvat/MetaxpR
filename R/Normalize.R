#' Image normalization
#'
#' Normalize image pixel intensities between 0 and 1 (defunct soon)
#'
#' @param ImObj An array, a matrix or an image 
#' @param inputRange The targeted min and max pixel intensities
#' @param autoRange A boolean indicating if inputRange should be calculated automatically
#' @param step A numeric value, indicating probs from quantile option for inputRange automated calculation
#' @param verbose logical. Shall normalization range be displayed?
#' @return An array, a matrix or an image with values ranged from 0 to 1
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples
#' Mat = replicate(100, rnorm(100))
#' Mat.norm = Normalize(Mat);Mat.autoNorm = Normalize(Mat,autoRange=T)
#' plot(density(Mat));
#' plot(density(Mat.norm));points(density(Mat.autoNorm),type='l',col='blue')
#' 
#'
#' @export
#'

Normalize = function(ImObj,inputRange,autoRange=T,step=5*10**-3,verbose=F){
  
  #-----------------------------------------------------
  if(missing(ImObj)){
    return('cannot do it without image!')
  }
  if(all(ImObj==0)){
    return(ImObj)
  }else{
  if(missing(inputRange) & !autoRange){
    inputRange = c(min(ImObj),max(ImObj))
  }else{
    if(autoRange){
      Im.q = unique(quantile(ImObj,probs=seq(0,1,step)))
      inputRange = c(Im.q[1],tail(Im.q,2)[1])
    }
  }
  #-----------------------------------------------------
  ImNorm = (ImObj - inputRange[1])/(diff(inputRange))
  ImNorm[which(ImNorm<0)] = 0
  ImNorm[which(ImNorm>1)] = 1
  #-----------------------------------------------------
  if(autoRange & verbose){
  print(paste0('inputRange used : ',paste0(inputRange,collapse = ',')))
  }
  #-----------------------------------------------------
  return(ImNorm)
  }
}
  
