#' p-values to stars
#'
#' @keywords internal
#' @export

pv2star = function(x,char = '*'){
  
  y=rep('',length(x))
  
  for(i in 1:length(x)){
    
    if(!is.na(x[i])){
      
      if(x[i]<=0.05){
        y[i]=char
      }
      if(x[i]<=0.01){
        y[i]=paste(rep(char,2),collapse='')
      }
      if(x[i]<=0.001){
        y[i]=paste(rep(char,3),collapse='')
      }
    }
  }
  
  return(y)
}