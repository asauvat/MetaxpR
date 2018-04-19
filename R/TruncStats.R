#' Truncated stats

#' @keywords internal
#' @export


trunc.mean <- function(x){
  
  x=x[!is.na(x)]
  
  PrimMean = mean(x, na.rm=T)
  n <- length(x)
  
  Dev = rep(0,n)
  for (i in 1:n) {
    
    Dev[i]=(x[i]-PrimMean)^2
  }
  
  out = max(Dev)
  x2 = x[which(Dev != out)]
  n2=length(x2)
  
  if (n<=2| n2 ==0){
    return(mean(x,na.rm=T))
  }else{
    return(mean(x2,na.rm=T))
  }
  
}

trunc.sd <- function(x){
  
  x=x[!is.na(x)]
    
  PrimMean = mean(x, na.rm=T)
  n <- length(x)
  
  Dev = rep(0,n)
  for (i in 1:n) {
    
    Dev[i]=(x[i]-PrimMean)^2
  }
  
  out = max(Dev)
  x2 = x[which(Dev != out)]
  
  n2=length(x2)
  
  if (n<=2| n2==0){
    return(sd(x,na.rm=T))
  }else{
    return(sd(x2,na.rm=T))
  }
}
  
  trunc.val <- function(x){
    
    x=x[!is.na(x)]
        
    PrimMean = mean(x, na.rm=T)
    n <- length(x)
    
    Dev = rep(0,n)
    for (i in 1:n) {
      
      Dev[i]=(x[i]-PrimMean)^2
    }
    
    out = max(Dev)
    x2 = x[which(Dev != out)]
    
    n2=length(x2)
    
    if (n <=2| n2 ==0){
      return(x)
    }else{
      return(x2)
    }
  }

    trunc.std <- function(x) trunc.sd(x)/sqrt(length(x[!is.na(x)]))

    std <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
  

auto.trunc <- function(x, t){
  
  if(missing(t)){
    t = 0.4
  }
  
  #
  x=x[!is.na(x)]
  Dens = density(x)
  Approx.Mean = Dens$x[which.max(Dens$y)]
  #
  n <- length(x)
  Dev = array(double())
  for(i in 1:n){
    
    Dev =c(Dev,abs(x[i]-Approx.Mean)/Approx.Mean)
    
  }
  
  x2 = x[which(Dev<=t)]
  if(length(x2)<2){
    x2 = head(x[order(Dev)],n=2)
  }
  
  return(x2)
  
} 
