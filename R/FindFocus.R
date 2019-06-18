#' Out of focus detection
#'
#' Return potential out of focus sites from the plate
#'
#' @import parallel
#' @import pbapply
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param PlateID the Plate Identifier
#' @param nCores the number of cores for parallel processing
#'
#' @return A list containing possible out ouf focus sites for each well
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples
#'#Not running
#'OF = FindFocus(SERVER = 'MDCStore',ID = 'sa', PWD = '***') 
#'
#' @export
#'

#===================================================================================================================================================#

FindFocus = function(SERVER, PlateID,nCores=detectCores()){
  PINF = GetPINFO(SERVER,PlateID=PlateID)
  alls = lapply(1:nrow(PINF$SID),function(i)PINF$SID[i,])
  #--------------------------------------------------------
  cl = parallel::makeCluster(nCores)
  parallel::clusterExport(cl,c('SERVER','PlateID','PINF','alls'), envir=environment())
  invisible({parallel::clusterEvalQ(cl,{library(MetaxpR);library(EBImage)})})
  #
  SC=pbapply::pblapply(PINF$WID,function(w){
    SCi=sapply(alls,function(sxy){
    #Import images----#
    IP = GetIMPath(SERVER,PlateID=PlateID,WellID=w,SiteID=sxy)[1] 
    #Calculate FS-----#
    IM = Normalize(suppressWarnings(readImage(IP)),autoRange=T,verbose=F)
    IS = dim(IM); IC = floor(min(IS)/2); ZOI = floor(0.75*IC)
    #Get Laplacian
    LAP = filter2(IM,1/9*(matrix(rep(1,9),nrow=3)),boundary='circular');LAP[which(LAP<0)]=0; 
    LAP = LAP[(IC-ZOI):(IC+ZOI),(IC-ZOI):(IC+ZOI)]
    #Calculate score
    FS = sd(LAP)/mean(LAP)
    return(FS)
    })
    names(SCi)=sapply(alls,function(x)paste(x,collapse='_'))
  },cl=cl)
  parallel::stopCluster(cl);rm(cl)
  names(SC)=PINF$WID
  #--------------------------------------------------------
  qu = quantile(unlist(SC),probs=seq(0,1,0.025))
  li = mean(unlist(SC))-(2*sd(unlist(SC)))
  #
  th = li; if(qu[3]<li){th=qu[3]}
  #--------------------------------------------------------
  OOF = lapply(SC,function(x)which(x<=th))
  OOF = OOF[sapply(OOF,function(x)length(x)!=0)]
  return(OOF)
}
