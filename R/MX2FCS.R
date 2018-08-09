#' Export cell data as Flow Cytometry Standard
#'
#' Retrieve cell data from database and export as Flow Cytometry Standard
#'
#' @import Biobase
#' @import flowCore
#'
#' @param dat data frame coming from \code{\link{GetMDCData}}
#' @param coi column Of Interest to be exported into FCS
#' @param export folder location where data will be exported
#' @param wlab column name that contain well identifier. Default ix 'Well.ID' which is return by \code{\link{GetMDCData}}
#' @param proj character string with project name
#' @return NULL
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples
#'#Not running
#'mydat=GetMDCData(SERVER='MDCStore',MeasurementID=100)
#'MX2FCS(mydat,coi=c('Hoechst_Intensity','Nuclear_area'),export='MYFCS')
#'
#' @export
#'

MX2FCS = function(dat,coi,export,wlab='Well.ID',proj=''){
  
  if(!dir.exists(export)){
    dir.create(export)
    print(paste0(export,' created'))
  }
  #Create FCS parameters
  fpar = data.frame(name=c(coi,'Event Count'),desc=c(coi,'Event Count'),range=c(apply(dat[,coi],2,function(x)round(max(x,na.rm=T))),nrow(dat)),minRange=c(apply(dat[,coi],2,function(x)round(min(x,na.rm=T))),0),maxRange=c(apply(dat[,coi],2,function(x)round(max(x,na.rm=T))),65535))
  rownames(fpar)=paste0('$P',1:nrow(fpar))
  
  #Create FCS descriptors
  fdesc = list(FCSversion="3",PROJ=proj,DATATYPE="F")
  fcat=c('R','B','N','S','E','L','O','P','V')
  for(N in rownames(fpar)){
    li=list(fpar[N,'range'],16,fpar[N,'name'],fpar[N,'desc'],"0,0","NAN","NAN","0","NAN")
    names(li)=paste0(N,fcat)
    fdesc=c(fdesc,li)
  };rm(li);fdesc=lapply(fdesc,as.character)
  
  #Create objects and files
  flf = flowFrame(exprs=as.matrix(cbind(dat[,coi],'Event Count'=1:nrow(dat))),parameters=as(fpar,'AnnotatedDataFrame'),description=fdesc)
  write.FCS(flf, paste0(export,'/','aggregate.fcs'))
  sapply(unique(dat[,wlab]),function(w){
    flf = flowFrame(exprs=as.matrix(cbind(dat[which(dat[,wlab]==w),coi],'Event Count'=1:length(which(dat[,wlab]==w)))),parameters=as(fpar,'AnnotatedDataFrame'),description=fdesc)
    write.FCS(flf, paste0(export,'/',w,'.fcs'))
  })
  
}