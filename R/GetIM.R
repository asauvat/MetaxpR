#' Whole Image Query
#'
#' Return the combined image of given plate location
#'
#' @import EBImage
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database. Ignored if DB given
#' @param ID the identifiant for connexion. Ignored if DB given
#' @param PWD the password for connexion. Ignored if DB given
#' @param PlateID the PlateID of the image
#' @param WellID the WellID of the image
#' @param SiteID the SiteID of the image
#' @param Z The Z position of the image. Default is 1, value if no stacks is recorded. 0 for max projection, if recorded
#' @param Range a list containing range values dor each channel image normalization (see normalize from EBImage)
#' @param col.order the order of colours to be applied when combining channels
#' @param channel.choice channel selection (3 maximum)
#' @param disp 'raster' or 'browser'. Where should be the image plotted. See display from EBImage package
#' @param TimeCourse does the image come from timecourse experiment ?
#' @param TimePoint the timepoint of the image. Ignored if TimeCourse=FALSE
#' @param resize shall the returned image be resized ?
#' @param dim.resize the size of resized image. Ignored if resize=FALSE
#' @param returnMat shall image matrix be returned instead of dispayed ?
#'
#'
#' @return a raster image or EBImage::Image class element
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples 
#' #Not running
#' GetIM(SERVER='YODA-SERVER', PlateID=1,WellID='E05',SiteID=c(2,2),disp='raster')
#'
#' @export
#'

GetIM = function(SERVER = 'MDCStore',ID='moldev',PWD='moldev',PlateID,WellID,SiteID = c(1,1),Z=1,Range = lapply(1:3,function(x)c(0,0.1)),use.autoRange = T,col.order=c('blue','green','red'),
                 channel.choice = 1:3,disp = 'raster',TimeCourse = F, TimePoint = 1, resize = F,dim.resize=600, returnMat = F,...){
  
  #======================================================================================

  if(length(col.order)!=length(channel.choice)){
    return('choosen channels must fit col.order!')
  }
  #======================================================================================

  ImagePath = GetIMPath(SERVER,ID,PWD,PlateID,WellID,SiteID,TimePoint,Z)
  
  #======================================================================================
  
  nchannels = length(ImagePath)
  if(nchannels<length(col.order)){
    return('More choices than channels!')
  }
  
  suppressWarnings({
    eval(parse(text=
                 paste0('RGB = rgbImage(',
                        paste0(sapply(seq_along(channel.choice),function(x)
                          paste0(col.order[x],sprintf("=Normalize(readImage(ImagePath[%d]),inputRange=Range[[%d]],autoRange=use.autoRange,...)",channel.choice[x],x))),
                          collapse=','),
                        ')')))
  })
  
  #======================================================================================
  
  if(resize){
    RGB = resize(RGB,dim.resize)
  }
  
  if(!returnMat){
    EBImage::display(RGB, method = disp)
  }else{
    return(RGB)
  }
  
}