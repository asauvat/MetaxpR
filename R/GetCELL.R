#' Cell Image Query
#'
#' Return the combined image of given plate location and mark cell coordinates with a white circle
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
#' @param Range a list containing range values dor each channel image normalization (see normalize from EBImage)
#' @param col.order the order of colours to be applied when combining channels
#' @param channel.choice channel selection (3 maximum)
#' @param disp 'raster' or 'browser'. Where should be the image plotted. See display from EBImage package
#' @param TimeCourse does the image come from timecourse experiment ?
#' @param TimePoint the timepoint of the image. Ignored if TimeCourse=FALSE
#' @param resize shall the returned image be resized ?
#' @param dim.resize the size of resized image. Ignored if resize=FALSE
#' @param returnMat shall image matrix be returned instead of dispayed ?
#' @param CellStep radius of the circle around from cell center coordinates.
#' @param xcoord x coordinate of cell pixel centroid
#' @param ycoord y coordinate of cell pixel centroid
#' @param fullIm shall full image be returned or cell zoom only?
#' @return a raster image or Image class element
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples 
#' #Not running
#' #Bi-parametric plot of cell properties
#' smoothScatter(log10(GlobalData$Hoechst.Norm),log10(GlobalData$Nuc.Corr),colramp=myRamp,useRaster=T,
#' nrpoints=0,bandwidth=0.001,nbin=500,ylab='Nuclear Area',xlab='Corrected Hoechst Intensity',main='Debris Segmentation')
#' 
#' #Get coordinates of given point
#' test = identify(log10(GlobalData$Hoechst.Norm),log10(GlobalData$Nuc.Corr),n=1,plot=F)
#' 
#' #Draw image where cell is found
#' GetCELL(PlateID = GlobalData$Plate.ID[test],SiteID = GlobalData$Site.ID[test],WellID = GlobalData$Well.ID[test],xcoord = GlobalData$'Pixel Centroid X'[test],ycoord = GlobalData$'Pixel Centroid Y'[test],
#'            SERVER =SERVER,Range=list(c(0,0.4),c(0,0.1),c(0,0.05)),disp='browser',CellStep = 50, fullIm = T) #Return cell located on plot
#'
#' @export
#'

GetCELL = function(SERVER = 'MDCStore',PlateID,WellID,SiteID = 1,Range = lapply(1:3,function(x)c(0,0.1)),use.autoRange = T,col.order=c('blue','green','red'),
                 channel.choice = 1:3,disp = 'raster',TimeCourse = F, TimePoint = 1, returnMat = F,CellStep = 50, xcoord, ycoord,fullIm=T,...){
  
  #======================================================================================
  
   ImagePath = GetIMPath(SERVER,ID,PWD,PlateID,WellID,SiteID,TimePoint)
  
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
  
  X= floor(as.numeric(xcoord))
  Y= floor(as.numeric(ycoord))
  
  if ((X - CellStep) < 1){
    Xmin=1
  }else {Xmin = X - CellStep}
  
  if (X + CellStep > ncol(RGB)){
    Xmax=ncol(RGB)
  }else {Xmax = X + CellStep}
  
  
  if ((Y - CellStep) < 1){
    Ymin=1
  }else {Ymin = Y - CellStep}
  
  if (Y + CellStep > nrow(RGB)){
    Ymax=nrow(RGB)
  }else {Ymax = Y + CellStep}
  
  #======================================================================================
  
  Circol = 'white'
  
  if(!returnMat){
    if(fullIm){
      EBImage::display(drawCircle(RGB,xcoord,ycoord,CellStep,col =Circol), method = disp,title = paste(PlateID, WellID, SiteID, sep = ' '))
    }else{
      EBImage::display(drawCircle(RGB[Xmin:Xmax,Ymin:Ymax,],xcoord,ycoord,CellStep,col =Circol), method = disp,title = paste(PlateID, WellID, SiteID, sep = ' '))
    }
  }else{
    if(fullIm){
      return(drawCircle(RGB,xcoord,ycoord,CellStep,col =Circol))
    }else{
      return(drawCircle(RGB[Xmin:Xmax,Ymin:Ymax,],xcoord,ycoord,CellStep,col =Circol))
    }
  }

}