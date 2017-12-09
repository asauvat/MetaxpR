#' HTD file creation
#'
#' Function that create the HTD file necessary to import images into MX database
#'
#' @param Pty 1 for 384-well plate, 2 for 96-well plate 
#' @param WellList List of wells "acquired" in plate
#' @param TimePt Timepoint number
#' @param PlateName Plate name 
#' @param export Where to write the HTD file
#' @param Sites Number of sites along x and y dimensions
#' @param Channels MX channels
#' @return An HTD file for plate import
#' 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' 
#' @export

HTD.create = function(Pty = 1, WellList = c('A01','A02'), TimePt = 1,PlateName = 'Template',export,
                      Sites = c(3,3), Channels = c("DAPI","GFP")){
  
  #Define Header======================================================================================
  if(Pty == 1){
    xl = 24
    yl = 16
  }else{
    if(Pty == 2){
      xl = 12
      yl = 8
    }else{
      print("Pty has to be 96 or 384 !")
      return(F)
    }
  }
  MyHTD = data.frame(matrix(ncol=xl+1)) #Initialize table
  MyHTD[1,c(1,2)] = c('"HTSInfoFile"','Version 1.0')
  MyHTD[2,c(1,2)] = c('"Description"',paste('"',PlateName,'"',sep=''))
  MyHTD[3,c(1,2)] = c('"PlateType"',Pty) #Pty = 1 is 384well plate
  #
  MyHTD[4,c(1,2)] = c('"TimePoints"',max(TimePt))
  MyHTD[5,c(1,2)] = c('"XWells"',xl)
  MyHTD[6,c(1,2)] = c('"YWells"',yl)
  #====================================================================================================
  
  #Define Well Selection#
  WellSelect = data.frame((matrix(ncol=xl+1)))
  for(i in 1:yl){
    WellSelect = rbind(WellSelect,c(paste('"',paste('WellsSelection',i,sep=''),'"',sep=''),rep(FALSE,xl)))
  }
  WellSelect = WellSelect[-1,]
  #
  for(i in 1:length(WellList)){
    xi = which(LETTERS %in% substring(WellList[i], 1,1))
    yi = as.numeric(substring(WellList[i],2,3))
    WellSelect[xi,yi+1] = TRUE
  }
  #
  MyHTD = rbind(MyHTD,WellSelect)
  
  #Define Sites#
  if(length(Sites) ==1){
    MyHTD = rbind(MyHTD, c('"Sites"',FALSE, rep(NA,xl-1)))
    MyHTD = rbind(MyHTD, c('"XSites"',1, rep(NA,xl-1)))
    MyHTD = rbind(MyHTD, c('"YSites"',1, rep(NA,xl-1)))
  }else{
    
    MyHTD = rbind(MyHTD, c('"Sites"',TRUE, rep(NA,xl-1)))
    MyHTD = rbind(MyHTD, c('"XSites"',Sites[1], rep(NA,xl-1)))
    MyHTD = rbind(MyHTD, c('"YSites"',Sites[2], rep(NA,xl-1)))
    for(i in 1:Sites[2]){
      MyHTD = rbind(MyHTD, c(paste('"','SiteSelection',i,'"',sep=''),rep(TRUE,Sites[1]), rep(NA,xl-Sites[1])))
    }
  }
  
  #Define Channels
  if(length(Channels)==1){
    MyHTD = rbind(MyHTD, c('"Waves"',FALSE, rep(NA,xl-1)))
  }else{
    MyHTD = rbind(MyHTD, c('"Waves"',TRUE, rep(NA,xl-1)))
  }
  MyHTD = rbind(MyHTD, c('"NWavelengths"',length(Channels), rep(NA,xl-1)))
  for(i in 1:length(Channels)){
    MyHTD = rbind(MyHTD, c(paste('"','WaveName',i,'"',sep=''),paste('"',Channels[i],'"',sep=''),rep(NA,xl-1)))
  }
  for(i in 1:length(Channels)){
    MyHTD = rbind(MyHTD, c(paste('"','WaveCollect',i,'"',sep=''),1,rep(NA,xl-1)))
  }
  
  #End File
  hex = list()
  for(i in 1:32){
    hex = c(hex,list(c(0:9,letters[1:6])))
  }
  ID = unlist(lapply(hex, function(x)x[sample(1:16,size=1,replace=F)]))
  ID = paste('"',paste(ID[1:8],collapse=''),
             '-',paste(ID[9:12],collapse=''),
             '-',paste(ID[13:16],collapse=''),
             '-',paste(ID[17:20],collapse=''),
             '-',paste(ID[21:(length(ID))],collapse=''),'"',sep='')
  MyHTD = rbind(MyHTD, c('"UniquePlateIdentifier"',ID, rep(NA,xl-1)))
  #"f499f24c-15e1-4402-92eb-1a2cc872e8e3"
  MyHTD = rbind(MyHTD, c('"EndFile"',rep(NA,xl)))
  
  #Export
  fi = paste(export,'/',PlateName,'.HTD',sep='')
  if(!missing(export)){
    write.table(MyHTD,fi,sep=', ',row.names=F,col.names=F,quote=F,na='')
    Mod = readLines(fi)
    Mod=gsub(', , , , , , , , , , , , |, , , , , , , , , , , |, , , , , , , , , ','',Mod)
    writeLines(Mod,fi)
  }
  
  return(MyHTD)
}