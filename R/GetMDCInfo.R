#' MDCStore information Query
#'
#' Retrieve plate informations stored in MDCStore database
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param Unix.diff the string replacement for the mounting point of UNC image location, when accessed from linux machine
#'
#' @return a data.frame with set of information about acquired plates
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCData}}
#' @keywords utilities
#'
#' @examples
#'#Not running
#'DB = GetMDCInfo(SERVER = 'MDCStore',ID = 'sa', PWD = '***') 
#'View(DB[DB$PlateID==1,]) #Get information about plate 1
#'
#' @export
#'

GetMDCInfo = function(SERVER='MDCStore',ID='moldev', PWD='moldev', Unix.diff=c('//','/media/')){
  
  #==========================================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #==========================================================================
  
  INFO = Reduce(function(x,y)merge(x,y,by='PLATE_ID',all=T),
                list(
                  merge(sqlFetch(ch,'ASSAY_PLATES'),
                        sqlQuery(ch, "SELECT ASSAY_ID,ASSAY_NAME, SETTINGS_NAME, TABLE_ID FROM ASSAYS"), by = 'ASSAY_ID',
                        all=T),
                  sqlFetch(ch,'PLATES'),
                  sqlFetch(ch,'PLATE_PROPERTY')
                )
  )
  
  if(tail(colnames(INFO),1)!='MDCS5'){
    suppressWarnings({
      INFO$MDCS5[is.na(INFO$MDCS5)] = INFO[is.na(INFO$MDCS5),which(colnames(INFO)=='MDCS5')+1]
    })
  }
  
  #===========================================================================
  
  FileLoc = sqlFetch(ch, 'FILE_LOCATION')
  odbcClose(ch)
  
  #===========================================================================
  
  FileLoc = FileLoc[!is.na(FileLoc$DIRECTORY),]
  FileLoc[,c('SERVER_NAME','DIRECTORY')] = apply(FileLoc[,c('SERVER_NAME','DIRECTORY')],2,function(x)gsub('[\\]','/',x))
  #
  if(.Platform$OS.type=='unix'){
    FileLoc$SERVER_NAME = sapply(FileLoc$SERVER_NAME, function(x)gsub(Unix.diff[1],Unix.diff[2],as.character(x)))
  }
  FileLoc$Full.Name = apply(FileLoc[,c('SERVER_NAME','DIRECTORY')],1,function(x){
    if(substr(x[1],nchar(x[1]),nchar(x[1]))=='/' & substr(x[2],1,1)=='/'){
     return(gsub('/TimePoint_.*','',paste0(x[1],substr(x[2],2,nchar(x[2]))))) 
    }else{
      return(gsub('/TimePoint_.*','',paste0(x[1],x[2])))
    }
  })
  #
  suppressWarnings({FileLoc$PLATE_ID = as.numeric(basename(FileLoc$Full.Name))})
  FileLoc = FileLoc[!duplicated(FileLoc$PLATE_ID),]
  #
  INFO = merge(INFO, FileLoc[c('Full.Name','PLATE_ID')], by = 'PLATE_ID',all=T)
  
  #===========================================================================
  
  INFO = INFO[,c("PLATE_ID","ASSAY_ID","TO_DELETE.x","ASSAY_NAME","SETTINGS_NAME","BARCODE",
                 "GLOBAL_ID","PLATE_NAME","PLATE_DESCRIPTION","TIME_CREATED","TIME_MODIFIED",
                 "MDCS5","Full.Name")]
  
  colnames(INFO) = c('PlateID','MeasurementID','BeingDeleted','AssayName','SettingsName','BarCode','Identifier',
                     'PlateName','PlateDesc','TimeCreated','TimeModified','ExpSet','PlateLoc')
  
  return(INFO[order(INFO$PlateID, INFO$MeasurementID),,drop=F])
  
}