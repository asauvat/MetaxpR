#' MDCStore information Query
#'
#' Retrieve plate informations stored in MDCStore database
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param rdif the string replacement for the mounting point of UNC image location.
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

GetMDCInfo = function(SERVER='MDCStore',ID='moldev', PWD='moldev', rdif=c('//','//')){
  
  #==========================================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #==========================================================================
  
  query = paste0(
    "SELECT ",
    "AP.*, ASY.ASSAY_NAME, ASY.SETTINGS_NAME, ASY.TABLE_ID, ",
    "PL.*, PP.* ",
    "FROM PLATES AS PL ",
    "LEFT JOIN ASSAY_PLATES AS AP ON PL.PLATE_ID = AP.PLATE_ID ",
    "LEFT JOIN ASSAYS AS ASY ON AP.ASSAY_ID = ASY.ASSAY_ID ",
    "LEFT JOIN PLATE_PROPERTY AS PP ON PL.PLATE_ID = PP.PLATE_ID "
  )
  INFO = sqlQuery(ch, query)
  #
  if(tail(colnames(INFO),1)!='MDCS5'){
    suppressWarnings({
      INFO$MDCS5[is.na(INFO$MDCS5)] = INFO[is.na(INFO$MDCS5),which(colnames(INFO)=='MDCS5')+1]
    })
  }
  
  #===========================================================================
  
  FLOC = sqlFetch(ch, 'FILE_LOCATION')
  odbcClose(ch)
  
  #===========================================================================
  
  FLOC = FLOC[!is.na(FLOC$DIRECTORY),]
  FLOC[,c('SERVER_NAME','DIRECTORY')] = apply(FLOC[,c('SERVER_NAME','DIRECTORY')],2,function(x)gsub('[\\]','/',x))
  FLOC$SERVER_NAME = gsub(rdif[1],rdif[2],FLOC$SERVER_NAME)
  FLOC$FULL_NAME = gsub('/TimePoint.*|/ZStep.*','',paste0(FLOC$SERVER_NAME, FLOC$DIRECTORY))
  #
  suppressWarnings({FLOC$PLATE_ID = as.numeric(basename(FLOC$FULL_NAME))})
  FLOC = FLOC[!duplicated(FLOC$PLATE_ID),]
  #
  INFO = merge(INFO, FLOC[c('FULL_NAME','PLATE_ID')], by.x = 'PLATE_ID.1', by.y = 'PLATE_ID',all.x=T, all.y=F)
  
  #===========================================================================
  INFO = INFO[,c("PLATE_ID.1","ASSAY_ID","TO_DELETE.1","ASSAY_NAME","SETTINGS_NAME","BARCODE",
                 "GLOBAL_ID","PLATE_NAME","PLATE_DESCRIPTION","TIME_CREATED","TIME_MODIFIED",
                 "MDCS5","FULL_NAME")]
  
  colnames(INFO) = c('PlateID','MeasurementID','BeingDeleted','AssayName','SettingsName','BarCode','Identifier',
                     'PlateName','PlateDesc','TimeCreated','TimeModified','ExpSet','PlateLoc')
  
  return(INFO[order(INFO$PlateID, INFO$MeasurementID),])
  
}
