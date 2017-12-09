#' Quick image name finding
#'
#' Get MX image name(s) given its coordinates
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param PlateID the Plate Identifier
#' @param WellID the Well Identifier
#' @param SiteID a vector of 2 integers showing x and y site positions. If only one site present, x and y equal 0
#' @param TimePoint From what timepoint the image name should be returned. Default is 1.
#'
#' @return A string array containing full image names
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCInfo}}
#'
#' @examples
#'#Not running
#'DB = GetMDCInfo(SERVER = 'MDCStore',ID = 'sa', PWD = '***') 
#'UniqueIDs=DB$MeasurementID[grep(paste0(1500:1515,collapse='|'),DB$PlateID)]) #Get uniquesIDs from plate 1500 to 1515
#'MyData = lapply(UniqueIDs,function(x)GetMDCData(SERVER='YODA-SERVER',MeasurementID=x)) #Create a list that contains cellular data from each plate
#'
#' @export
#'

GetIMPath = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',PlateID, WellID,SiteID,TimePoint=1){
  
  #======================================================
  
  WellY = which(substr(WellID,1,1)==LETTERS)
  WellX = as.numeric(substr(WellID,2,3))
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #======================================================
  
  WTAB = sqlQuery(ch,paste0("SELECT * FROM SITE WHERE PLATE_ID=",PlateID," AND WELL_X=",WellX,"AND WELL_Y=",WellY))
  WTAB = WTAB[which(WTAB$X_POSITION==SiteID[1] & WTAB$Y_POSITION==SiteID[2]),]
  
  ID = sqlQuery(ch,paste0("SELECT IMAGE_ID FROM PLATE_IMAGES WHERE SITE_ID=",WTAB$SITE_ID,"AND T_INDEX=",TimePoint))
  IMINFO = sqlQuery(ch,paste0("SELECT OBJ_SERVER_NAME,LOCATION_ID FROM PLATE_IMAGE_DATA WHERE OBJ_ID IN (",paste0(ID[,"IMAGE_ID"],collapse=','),")"))
  
  ROOT = sqlQuery(ch,paste0("SELECT SERVER_NAME,DIRECTORY FROM FILE_LOCATION WHERE LOCATION_ID=",IMINFO$LOCATION_ID[1]))
  PATH = paste0(ROOT$SERVER_NAME,ROOT$DIRECTORY,'\\',IMINFO$OBJ_SERVER_NAME)
  
  #======================================================
  odbcClose(ch)
  
  return(PATH)
  
}