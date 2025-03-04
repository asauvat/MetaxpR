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
#' @param Z The Z position of the image. Default is 1, value if no stacks is recorded. 0 for max projection, if recorded
#' @param TimePoint From what timepoint the image name should be returned. Default is 1.
#' @param rdif the string replacement for the mounting point of UNC image location
#'
#' @return A string array containing full image names
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCInfo}}
#'
#' @examples
#'#Not running
#'ips = GetIMPath(SERVER = 'MDCStore',ID = 'sa', PWD = '***',PlateID=10, WellID = 'A02', SiteID=c(1,1), TimePoint=1, Z=1) 
#'imgs = readImage(ips)
#'
#' @export
#'

GetIMPath = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',PlateID, WellID,SiteID,TimePoint=1,Z=1,rdif=c('//','//')){
  
  #======================================================
  
  WellY = which(substr(WellID,1,1)==LETTERS)
  WellX = as.numeric(substr(WellID,2,3))
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #=====================================================   
  query = paste0(
    "SELECT ",
    "PLS.OBJ_SERVER_NAME, LOC.SERVER_NAME, LOC.DIRECTORY, PLS.LOCATION_ID ",
    "FROM SITE AS S ",
    "JOIN PLATE_IMAGES AS PI ON S.SITE_ID = PI.SITE_ID ",
    "JOIN PLATE_IMAGE_DATA AS PLS ON PI.IMAGE_ID = PLS.OBJ_ID ",
    "JOIN FILE_LOCATION AS LOC ON PLS.LOCATION_ID = LOC.LOCATION_ID ",
    "WHERE S.PLATE_ID = ", PlateID, 
    " AND S.WELL_X = ", WellX, 
    " AND S.WELL_Y = ", WellY, 
    " AND S.X_POSITION = ", SiteID[1], 
    " AND S.Y_POSITION = ", SiteID[2], 
    " AND PI.T_INDEX = ", TimePoint, 
    " AND PI.Z_INDEX = ", Z
  )
  res = sqlQuery(ch, query)
  if(nrow(res)==0){
    PATH=NULL
  }else{
    PATH = file.path(paste0(res$SERVER_NAME,res$DIRECTORY),res$OBJ_SERVER_NAME)
    PATH = gsub("[\\]","/",PATH) 
    PATH = gsub(rdif[1],rdif[2],PATH)
  }
  PATH = PATH[order(as.numeric(substr(sapply(strsplit(basename(PATH),'_'),function(x)tail(x,1)),2,2)))]
  
  #======================================================
  odbcClose(ch)
  
  return(PATH)
  
}
