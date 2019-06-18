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
#' @param Unix.diff the string replacement for the mounting point of UNC image location, when accessed from linux machine
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

GetIMPath = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',PlateID, WellID,SiteID,TimePoint=1,Z=1,Unix.diff=c('//','/media/')){
  
  #======================================================
  
  WellY = which(substr(WellID,1,1)==LETTERS)
  WellX = as.numeric(substr(WellID,2,3))
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #======================================================
  
  WTAB = sqlQuery(ch,paste0("SELECT * FROM SITE WHERE PLATE_ID=",PlateID," AND WELL_X=",WellX," AND WELL_Y=",WellY," AND X_POSITION=",SiteID[1]," AND Y_POSITION=",SiteID[2]))

  if(nrow(WTAB)==0){
    PATH=NULL
  }else{
  IMID = sqlQuery(ch,paste0("SELECT IMAGE_ID FROM PLATE_IMAGES WHERE SITE_ID=",WTAB$SITE_ID,"AND T_INDEX=",TimePoint,"AND Z_INDEX=",Z))
  IMINFO = sqlQuery(ch,paste0("SELECT OBJ_SERVER_NAME,LOCATION_ID FROM PLATE_IMAGE_DATA WHERE OBJ_ID IN (",paste0(IMID[,"IMAGE_ID"],collapse=','),")"))
  #
  #ROOT = sqlQuery(ch,paste0("SELECT SERVER_NAME,DIRECTORY FROM FILE_LOCATION WHERE LOCATION_ID=",IMINFO$LOCATION_ID[1]))
  ROOT = do.call(rbind,lapply(1:nrow(IMINFO),function(i)sqlQuery(ch,paste0("SELECT SERVER_NAME,DIRECTORY FROM FILE_LOCATION WHERE LOCATION_ID=",IMINFO$LOCATION_ID[i]))))
  ROOT = paste(ROOT$SERVER_NAME,ROOT$DIRECTORY,sep='\\');ROOT=gsub('//|///','/',gsub('[\\]','/',ROOT)) 
  #
  ROOT=apply(as.matrix(ROOT),1,function(li){
    if(substr(li,1,2)!='//'){li=paste0('/',li)} 
    if(substr(li,nchar(li),nchar(li))!='/'){li=paste0(li,'/')}
    return(li)
  })
  #
  SERV = unlist(strsplit(ROOT,'/'))[3]
  nSERV = paste0(toupper(substr(SERV,1,1)),tolower(substr(SERV,2,nchar(SERV))))
  ROOT = gsub(SERV,nSERV,ROOT)
  #
  PATH = paste0(ROOT,IMINFO$OBJ_SERVER_NAME)
  #
  if(.Platform$OS.type=='unix'){PATH=gsub(Unix.diff[1],Unix.diff[2],PATH)}
  }
  #======================================================
  odbcClose(ch)
  
  return(PATH)
  
}