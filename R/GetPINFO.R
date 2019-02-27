#' Retrieve plate information
#'
#' Get MX plate layout and series information
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param PlateID the Plate Identifier
#'
#' @return Plate information as a list
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCInfo}}
#'
#' @examples
#'#Not running
#'DB = GetPINFO(SERVER = 'MDCStore',ID = 'sa', PWD = '***',PLateID=1) 
#'
#' @export
#'

GetPINFO = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',PlateID){
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #======================================================
  
  CONF = sqlQuery(ch,paste0("SELECT * FROM SITE WHERE PLATE_ID=",PlateID))
  CONF$WELL=LETTERS[CONF$WELL_Y]
  for(i in 1:nrow(CONF)){
    if(CONF$WELL_X[i]<10){
      CONF$WELL[i]=paste0(CONF$WELL[i],0,CONF$WELL_X[i])
    }else{
      CONF$WELL[i]=paste0(CONF$WELL[i],CONF$WELL_X[i])
    }
  };rm(i)
  
  TIME = as.numeric(sqlQuery(ch,paste0("SELECT MIN(T_INDEX),MAX(T_INDEX) FROM PLATE_IMAGES WHERE SITE_ID=",CONF$SITE_ID[1])))
  SID = t(apply(CONF[which(CONF$WELL==unique(CONF$WELL)[1]),c('X_POSITION','Y_POSITION')],1,function(x)c(x)));colnames(SID)=c('sx','sy')
  if(TIME[1]==TIME[2]){TIME=TIME[1]}

  #======================================================
  odbcClose(ch)
  
  
  return(list(WID=unique(CONF$WELL),SID=SID,TID=TIME))
  
}