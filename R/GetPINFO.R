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
#' @param IR the number of rows at a time to fetch
#' @param HTS Shall HTS info also be retrieved ?

#'
#' @return Plate information as a list
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCInfo}}
#'
#' @examples
#'#Not running
#'PIF = GetPINFO(SERVER = 'MDCStore',ID = 'sa', PWD = '***',PLateID=1, HTS = T)
#'writeLines(PIF$HTS, 'myHTS.HTS') 
#'
#' @export
#'

GetPINFO = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',PlateID, IR = 1, HTS = FALSE){
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD, , rows_at_time = IR) 
  
  #======================================================
  
  CONF = sqlQuery(ch,paste0("SELECT * FROM SITE WHERE PLATE_ID=",PlateID))
  CONF$WELL=LETTERS[CONF$WELL_Y]
  for(i in 1:nrow(CONF)){
    s = '0';if(CONF$WELL_X[i]>9){s=''}
    CONF$WELL[i]=paste0(CONF$WELL[i],s,CONF$WELL_X[i])
  };rm(i)
  
  TIME = as.numeric(sqlQuery(ch,paste0("SELECT MAX(T_INDEX) FROM PLATE_IMAGES WHERE SITE_ID=",CONF$SITE_ID[1])))
  ZP = as.numeric(sqlQuery(ch,paste0("SELECT MAX(Z_INDEX) FROM PLATE_IMAGES WHERE SITE_ID=",CONF$SITE_ID[1])))
  SID = t(apply(CONF[which(CONF$WELL==unique(CONF$WELL)[1]),c('X_POSITION','Y_POSITION')],1,function(x)c(x)));colnames(SID)=c('sx','sy')
  
  #======================================================
  
  SRID = sqlQuery(ch,paste0("SELECT IMAGE_SOURCE_ID FROM PLATE_IMAGES WHERE SITE_ID=",CONF$SITE_ID[1],"AND T_INDEX=1 AND Z_INDEX=1"))
  CN = unlist(sqlQuery(ch,paste0('SELECT SOURCE_DESCRIPTION FROM IMAGE_SOURCE WHERE IMAGE_SOURCE_ID IN(',paste(unlist(SRID),collapse=','),')')));names(CN)=CN
  
  #======================================================
  
  RET = list(WID = sort(unique(CONF$WELL)),SID = SID,TID = TIME,ZP = ZP,CN = CN)
  #
  if(HTS){
    ACQINF = sqlQuery(ch,paste0("SELECT ACQ_DESC,ACQ_DATA FROM ACQ_INSTANCE WHERE ACQ_ID=",
                                sqlQuery(ch,paste0("SELECT ACQ_ID FROM PLATES WHERE PLATE_ID=",PlateID))))
    ACQINF = rawToChar(ACQINF$ACQ_DATA[[1]])
    RET = c(RET,list(HTS=ACQINF))
  }
  #
  odbcClose(ch)
  
  #======================================================
  return(RET)
  
}
