#' Retrieve plate information
#'
#' Export all Custom modules from database as xml files
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param IR the number of rows at a time to fetch
#' @param exp_dir where to export xml files

#'
#' @return Plate information as a list
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#' @seealso \code{\link{GetMDCInfo}}
#'
#' @examples
#'#Not running
#'SaveCM(SERVER = 'MDCStore',ID = 'sa', PWD = '***') 
#'
#' @export
#'

SaveCM = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev', IR = 1, exp_dir = getwd()){
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD,  rows_at_time = IR) 
  
  #======================================================
  
  ASSAYS =   sqlQuery(ch, "SELECT ASSAY_NAME,SETTINGS_NAME,OBJ_DATA FROM ASSAY_PROFILE")
  
  #======================================================
  odbcClose(ch)
  
  #======================================================
  if(!dir.exists(exp_dir)){dir.create(exp_dir)}
  sapply(1:nrow(ASSAYS),function(i){
    try({
    writeLines(rawToChar(ASSAYS$OBJ_DATA[[i]]),paste0(exp_dir,'/',ASSAYS$ASSAY_NAME[i],'__',ASSAYS$SETTINGS_NAME[i],'.xml'))
    },silent = TRUE)
  })
  
  return(NULL)
  
}