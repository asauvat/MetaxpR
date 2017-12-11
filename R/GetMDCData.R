#' Cell-by-Cell data Query
#'
#' Get Cell-by-Cell segmentation data stored in MDCStore database given uniqueID
#'
#' @import RODBC
#'
#' @param SERVER the odbc connexion to MDCStore database
#' @param ID the identifiant for connexion
#' @param PWD the password for connexion
#' @param MeasurementID the uniqueID created by MetaXpress/PowerCore segmentation
#' @param getsite Shall siteID be added to data set ?
#' @param TimeCourse Is it a timecourse experiment?
#'
#' @return A data.frame with cell-by-cell data stemming from MetaXpress/PowerCore segmentation
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

GetMDCData = function(SERVER = 'MDCStore',ID='moldev', PWD='moldev',MeasurementID, getsite = T, TimeCourse = F){
  
  if(missing(MeasurementID)){
    return('Please give the measurement ID !')
  }
  
  #======================================================
  
  ch = odbcConnect(SERVER, uid = ID, pwd = PWD) 
  
  #======================================================
  
  TAB = sqlQuery(ch,"SELECT * FROM information_schema.tables WHERE table_name  LIKE '%MIC%'")
  TAB$UNIQUE_ID = as.numeric(gsub('MIC','',TAB$TABLE_NAME))
  TAB = TAB[order(TAB$UNIQUE_ID),,drop=F]
  
  if(!any(TAB$UNIQUE_ID==MeasurementID)){
    return('No matching analysis in database')
  }
  
  #=====================================================
  
  RES = sqlFetch(ch,TAB$TABLE_NAME[which(TAB$UNIQUE_ID==MeasurementID)])  
  PLATE_ID = RES$PLATE_ID[1]
  
  #=====================================================
  
  if(getsite){
    SITES = sqlQuery(ch,paste('SELECT SITE_ID, X_POSITION, Y_POSITION FROM SITE WHERE PLATE_ID = ',PLATE_ID))
  }
  
  if(TimeCourse){
    TIME = sqlQuery(ch,paste('SELECT SERIES_ID, T_INDEX, T_MICROSECONDS FROM SERIES_INFO WHERE SERIES_ID IN ',paste('(',paste(unique(RES$SERIES_ID),collapse=','),')',sep='')))
    RES = merge(RES, TIME, by = 'SERIES_ID' )
  }
  
  #=====================================================
  
  FEAT = sqlQuery(ch,"SELECT COLUMN_NAME,MEAS_NAME FROM TABLE_COLUMNS WHERE COLUMN_NAME LIKE '%MDCS%'")
  
  odbcClose(ch)
  
  #=====================================================
  
  FEAT = tapply(as.character(FEAT$MEAS_NAME), FEAT$COLUMN_NAME, function(x)head(x,n=1))
  FEAT = data.frame(COLUMN_NAME = names(FEAT), MEAS_NAME = as.character(FEAT))
  colnames(RES)[grep('MDCS',colnames(RES))] = gsub('Cell: ','',merge(data.frame(COLUMN_NAME=colnames(RES)[grep('MDCS',colnames(RES))]),FEAT,sort=F)$MEAS_NAME)
  
  #=====================================================
  
  RES=RES[,!grepl("SERIES_ID|PRINTED_SPOT_ID|SUBSTANCE_NAME|SETTING_ID|RUN_ID|INSTANCE|WELL_X|WELL_Y",colnames(RES))] #Useless columns...
  RES[,c(4:ncol(RES))] = apply(RES[,c(4:ncol(RES))],2,function(x)as.numeric(as.character(x)))
  colnames(RES)[1:4] = c('Cell.ID','Site.ID','Well.ID','Plate.ID')
  
  if(TimeCourse){
    colnames(RES)=gsub('T_INDEX','TimePoint',colnames(RES))
    colnames(RES)=gsub('T_MICROSECONDS','Time.Microseconds',colnames(RES))
    RES=RES[order(RES$TimePoint),,drop=F]
  }
  
  RES=RES[order(RES$Well.ID), , drop = FALSE]
  if(length(RES$ObjectID)!=0){
    RES = RES[!is.na(RES$ObjectID),] 
  }
  
  #====================================================
  if(getsite){
    RES=merge(RES,SITES,by.x='Site.ID',by.y='SITE_ID')
    colnames(RES)[colnames(RES)=='X_POSITION']='Site.X'
    colnames(RES)[colnames(RES)=='Y_POSITION']='Site.Y'
    
    Xmax = max(SITES$X_POSITION)
    Ymax = max(SITES$Y_POSITION)
    Nsites = Xmax*Ymax
    
    RES$Site.ID=RES$Site.X+Xmax*(RES$Site.Y-1)
    RES=RES[order(RES$Well.ID,RES$Site.ID),,drop=F]
  }
  #====================================================
  
  return(RES)
  
}