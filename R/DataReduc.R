#' Basic statistics
#'
#' Calculate basic statistics from well-by-well data to condition data
#'
#' @import reshape
#'
#' @param data.table A data.frame on which stats are applied
#' @param NameIndex The column name/index from data.table where condition name is found
#' @param VarIndexes The column names/indexes whit variables of interest
#' @param Ctrl A character string that contains the name of the control against which t.test will be performed
#' @param it Shall t.test/normalization be applied recursively ?
#' @param spt The linker used in condition names (see example)
#' @param sem Shall sem be calculated instead of sd ?
#' @param trunc.set Shall data outlyers be removed automatically?
#' @param d.norm Shall normalization to control be applied ?
#' @param alt See alt parameter from t.test function.
#'
#' @return A data.frame containing mean,sd/sem,p-value from t-test, asterisks of signativity for each variable
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @examples
#' #Not running
#' Results = data.reduc(data.table = Summary,NameIndex=1,VarIndexes=10:13,
#' Ctrl='Ctr_8H',sem=T, trunc.set=T, d.norm=T, alt='greater')
#'
#' @export
#'

data.reduc = function(data.table, NameIndex, VarIndexes, Ctrl,it=0,spt='_',sem=F,trunc.set=F,d.norm=F,alt = "two.sided"){
  
  #=========
  if(sem){
    sdcal = function(x,...)sd(x,...)/sqrt(length(x,...))
  }else{
    sdcal = function(x,...)sd(x,...)
  }
  #=========
  if(trunc.set){
    meancal = function(x,...)mean(trunc.val(x),...)
    varcal = function(x,...)sdcal(trunc.val(x),...)
    t.testcal = function(x,y,...)t.test(trunc.val(x),trunc.val(y),...)
  }else{
    meancal = function(x,...)mean(x,...)
    varcal = function(x,...)sdcal(x,...)
    t.testcal = function(x,y,...)t.test(x,y,...)
  }

  #1.Get means ,sds, and it0 p-values==========================================
  
  if(!any(grepl(Ctrl,data.table[,NameIndex]))){return('Cannot find control in data set')}
  data.table[,NameIndex] = gsub('[.]',',',data.table[,NameIndex]) #dot can cause problems...
  #
  Stat = by(data.table[,sort(VarIndexes)],data.table[,NameIndex],function(x)lapply(seq_along(x),
                                                                       function(y,i)cbind(meancal(y[[i]],na.rm=T),varcal(y[[i]],na.rm=T),as.numeric(try({t.testcal(y[[i]],data.table[grep(Ctrl,data.table[,NameIndex]),sort(VarIndexes)[i]],alternative = alt)$p.value},silent=T))),y=x))
  rlab = names(Stat)
  clab = colnames(data.table)[sort(VarIndexes)]
  #  
  Stat = data.frame(do.call('rbind',lapply(Stat,function(x)do.call('cbind',x))))
  colnames(Stat) =unlist(lapply(clab,function(x)cbind(paste0(x,'.Av'),paste0(x,'.SD'),paste0(x,'.pVal'))))
  #
  Stars = apply(Stat[,grep('.pVal',colnames(Stat),value=T)],2,function(x)pv2star(x))
  colnames(Stars) = paste0(clab,'.Star')
  Stat = cbind(Stat,Stars)
  Stat$Cond = rlab
  rm(Stars)
  
  #2. Do some iterative stats ...!=================================================
  
  if(it!=0){
    NewName = lapply(data.table[,NameIndex],function(x)lapply(strsplit(x,spt),function(y)cbind(paste(y[1:(length(y)-it)],collapse=spt),t(tail(y,n=it)))))
    Newdata.table = cbind(do.call('rbind',lapply(NewName,function(x)unlist(x))),data.table)
    
    #Get list of splitted groups and apply stats at each step
    
    Sub = list(Newdata.table)
    Newtest = list()
    
    for(co in 1:it){
      Sub = unlist(lapply(Sub,function(x)by(x, x[,it+2-co],function(y)y)),recursive=F)
      NewCtrl = unique(apply(as.matrix(Newdata.table[grep(Ctrl,data.table[,NameIndex]),1:(it+1-co)]),1,function(p)paste(p,collapse='_')))
      Newtest = c(Newtest,list(do.call('rbind',unlist(lapply(Sub,function(x)by(x[,c((it+2):ncol(x))][,sort(VarIndexes)],apply(as.matrix(x[,1:(it+1-co)]),1,function(p)paste(p,collapse='_')),function(y)lapply(seq_along(y),function(z,i)as.numeric(try({t.testcal(z[[i]],x[,c((it+2):ncol(x))][grep(NewCtrl,apply(as.matrix(x[,1:(it+1-co)]),1,function(p)paste(p,collapse='_'))),sort(VarIndexes)[i]],alternative=alt)$p.value},silent=T)),z=y))),recursive=F))))
    }
    
    #Let's put some order in the names
    Names = lapply(Newtest,function(x)colsplit(rownames(x),split='[.]',names=''))
    Names = lapply(Names, function(x)apply(x,1,function(y)paste(y[length(y)],paste(rev(y[1:(length(y)-1)]),collapse=spt),sep='_')))
    for(co in 1:it){
      rownames(Newtest[[co]]) = Names[[co]]
    }
    if(it>1){
    Newtest = Reduce(function(x,y)merge(x,y,by='row.names'),Newtest)
    }else{
      Newtest = data.frame(Newtest)
      Newtest = data.frame(Cond=row.names(Newtest),Newtest)
    }
    colnames(Newtest)= c('Cond',unlist(lapply(1:it,function(x)paste0(paste0(clab,'.pVal'),'.it',x))))
    
    #And create stars...
    NewStars = apply(Newtest[,setdiff(colnames(Newtest),'Cond')],2,function(x)pv2star(x))
    colnames(NewStars) = unlist(lapply(1:it,function(x)paste0(paste0(clab,'.Star'),'.it',x)))
    Newtest = cbind(Newtest,NewStars)
    
    #Merge with Stat table
    Stat = merge(Stat,Newtest,by='Cond',all=T)
    
  }
  
  #3.Normalize data.table if asked to !==================================================
  if(d.norm){
    if(it==0){
      Norm = by(data.table[,sort(VarIndexes)],data.table[,NameIndex],function(x)lapply(seq_along(x),
                                                                           function(y,i)cbind(meancal(y[[i]],na.rm=T)/meancal(data.table[grep(Ctrl,data.table[,NameIndex]),VarIndexes[i]],na.rm=T),varcal(y[[i]],na.rm=T)/meancal(data.table[grep(Ctrl,data.table[,NameIndex]),VarIndexes[i]],na.rm=T)),y=x))
      Norm = data.frame(do.call('rbind',lapply(Norm,function(x)do.call('cbind',x))))
      rownames(Norm) = Stat$Cond
      
    }else{
      Norm = lapply(Sub,function(x)by(x[,c((it+2):ncol(x))][,sort(VarIndexes)],x[,1],function(y)lapply(seq_along(y),function(z,i)cbind(meancal(z[[i]])/meancal(x[,c((it+2):ncol(x))][grep(NewCtrl,x[,1]),VarIndexes[i]],na.rm=T),varcal(z[[i]])/meancal(x[,c((it+2):ncol(x))][grep(NewCtrl,x[,1]),VarIndexes[i]],na.rm=T)),z=y)))
      for(i in it){
        Norm = unlist(Norm,recursive = F)
      }
      Norm = do.call('rbind',lapply(Norm,function(x)unlist(x)))
      rownames(Norm) = Names[[co]]
    }
    colnames(Norm) = unlist(lapply(clab,function(x)cbind(paste0(x,'.Av.Norm'),paste0(x,'.SD.Norm'))))
    Stat = merge(Stat,Norm,by.x='Cond',by.y='row.names',all=T)
  }
  
  Stat$Cond = gsub(',','.',Stat$Cond)
  return(Stat)
  
}