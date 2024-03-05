#'SetExpParamsFun - function to set the export parameters in Echoview given an 
#'Echoview export object
#'
#' @description Function to take an EvFile Export object and set the export parameters within it. 
#' Parameters to export come in as ToExport. Parameters not to export come in as NoExport
#' 
#' @param Obj_ExpVar- an Evfile export object of the form EVfile[['Properties']][['Export']][['Variables']]
#' 
#' @example SetBiomassExpParamsFun(Obj_ExpVar, ToExport,NoExport)
#' 
#' @export
SetExpParamsFun<-function(Obj_ExpVar, ToExport,NoExport){

     nToExport = length(ToExport)
     nNoExport = length(NoExport)
     for (k in seq_len(nToExport)){
          Obj<-Obj_ExpVar$Item(ToExport[k])
          Obj[["Enabled"]]<-TRUE
     }
     for (k in seq_len(nNoExport)){
          Obj<-Obj_ExpVar$Item(NoExport[k])
          Obj[["Enabled"]]<-FALSE
     }
}

