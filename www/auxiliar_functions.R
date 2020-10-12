ManipulateData <- function(data){
  if(!is.null(data)){
    shipType <- unique(data$ship_type)
    shipName <- unique(data$SHIPNAME)
    shipLength <- unique(data$LENGTH)
    shipWidth <- unique(data$WIDTH)
    shipDestination <- unique(data$port)[length(unique(data$port))]
    flag <- tolower(unique(data$FLAG))
    shipDWT <- unique(data$DWT)
    if(length(unique(data$is_parked))==1 && unique(data$is_parked)=='1'){
      isParked=T
    }else{
      data <- data[is_parked!=1,]
      isParked=F
    }
    data <- data[order(lubridate::ymd_hms(DATETIME)),]
    startEndPoints <- rbind(data[1,],data[nrow(data),])
    startEndPoints[1,group:='start']
    startEndPoints[2,group:='end']
    data[,c("LatPrev",'LongPrev'):=.(shift(LAT),shift(LON))]
    data[,DateTimePrev:=.(shift(DATETIME))]
    data[,PosDiff:=geosphere::distGeo(cbind(LON,LAT),cbind(LongPrev,LatPrev))]
    data[,timeDiff:=DATETIME-DateTimePrev]
    data[,timeDiffInH:=time_length(timeDiff,'h')]
    distanceMoved <- data[,sum(PosDiff,na.rm=T)]
    maxSpeed <- max(data$SPEED,na.rm=T)
    minSpeed <- min(data$SPEED,na.rm=T)
    meanSpeed <- mean(data$SPEED,na.rm=T)
    maxDistTraveled <- data[PosDiff==max(PosDiff,na.rm=T),]
    if(nrow(maxDistTraveled)>1){
      maxDistTraveled <- maxDistTraveled[nrow(maxDistTraveled),]
    }
    infoOfMaxVel <-  data[SPEED==maxSpeed]
    if(nrow(infoOfMaxVel)>1){
      infoOfMaxVel <- infoOfMaxVel[nrow(infoOfMaxVel),]
    }
    data[,meanSPEED:=frollapply(SPEED, n = 3,FUN = mean,na.rm=T, align = "left")]
    list('shipType'=shipType,'shipName'=shipName,'flag'=flag,
         'shipLength'= shipLength,'shipWidth'=shipWidth,'shipDWT'=shipDWT,
         'shipDestination'=shipDestination,
         'startEndPoints'=startEndPoints,
         'distanceMoved'=distanceMoved,'maxSPEED'=maxSpeed,
         'averageSPEED'=meanSpeed,'minSPEED'=minSpeed,
         'maxDistTraveled'=maxDistTraveled,
         'infoOfMaxVel'=infoOfMaxVel, 'isParked'=isParked,'fullData'=data)
  }else return(NULL)
}