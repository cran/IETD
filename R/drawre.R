#' @title Extraction of independent rainfall events from a sub-daily time series
#'
#' @description This function draws rainfall events from a sub-daily rainfall time series based on the inter-event
#' time definition (IETD) method and computes the event characteristics such as duration, total rainfall depth, and
#' intensity. The function allows slight rainfall events to be characterized, which are, in turn, not considered in
#' the extraction of rainfall events.
#'
#' @usage drawre(Time_series,IETD,Thres)
#'
#'
#' @param Time_series A dataframe. The first column contains the time and day of a rainfall pulse and the second one the depth
#'                   of rainfall in each time step. The date must be as POSIXct class.
#' @param IETD The minimum rainless period or dry period (hours) to be considered between two independent rainfall events.
#' @param Thres A rainfall depth threshold to define slight rainfall events (default value 0.5).
#'
#' @details IETD is defined as the minimum dry or rainless period between two independent events. This time interval is
#' applied to a continuous time series: if two groups of consecutive pulses of rainfall are separated by a rainless period
#' longer than or equal to IETD, they are considered as two independent rainfall events; otherwise, these two groups are categorized
#' as belonging to the same event \insertCite{Restrepo-Posada1982,Adams2000}{IETD}. A rainless period between two independent
#' events is known as inter-event time (b) and by definition b>= IETD. A rainfall event whose rainfall pulses are lower than the
#' threshold Thres is characterized as a slight rainfall event.
#'
#' @return A list with a dataframe, named Rainfall_Characteristics, and a sublist, named Rainfall_Events, is provided.
#' Rainfall_Characteristics contains the main information of each extracted rainfall event such as event number,
#' the beginning and end of the event, duration (in hours), total rainfall depth, and average intensity (total rainfall depth/duration).
#' Rainfall_Events contains several dataframes with the values of rainfall pulses of each extracted rainfall event.
#' The first dataframe in Rainfall_Events corresponds to the first event in Rainfall_Characteristics, the second
#' dataframe in Rainfall_Events corresponds to the second event in Rainfall_Characteristics, and so on.
#'
#' @note This function does not accept missing values in the sub-daily rainfall time series.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#'
#'
#' @examples \donttest{drawre(Time_series=hourly_time_series,IETD=5,Thres=0.5)}
#' @export
drawre <- function(Time_series,IETD,Thres=0.5){

# define Global variables
i<-j<-k<-NULL

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
# Conditional for missing values and POSIXct class

if (length(which(is.na(Time_series[,2])))>=1){stop("There are missing values!")}
if (length(which(class(Time_series[,1])=="POSIXct"))==0){stop("Dates should be as POSIXct class!")}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Obtain the time step

# Time step
Time_Step<-difftime(Time_series[2,1],Time_series[1,1], units="hours")
Time_Step<-as.numeric(Time_Step, units="hours")


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Build the vector of rainfall which starts and ends with a rainfall value

IndexRainfall<-which(Time_series[,2]>0)  # Index of rainfall > 0
Rp<-Time_series
Rp<-Rp[seq(IndexRainfall[1],max(IndexRainfall)),]

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Built the vectors of numbers of consecutive rainfall pulses and consecutive dry pulses.

#  Number of consecutive dry pulses
dryp<-which(Rp[,2]>0)
dryp<-diff(dryp)
dryp<-dryp[dryp>1]
dryp<-dryp-1

# Number of consecutive rainfall pulses
a1<- which(Rp[,2]>0)
Iaf<-diff(a1)
Iaf<-which(Iaf>1)

numCores <- if (parallel::detectCores()>=2){2} else {1}
doParallel::registerDoParallel(numCores)

tmp1 <-foreach::foreach(i=1:length(Iaf)) %dopar% {
  af<-a1[Iaf[i]]
  ai<-a1[Iaf[i]+1]
  c<-c(af,ai)
}
doParallel::stopImplicitCluster()

af<-c(sapply(tmp1, `[`, 1),length(Rp[,2]))
ai<-c(1,sapply(tmp1, `[`, 2))
a<-(af-ai)+1   # Number of consecutive rainfall pulses

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

# Define the beginning and end of the Rainfall events according to EITD

IndexEID<-which(dryp*Time_Step>=IETD) # definition of theindexes of interevent times (b)
numCores <- if (parallel::detectCores()>=2){2} else {1}
doParallel::registerDoParallel(numCores)

tmp2 <-foreach::foreach(j=1:length(IndexEID)) %dopar% {
  SdateEvent<-1+ sum(a[1:IndexEID[j]])+sum(dryp[1:IndexEID[j]])
  FdateEvent<-SdateEvent-dryp[IndexEID[j]]-1
  c<-c(SdateEvent,FdateEvent)
}
doParallel::stopImplicitCluster()

FdateEvent<-c(sapply(tmp2, `[`, 2),length(Rp[,2]))
SdateEvent<-c(1,sapply(tmp2, `[`, 1))


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Obtain events and its characteristics


numCores <- if (parallel::detectCores()>=2){2} else {1}
doParallel::registerDoParallel(numCores)


tmp3 <-foreach::foreach(k=1:length(SdateEvent)) %dopar% {
  Event<-Rp[SdateEvent[k]:FdateEvent[k],]
  Duration<-length(Event[,2])
  Volume<-sum(Event[,2])
  Average_Intensity<-sum(Event[,2])/(length(Event[,2])*Time_Step) # added for 5-min rainfall events
  Event_characteristics<-list(Duration,Volume,Average_Intensity,Event)
}
doParallel::stopImplicitCluster()

Duration<-sapply(tmp3, `[[`, 1)*Time_Step # added for 5-min rainfall events
Volume<-sapply(tmp3, `[[`, 2)
Average_Intensity<-sapply(tmp3, `[[`, 3)
Events<-sapply(tmp3, `[`, 4)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Obtain the events with rainfall pulses higher than threshold

# Obtain the indexes of the events with rainfall pulses lower than the threshold

Event.below.thre<-lapply(Events,function(x){
  df<-as.data.frame(x)
  dur<-length(df[,2])
  Index.Aux<-length(which(df[,2]<=Thres))
  binvalue<-ifelse(dur==Index.Aux,1,0)})

IndexEvents<-which(sapply(Event.below.thre,function(x)1 %in% x))


# Filter the events
Events[IndexEvents]<-NULL
Duration<-if(Thres==0){Duration} else {Duration[-IndexEvents]}
Volume<-if(Thres==0){Volume} else {Volume[-IndexEvents]}
Average_Intensity<-if(Thres==0){Average_Intensity} else {Average_Intensity[-IndexEvents]}
SdateEvent<-if(Thres==0) {SdateEvent} else {SdateEvent[-IndexEvents]}
FdateEvent<-if (Thres==0) {FdateEvent} else {FdateEvent[-IndexEvents]}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Export
Results<-list()

Rainfall_Characteristics<-data.frame(seq(1,length(Rp[SdateEvent,1])),Rp[SdateEvent,1],Rp[FdateEvent,1],Duration,Volume,Average_Intensity)
colnames(Rainfall_Characteristics)<-c("Number.Event","Starting","End","Duration","Volume","Intensity")

Results[["Rainfall_Characteristics"]]<-Rainfall_Characteristics
Results[["Rainfall_Events"]]<-Events
return(Results)

}

