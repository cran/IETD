#' @title Average annual number of events analysis
#'
#' @description This function provides the required figure to define the inter-event time definition (IETD) based on
#' the average annual number of events analysis.
#'
#' @usage AAEA(Time_series,MaxIETD,xlabel,ylabel)
#'
#' @param Time_series A dataframe. The first column contains the time and day of a rainfall pulse and the second one the depth
#'                   of rainfall in each time step. The date must be as POSIXct class.
#' @param MaxIETD The maximum value of IETD to be analyzed (in hours). Default value 24.
#' @param xlabel Label of the x-axis.
#' @param ylabel Label of the y-axis.
#'
#' @details This analysis is based on the computation of the average annual number of events (AAE) for several IETD values, the appropriate value
#' of IETD is determined as the point where increasing IETD does not change AAE significantly \insertCite{Joo2014,Adams2000}{IETD}.
#' The analyst, thus, uses the plot of IETD vs AAE to define that value.
#'
#' @return A list with the figure of IETD (in hours) vs AAE and a dataframe with its values.
#'
#' @note To review the concept of IETD, go to the details of  \code{\link{drawre}} function.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom lubridate year
#'
#'
#' @examples AAEA(Time_series=hourly_time_series)
#' @export
AAEA<- function(Time_series,MaxIETD=24,xlabel="IETD [h]",ylabel="AAE") {

# define Global variables
i<-IETD<-AAE<-NULL

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
# Conditional for POSIXct class

if (length(which(class(Time_series[,1])=="POSIXct"))==0){stop("Dates should be as POSIXct class!")}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#Obtain the time step

# Time step
Time_Step<-difftime(Time_series[2,1],Time_series[1,1], units="hours")
Time_Step<-as.numeric(Time_Step, units="hours")

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Split the time series into years

dfYears<- Time_series %>% split(lubridate::year(Time_series[,1]))

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#  Obtain the potential values of IETD and length of dry periods (inter-event times (IET)) for each year

# Potential values of IETD
PIETD<-seq(1,MaxIETD,by=1)

# Length of IET for each year in hours
LIET<-lapply(dfYears, function(x) {
       Indexrp<-which(x[,2]>0)
       IET<-diff(Indexrp)
       IET<-(IET[IET>1]-1)*Time_Step
      })


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Compute the annual number of rainfall events for each year and for each potential inter event time definition (PIETD)

# Note: number of rainfall events = nIET+1, where: nIET is the number of rainless periods > IETD

numCores <- if(parallel::detectCores()>=2){2}else(1)
doParallel::registerDoParallel(numCores)
arey<-foreach::foreach(i=1:length(PIETD)) %dopar% {
  lapply(LIET,function(x){length(which(x>PIETD[i]))+1})
  }
doParallel::stopImplicitCluster()

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Compute the average annual events for each potential IETD

anrey<-lapply(arey,function(x){mean(as.numeric(x))})
anrey<-unlist(anrey, use.names = FALSE)


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Plot
df<-data.frame(PIETD,anrey)
colnames(df)<-c("IETD","AAE")

AAE_Plot<-ggplot2::ggplot(data = df, aes(x=IETD,y=AAE))+
  theme_bw()+
  geom_line()+
  geom_point()+
  xlab(xlabel)+ ylab(ylabel)


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Export Results

Results<-list()

Results[["Figure"]]<-AAE_Plot
Results[["Values"]]<-df
return(Results)
}



