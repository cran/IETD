#' @title Coefficient of variation analysis
#'
#' @description This function computes the inter-event time definition (IETD) based on the coefficient of variation analysis.
#'
#' @usage CVA(Time_series,MaxIETD,xlabel,ylabel)
#'
#' @param Time_series A dataframe. The first column contains the time and day of a rainfall pulse and the second one the depth
#'                   of rainfall in each time step. The date must be as POSIXct class.
#' @param MaxIETD The maximum value of IETD to be analyzed (in hours). Default value 24.
#' @param xlabel Label of the x-axis of the figure IETD vs CV.
#' @param ylabel Label of the y-axis of the figure IETD vs CV.
#'
#' @details This method assumes that inter-event times (b) are represented well by a exponential distribution. Since
#' by definition b>= IETD, IETD is computed as the value whose resulting coefficient of variation (CV) of b equal to unity \insertCite{Restrepo-Posada1982,Adams2000}{IETD}.
#' This analysis is done by testing several values of IETD and analyzing the resulting CV. The computed IETD is obtained via interpolation from the figure of
#' IETD vs CV.
#'
#' @return A list with a figure of IETD vs CV, a dataframe with the values of that figure, and the computed value of IETD.
#'
#' @note To review the concepts of b and IETD, go to the details of  \code{\link{drawre}} function.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @importFrom stats sd approx
#' @import ggplot2
#'
#' @examples CVA (Time_series=hourly_time_series)
#' @export
CVA<-function(Time_series,MaxIETD=24,xlabel="IETD [h]",ylabel="Coefficient of Variation (CV)"){

# define Global variables
IETD<-CV<-NULL

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
#-----------------------------------------------------------------------------------------------------------------------
# Compute CV for each potential inter event time definition (IETD)


# Potential values of IETD
PIETD<-seq(1,MaxIETD,by=1)

# Length of dry periods.
b<-which(Time_series[,2]>0)
b<-diff(b)
b<-b[b>1]
b<-(b-1)*Time_Step

# Compute CV for each PIETD

CV<-sapply(PIETD, function(x){
       IET<-b[b>x] # Intervent times associated to each PIETD.
       CV<-stats::sd(IET)/mean(IET)
      })

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Compute the EITD with CV=1

EITD<-stats::approx(CV,PIETD,1)
EITD<-round(as.numeric(EITD$y),1)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Plot

df<-data.frame(PIETD,CV)
colnames(df)<-c("IETD","CV")

CV_Plot<- ggplot2::ggplot(data = df, aes(x=IETD,y=CV))+
  theme_bw()+
  geom_line()+
  geom_point()+
  xlab(xlabel)+ ylab(ylabel)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Export Results

Results<-list()

Results[["Figure"]]<-CV_Plot
Results[["Values"]]<-df
Results[["EITD"]]<-EITD
return(Results)
}
