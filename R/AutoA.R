#' @title Autocorrelation analysis
#'
#' @description This function provides the required figure (an autocorrelogram) to define the inter-event time definition (IETD) based on
#' the autocorrelation analysis.
#'
#'
#' @usage AutoA(Time_series,MaxLag,CL,xlabel,ylabel)
#'
#' @param Time_series A dataframe. The first column contains the time and day of a rainfall pulse and the second one the depth
#'                    of rainfall in each time step. The date must be as POSIXct class.
#' @param MaxLag The maximum lag time to be analyzed (in hours). Default value 24.
#' @param CL The confidence level of the autocorrelation function (ACF)(in percentage). Default value 95\%.
#' @param xlabel Label of the x-axis of the autocorrelogram.
#' @param ylabel Label of the y-axis of the autocorrelogram.
#'
#' @details IETD is here defined as the lag time where the autocorrelation coefficient of
#' rain pulses, i.e., the autocorrelation function(ACF), converges to zero \insertCite{Joo2014,Adams2000}{IETD}. The
#' analyst uses an autocorrelogram to define that value within a specific level of tolerance. This function is
#' based on the function \code{\link[stats]{acf}} of the \code{\link{stats}} package.
#'
#' @return A list with a figure of lag time (in hours) vs ACF, i.e., an autocorrelogram, and a dataframe with its values.
#'
#' @note To review the concept of IETD, go to the details of  \code{\link{drawre}} function.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @importFrom stats acf qnorm
#' @import ggplot2
#'
#'
#' @export
#' @examples AutoA(Time_series=hourly_time_series)
AutoA<- function(Time_series,MaxLag=24,CL=95,xlabel="Lag time [h]",ylabel="ACF") {

# define Global variables
Lag_Time<-NULL

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
# Conditional for POSIXct class

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
#Obtain the maximun lag time (MaxLgT) to be considered in the analysis based on MaxIETD.

# Lag time
MaxLgT<-MaxLag/Time_Step

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Computes the autocorrelation coeffient (ACF)

ACFA<-stats::acf(Time_series[,2],lag.max = MaxLgT, plot = FALSE)
ACF<-ACFA$acf
Lags<-ACFA$lag
hourly_Lags<-Lags*Time_Step #Lag time in hours

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Computes the point of statistical significance of ACF 95%.

significance_level <- stats::qnorm((1 + CL/100)/2)/sqrt(length(Time_series[,2]))

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Plot

df<-data.frame(hourly_Lags,ACF)
colnames(df)[1]<-"Lag_Time"

AutoA_Plot<-ggplot2::ggplot(data = df, aes(x=Lag_Time,y=ACF))+
  theme_bw()+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = significance_level,linetype = "dashed", colour="blue")+
  geom_hline(yintercept = -significance_level,linetype = "dashed", colour="blue")+
  xlab(xlabel)+ ylab(ylabel)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# Export Results

Results<-list()
Results[["Figure"]]<-AutoA_Plot
Results[["Values"]]<-df
return(Results)
}
