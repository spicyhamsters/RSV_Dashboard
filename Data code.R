#Read in weather daily data for each of the 13 states and calculate weekly average for each variable
##################################################################################################################
#Utah
ut_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Utah_salt_lake_city_weather.csv", header=TRUE, sep=',')
ut_w$date<- paste(ut_w$MO, ut_w$DY,ut_w$YEAR, sep = "/")
ut_w$state<- 'Utah'
n<-length(ut_w$date)
ut_w[ut_w==-999]<-NA

#Create new variables to store Weekly average
ut_w$WS2M_1<-0
ut_w$T2M_1<-0
ut_w$T2MDEW_1<-0
ut_w$T2MWET_1<-0
ut_w$TS_1<-0
ut_w$T2M_RANGE_1<-0
ut_w$T2M_MAX_1<-0
ut_w$T2M_MIN_1<-0
ut_w$QV2M_1<-0
ut_w$RH2M_1<-0
ut_w$PRECTOTCORR_1<-0
ut_w$PS_1<-0
ut_w$WS10M_1<-0
ut_w$WS10M_MAX_1<-0
ut_w$WS10M_MIN_1<-0
ut_w$WS10M_RANGE_1<-0
ut_w$WD10MN_1<-0

#Weekly rolling average calculator(Week starts Sunday ends Saturday)
for (i in 7:n) 
{
  ut_w$WS2M_1[i]<-mean(c(ut_w$WS2M[i],ut_w$WS2M[i-1],ut_w$WS2M[i-2],ut_w$WS2M[i-3],ut_w$WS2M[i-4],ut_w$WS2M[i-5],ut_w$WS2M[i-6]), na.rm=TRUE)
  ut_w$T2M_1[i]<-mean(c(ut_w$T2M[i],ut_w$T2M[i-1],ut_w$T2M[i-2],ut_w$T2M[i-3],ut_w$T2M[i-4],ut_w$T2M[i-5],ut_w$T2M[i-6]), na.rm=TRUE)
  ut_w$T2MDEW_1[i]<-mean(c(ut_w$T2MDEW[i],ut_w$T2MDEW[i-1],ut_w$T2MDEW[i-2],ut_w$T2MDEW[i-3],ut_w$T2MDEW[i-4],ut_w$T2MDEW[i-5],ut_w$T2MDEW[i-6]), na.rm=TRUE)
  ut_w$T2MWET_1[i]<-mean(c(ut_w$T2MWET[i],ut_w$T2MWET[i-1],ut_w$T2MWET[i-2],ut_w$T2MWET[i-3],ut_w$T2MWET[i-4],ut_w$T2MWET[i-5],ut_w$T2MWET[i-6]), na.rm=TRUE)
  ut_w$TS_1[i]<-mean(c(ut_w$TS[i],ut_w$TS[i-1],ut_w$TS[i-2],ut_w$TS[i-3],ut_w$TS[i-4],ut_w$TS[i-5],ut_w$TS[i-6]), na.rm=TRUE)
  ut_w$T2M_RANGE_1[i]<-mean(c(ut_w$T2M_RANGE[i],ut_w$T2M_RANGE[i-1],ut_w$T2M_RANGE[i-2],ut_w$T2M_RANGE[i-3],ut_w$T2M_RANGE[i-4],ut_w$T2M_RANGE[i-5],ut_w$T2M_RANGE[i-6]), na.rm=TRUE)
  ut_w$T2M_MAX_1[i]<-mean(c(ut_w$T2M_MAX[i],ut_w$T2M_MAX[i-1],ut_w$T2M_MAX[i-2],ut_w$T2M_MAX[i-3],ut_w$T2M_MAX[i-4],ut_w$T2M_MAX[i-5],ut_w$T2M_MAX[i-6]), na.rm=TRUE)
  ut_w$T2M_MIN_1[i]<-mean(c(ut_w$T2M_MIN[i],ut_w$T2M_MIN[i-1],ut_w$T2M_MIN[i-2],ut_w$T2M_MIN[i-3],ut_w$T2M_MIN[i-4],ut_w$T2M_MIN[i-5],ut_w$T2M_MIN[i-6]), na.rm=TRUE)
  ut_w$QV2M_1[i]<-mean(c(ut_w$QV2M[i],ut_w$QV2M[i-1],ut_w$QV2M[i-2],ut_w$QV2M[i-3],ut_w$QV2M[i-4],ut_w$QV2M[i-5],ut_w$QV2M[i-6]), na.rm=TRUE)
  ut_w$RH2M_1[i]<-mean(c(ut_w$RH2M[i],ut_w$RH2M[i-1],ut_w$RH2M[i-2],ut_w$RH2M[i-3],ut_w$RH2M[i-4],ut_w$RH2M[i-5],ut_w$RH2M[i-6]), na.rm=TRUE)
  ut_w$PRECTOTCORR_1[i]<-mean(c(ut_w$PRECTOTCORR[i],ut_w$PRECTOTCORR[i-1],ut_w$PRECTOTCORR[i-2],ut_w$PRECTOTCORR[i-3],ut_w$PRECTOTCORR[i-4],ut_w$PRECTOTCORR[i-5],ut_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  ut_w$PS_1[i]<-mean(c(ut_w$PS[i],ut_w$PS[i-1],ut_w$PS[i-2],ut_w$PS[i-3],ut_w$PS[i-4],ut_w$PS[i-5],ut_w$PS[i-6]), na.rm=TRUE)
  ut_w$WS10M_1[i]<-mean(c(ut_w$WS10M[i],ut_w$WS10M[i-1],ut_w$WS10M[i-2],ut_w$WS10M[i-3],ut_w$WS10M[i-4],ut_w$WS10M[i-5],ut_w$WS10M[i-6]), na.rm=TRUE)
  ut_w$WS10M_MAX_1[i]<-mean(c(ut_w$WS10M_MAX[i],ut_w$WS10M_MAX[i-1],ut_w$WS10M_MAX[i-2],ut_w$WS10M_MAX[i-3],ut_w$WS10M_MAX[i-4],ut_w$WS10M_MAX[i-5],ut_w$WS10M_MAX[i-6]), na.rm=TRUE)
  ut_w$WS10M_MIN_1[i]<-mean(c(ut_w$WS10M_MIN[i],ut_w$WS10M_MIN[i-1],ut_w$WS10M_MIN[i-2],ut_w$WS10M_MIN[i-3],ut_w$WS10M_MIN[i-4],ut_w$WS10M_MIN[i-5],ut_w$WS10M_MIN[i-6]), na.rm=TRUE)
  ut_w$WS10M_RANGE_1[i]<-mean(c(ut_w$WS10M_RANGE[i],ut_w$WS10M_RANGE[i-1],ut_w$WS10M_RANGE[i-2],ut_w$WS10M_RANGE[i-3],ut_w$WS10M_RANGE[i-4],ut_w$WS10M_RANGE[i-5],ut_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  ut_w$WD10MN_1[i]<-mean(c(ut_w$WD10M[i],ut_w$WD10M[i-1],ut_w$WD10M[i-2],ut_w$WD10M[i-3],ut_w$WD10M[i-4],ut_w$WD10M[i-5],ut_w$WD10M[i-6]), na.rm=TRUE)
}

#Tennessee
tn_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Tenesse_Nashiville_weather.csv", header=TRUE)
tn_w$date<-NA
tn_w$date<-paste(tn_w$MO, tn_w$DY,tn_w$YEAR, sep = "/")
tn_w$state<-NA
tn_w$state<- 'Tennessee'
n<-length(tn_w$date)
tn_w[tn_w==-999]<-NA

tn_w$WS2M_1<-0
tn_w$T2M_1<-0
tn_w$T2MDEW_1<-0
tn_w$T2MWET_1<-0
tn_w$TS_1<-0
tn_w$T2M_RANGE_1<-0
tn_w$T2M_MAX_1<-0
tn_w$T2M_MIN_1<-0
tn_w$QV2M_1<-0
tn_w$RH2M_1<-0
tn_w$PRECTOTCORR_1<-0
tn_w$PS_1<-0
tn_w$WS10M_1<-0
tn_w$WS10M_MAX_1<-0
tn_w$WS10M_MIN_1<-0
tn_w$WS10M_RANGE_1<-0
tn_w$WD10MN_1<-0

for (i in 7:n) 
{
  tn_w$WS2M_1[i]<-mean(c(tn_w$WS2M[i],tn_w$WS2M[i-1],tn_w$WS2M[i-2],tn_w$WS2M[i-3],tn_w$WS2M[i-4],tn_w$WS2M[i-5],tn_w$WS2M[i-6]), na.rm=TRUE)
  tn_w$T2M_1[i]<-mean(c(tn_w$T2M[i],tn_w$T2M[i-1],tn_w$T2M[i-2],tn_w$T2M[i-3],tn_w$T2M[i-4],tn_w$T2M[i-5],tn_w$T2M[i-6]), na.rm=TRUE)
  tn_w$T2MDEW_1[i]<-mean(c(tn_w$T2MDEW[i],tn_w$T2MDEW[i-1],tn_w$T2MDEW[i-2],tn_w$T2MDEW[i-3],tn_w$T2MDEW[i-4],tn_w$T2MDEW[i-5],tn_w$T2MDEW[i-6]), na.rm=TRUE)
  tn_w$T2MWET_1[i]<-mean(c(tn_w$T2MWET[i],tn_w$T2MWET[i-1],tn_w$T2MWET[i-2],tn_w$T2MWET[i-3],tn_w$T2MWET[i-4],tn_w$T2MWET[i-5],tn_w$T2MWET[i-6]), na.rm=TRUE)
  tn_w$TS_1[i]<-mean(c(tn_w$TS[i],tn_w$TS[i-1],tn_w$TS[i-2],tn_w$TS[i-3],tn_w$TS[i-4],tn_w$TS[i-5],tn_w$TS[i-6]), na.rm=TRUE)
  tn_w$T2M_RANGE_1[i]<-mean(c(tn_w$T2M_RANGE[i],tn_w$T2M_RANGE[i-1],tn_w$T2M_RANGE[i-2],tn_w$T2M_RANGE[i-3],tn_w$T2M_RANGE[i-4],tn_w$T2M_RANGE[i-5],tn_w$T2M_RANGE[i-6]), na.rm=TRUE)
  tn_w$T2M_MAX_1[i]<-mean(c(tn_w$T2M_MAX[i],tn_w$T2M_MAX[i-1],tn_w$T2M_MAX[i-2],tn_w$T2M_MAX[i-3],tn_w$T2M_MAX[i-4],tn_w$T2M_MAX[i-5],tn_w$T2M_MAX[i-6]), na.rm=TRUE)
  tn_w$T2M_MIN_1[i]<-mean(c(tn_w$T2M_MIN[i],tn_w$T2M_MIN[i-1],tn_w$T2M_MIN[i-2],tn_w$T2M_MIN[i-3],tn_w$T2M_MIN[i-4],tn_w$T2M_MIN[i-5],tn_w$T2M_MIN[i-6]), na.rm=TRUE)
  tn_w$QV2M_1[i]<-mean(c(tn_w$QV2M[i],tn_w$QV2M[i-1],tn_w$QV2M[i-2],tn_w$QV2M[i-3],tn_w$QV2M[i-4],tn_w$QV2M[i-5],tn_w$QV2M[i-6]), na.rm=TRUE)
  tn_w$RH2M_1[i]<-mean(c(tn_w$RH2M[i],tn_w$RH2M[i-1],tn_w$RH2M[i-2],tn_w$RH2M[i-3],tn_w$RH2M[i-4],tn_w$RH2M[i-5],tn_w$RH2M[i-6]), na.rm=TRUE)
  tn_w$PRECTOTCORR_1[i]<-mean(c(tn_w$PRECTOTCORR[i],tn_w$PRECTOTCORR[i-1],tn_w$PRECTOTCORR[i-2],tn_w$PRECTOTCORR[i-3],tn_w$PRECTOTCORR[i-4],tn_w$PRECTOTCORR[i-5],tn_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  tn_w$PS_1[i]<-mean(c(tn_w$PS[i],tn_w$PS[i-1],tn_w$PS[i-2],tn_w$PS[i-3],tn_w$PS[i-4],tn_w$PS[i-5],tn_w$PS[i-6]), na.rm=TRUE)
  tn_w$WS10M_1[i]<-mean(c(tn_w$WS10M[i],tn_w$WS10M[i-1],tn_w$WS10M[i-2],tn_w$WS10M[i-3],tn_w$WS10M[i-4],tn_w$WS10M[i-5],tn_w$WS10M[i-6]), na.rm=TRUE)
  tn_w$WS10M_MAX_1[i]<-mean(c(tn_w$WS10M_MAX[i],tn_w$WS10M_MAX[i-1],tn_w$WS10M_MAX[i-2],tn_w$WS10M_MAX[i-3],tn_w$WS10M_MAX[i-4],tn_w$WS10M_MAX[i-5],tn_w$WS10M_MAX[i-6]), na.rm=TRUE)
  tn_w$WS10M_MIN_1[i]<-mean(c(tn_w$WS10M_MIN[i],tn_w$WS10M_MIN[i-1],tn_w$WS10M_MIN[i-2],tn_w$WS10M_MIN[i-3],tn_w$WS10M_MIN[i-4],tn_w$WS10M_MIN[i-5],tn_w$WS10M_MIN[i-6]), na.rm=TRUE)
  tn_w$WS10M_RANGE_1[i]<-mean(c(tn_w$WS10M_RANGE[i],tn_w$WS10M_RANGE[i-1],tn_w$WS10M_RANGE[i-2],tn_w$WS10M_RANGE[i-3],tn_w$WS10M_RANGE[i-4],tn_w$WS10M_RANGE[i-5],tn_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  tn_w$WD10MN_1[i]<-mean(c(tn_w$WD10M[i],tn_w$WD10M[i-1],tn_w$WD10M[i-2],tn_w$WD10M[i-3],tn_w$WD10M[i-4],tn_w$WD10M[i-5],tn_w$WD10M[i-6]), na.rm=TRUE)
}

#Oregon
or_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Oregon_salem_weather.csv", header=TRUE, sep=',')
or_w$date<-NA
or_w$date<- paste(or_w$MO, or_w$DY,or_w$YEAR, sep = "/")
or_w$state<- NA
or_w$state<- 'Oregon'
n<-length(or_w$date)

or_w[or_w==-999]<-NA

or_w$WS2M_1<-0
or_w$T2M_1<-0
or_w$T2MDEW_1<-0
or_w$T2MWET_1<-0
or_w$TS_1<-0
or_w$T2M_RANGE_1<-0
or_w$T2M_MAX_1<-0
or_w$T2M_MIN_1<-0
or_w$QV2M_1<-0
or_w$RH2M_1<-0
or_w$PRECTOTCORR_1<-0
or_w$PS_1<-0
or_w$WS10M_1<-0
or_w$WS10M_MAX_1<-0
or_w$WS10M_MIN_1<-0
or_w$WS10M_RANGE_1<-0
or_w$WD10MN_1<-0

for (i in 7:n) 
{
  or_w$WS2M_1[i]<-mean(c(or_w$WS2M[i],or_w$WS2M[i-1],or_w$WS2M[i-2],or_w$WS2M[i-3],or_w$WS2M[i-4],or_w$WS2M[i-5],or_w$WS2M[i-6]), na.rm=TRUE)
  or_w$T2M_1[i]<-mean(c(or_w$T2M[i],or_w$T2M[i-1],or_w$T2M[i-2],or_w$T2M[i-3],or_w$T2M[i-4],or_w$T2M[i-5],or_w$T2M[i-6]), na.rm=TRUE)
  or_w$T2MDEW_1[i]<-mean(c(or_w$T2MDEW[i],or_w$T2MDEW[i-1],or_w$T2MDEW[i-2],or_w$T2MDEW[i-3],or_w$T2MDEW[i-4],or_w$T2MDEW[i-5],or_w$T2MDEW[i-6]), na.rm=TRUE)
  or_w$T2MWET_1[i]<-mean(c(or_w$T2MWET[i],or_w$T2MWET[i-1],or_w$T2MWET[i-2],or_w$T2MWET[i-3],or_w$T2MWET[i-4],or_w$T2MWET[i-5],or_w$T2MWET[i-6]), na.rm=TRUE)
  or_w$TS_1[i]<-mean(c(or_w$TS[i],or_w$TS[i-1],or_w$TS[i-2],or_w$TS[i-3],or_w$TS[i-4],or_w$TS[i-5],or_w$TS[i-6]), na.rm=TRUE)
  or_w$T2M_RANGE_1[i]<-mean(c(or_w$T2M_RANGE[i],or_w$T2M_RANGE[i-1],or_w$T2M_RANGE[i-2],or_w$T2M_RANGE[i-3],or_w$T2M_RANGE[i-4],or_w$T2M_RANGE[i-5],or_w$T2M_RANGE[i-6]), na.rm=TRUE)
  or_w$T2M_MAX_1[i]<-mean(c(or_w$T2M_MAX[i],or_w$T2M_MAX[i-1],or_w$T2M_MAX[i-2],or_w$T2M_MAX[i-3],or_w$T2M_MAX[i-4],or_w$T2M_MAX[i-5],or_w$T2M_MAX[i-6]), na.rm=TRUE)
  or_w$T2M_MIN_1[i]<-mean(c(or_w$T2M_MIN[i],or_w$T2M_MIN[i-1],or_w$T2M_MIN[i-2],or_w$T2M_MIN[i-3],or_w$T2M_MIN[i-4],or_w$T2M_MIN[i-5],or_w$T2M_MIN[i-6]), na.rm=TRUE)
  or_w$QV2M_1[i]<-mean(c(or_w$QV2M[i],or_w$QV2M[i-1],or_w$QV2M[i-2],or_w$QV2M[i-3],or_w$QV2M[i-4],or_w$QV2M[i-5],or_w$QV2M[i-6]), na.rm=TRUE)
  or_w$RH2M_1[i]<-mean(c(or_w$RH2M[i],or_w$RH2M[i-1],or_w$RH2M[i-2],or_w$RH2M[i-3],or_w$RH2M[i-4],or_w$RH2M[i-5],or_w$RH2M[i-6]), na.rm=TRUE)
  or_w$PRECTOTCORR_1[i]<-mean(c(or_w$PRECTOTCORR[i],or_w$PRECTOTCORR[i-1],or_w$PRECTOTCORR[i-2],or_w$PRECTOTCORR[i-3],or_w$PRECTOTCORR[i-4],or_w$PRECTOTCORR[i-5],or_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  or_w$PS_1[i]<-mean(c(or_w$PS[i],or_w$PS[i-1],or_w$PS[i-2],or_w$PS[i-3],or_w$PS[i-4],or_w$PS[i-5],or_w$PS[i-6]), na.rm=TRUE)
  or_w$WS10M_1[i]<-mean(c(or_w$WS10M[i],or_w$WS10M[i-1],or_w$WS10M[i-2],or_w$WS10M[i-3],or_w$WS10M[i-4],or_w$WS10M[i-5],or_w$WS10M[i-6]), na.rm=TRUE)
  or_w$WS10M_MAX_1[i]<-mean(c(or_w$WS10M_MAX[i],or_w$WS10M_MAX[i-1],or_w$WS10M_MAX[i-2],or_w$WS10M_MAX[i-3],or_w$WS10M_MAX[i-4],or_w$WS10M_MAX[i-5],or_w$WS10M_MAX[i-6]), na.rm=TRUE)
  or_w$WS10M_MIN_1[i]<-mean(c(or_w$WS10M_MIN[i],or_w$WS10M_MIN[i-1],or_w$WS10M_MIN[i-2],or_w$WS10M_MIN[i-3],or_w$WS10M_MIN[i-4],or_w$WS10M_MIN[i-5],or_w$WS10M_MIN[i-6]), na.rm=TRUE)
  or_w$WS10M_RANGE_1[i]<-mean(c(or_w$WS10M_RANGE[i],or_w$WS10M_RANGE[i-1],or_w$WS10M_RANGE[i-2],or_w$WS10M_RANGE[i-3],or_w$WS10M_RANGE[i-4],or_w$WS10M_RANGE[i-5],or_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  or_w$WD10MN_1[i]<-mean(c(or_w$WD10M[i],or_w$WD10M[i-1],or_w$WD10M[i-2],or_w$WD10M[i-3],or_w$WD10M[i-4],or_w$WD10M[i-5],or_w$WD10M[i-6]), na.rm=TRUE)
}

#NY
ny_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/NewYork_Albany_weather.csv", header=TRUE, sep=',')
ny_w$date<-NA
ny_w$date<- paste(ny_w$MO, ny_w$DY,ny_w$YEAR, sep = "/")
ny_w$state<- NA
ny_w$state<- 'New York'
n<-length(ny_w$date)

ny_w[ny_w==-999]<-NA

ny_w$WS2M_1<-0
ny_w$T2M_1<-0
ny_w$T2MDEW_1<-0
ny_w$T2MWET_1<-0
ny_w$TS_1<-0
ny_w$T2M_RANGE_1<-0
ny_w$T2M_MAX_1<-0
ny_w$T2M_MIN_1<-0
ny_w$QV2M_1<-0
ny_w$RH2M_1<-0
ny_w$PRECTOTCORR_1<-0
ny_w$PS_1<-0
ny_w$WS10M_1<-0
ny_w$WS10M_MAX_1<-0
ny_w$WS10M_MIN_1<-0
ny_w$WS10M_RANGE_1<-0
ny_w$WD10MN_1<-0

for (i in 7:n) 
{
  ny_w$WS2M_1[i]<-mean(c(ny_w$WS2M[i],ny_w$WS2M[i-1],ny_w$WS2M[i-2],ny_w$WS2M[i-3],ny_w$WS2M[i-4],ny_w$WS2M[i-5],ny_w$WS2M[i-6]), na.rm=TRUE)
  ny_w$T2M_1[i]<-mean(c(ny_w$T2M[i],ny_w$T2M[i-1],ny_w$T2M[i-2],ny_w$T2M[i-3],ny_w$T2M[i-4],ny_w$T2M[i-5],ny_w$T2M[i-6]), na.rm=TRUE)
  ny_w$T2MDEW_1[i]<-mean(c(ny_w$T2MDEW[i],ny_w$T2MDEW[i-1],ny_w$T2MDEW[i-2],ny_w$T2MDEW[i-3],ny_w$T2MDEW[i-4],ny_w$T2MDEW[i-5],ny_w$T2MDEW[i-6]), na.rm=TRUE)
  ny_w$T2MWET_1[i]<-mean(c(ny_w$T2MWET[i],ny_w$T2MWET[i-1],ny_w$T2MWET[i-2],ny_w$T2MWET[i-3],ny_w$T2MWET[i-4],ny_w$T2MWET[i-5],ny_w$T2MWET[i-6]), na.rm=TRUE)
  ny_w$TS_1[i]<-mean(c(ny_w$TS[i],ny_w$TS[i-1],ny_w$TS[i-2],ny_w$TS[i-3],ny_w$TS[i-4],ny_w$TS[i-5],ny_w$TS[i-6]), na.rm=TRUE)
  ny_w$T2M_RANGE_1[i]<-mean(c(ny_w$T2M_RANGE[i],ny_w$T2M_RANGE[i-1],ny_w$T2M_RANGE[i-2],ny_w$T2M_RANGE[i-3],ny_w$T2M_RANGE[i-4],ny_w$T2M_RANGE[i-5],ny_w$T2M_RANGE[i-6]), na.rm=TRUE)
  ny_w$T2M_MAX_1[i]<-mean(c(ny_w$T2M_MAX[i],ny_w$T2M_MAX[i-1],ny_w$T2M_MAX[i-2],ny_w$T2M_MAX[i-3],ny_w$T2M_MAX[i-4],ny_w$T2M_MAX[i-5],ny_w$T2M_MAX[i-6]), na.rm=TRUE)
  ny_w$T2M_MIN_1[i]<-mean(c(ny_w$T2M_MIN[i],ny_w$T2M_MIN[i-1],ny_w$T2M_MIN[i-2],ny_w$T2M_MIN[i-3],ny_w$T2M_MIN[i-4],ny_w$T2M_MIN[i-5],ny_w$T2M_MIN[i-6]), na.rm=TRUE)
  ny_w$QV2M_1[i]<-mean(c(ny_w$QV2M[i],ny_w$QV2M[i-1],ny_w$QV2M[i-2],ny_w$QV2M[i-3],ny_w$QV2M[i-4],ny_w$QV2M[i-5],ny_w$QV2M[i-6]), na.rm=TRUE)
  ny_w$RH2M_1[i]<-mean(c(ny_w$RH2M[i],ny_w$RH2M[i-1],ny_w$RH2M[i-2],ny_w$RH2M[i-3],ny_w$RH2M[i-4],ny_w$RH2M[i-5],ny_w$RH2M[i-6]), na.rm=TRUE)
  ny_w$PRECTOTCORR_1[i]<-mean(c(ny_w$PRECTOTCORR[i],ny_w$PRECTOTCORR[i-1],ny_w$PRECTOTCORR[i-2],ny_w$PRECTOTCORR[i-3],ny_w$PRECTOTCORR[i-4],ny_w$PRECTOTCORR[i-5],ny_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  ny_w$PS_1[i]<-mean(c(ny_w$PS[i],ny_w$PS[i-1],ny_w$PS[i-2],ny_w$PS[i-3],ny_w$PS[i-4],ny_w$PS[i-5],ny_w$PS[i-6]), na.rm=TRUE)
  ny_w$WS10M_1[i]<-mean(c(ny_w$WS10M[i],ny_w$WS10M[i-1],ny_w$WS10M[i-2],ny_w$WS10M[i-3],ny_w$WS10M[i-4],ny_w$WS10M[i-5],ny_w$WS10M[i-6]), na.rm=TRUE)
  ny_w$WS10M_MAX_1[i]<-mean(c(ny_w$WS10M_MAX[i],ny_w$WS10M_MAX[i-1],ny_w$WS10M_MAX[i-2],ny_w$WS10M_MAX[i-3],ny_w$WS10M_MAX[i-4],ny_w$WS10M_MAX[i-5],ny_w$WS10M_MAX[i-6]), na.rm=TRUE)
  ny_w$WS10M_MIN_1[i]<-mean(c(ny_w$WS10M_MIN[i],ny_w$WS10M_MIN[i-1],ny_w$WS10M_MIN[i-2],ny_w$WS10M_MIN[i-3],ny_w$WS10M_MIN[i-4],ny_w$WS10M_MIN[i-5],ny_w$WS10M_MIN[i-6]), na.rm=TRUE)
  ny_w$WS10M_RANGE_1[i]<-mean(c(ny_w$WS10M_RANGE[i],ny_w$WS10M_RANGE[i-1],ny_w$WS10M_RANGE[i-2],ny_w$WS10M_RANGE[i-3],ny_w$WS10M_RANGE[i-4],ny_w$WS10M_RANGE[i-5],ny_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  ny_w$WD10MN_1[i]<-mean(c(ny_w$WD10M[i],ny_w$WD10M[i-1],ny_w$WD10M[i-2],ny_w$WD10M[i-3],ny_w$WD10M[i-4],ny_w$WD10M[i-5],ny_w$WD10M[i-6]), na.rm=TRUE)
}

#New Mexico
nm_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/NewMexico_SantaFe_weather.csv", header=TRUE, sep=',')
nm_w$date<-NA
nm_w$date<- paste(nm_w$MO, nm_w$DY,nm_w$YEAR, sep = "/")
nm_w$state<- NA
nm_w$state<- 'New Mexico'
n<-length(nm_w$date)

nm_w[nm_w==-999]<-NA

nm_w$WS2M_1<-0
nm_w$T2M_1<-0
nm_w$T2MDEW_1<-0
nm_w$T2MWET_1<-0
nm_w$TS_1<-0
nm_w$T2M_RANGE_1<-0
nm_w$T2M_MAX_1<-0
nm_w$T2M_MIN_1<-0
nm_w$QV2M_1<-0
nm_w$RH2M_1<-0
nm_w$PRECTOTCORR_1<-0
nm_w$PS_1<-0
nm_w$WS10M_1<-0
nm_w$WS10M_MAX_1<-0
nm_w$WS10M_MIN_1<-0
nm_w$WS10M_RANGE_1<-0
nm_w$WD10MN_1<-0

for (i in 7:n) 
{
  nm_w$WS2M_1[i]<-mean(c(nm_w$WS2M[i],nm_w$WS2M[i-1],nm_w$WS2M[i-2],nm_w$WS2M[i-3],nm_w$WS2M[i-4],nm_w$WS2M[i-5],nm_w$WS2M[i-6]), na.rm=TRUE)
  nm_w$T2M_1[i]<-mean(c(nm_w$T2M[i],nm_w$T2M[i-1],nm_w$T2M[i-2],nm_w$T2M[i-3],nm_w$T2M[i-4],nm_w$T2M[i-5],nm_w$T2M[i-6]), na.rm=TRUE)
  nm_w$T2MDEW_1[i]<-mean(c(nm_w$T2MDEW[i],nm_w$T2MDEW[i-1],nm_w$T2MDEW[i-2],nm_w$T2MDEW[i-3],nm_w$T2MDEW[i-4],nm_w$T2MDEW[i-5],nm_w$T2MDEW[i-6]), na.rm=TRUE)
  nm_w$T2MWET_1[i]<-mean(c(nm_w$T2MWET[i],nm_w$T2MWET[i-1],nm_w$T2MWET[i-2],nm_w$T2MWET[i-3],nm_w$T2MWET[i-4],nm_w$T2MWET[i-5],nm_w$T2MWET[i-6]), na.rm=TRUE)
  nm_w$TS_1[i]<-mean(c(nm_w$TS[i],nm_w$TS[i-1],nm_w$TS[i-2],nm_w$TS[i-3],nm_w$TS[i-4],nm_w$TS[i-5],nm_w$TS[i-6]), na.rm=TRUE)
  nm_w$T2M_RANGE_1[i]<-mean(c(nm_w$T2M_RANGE[i],nm_w$T2M_RANGE[i-1],nm_w$T2M_RANGE[i-2],nm_w$T2M_RANGE[i-3],nm_w$T2M_RANGE[i-4],nm_w$T2M_RANGE[i-5],nm_w$T2M_RANGE[i-6]), na.rm=TRUE)
  nm_w$T2M_MAX_1[i]<-mean(c(nm_w$T2M_MAX[i],nm_w$T2M_MAX[i-1],nm_w$T2M_MAX[i-2],nm_w$T2M_MAX[i-3],nm_w$T2M_MAX[i-4],nm_w$T2M_MAX[i-5],nm_w$T2M_MAX[i-6]), na.rm=TRUE)
  nm_w$T2M_MIN_1[i]<-mean(c(nm_w$T2M_MIN[i],nm_w$T2M_MIN[i-1],nm_w$T2M_MIN[i-2],nm_w$T2M_MIN[i-3],nm_w$T2M_MIN[i-4],nm_w$T2M_MIN[i-5],nm_w$T2M_MIN[i-6]), na.rm=TRUE)
  nm_w$QV2M_1[i]<-mean(c(nm_w$QV2M[i],nm_w$QV2M[i-1],nm_w$QV2M[i-2],nm_w$QV2M[i-3],nm_w$QV2M[i-4],nm_w$QV2M[i-5],nm_w$QV2M[i-6]), na.rm=TRUE)
  nm_w$RH2M_1[i]<-mean(c(nm_w$RH2M[i],nm_w$RH2M[i-1],nm_w$RH2M[i-2],nm_w$RH2M[i-3],nm_w$RH2M[i-4],nm_w$RH2M[i-5],nm_w$RH2M[i-6]), na.rm=TRUE)
  nm_w$PRECTOTCORR_1[i]<-mean(c(nm_w$PRECTOTCORR[i],nm_w$PRECTOTCORR[i-1],nm_w$PRECTOTCORR[i-2],nm_w$PRECTOTCORR[i-3],nm_w$PRECTOTCORR[i-4],nm_w$PRECTOTCORR[i-5],nm_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  nm_w$PS_1[i]<-mean(c(nm_w$PS[i],nm_w$PS[i-1],nm_w$PS[i-2],nm_w$PS[i-3],nm_w$PS[i-4],nm_w$PS[i-5],nm_w$PS[i-6]), na.rm=TRUE)
  nm_w$WS10M_1[i]<-mean(c(nm_w$WS10M[i],nm_w$WS10M[i-1],nm_w$WS10M[i-2],nm_w$WS10M[i-3],nm_w$WS10M[i-4],nm_w$WS10M[i-5],nm_w$WS10M[i-6]), na.rm=TRUE)
  nm_w$WS10M_MAX_1[i]<-mean(c(nm_w$WS10M_MAX[i],nm_w$WS10M_MAX[i-1],nm_w$WS10M_MAX[i-2],nm_w$WS10M_MAX[i-3],nm_w$WS10M_MAX[i-4],nm_w$WS10M_MAX[i-5],nm_w$WS10M_MAX[i-6]), na.rm=TRUE)
  nm_w$WS10M_MIN_1[i]<-mean(c(nm_w$WS10M_MIN[i],nm_w$WS10M_MIN[i-1],nm_w$WS10M_MIN[i-2],nm_w$WS10M_MIN[i-3],nm_w$WS10M_MIN[i-4],nm_w$WS10M_MIN[i-5],nm_w$WS10M_MIN[i-6]), na.rm=TRUE)
  nm_w$WS10M_RANGE_1[i]<-mean(c(nm_w$WS10M_RANGE[i],nm_w$WS10M_RANGE[i-1],nm_w$WS10M_RANGE[i-2],nm_w$WS10M_RANGE[i-3],nm_w$WS10M_RANGE[i-4],nm_w$WS10M_RANGE[i-5],nm_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  nm_w$WD10MN_1[i]<-mean(c(nm_w$WD10M[i],nm_w$WD10M[i-1],nm_w$WD10M[i-2],nm_w$WD10M[i-3],nm_w$WD10M[i-4],nm_w$WD10M[i-5],nm_w$WD10M[i-6]), na.rm=TRUE)
}

#Minnesota
mn_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Minnesota_StPaul_weather.csv", header=TRUE, sep=',')
mn_w$date<-NA
mn_w$date<- paste(mn_w$MO, mn_w$DY,mn_w$YEAR, sep = "/")
mn_w$state<- NA
mn_w$state<- 'Minnesota'
n<-length(mn_w$date)

mn_w[mn_w==-999]<-NA

mn_w$WS2M_1<-0
mn_w$T2M_1<-0
mn_w$T2MDEW_1<-0
mn_w$T2MWET_1<-0
mn_w$TS_1<-0
mn_w$T2M_RANGE_1<-0
mn_w$T2M_MAX_1<-0
mn_w$T2M_MIN_1<-0
mn_w$QV2M_1<-0
mn_w$RH2M_1<-0
mn_w$PRECTOTCORR_1<-0
mn_w$PS_1<-0
mn_w$WS10M_1<-0
mn_w$WS10M_MAX_1<-0
mn_w$WS10M_MIN_1<-0
mn_w$WS10M_RANGE_1<-0
mn_w$WD10MN_1<-0

for (i in 7:n) 
{
  mn_w$WS2M_1[i]<-mean(c(mn_w$WS2M[i],mn_w$WS2M[i-1],mn_w$WS2M[i-2],mn_w$WS2M[i-3],mn_w$WS2M[i-4],mn_w$WS2M[i-5],mn_w$WS2M[i-6]), na.rm=TRUE)
  mn_w$T2M_1[i]<-mean(c(mn_w$T2M[i],mn_w$T2M[i-1],mn_w$T2M[i-2],mn_w$T2M[i-3],mn_w$T2M[i-4],mn_w$T2M[i-5],mn_w$T2M[i-6]), na.rm=TRUE)
  mn_w$T2MDEW_1[i]<-mean(c(mn_w$T2MDEW[i],mn_w$T2MDEW[i-1],mn_w$T2MDEW[i-2],mn_w$T2MDEW[i-3],mn_w$T2MDEW[i-4],mn_w$T2MDEW[i-5],mn_w$T2MDEW[i-6]), na.rm=TRUE)
  mn_w$T2MWET_1[i]<-mean(c(mn_w$T2MWET[i],mn_w$T2MWET[i-1],mn_w$T2MWET[i-2],mn_w$T2MWET[i-3],mn_w$T2MWET[i-4],mn_w$T2MWET[i-5],mn_w$T2MWET[i-6]), na.rm=TRUE)
  mn_w$TS_1[i]<-mean(c(mn_w$TS[i],mn_w$TS[i-1],mn_w$TS[i-2],mn_w$TS[i-3],mn_w$TS[i-4],mn_w$TS[i-5],mn_w$TS[i-6]), na.rm=TRUE)
  mn_w$T2M_RANGE_1[i]<-mean(c(mn_w$T2M_RANGE[i],mn_w$T2M_RANGE[i-1],mn_w$T2M_RANGE[i-2],mn_w$T2M_RANGE[i-3],mn_w$T2M_RANGE[i-4],mn_w$T2M_RANGE[i-5],mn_w$T2M_RANGE[i-6]), na.rm=TRUE)
  mn_w$T2M_MAX_1[i]<-mean(c(mn_w$T2M_MAX[i],mn_w$T2M_MAX[i-1],mn_w$T2M_MAX[i-2],mn_w$T2M_MAX[i-3],mn_w$T2M_MAX[i-4],mn_w$T2M_MAX[i-5],mn_w$T2M_MAX[i-6]), na.rm=TRUE)
  mn_w$T2M_MIN_1[i]<-mean(c(mn_w$T2M_MIN[i],mn_w$T2M_MIN[i-1],mn_w$T2M_MIN[i-2],mn_w$T2M_MIN[i-3],mn_w$T2M_MIN[i-4],mn_w$T2M_MIN[i-5],mn_w$T2M_MIN[i-6]), na.rm=TRUE)
  mn_w$QV2M_1[i]<-mean(c(mn_w$QV2M[i],mn_w$QV2M[i-1],mn_w$QV2M[i-2],mn_w$QV2M[i-3],mn_w$QV2M[i-4],mn_w$QV2M[i-5],mn_w$QV2M[i-6]), na.rm=TRUE)
  mn_w$RH2M_1[i]<-mean(c(mn_w$RH2M[i],mn_w$RH2M[i-1],mn_w$RH2M[i-2],mn_w$RH2M[i-3],mn_w$RH2M[i-4],mn_w$RH2M[i-5],mn_w$RH2M[i-6]), na.rm=TRUE)
  mn_w$PRECTOTCORR_1[i]<-mean(c(mn_w$PRECTOTCORR[i],mn_w$PRECTOTCORR[i-1],mn_w$PRECTOTCORR[i-2],mn_w$PRECTOTCORR[i-3],mn_w$PRECTOTCORR[i-4],mn_w$PRECTOTCORR[i-5],mn_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  mn_w$PS_1[i]<-mean(c(mn_w$PS[i],mn_w$PS[i-1],mn_w$PS[i-2],mn_w$PS[i-3],mn_w$PS[i-4],mn_w$PS[i-5],mn_w$PS[i-6]), na.rm=TRUE)
  mn_w$WS10M_1[i]<-mean(c(mn_w$WS10M[i],mn_w$WS10M[i-1],mn_w$WS10M[i-2],mn_w$WS10M[i-3],mn_w$WS10M[i-4],mn_w$WS10M[i-5],mn_w$WS10M[i-6]), na.rm=TRUE)
  mn_w$WS10M_MAX_1[i]<-mean(c(mn_w$WS10M_MAX[i],mn_w$WS10M_MAX[i-1],mn_w$WS10M_MAX[i-2],mn_w$WS10M_MAX[i-3],mn_w$WS10M_MAX[i-4],mn_w$WS10M_MAX[i-5],mn_w$WS10M_MAX[i-6]), na.rm=TRUE)
  mn_w$WS10M_MIN_1[i]<-mean(c(mn_w$WS10M_MIN[i],mn_w$WS10M_MIN[i-1],mn_w$WS10M_MIN[i-2],mn_w$WS10M_MIN[i-3],mn_w$WS10M_MIN[i-4],mn_w$WS10M_MIN[i-5],mn_w$WS10M_MIN[i-6]), na.rm=TRUE)
  mn_w$WS10M_RANGE_1[i]<-mean(c(mn_w$WS10M_RANGE[i],mn_w$WS10M_RANGE[i-1],mn_w$WS10M_RANGE[i-2],mn_w$WS10M_RANGE[i-3],mn_w$WS10M_RANGE[i-4],mn_w$WS10M_RANGE[i-5],mn_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  mn_w$WD10MN_1[i]<-mean(c(mn_w$WD10M[i],mn_w$WD10M[i-1],mn_w$WD10M[i-2],mn_w$WD10M[i-3],mn_w$WD10M[i-4],mn_w$WD10M[i-5],mn_w$WD10M[i-6]), na.rm=TRUE)
}

#Michigan
mi_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Michigan_Lansing_weather.csv", header=TRUE, sep=',')
mi_w$date<-NA
mi_w$date<- paste(mi_w$MO, mi_w$DY,mi_w$YEAR, sep = "/")
mi_w$state<- NA
mi_w$state<- 'Michigan'
n<-length(mi_w$date)

mi_w[mi_w==-999]<-NA

mi_w$WS2M_1<-0
mi_w$T2M_1<-0
mi_w$T2MDEW_1<-0
mi_w$T2MWET_1<-0
mi_w$TS_1<-0
mi_w$T2M_RANGE_1<-0
mi_w$T2M_MAX_1<-0
mi_w$T2M_MIN_1<-0
mi_w$QV2M_1<-0
mi_w$RH2M_1<-0
mi_w$PRECTOTCORR_1<-0
mi_w$PS_1<-0
mi_w$WS10M_1<-0
mi_w$WS10M_MAX_1<-0
mi_w$WS10M_MIN_1<-0
mi_w$WS10M_RANGE_1<-0
mi_w$WD10MN_1<-0

for (i in 7:n) 
{
  mi_w$WS2M_1[i]<-mean(c(mi_w$WS2M[i],mi_w$WS2M[i-1],mi_w$WS2M[i-2],mi_w$WS2M[i-3],mi_w$WS2M[i-4],mi_w$WS2M[i-5],mi_w$WS2M[i-6]), na.rm=TRUE)
  mi_w$T2M_1[i]<-mean(c(mi_w$T2M[i],mi_w$T2M[i-1],mi_w$T2M[i-2],mi_w$T2M[i-3],mi_w$T2M[i-4],mi_w$T2M[i-5],mi_w$T2M[i-6]), na.rm=TRUE)
  mi_w$T2MDEW_1[i]<-mean(c(mi_w$T2MDEW[i],mi_w$T2MDEW[i-1],mi_w$T2MDEW[i-2],mi_w$T2MDEW[i-3],mi_w$T2MDEW[i-4],mi_w$T2MDEW[i-5],mi_w$T2MDEW[i-6]), na.rm=TRUE)
  mi_w$T2MWET_1[i]<-mean(c(mi_w$T2MWET[i],mi_w$T2MWET[i-1],mi_w$T2MWET[i-2],mi_w$T2MWET[i-3],mi_w$T2MWET[i-4],mi_w$T2MWET[i-5],mi_w$T2MWET[i-6]), na.rm=TRUE)
  mi_w$TS_1[i]<-mean(c(mi_w$TS[i],mi_w$TS[i-1],mi_w$TS[i-2],mi_w$TS[i-3],mi_w$TS[i-4],mi_w$TS[i-5],mi_w$TS[i-6]), na.rm=TRUE)
  mi_w$T2M_RANGE_1[i]<-mean(c(mi_w$T2M_RANGE[i],mi_w$T2M_RANGE[i-1],mi_w$T2M_RANGE[i-2],mi_w$T2M_RANGE[i-3],mi_w$T2M_RANGE[i-4],mi_w$T2M_RANGE[i-5],mi_w$T2M_RANGE[i-6]), na.rm=TRUE)
  mi_w$T2M_MAX_1[i]<-mean(c(mi_w$T2M_MAX[i],mi_w$T2M_MAX[i-1],mi_w$T2M_MAX[i-2],mi_w$T2M_MAX[i-3],mi_w$T2M_MAX[i-4],mi_w$T2M_MAX[i-5],mi_w$T2M_MAX[i-6]), na.rm=TRUE)
  mi_w$T2M_MIN_1[i]<-mean(c(mi_w$T2M_MIN[i],mi_w$T2M_MIN[i-1],mi_w$T2M_MIN[i-2],mi_w$T2M_MIN[i-3],mi_w$T2M_MIN[i-4],mi_w$T2M_MIN[i-5],mi_w$T2M_MIN[i-6]), na.rm=TRUE)
  mi_w$QV2M_1[i]<-mean(c(mi_w$QV2M[i],mi_w$QV2M[i-1],mi_w$QV2M[i-2],mi_w$QV2M[i-3],mi_w$QV2M[i-4],mi_w$QV2M[i-5],mi_w$QV2M[i-6]), na.rm=TRUE)
  mi_w$RH2M_1[i]<-mean(c(mi_w$RH2M[i],mi_w$RH2M[i-1],mi_w$RH2M[i-2],mi_w$RH2M[i-3],mi_w$RH2M[i-4],mi_w$RH2M[i-5],mi_w$RH2M[i-6]), na.rm=TRUE)
  mi_w$PRECTOTCORR_1[i]<-mean(c(mi_w$PRECTOTCORR[i],mi_w$PRECTOTCORR[i-1],mi_w$PRECTOTCORR[i-2],mi_w$PRECTOTCORR[i-3],mi_w$PRECTOTCORR[i-4],mi_w$PRECTOTCORR[i-5],mi_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  mi_w$PS_1[i]<-mean(c(mi_w$PS[i],mi_w$PS[i-1],mi_w$PS[i-2],mi_w$PS[i-3],mi_w$PS[i-4],mi_w$PS[i-5],mi_w$PS[i-6]), na.rm=TRUE)
  mi_w$WS10M_1[i]<-mean(c(mi_w$WS10M[i],mi_w$WS10M[i-1],mi_w$WS10M[i-2],mi_w$WS10M[i-3],mi_w$WS10M[i-4],mi_w$WS10M[i-5],mi_w$WS10M[i-6]), na.rm=TRUE)
  mi_w$WS10M_MAX_1[i]<-mean(c(mi_w$WS10M_MAX[i],mi_w$WS10M_MAX[i-1],mi_w$WS10M_MAX[i-2],mi_w$WS10M_MAX[i-3],mi_w$WS10M_MAX[i-4],mi_w$WS10M_MAX[i-5],mi_w$WS10M_MAX[i-6]), na.rm=TRUE)
  mi_w$WS10M_MIN_1[i]<-mean(c(mi_w$WS10M_MIN[i],mi_w$WS10M_MIN[i-1],mi_w$WS10M_MIN[i-2],mi_w$WS10M_MIN[i-3],mi_w$WS10M_MIN[i-4],mi_w$WS10M_MIN[i-5],mi_w$WS10M_MIN[i-6]), na.rm=TRUE)
  mi_w$WS10M_RANGE_1[i]<-mean(c(mi_w$WS10M_RANGE[i],mi_w$WS10M_RANGE[i-1],mi_w$WS10M_RANGE[i-2],mi_w$WS10M_RANGE[i-3],mi_w$WS10M_RANGE[i-4],mi_w$WS10M_RANGE[i-5],mi_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  mi_w$WD10MN_1[i]<-mean(c(mi_w$WD10M[i],mi_w$WD10M[i-1],mi_w$WD10M[i-2],mi_w$WD10M[i-3],mi_w$WD10M[i-4],mi_w$WD10M[i-5],mi_w$WD10M[i-6]), na.rm=TRUE)
}

#Maryland
md_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Maryland_Annapolis_weather.csv", header=TRUE, sep=',')
md_w$date<-NA
md_w$date<- paste(md_w$MO, md_w$DY,md_w$YEAR, sep = "/")
md_w$state<- NA
md_w$state<- 'Maryland'
n<-length(md_w$date)

md_w[md_w==-999]<-NA

md_w$WS2M_1<-0
md_w$T2M_1<-0
md_w$T2MDEW_1<-0
md_w$T2MWET_1<-0
md_w$TS_1<-0
md_w$T2M_RANGE_1<-0
md_w$T2M_MAX_1<-0
md_w$T2M_MIN_1<-0
md_w$QV2M_1<-0
md_w$RH2M_1<-0
md_w$PRECTOTCORR_1<-0
md_w$PS_1<-0
md_w$WS10M_1<-0
md_w$WS10M_MAX_1<-0
md_w$WS10M_MIN_1<-0
md_w$WS10M_RANGE_1<-0
md_w$WD10MN_1<-0

for (i in 7:n) 
{
  md_w$WS2M_1[i]<-mean(c(md_w$WS2M[i],md_w$WS2M[i-1],md_w$WS2M[i-2],md_w$WS2M[i-3],md_w$WS2M[i-4],md_w$WS2M[i-5],md_w$WS2M[i-6]), na.rm=TRUE)
  md_w$T2M_1[i]<-mean(c(md_w$T2M[i],md_w$T2M[i-1],md_w$T2M[i-2],md_w$T2M[i-3],md_w$T2M[i-4],md_w$T2M[i-5],md_w$T2M[i-6]), na.rm=TRUE)
  md_w$T2MDEW_1[i]<-mean(c(md_w$T2MDEW[i],md_w$T2MDEW[i-1],md_w$T2MDEW[i-2],md_w$T2MDEW[i-3],md_w$T2MDEW[i-4],md_w$T2MDEW[i-5],md_w$T2MDEW[i-6]), na.rm=TRUE)
  md_w$T2MWET_1[i]<-mean(c(md_w$T2MWET[i],md_w$T2MWET[i-1],md_w$T2MWET[i-2],md_w$T2MWET[i-3],md_w$T2MWET[i-4],md_w$T2MWET[i-5],md_w$T2MWET[i-6]), na.rm=TRUE)
  md_w$TS_1[i]<-mean(c(md_w$TS[i],md_w$TS[i-1],md_w$TS[i-2],md_w$TS[i-3],md_w$TS[i-4],md_w$TS[i-5],md_w$TS[i-6]), na.rm=TRUE)
  md_w$T2M_RANGE_1[i]<-mean(c(md_w$T2M_RANGE[i],md_w$T2M_RANGE[i-1],md_w$T2M_RANGE[i-2],md_w$T2M_RANGE[i-3],md_w$T2M_RANGE[i-4],md_w$T2M_RANGE[i-5],md_w$T2M_RANGE[i-6]), na.rm=TRUE)
  md_w$T2M_MAX_1[i]<-mean(c(md_w$T2M_MAX[i],md_w$T2M_MAX[i-1],md_w$T2M_MAX[i-2],md_w$T2M_MAX[i-3],md_w$T2M_MAX[i-4],md_w$T2M_MAX[i-5],md_w$T2M_MAX[i-6]), na.rm=TRUE)
  md_w$T2M_MIN_1[i]<-mean(c(md_w$T2M_MIN[i],md_w$T2M_MIN[i-1],md_w$T2M_MIN[i-2],md_w$T2M_MIN[i-3],md_w$T2M_MIN[i-4],md_w$T2M_MIN[i-5],md_w$T2M_MIN[i-6]), na.rm=TRUE)
  md_w$QV2M_1[i]<-mean(c(md_w$QV2M[i],md_w$QV2M[i-1],md_w$QV2M[i-2],md_w$QV2M[i-3],md_w$QV2M[i-4],md_w$QV2M[i-5],md_w$QV2M[i-6]), na.rm=TRUE)
  md_w$RH2M_1[i]<-mean(c(md_w$RH2M[i],md_w$RH2M[i-1],md_w$RH2M[i-2],md_w$RH2M[i-3],md_w$RH2M[i-4],md_w$RH2M[i-5],md_w$RH2M[i-6]), na.rm=TRUE)
  md_w$PRECTOTCORR_1[i]<-mean(c(md_w$PRECTOTCORR[i],md_w$PRECTOTCORR[i-1],md_w$PRECTOTCORR[i-2],md_w$PRECTOTCORR[i-3],md_w$PRECTOTCORR[i-4],md_w$PRECTOTCORR[i-5],md_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  md_w$PS_1[i]<-mean(c(md_w$PS[i],md_w$PS[i-1],md_w$PS[i-2],md_w$PS[i-3],md_w$PS[i-4],md_w$PS[i-5],md_w$PS[i-6]), na.rm=TRUE)
  md_w$WS10M_1[i]<-mean(c(md_w$WS10M[i],md_w$WS10M[i-1],md_w$WS10M[i-2],md_w$WS10M[i-3],md_w$WS10M[i-4],md_w$WS10M[i-5],md_w$WS10M[i-6]), na.rm=TRUE)
  md_w$WS10M_MAX_1[i]<-mean(c(md_w$WS10M_MAX[i],md_w$WS10M_MAX[i-1],md_w$WS10M_MAX[i-2],md_w$WS10M_MAX[i-3],md_w$WS10M_MAX[i-4],md_w$WS10M_MAX[i-5],md_w$WS10M_MAX[i-6]), na.rm=TRUE)
  md_w$WS10M_MIN_1[i]<-mean(c(md_w$WS10M_MIN[i],md_w$WS10M_MIN[i-1],md_w$WS10M_MIN[i-2],md_w$WS10M_MIN[i-3],md_w$WS10M_MIN[i-4],md_w$WS10M_MIN[i-5],md_w$WS10M_MIN[i-6]), na.rm=TRUE)
  md_w$WS10M_RANGE_1[i]<-mean(c(md_w$WS10M_RANGE[i],md_w$WS10M_RANGE[i-1],md_w$WS10M_RANGE[i-2],md_w$WS10M_RANGE[i-3],md_w$WS10M_RANGE[i-4],md_w$WS10M_RANGE[i-5],md_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  md_w$WD10MN_1[i]<-mean(c(md_w$WD10M[i],md_w$WD10M[i-1],md_w$WD10M[i-2],md_w$WD10M[i-3],md_w$WD10M[i-4],md_w$WD10M[i-5],md_w$WD10M[i-6]), na.rm=TRUE)
}

#Georgia
ga_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Georgia_Atlanta_weather.csv", header=TRUE, sep=',')
ga_w$date<-NA
ga_w$date<- paste(ga_w$MO, ga_w$DY,ga_w$YEAR, sep = "/")
ga_w$state<-NA
ga_w$state<-'Georgia'
n<-length(ga_w$date)

ga_w[ga_w==-999]<-NA

ga_w$WS2M_1<-0
ga_w$T2M_1<-0
ga_w$T2MDEW_1<-0
ga_w$T2MWET_1<-0
ga_w$TS_1<-0
ga_w$T2M_RANGE_1<-0
ga_w$T2M_MAX_1<-0
ga_w$T2M_MIN_1<-0
ga_w$QV2M_1<-0
ga_w$RH2M_1<-0
ga_w$PRECTOTCORR_1<-0
ga_w$PS_1<-0
ga_w$WS10M_1<-0
ga_w$WS10M_MAX_1<-0
ga_w$WS10M_MIN_1<-0
ga_w$WS10M_RANGE_1<-0
ga_w$WD10MN_1<-0

for (i in 7:n) 
{
  ga_w$WS2M_1[i]<-mean(c(ga_w$WS2M[i],ga_w$WS2M[i-1],ga_w$WS2M[i-2],ga_w$WS2M[i-3],ga_w$WS2M[i-4],ga_w$WS2M[i-5],ga_w$WS2M[i-6]), na.rm=TRUE)
  ga_w$T2M_1[i]<-mean(c(ga_w$T2M[i],ga_w$T2M[i-1],ga_w$T2M[i-2],ga_w$T2M[i-3],ga_w$T2M[i-4],ga_w$T2M[i-5],ga_w$T2M[i-6]), na.rm=TRUE)
  ga_w$T2MDEW_1[i]<-mean(c(ga_w$T2MDEW[i],ga_w$T2MDEW[i-1],ga_w$T2MDEW[i-2],ga_w$T2MDEW[i-3],ga_w$T2MDEW[i-4],ga_w$T2MDEW[i-5],ga_w$T2MDEW[i-6]), na.rm=TRUE)
  ga_w$T2MWET_1[i]<-mean(c(ga_w$T2MWET[i],ga_w$T2MWET[i-1],ga_w$T2MWET[i-2],ga_w$T2MWET[i-3],ga_w$T2MWET[i-4],ga_w$T2MWET[i-5],ga_w$T2MWET[i-6]), na.rm=TRUE)
  ga_w$TS_1[i]<-mean(c(ga_w$TS[i],ga_w$TS[i-1],ga_w$TS[i-2],ga_w$TS[i-3],ga_w$TS[i-4],ga_w$TS[i-5],ga_w$TS[i-6]), na.rm=TRUE)
  ga_w$T2M_RANGE_1[i]<-mean(c(ga_w$T2M_RANGE[i],ga_w$T2M_RANGE[i-1],ga_w$T2M_RANGE[i-2],ga_w$T2M_RANGE[i-3],ga_w$T2M_RANGE[i-4],ga_w$T2M_RANGE[i-5],ga_w$T2M_RANGE[i-6]), na.rm=TRUE)
  ga_w$T2M_MAX_1[i]<-mean(c(ga_w$T2M_MAX[i],ga_w$T2M_MAX[i-1],ga_w$T2M_MAX[i-2],ga_w$T2M_MAX[i-3],ga_w$T2M_MAX[i-4],ga_w$T2M_MAX[i-5],ga_w$T2M_MAX[i-6]), na.rm=TRUE)
  ga_w$T2M_MIN_1[i]<-mean(c(ga_w$T2M_MIN[i],ga_w$T2M_MIN[i-1],ga_w$T2M_MIN[i-2],ga_w$T2M_MIN[i-3],ga_w$T2M_MIN[i-4],ga_w$T2M_MIN[i-5],ga_w$T2M_MIN[i-6]), na.rm=TRUE)
  ga_w$QV2M_1[i]<-mean(c(ga_w$QV2M[i],ga_w$QV2M[i-1],ga_w$QV2M[i-2],ga_w$QV2M[i-3],ga_w$QV2M[i-4],ga_w$QV2M[i-5],ga_w$QV2M[i-6]), na.rm=TRUE)
  ga_w$RH2M_1[i]<-mean(c(ga_w$RH2M[i],ga_w$RH2M[i-1],ga_w$RH2M[i-2],ga_w$RH2M[i-3],ga_w$RH2M[i-4],ga_w$RH2M[i-5],ga_w$RH2M[i-6]), na.rm=TRUE)
  ga_w$PRECTOTCORR_1[i]<-mean(c(ga_w$PRECTOTCORR[i],ga_w$PRECTOTCORR[i-1],ga_w$PRECTOTCORR[i-2],ga_w$PRECTOTCORR[i-3],ga_w$PRECTOTCORR[i-4],ga_w$PRECTOTCORR[i-5],ga_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  ga_w$PS_1[i]<-mean(c(ga_w$PS[i],ga_w$PS[i-1],ga_w$PS[i-2],ga_w$PS[i-3],ga_w$PS[i-4],ga_w$PS[i-5],ga_w$PS[i-6]), na.rm=TRUE)
  ga_w$WS10M_1[i]<-mean(c(ga_w$WS10M[i],ga_w$WS10M[i-1],ga_w$WS10M[i-2],ga_w$WS10M[i-3],ga_w$WS10M[i-4],ga_w$WS10M[i-5],ga_w$WS10M[i-6]), na.rm=TRUE)
  ga_w$WS10M_MAX_1[i]<-mean(c(ga_w$WS10M_MAX[i],ga_w$WS10M_MAX[i-1],ga_w$WS10M_MAX[i-2],ga_w$WS10M_MAX[i-3],ga_w$WS10M_MAX[i-4],ga_w$WS10M_MAX[i-5],ga_w$WS10M_MAX[i-6]), na.rm=TRUE)
  ga_w$WS10M_MIN_1[i]<-mean(c(ga_w$WS10M_MIN[i],ga_w$WS10M_MIN[i-1],ga_w$WS10M_MIN[i-2],ga_w$WS10M_MIN[i-3],ga_w$WS10M_MIN[i-4],ga_w$WS10M_MIN[i-5],ga_w$WS10M_MIN[i-6]), na.rm=TRUE)
  ga_w$WS10M_RANGE_1[i]<-mean(c(ga_w$WS10M_RANGE[i],ga_w$WS10M_RANGE[i-1],ga_w$WS10M_RANGE[i-2],ga_w$WS10M_RANGE[i-3],ga_w$WS10M_RANGE[i-4],ga_w$WS10M_RANGE[i-5],ga_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  ga_w$WD10MN_1[i]<-mean(c(ga_w$WD10M[i],ga_w$WD10M[i-1],ga_w$WD10M[i-2],ga_w$WD10M[i-3],ga_w$WD10M[i-4],ga_w$WD10M[i-5],ga_w$WD10M[i-6]), na.rm=TRUE)
}

#Connecticut
ct_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Conneticut_Hartford_weather.csv", header=TRUE, sep=',')
ct_w$date<-NA
ct_w$date<- paste(ct_w$MO, ct_w$DY,ct_w$YEAR, sep = "/")
ct_w$state<-NA
ct_w$state<-'Connecticut'
n<-length(ct_w$date)

ct_w[ct_w==-999]<-NA

ct_w$WS2M_1<-0
ct_w$T2M_1<-0
ct_w$T2MDEW_1<-0
ct_w$T2MWET_1<-0
ct_w$TS_1<-0
ct_w$T2M_RANGE_1<-0
ct_w$T2M_MAX_1<-0
ct_w$T2M_MIN_1<-0
ct_w$QV2M_1<-0
ct_w$RH2M_1<-0
ct_w$PRECTOTCORR_1<-0
ct_w$PS_1<-0
ct_w$WS10M_1<-0
ct_w$WS10M_MAX_1<-0
ct_w$WS10M_MIN_1<-0
ct_w$WS10M_RANGE_1<-0
ct_w$WD10MN_1<-0

for (i in 7:n) 
{
  ct_w$WS2M_1[i]<-mean(c(ct_w$WS2M[i],ct_w$WS2M[i-1],ct_w$WS2M[i-2],ct_w$WS2M[i-3],ct_w$WS2M[i-4],ct_w$WS2M[i-5],ct_w$WS2M[i-6]), na.rm=TRUE)
  ct_w$T2M_1[i]<-mean(c(ct_w$T2M[i],ct_w$T2M[i-1],ct_w$T2M[i-2],ct_w$T2M[i-3],ct_w$T2M[i-4],ct_w$T2M[i-5],ct_w$T2M[i-6]), na.rm=TRUE)
  ct_w$T2MDEW_1[i]<-mean(c(ct_w$T2MDEW[i],ct_w$T2MDEW[i-1],ct_w$T2MDEW[i-2],ct_w$T2MDEW[i-3],ct_w$T2MDEW[i-4],ct_w$T2MDEW[i-5],ct_w$T2MDEW[i-6]), na.rm=TRUE)
  ct_w$T2MWET_1[i]<-mean(c(ct_w$T2MWET[i],ct_w$T2MWET[i-1],ct_w$T2MWET[i-2],ct_w$T2MWET[i-3],ct_w$T2MWET[i-4],ct_w$T2MWET[i-5],ct_w$T2MWET[i-6]), na.rm=TRUE)
  ct_w$TS_1[i]<-mean(c(ct_w$TS[i],ct_w$TS[i-1],ct_w$TS[i-2],ct_w$TS[i-3],ct_w$TS[i-4],ct_w$TS[i-5],ct_w$TS[i-6]), na.rm=TRUE)
  ct_w$T2M_RANGE_1[i]<-mean(c(ct_w$T2M_RANGE[i],ct_w$T2M_RANGE[i-1],ct_w$T2M_RANGE[i-2],ct_w$T2M_RANGE[i-3],ct_w$T2M_RANGE[i-4],ct_w$T2M_RANGE[i-5],ct_w$T2M_RANGE[i-6]), na.rm=TRUE)
  ct_w$T2M_MAX_1[i]<-mean(c(ct_w$T2M_MAX[i],ct_w$T2M_MAX[i-1],ct_w$T2M_MAX[i-2],ct_w$T2M_MAX[i-3],ct_w$T2M_MAX[i-4],ct_w$T2M_MAX[i-5],ct_w$T2M_MAX[i-6]), na.rm=TRUE)
  ct_w$T2M_MIN_1[i]<-mean(c(ct_w$T2M_MIN[i],ct_w$T2M_MIN[i-1],ct_w$T2M_MIN[i-2],ct_w$T2M_MIN[i-3],ct_w$T2M_MIN[i-4],ct_w$T2M_MIN[i-5],ct_w$T2M_MIN[i-6]), na.rm=TRUE)
  ct_w$QV2M_1[i]<-mean(c(ct_w$QV2M[i],ct_w$QV2M[i-1],ct_w$QV2M[i-2],ct_w$QV2M[i-3],ct_w$QV2M[i-4],ct_w$QV2M[i-5],ct_w$QV2M[i-6]), na.rm=TRUE)
  ct_w$RH2M_1[i]<-mean(c(ct_w$RH2M[i],ct_w$RH2M[i-1],ct_w$RH2M[i-2],ct_w$RH2M[i-3],ct_w$RH2M[i-4],ct_w$RH2M[i-5],ct_w$RH2M[i-6]), na.rm=TRUE)
  ct_w$PRECTOTCORR_1[i]<-mean(c(ct_w$PRECTOTCORR[i],ct_w$PRECTOTCORR[i-1],ct_w$PRECTOTCORR[i-2],ct_w$PRECTOTCORR[i-3],ct_w$PRECTOTCORR[i-4],ct_w$PRECTOTCORR[i-5],ct_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  ct_w$PS_1[i]<-mean(c(ct_w$PS[i],ct_w$PS[i-1],ct_w$PS[i-2],ct_w$PS[i-3],ct_w$PS[i-4],ct_w$PS[i-5],ct_w$PS[i-6]), na.rm=TRUE)
  ct_w$WS10M_1[i]<-mean(c(ct_w$WS10M[i],ct_w$WS10M[i-1],ct_w$WS10M[i-2],ct_w$WS10M[i-3],ct_w$WS10M[i-4],ct_w$WS10M[i-5],ct_w$WS10M[i-6]), na.rm=TRUE)
  ct_w$WS10M_MAX_1[i]<-mean(c(ct_w$WS10M_MAX[i],ct_w$WS10M_MAX[i-1],ct_w$WS10M_MAX[i-2],ct_w$WS10M_MAX[i-3],ct_w$WS10M_MAX[i-4],ct_w$WS10M_MAX[i-5],ct_w$WS10M_MAX[i-6]), na.rm=TRUE)
  ct_w$WS10M_MIN_1[i]<-mean(c(ct_w$WS10M_MIN[i],ct_w$WS10M_MIN[i-1],ct_w$WS10M_MIN[i-2],ct_w$WS10M_MIN[i-3],ct_w$WS10M_MIN[i-4],ct_w$WS10M_MIN[i-5],ct_w$WS10M_MIN[i-6]), na.rm=TRUE)
  ct_w$WS10M_RANGE_1[i]<-mean(c(ct_w$WS10M_RANGE[i],ct_w$WS10M_RANGE[i-1],ct_w$WS10M_RANGE[i-2],ct_w$WS10M_RANGE[i-3],ct_w$WS10M_RANGE[i-4],ct_w$WS10M_RANGE[i-5],ct_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  ct_w$WD10MN_1[i]<-mean(c(ct_w$WD10M[i],ct_w$WD10M[i-1],ct_w$WD10M[i-2],ct_w$WD10M[i-3],ct_w$WD10M[i-4],ct_w$WD10M[i-5],ct_w$WD10M[i-6]), na.rm=TRUE)
}

#Colorado
co_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Colorado_Denver_weather.csv", header=TRUE, sep=',')
co_w$date<-NA
co_w$date<- paste(co_w$MO, co_w$DY,co_w$YEAR, sep = "/")
co_w$state<-NA
co_w$state<-'Colorado'
n<-length(co_w$date)

co_w[co_w==-999]<-NA

co_w$WS2M_1<-0
co_w$T2M_1<-0
co_w$T2MDEW_1<-0
co_w$T2MWET_1<-0
co_w$TS_1<-0
co_w$T2M_RANGE_1<-0
co_w$T2M_MAX_1<-0
co_w$T2M_MIN_1<-0
co_w$QV2M_1<-0
co_w$RH2M_1<-0
co_w$PRECTOTCORR_1<-0
co_w$PS_1<-0
co_w$WS10M_1<-0
co_w$WS10M_MAX_1<-0
co_w$WS10M_MIN_1<-0
co_w$WS10M_RANGE_1<-0
co_w$WD10MN_1<-0

for (i in 7:n) 
{
  co_w$WS2M_1[i]<-mean(c(co_w$WS2M[i],co_w$WS2M[i-1],co_w$WS2M[i-2],co_w$WS2M[i-3],co_w$WS2M[i-4],co_w$WS2M[i-5],co_w$WS2M[i-6]), na.rm=TRUE)
  co_w$T2M_1[i]<-mean(c(co_w$T2M[i],co_w$T2M[i-1],co_w$T2M[i-2],co_w$T2M[i-3],co_w$T2M[i-4],co_w$T2M[i-5],co_w$T2M[i-6]), na.rm=TRUE)
  co_w$T2MDEW_1[i]<-mean(c(co_w$T2MDEW[i],co_w$T2MDEW[i-1],co_w$T2MDEW[i-2],co_w$T2MDEW[i-3],co_w$T2MDEW[i-4],co_w$T2MDEW[i-5],co_w$T2MDEW[i-6]), na.rm=TRUE)
  co_w$T2MWET_1[i]<-mean(c(co_w$T2MWET[i],co_w$T2MWET[i-1],co_w$T2MWET[i-2],co_w$T2MWET[i-3],co_w$T2MWET[i-4],co_w$T2MWET[i-5],co_w$T2MWET[i-6]), na.rm=TRUE)
  co_w$TS_1[i]<-mean(c(co_w$TS[i],co_w$TS[i-1],co_w$TS[i-2],co_w$TS[i-3],co_w$TS[i-4],co_w$TS[i-5],co_w$TS[i-6]), na.rm=TRUE)
  co_w$T2M_RANGE_1[i]<-mean(c(co_w$T2M_RANGE[i],co_w$T2M_RANGE[i-1],co_w$T2M_RANGE[i-2],co_w$T2M_RANGE[i-3],co_w$T2M_RANGE[i-4],co_w$T2M_RANGE[i-5],co_w$T2M_RANGE[i-6]), na.rm=TRUE)
  co_w$T2M_MAX_1[i]<-mean(c(co_w$T2M_MAX[i],co_w$T2M_MAX[i-1],co_w$T2M_MAX[i-2],co_w$T2M_MAX[i-3],co_w$T2M_MAX[i-4],co_w$T2M_MAX[i-5],co_w$T2M_MAX[i-6]), na.rm=TRUE)
  co_w$T2M_MIN_1[i]<-mean(c(co_w$T2M_MIN[i],co_w$T2M_MIN[i-1],co_w$T2M_MIN[i-2],co_w$T2M_MIN[i-3],co_w$T2M_MIN[i-4],co_w$T2M_MIN[i-5],co_w$T2M_MIN[i-6]), na.rm=TRUE)
  co_w$QV2M_1[i]<-mean(c(co_w$QV2M[i],co_w$QV2M[i-1],co_w$QV2M[i-2],co_w$QV2M[i-3],co_w$QV2M[i-4],co_w$QV2M[i-5],co_w$QV2M[i-6]), na.rm=TRUE)
  co_w$RH2M_1[i]<-mean(c(co_w$RH2M[i],co_w$RH2M[i-1],co_w$RH2M[i-2],co_w$RH2M[i-3],co_w$RH2M[i-4],co_w$RH2M[i-5],co_w$RH2M[i-6]), na.rm=TRUE)
  co_w$PRECTOTCORR_1[i]<-mean(c(co_w$PRECTOTCORR[i],co_w$PRECTOTCORR[i-1],co_w$PRECTOTCORR[i-2],co_w$PRECTOTCORR[i-3],co_w$PRECTOTCORR[i-4],co_w$PRECTOTCORR[i-5],co_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  co_w$PS_1[i]<-mean(c(co_w$PS[i],co_w$PS[i-1],co_w$PS[i-2],co_w$PS[i-3],co_w$PS[i-4],co_w$PS[i-5],co_w$PS[i-6]), na.rm=TRUE)
  co_w$WS10M_1[i]<-mean(c(co_w$WS10M[i],co_w$WS10M[i-1],co_w$WS10M[i-2],co_w$WS10M[i-3],co_w$WS10M[i-4],co_w$WS10M[i-5],co_w$WS10M[i-6]), na.rm=TRUE)
  co_w$WS10M_MAX_1[i]<-mean(c(co_w$WS10M_MAX[i],co_w$WS10M_MAX[i-1],co_w$WS10M_MAX[i-2],co_w$WS10M_MAX[i-3],co_w$WS10M_MAX[i-4],co_w$WS10M_MAX[i-5],co_w$WS10M_MAX[i-6]), na.rm=TRUE)
  co_w$WS10M_MIN_1[i]<-mean(c(co_w$WS10M_MIN[i],co_w$WS10M_MIN[i-1],co_w$WS10M_MIN[i-2],co_w$WS10M_MIN[i-3],co_w$WS10M_MIN[i-4],co_w$WS10M_MIN[i-5],co_w$WS10M_MIN[i-6]), na.rm=TRUE)
  co_w$WS10M_RANGE_1[i]<-mean(c(co_w$WS10M_RANGE[i],co_w$WS10M_RANGE[i-1],co_w$WS10M_RANGE[i-2],co_w$WS10M_RANGE[i-3],co_w$WS10M_RANGE[i-4],co_w$WS10M_RANGE[i-5],co_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  co_w$WD10MN_1[i]<-mean(c(co_w$WD10M[i],co_w$WD10M[i-1],co_w$WD10M[i-2],co_w$WD10M[i-3],co_w$WD10M[i-4],co_w$WD10M[i-5],co_w$WD10M[i-6]), na.rm=TRUE)
}

#California
ca_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/California_Sacramento_weather.csv", header=TRUE, sep=',')
ca_w$date<-NA
ca_w$date<- paste(ca_w$MO, ca_w$DY,ca_w$YEAR, sep = "/")
ca_w$state<-NA
ca_w$state<-'California'
n<-length(ca_w$date)

ca_w[ca_w==-999]<-NA

ca_w$WS2M_1<-0
ca_w$T2M_1<-0
ca_w$T2MDEW_1<-0
ca_w$T2MWET_1<-0
ca_w$TS_1<-0
ca_w$T2M_RANGE_1<-0
ca_w$T2M_MAX_1<-0
ca_w$T2M_MIN_1<-0
ca_w$QV2M_1<-0
ca_w$RH2M_1<-0
ca_w$PRECTOTCORR_1<-0
ca_w$PS_1<-0
ca_w$WS10M_1<-0
ca_w$WS10M_MAX_1<-0
ca_w$WS10M_MIN_1<-0
ca_w$WS10M_RANGE_1<-0
ca_w$WD10MN_1<-0

for (i in 7:n) 
{
  ca_w$WS2M_1[i]<-mean(c(ca_w$WS2M[i],ca_w$WS2M[i-1],ca_w$WS2M[i-2],ca_w$WS2M[i-3],ca_w$WS2M[i-4],ca_w$WS2M[i-5],ca_w$WS2M[i-6]), na.rm=TRUE)
  ca_w$T2M_1[i]<-mean(c(ca_w$T2M[i],ca_w$T2M[i-1],ca_w$T2M[i-2],ca_w$T2M[i-3],ca_w$T2M[i-4],ca_w$T2M[i-5],ca_w$T2M[i-6]), na.rm=TRUE)
  ca_w$T2MDEW_1[i]<-mean(c(ca_w$T2MDEW[i],ca_w$T2MDEW[i-1],ca_w$T2MDEW[i-2],ca_w$T2MDEW[i-3],ca_w$T2MDEW[i-4],ca_w$T2MDEW[i-5],ca_w$T2MDEW[i-6]), na.rm=TRUE)
  ca_w$T2MWET_1[i]<-mean(c(ca_w$T2MWET[i],ca_w$T2MWET[i-1],ca_w$T2MWET[i-2],ca_w$T2MWET[i-3],ca_w$T2MWET[i-4],ca_w$T2MWET[i-5],ca_w$T2MWET[i-6]), na.rm=TRUE)
  ca_w$TS_1[i]<-mean(c(ca_w$TS[i],ca_w$TS[i-1],ca_w$TS[i-2],ca_w$TS[i-3],ca_w$TS[i-4],ca_w$TS[i-5],ca_w$TS[i-6]), na.rm=TRUE)
  ca_w$T2M_RANGE_1[i]<-mean(c(ca_w$T2M_RANGE[i],ca_w$T2M_RANGE[i-1],ca_w$T2M_RANGE[i-2],ca_w$T2M_RANGE[i-3],ca_w$T2M_RANGE[i-4],ca_w$T2M_RANGE[i-5],ca_w$T2M_RANGE[i-6]), na.rm=TRUE)
  ca_w$T2M_MAX_1[i]<-mean(c(ca_w$T2M_MAX[i],ca_w$T2M_MAX[i-1],ca_w$T2M_MAX[i-2],ca_w$T2M_MAX[i-3],ca_w$T2M_MAX[i-4],ca_w$T2M_MAX[i-5],ca_w$T2M_MAX[i-6]), na.rm=TRUE)
  ca_w$T2M_MIN_1[i]<-mean(c(ca_w$T2M_MIN[i],ca_w$T2M_MIN[i-1],ca_w$T2M_MIN[i-2],ca_w$T2M_MIN[i-3],ca_w$T2M_MIN[i-4],ca_w$T2M_MIN[i-5],ca_w$T2M_MIN[i-6]), na.rm=TRUE)
  ca_w$QV2M_1[i]<-mean(c(ca_w$QV2M[i],ca_w$QV2M[i-1],ca_w$QV2M[i-2],ca_w$QV2M[i-3],ca_w$QV2M[i-4],ca_w$QV2M[i-5],ca_w$QV2M[i-6]), na.rm=TRUE)
  ca_w$RH2M_1[i]<-mean(c(ca_w$RH2M[i],ca_w$RH2M[i-1],ca_w$RH2M[i-2],ca_w$RH2M[i-3],ca_w$RH2M[i-4],ca_w$RH2M[i-5],ca_w$RH2M[i-6]), na.rm=TRUE)
  ca_w$PRECTOTCORR_1[i]<-mean(c(ca_w$PRECTOTCORR[i],ca_w$PRECTOTCORR[i-1],ca_w$PRECTOTCORR[i-2],ca_w$PRECTOTCORR[i-3],ca_w$PRECTOTCORR[i-4],ca_w$PRECTOTCORR[i-5],ca_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  ca_w$PS_1[i]<-mean(c(ca_w$PS[i],ca_w$PS[i-1],ca_w$PS[i-2],ca_w$PS[i-3],ca_w$PS[i-4],ca_w$PS[i-5],ca_w$PS[i-6]), na.rm=TRUE)
  ca_w$WS10M_1[i]<-mean(c(ca_w$WS10M[i],ca_w$WS10M[i-1],ca_w$WS10M[i-2],ca_w$WS10M[i-3],ca_w$WS10M[i-4],ca_w$WS10M[i-5],ca_w$WS10M[i-6]), na.rm=TRUE)
  ca_w$WS10M_MAX_1[i]<-mean(c(ca_w$WS10M_MAX[i],ca_w$WS10M_MAX[i-1],ca_w$WS10M_MAX[i-2],ca_w$WS10M_MAX[i-3],ca_w$WS10M_MAX[i-4],ca_w$WS10M_MAX[i-5],ca_w$WS10M_MAX[i-6]), na.rm=TRUE)
  ca_w$WS10M_MIN_1[i]<-mean(c(ca_w$WS10M_MIN[i],ca_w$WS10M_MIN[i-1],ca_w$WS10M_MIN[i-2],ca_w$WS10M_MIN[i-3],ca_w$WS10M_MIN[i-4],ca_w$WS10M_MIN[i-5],ca_w$WS10M_MIN[i-6]), na.rm=TRUE)
  ca_w$WS10M_RANGE_1[i]<-mean(c(ca_w$WS10M_RANGE[i],ca_w$WS10M_RANGE[i-1],ca_w$WS10M_RANGE[i-2],ca_w$WS10M_RANGE[i-3],ca_w$WS10M_RANGE[i-4],ca_w$WS10M_RANGE[i-5],ca_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  ca_w$WD10MN_1[i]<-mean(c(ca_w$WD10M[i],ca_w$WD10M[i-1],ca_w$WD10M[i-2],ca_w$WD10M[i-3],ca_w$WD10M[i-4],ca_w$WD10M[i-5],ca_w$WD10M[i-6]), na.rm=TRUE)
}

#North Carolina
nc_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/NorthCarolina_Raleigh_weather.csv", header=TRUE, sep=',')
nc_w$date<-NA
nc_w$date<- paste(nc_w$MO, nc_w$DY,nc_w$YEAR, sep = "/")
nc_w$state<-NA
nc_w$state<-'North Carolina'
n<-length(nc_w$date)

nc_w[nc_w==-999]<-NA

nc_w$WS2M_1<-0
nc_w$T2M_1<-0
nc_w$T2MDEW_1<-0
nc_w$T2MWET_1<-0
nc_w$TS_1<-0
nc_w$T2M_RANGE_1<-0
nc_w$T2M_MAX_1<-0
nc_w$T2M_MIN_1<-0
nc_w$QV2M_1<-0
nc_w$RH2M_1<-0
nc_w$PRECTOTCORR_1<-0
nc_w$PS_1<-0
nc_w$WS10M_1<-0
nc_w$WS10M_MAX_1<-0
nc_w$WS10M_MIN_1<-0
nc_w$WS10M_RANGE_1<-0
nc_w$WD10MN_1<-0

for (i in 7:n) 
{
  nc_w$WS2M_1[i]<-mean(c(nc_w$WS2M[i],nc_w$WS2M[i-1],nc_w$WS2M[i-2],nc_w$WS2M[i-3],nc_w$WS2M[i-4],nc_w$WS2M[i-5],nc_w$WS2M[i-6]), na.rm=TRUE)
  nc_w$T2M_1[i]<-mean(c(nc_w$T2M[i],nc_w$T2M[i-1],nc_w$T2M[i-2],nc_w$T2M[i-3],nc_w$T2M[i-4],nc_w$T2M[i-5],nc_w$T2M[i-6]), na.rm=TRUE)
  nc_w$T2MDEW_1[i]<-mean(c(nc_w$T2MDEW[i],nc_w$T2MDEW[i-1],nc_w$T2MDEW[i-2],nc_w$T2MDEW[i-3],nc_w$T2MDEW[i-4],nc_w$T2MDEW[i-5],nc_w$T2MDEW[i-6]), na.rm=TRUE)
  nc_w$T2MWET_1[i]<-mean(c(nc_w$T2MWET[i],nc_w$T2MWET[i-1],nc_w$T2MWET[i-2],nc_w$T2MWET[i-3],nc_w$T2MWET[i-4],nc_w$T2MWET[i-5],nc_w$T2MWET[i-6]), na.rm=TRUE)
  nc_w$TS_1[i]<-mean(c(nc_w$TS[i],nc_w$TS[i-1],nc_w$TS[i-2],nc_w$TS[i-3],nc_w$TS[i-4],nc_w$TS[i-5],nc_w$TS[i-6]), na.rm=TRUE)
  nc_w$T2M_RANGE_1[i]<-mean(c(nc_w$T2M_RANGE[i],nc_w$T2M_RANGE[i-1],nc_w$T2M_RANGE[i-2],nc_w$T2M_RANGE[i-3],nc_w$T2M_RANGE[i-4],nc_w$T2M_RANGE[i-5],nc_w$T2M_RANGE[i-6]), na.rm=TRUE)
  nc_w$T2M_MAX_1[i]<-mean(c(nc_w$T2M_MAX[i],nc_w$T2M_MAX[i-1],nc_w$T2M_MAX[i-2],nc_w$T2M_MAX[i-3],nc_w$T2M_MAX[i-4],nc_w$T2M_MAX[i-5],nc_w$T2M_MAX[i-6]), na.rm=TRUE)
  nc_w$T2M_MIN_1[i]<-mean(c(nc_w$T2M_MIN[i],nc_w$T2M_MIN[i-1],nc_w$T2M_MIN[i-2],nc_w$T2M_MIN[i-3],nc_w$T2M_MIN[i-4],nc_w$T2M_MIN[i-5],nc_w$T2M_MIN[i-6]), na.rm=TRUE)
  nc_w$QV2M_1[i]<-mean(c(nc_w$QV2M[i],nc_w$QV2M[i-1],nc_w$QV2M[i-2],nc_w$QV2M[i-3],nc_w$QV2M[i-4],nc_w$QV2M[i-5],nc_w$QV2M[i-6]), na.rm=TRUE)
  nc_w$RH2M_1[i]<-mean(c(nc_w$RH2M[i],nc_w$RH2M[i-1],nc_w$RH2M[i-2],nc_w$RH2M[i-3],nc_w$RH2M[i-4],nc_w$RH2M[i-5],nc_w$RH2M[i-6]), na.rm=TRUE)
  nc_w$PRECTOTCORR_1[i]<-mean(c(nc_w$PRECTOTCORR[i],nc_w$PRECTOTCORR[i-1],nc_w$PRECTOTCORR[i-2],nc_w$PRECTOTCORR[i-3],nc_w$PRECTOTCORR[i-4],nc_w$PRECTOTCORR[i-5],nc_w$PRECTOTCORR[i-6]), na.rm=TRUE)
  nc_w$PS_1[i]<-mean(c(nc_w$PS[i],nc_w$PS[i-1],nc_w$PS[i-2],nc_w$PS[i-3],nc_w$PS[i-4],nc_w$PS[i-5],nc_w$PS[i-6]), na.rm=TRUE)
  nc_w$WS10M_1[i]<-mean(c(nc_w$WS10M[i],nc_w$WS10M[i-1],nc_w$WS10M[i-2],nc_w$WS10M[i-3],nc_w$WS10M[i-4],nc_w$WS10M[i-5],nc_w$WS10M[i-6]), na.rm=TRUE)
  nc_w$WS10M_MAX_1[i]<-mean(c(nc_w$WS10M_MAX[i],nc_w$WS10M_MAX[i-1],nc_w$WS10M_MAX[i-2],nc_w$WS10M_MAX[i-3],nc_w$WS10M_MAX[i-4],nc_w$WS10M_MAX[i-5],nc_w$WS10M_MAX[i-6]), na.rm=TRUE)
  nc_w$WS10M_MIN_1[i]<-mean(c(nc_w$WS10M_MIN[i],nc_w$WS10M_MIN[i-1],nc_w$WS10M_MIN[i-2],nc_w$WS10M_MIN[i-3],nc_w$WS10M_MIN[i-4],nc_w$WS10M_MIN[i-5],nc_w$WS10M_MIN[i-6]), na.rm=TRUE)
  nc_w$WS10M_RANGE_1[i]<-mean(c(nc_w$WS10M_RANGE[i],nc_w$WS10M_RANGE[i-1],nc_w$WS10M_RANGE[i-2],nc_w$WS10M_RANGE[i-3],nc_w$WS10M_RANGE[i-4],nc_w$WS10M_RANGE[i-5],nc_w$WS10M_RANGE[i-6]), na.rm=TRUE)
  nc_w$WD10MN_1[i]<-mean(c(nc_w$WD10M[i],nc_w$WD10M[i-1],nc_w$WD10M[i-2],nc_w$WD10M[i-3],nc_w$WD10M[i-4],nc_w$WD10M[i-5],nc_w$WD10M[i-6]), na.rm=TRUE)
}


#Stack all 13 states together

all_w<-rbind(ut_w, tn_w, or_w, ny_w, nm_w, mn_w, mi_w, md_w, ga_w, ct_w, co_w, ca_w, nc_w)


################Plot weather Data trend
all_w$newdate <- as.Date(all_w$date, format = "%m/%d/%Y")
tmp <- all_w[order(all_w$newdate), c("newdate", "WS2M_1",	"T2M_1","T2MDEW_1",	"T2MWET_1",	"TS_1",	"QV2M_1",	"RH2M_1",	"PRECTOTCORR_1",	"PS_1",	"WS10M_1",	"WD10MN_1")]



plot(tmp$newdate, tmp$T2M_1, type="l", xlab = "Year", ylab = "Temperature at 2 Meters", col="red", xaxt="n", ylim=c(-30, 40))
axis(1, 
     at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
     labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
     cex.axis = 0.5)
title("Termperature at 2 Meters Over Time")

jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure7_weather_boxplot.jpg", width = 800, height = 600)
par(mfrow=c(3,4))
boxplot(tmp$T2M_1, main="Temp at 2M")
boxplot(tmp$T2MDEW_1, main="Dew Temp at 2M")
boxplot(tmp$T2MWET_1, main="Wet bulb Temp")
boxplot(tmp$TS_1, main="Earth skin Temp")
boxplot(tmp$QV2M_1, main="Specific Humidity")
boxplot(tmp$RH2M_1, main="Relative Humidity")
boxplot(tmp$PRECTOTCORR_1, main="Precipitation")
boxplot(tmp$PS_1, main="Surface Pressure")
boxplot(tmp$WS2M_1,main="Wind Speed at 2M")
boxplot(tmp$WS10M_1, main="Wind speed at 10M")
boxplot(tmp$WD10MN_1, main="Wind direction at 10M")
dev.off()

jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure8_weather_hist.jpg", width = 800, height = 600)
par(mfrow=c(3,4))
hist(tmp$T2M_1, main="Temp at 2M")
hist(tmp$T2MDEW_1, main="Dew Temp at 2M")
hist(tmp$T2MWET_1, main="Web bulb Temp")
hist(tmp$TS_1, main="Earth skin Temp")
hist(tmp$QV2M_1, main="Specific Humidity")
hist(tmp$RH2M_1, main="Relative Humidity")
hist(tmp$PRECTOTCORR_1, main="Precipitation")
hist(tmp$PS_1, main="Surface Pressure")
hist(tmp$WS2M_1,main="Wind Speed at 2M")
hist(tmp$WS10M_1, main="Wind speed at 10M")
hist(tmp$WD10MN_1, main="Wind direction at 10M")
dev.off()

#Read in waste water RSV level data (it is weekly data for each state)
###############################################################################################################################

RSV_w<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Waste RSV level.csv", header=TRUE, sep=',')

RSV_w_1<-RSV_w[RSV_w$Data_Collection_Period== "All Results", ]

m<-merge(all_w, RSV_w_1, by.x = c("state", "date"), by.y = c("State.Territory", "Week_Ending_Date"), all =TRUE)

#Read in RSV hospitalization data (it is weekly data for 13 states)

RSV_h<-read.csv("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Weekly_Rates_RSV_Hospitalizations_20250728.csv", header=TRUE, sep=',')

m1<-merge(RSV_h, m, by.x=c("State", "Week.ending.date"), by.y=c("state", "date"), all.x=TRUE)
m1$newdate <- format(as.Date(m1$Week.ending.date, format = "%m/%d/%Y"), "%m/%d/%Y")

#Read in air pollutant data (weekly average for 13 states)
###############################################################################################
library(openxlsx)

air<-read.xlsx("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/combined_pollution_data_cbsa_3.xlsx", sheet=1)
air$state[air$cbsa_code == '35620'] <- 'New York'
air$state[air$cbsa_code == '12060'] <- 'Georgia'
air$state[air$cbsa_code == '37980'] <- 'Maryland'
air$state[air$cbsa_code == '35300'] <- 'Connecticut'
air$state[air$cbsa_code == '19820'] <- 'Michigan'
air$state[air$cbsa_code == '33460'] <- 'Minnesota'
air$state[air$cbsa_code == '28940'] <- 'Tennessee'
air$state[air$cbsa_code == '39580'] <- 'North Carolina'
air$state[air$cbsa_code == '40900'] <- 'California'
air$state[air$cbsa_code == '38900'] <- 'Oregon'
air$state[air$cbsa_code == '41620'] <- 'Utah'
air$state[air$cbsa_code == '10740'] <- 'New Mexico'
air$state[air$cbsa_code == '19740'] <- 'Colorado'
air$newdate <- format(as.Date(air$date_local), "%m/%d/%Y")


m2<-merge(m1, air, by.x=c("State", "newdate"), by.y=c("state", "newdate"), all.x =TRUE)
m2$newdate <- as.Date(m2$Week.ending.date, format = "%m/%d/%Y")
m2$year <- as.numeric(format(m2$newdate, "%Y"))
m2$month<-as.numeric(format(m2$newdate, "%m"))
m2$day<-as.numeric(format(m2$newdate, "%d"))
m2$WVAL<-m2$State.Territory_WVAL
#Drop columns that are not useful

m3<-m2[, !names(m2) %in% c("newdate","Season", "Cumulative.Rate","Type",	"YEAR", "MO", "DY",	"WS2M",	"T2M","T2MDEW",
                           "T2MWET",	"TS",	"T2M_RANGE",	"T2M_MAX",	"T2M_MIN",	"QV2M",	"RH2M",	"PRECTOTCORR",
                           "IMERG_PRECTOT",	"PS",	"WS10M",	"WS10M_MAX",	"WS10M_MIN",	"WS10M_RANGE",
                           "WD10M",	"T2M_RANGE_1",	"T2M_MAX_1",	"T2M_MIN_1",	"WS10M_MAX_1",	"WS10M_MIN_1",
                           "WS10M_RANGE_1",	"Data_Collection_Period",	"National_WVAL",
                           "Regional_WVAL",	"site_details_text",	"WVAL_Category",	"Coverage",	"date_updated",
                           "X1",	"cbsa_code",	"Lead", "CO_max",	"Lead_max",	"NO2_max",	"Ozone_max",	"PM10_max",
                           "PM2.5_max",	"SO2_max",	"date_local",	"weekday", "State.Territory_WVAL")]

#drop _1 in varaible names
names(m3) <- sub("_1$", "", names(m3))

#Final combined data set
write.csv(m3 , "C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/m3.csv")

#Check negative values
numeric_cols <- sapply(m3, is.numeric)
# Compute min for each numeric column
sapply(m3[, numeric_cols], min, na.rm = TRUE)

#CO. NO2 and SO2 have negative values

