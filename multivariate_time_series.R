library(xts)
library(forecast)
library(dplyr)
library(smbinning)

#setwd('/mnt2/client22/rjagtani22/time_series_analysis')
api4=read.csv('/mnt2/client22/rjagtani22/Data_Download/Dependent Variables/API4.csv',stringsAsFactors = F)
forex_data=read.csv('Forex_Quandl_Daily.csv',stringsAsFactors = F)
forex_data_sa=forex_data[,c(1,grep(colnames(forex_data),pattern='ZAR'))]
weather_data=read.csv('Weather_Ncdc_Cities_Daily.csv',stringsAsFactors = F)
weather_sa=weather_data[,c(1,2,3,26,27)]
setwd('/mnt2/client22/rjagtani22/Data_Download/Stock Prices')
stocks=read.csv("StockIndex_Futures_Daily_v1.csv",stringsAsFactors = F)
stock_sa=stocks[,c(1,3,4,5,7,11,12,16,24,22,20)]
colnames(stock_sa)[1]='Date'
colnames(weather_sa)[1]='Date'
colnames(forex_data_sa)[1]='Date'
colnames(api4)[1]='Date'
weather_sa$Date=as.Date(weather_sa$Date,format='%d-%m-%Y')
stock_sa$Date=as.Date(stock_sa$Date,format='%m/%d/%Y')
forex_data_sa$Date=as.Date(forex_data_sa$Date,format='%d/%m/%Y')
api4$Date=as.Date(api4$Date,format='%m/%d/%Y')
api4=merge(api4,stock_sa,by='Date',all.x=T)
api4=merge(api4,weather_sa,by='Date',all.x=T)
api4=merge(api4,forex_data_sa,by='Date',all.x=T)
class_type_api4=sapply(api4,class)
char_class=names(class_type_api4[class_type_api4=='character'])
change2na=function(x){
  x=ifelse(x=="#N/A",NA,x)
}
nullchange2na=function(x){
  x=ifelse(x=="null",NA,x)
}
api4[,char_class]=lapply(api4[,char_class],change2na)
api4[,char_class]=lapply(api4[,char_class],nullchange2na)
api4[,char_class]=lapply(api4[,char_class],as.numeric)
api4=arrange(api4,desc(Date))
api4_summ=smbinning.eda(api4)$eda
api4_summ=arrange(api4_summ,Miss)
selected_cols=as.character(api4_summ$Field[1:6])




#######

api4_v1=api4[,selected_cols]
#api4_v1=subset(api4_v1,!is.na(api4_v1$API4))
api4_v1[,c(2:6)]=lapply(api4_v1[,c(2:6)],na.interp)
api4_summ_v1=smbinning.eda(api4_v1)$eda
library(lubridate)
api4_ts=ts(api4_v1,freq=261,start=decimal_date(ymd("2001-08-23")))
api4_ts=api4_ts[,-1]
plot(api4_ts)
#api4_xts=as.xts(api4_ts)


#### Stationarity Test for each variable

for(i in 1:5)
{
  print(colnames(api4_ts)[i])
  acf2(api4_ts[,i],max.lag=120)
  stationary.test(api4_ts[,i])
}




