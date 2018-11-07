library(xts)
library(forecast)
library(dplyr)
library(smbinning)
library(astsa)
library(aTSA)
library(TSA)
setwd('C:\\Users\\Rohit Jagtani\\Desktop\\Coal_forecasting_external_data\\time_series_analysis')
api4=read.csv('API4final_data.csv',stringsAsFactors = F)
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


#######

api4_v1=api4[,c(1,2)]
api4_v1=subset(api4_v1,!is.na(api4_v1$API4))
library(lubridate)
api4_ts=ts(api4_v1$API4,freq=261,start=decimal_date(ymd("2001-08-23")))
plot(api4_ts)

#ptrain=subset(api4_ts,end=580)
#test=subset(api4_ts,start=581)

#########

plot(api4_ts)
acf2(api4_ts,max.lag=120)
stationary.test(api4_ts)


#### First Difference Time Series

api4_diff=diff(api4_ts)
plot(api4_diff)
acf2(api4_diff)
stationary.test(api4_diff)

### Model Specification

p=c(0,1,22,24)
q=c(0,1,2,9,14,23)
results1=data.frame()
for(i in 1:length(p))
{
  results=data.frame()
  for(j in 1:length(q))
  {
    ar_temp=sarima(api4_ts,p[i],1,q[j])
    results[j,'p']=p[i]
    results[j,'q']=q[j]
    results[j,'AIC']=ar_temp$AIC
    results[j,'BIC']=ar_temp$BIC
    results[j,'AICc']=ar_temp$AICc
  }
  results1=rbind(results1,results)
  print(paste0(p[i],'_',q[j]))
}

write.csv(results1,'api4_grid_search_results.csv',row.names = F)

best_arima=sarima(api4_ts,2,1,2)
ts.plot(api4_ts-best_arima$fit$residuals,col=c(6,1))
predictions=cbind.data.frame(api4_ts,best_arima$fit$residuals)
predictions$predicted=predictions$api4_ts-predictions$`best_arima$fit$residuals`
ts.plot(predictions$api4_ts,predictions$predicted,col=c(5,2))
stationary.test(best_arima$fit$residuals)

#################### Increasing Window 

pred_test=numeric()
for(k in 3785:3973)
{
  train=subset(api4_ts,end=k)
  #api4_arima = sarima(train,1,1,0)
  temp = sarima.for(train,n.ahead=15,p=2,d=1,q=2)$pred[15]
  pred_test=c(pred_test,temp)
  print(k)
}

actual_test=subset(api4_ts,start=3800)
test_pred=cbind.data.frame(actual_test,pred_test)
test_date=api4_v1$Date[1:189]
test_date=sort(test_date)
test_pred=cbind(test_date,test_pred)
ts.plot(test_pred$actual_test,test_pred$pred_test,col=c(5,2))
#legend(x=20,y=9.5,legend=c('a','b'),lty=c(1,1),lwd=c(2.5,2.5),col=c('blue','red'))
#legend("topright", legend=colnames(ts)[2:3], lty=1, col=c(5,2))
legend("topright", legend=colnames(test_pred)[2:3], lty=1, col=c(5,2))
#%>% legend(legend=1:2,col=1:2)
test_num=actual_test[1:189]
accuracy(f=pred_test,x=test_num)
write.csv(test_pred,'api4_12w_110.csv',row.names = F)


### ARIMA (2,1,2) - Benchmark accuracy for 15 days ahead forecast - last 6 months


#ME     RMSE      MAE      MPE     MAPE
#Test set 0.5294948 1.827373 1.361194 1.693678 5.429399

#sarima.for(api4_ts,n.ahead=2,p=1,d=1,q=0)


######################################## Original Time Series


plot(api4_ts)
acf2(train,max.lag=60)
stationary.test(api4_ts)


####################################### First Difference Time Series

api4_diff=diff(api4_ts)
plot(api4_diff)
acf2(api4_diff)
stationary.test(api4_diff)
api4_arima1 <- auto.arima(api4_ts,stepwise = T)
summary(api4_arima1)


####### ARIMA after deseasonalizing  #########################

decompose_api4 = decompose(api4_ts, "additive")
plot(as.ts(decompose_api4$seasonal))
plot(as.ts(decompose_api4$trend))
plot(as.ts(decompose_api4$random))
plot(decompose_api4)
api4_adj=api4_ts-decompose_api4$seasonal
api4_arima2 <- auto.arima(api4_adj,stepwise = T)
#api4_arima2=sarima(api4_adj,1,1,0,0,0,1,52)
#decompose_api4()


####################################################################

summary(stl_api4)

#################################


