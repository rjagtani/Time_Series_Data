library(forecast)
library(sarima)
library(astsa)
library(aTSA)
library(ML)
library(lubridate)
setwd('C:\\Users\\Rohit Jagtani\\Desktop\\Quick_Sarima')
ci=read.csv('coal_index_csv.csv',stringsAsFactors = F)
ci$RB1=NULL
newc=ci[,1:2]
newc$Period=as.Date(newc$Period,format='%m/%d/%Y')
newc=subset(newc,!is.na(NEWC))
newc_ts=ts(newc$NEWC,freq=365.25/7,start=decimal_date(ymd("2002-07-05")))
ptrain=subset(newc_ts,end=580)
test=subset(newc_ts,start=581)

#########

plot(newc_ts)
acf2(newc_ts,max.lag=120)
stationary.test(newc_ts)


#### First Difference Time Series

newc_diff=diff(newc_ts)
plot(newc_diff)
acf2(newc_diff)
stationary.test(newc_diff)

### Model Specification

p=c(0,1,22,24)
q=c(0,1,2,9,14,23)
results1=data.frame()
for(i in 1:length(p))
{
  results=data.frame()
  for(j in 1:length(q))
  {
    ar_temp=sarima(newc_ts,p[i],1,q[j])
    results[j,'p']=p[i]
    results[j,'q']=q[j]
    results[j,'AIC']=ar_temp$AIC
    results[j,'BIC']=ar_temp$BIC
    results[j,'AICc']=ar_temp$AICc
  }
  results1=rbind(results1,results)
  print(paste0(p[i],'_',q[j]))
}

write.csv(results1,'newc_grid_search_results.csv',row.names = F)

best_arima=sarima(newc_ts,1,1,0)
ts.plot(newc_ts-best_arima$fit$residuals,col=c(6,1))
predictions=cbind.data.frame(newc_ts,best_arima$fit$residuals)
predictions$predicted=predictions$newc_ts-predictions$`best_arima$fit$residuals`
ts.plot(predictions$newc_ts,predictions$predicted,col=c(5,2))


################

pred_test=numeric()
for(k in 569:714)
{
  train=subset(newc_ts,end=k)
  #newc_arima = sarima(train,1,1,0)
  temp = sarima.for(train,n.ahead=12,p=1,d=1,q=0)$pred[12]
  pred_test=c(pred_test,temp)
  print(k)
}

actual_test=test
test_pred=cbind.data.frame(actual_test,pred_test)
test_date=newc$Period[581:726]
test_pred=cbind(test_date,test_pred)
ts.plot(test_pred$actual_test,test_pred$pred_test,col=c(5,2))
#legend(x=20,y=9.5,legend=c('a','b'),lty=c(1,1),lwd=c(2.5,2.5),col=c('blue','red'))
#legend("topright", legend=colnames(ts)[2:3], lty=1, col=c(5,2))
legend("topright", legend=colnames(test_pred)[2:3], lty=1, col=c(5,2))
#%>% legend(legend=1:2,col=1:2)
test_num=test[1:146]
accuracy(f=pred_test,x=test_num)
write.csv(test_pred,'newc_12w_110.csv',row.names = F)

#sarima.for(newc_ts,n.ahead=2,p=1,d=1,q=0)


#################### Original Time Series


plot(newc_ts)
acf2(train,max.lag=60)
stationary.test(newc_ts)


#### First Difference Time Series

newc_diff=diff(newc_ts)
plot(newc_diff)
acf2(newc_diff)
stationary.test(newc_diff)
newc_arima1 <- auto.arima(newc_diff,stepwise = F)
summary(newc_arima1)


decompose_newc = decompose(newc_ts, "additive")
plot(as.ts(decompose_newc$seasonal))
plot(as.ts(decompose_newc$trend))
plot(as.ts(decompose_newc$random))
plot(decompose_newc)
#newc_adj=newc_ts-decompose_newc$seasonal
#newc_arima1 <- auto.arima(newc_adj,stepwise = F)
#newc_arima2=sarima(newc_adj,1,1,0,0,0,1,52)
#decompose_newc()


# stl_newc=stl(newc_ts,s.window='periodic')
# stl_newc$outer
# plot(newc_ts, main=" ")
# lines(ts(test))
# 
# 
# fit1 <- stlf(newc_diff)
# fit2 <- stlf(newc_diff, model="ANN")
# 
# deviance <- 2*c(logLik(fit1) - logLik(fit2))
# df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
# #P value
# 1-pchisq(deviance,df)