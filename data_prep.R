##### Working with new API file 


## Step 1 - Data Preparation

library(xts)
library(forecast)
library(dplyr)
library(smbinning)
library(astsa)
library(aTSA)
library(TSA)
library(lubridate)
library(ggplot2)
library(lmtest)
library(vars)
#library(plotly)
setwd('C:\\Users\\Rohit Jagtani\\Desktop\\Coal_forecasting_external_data\\time_series_analysis')
api4=read.csv('API4_Daily_Final.csv',stringsAsFactors = F)

### Removing all columns with yearly granularity

api4$DATE=as.Date(api4$DATE,format='%d-%m-%Y')
api4[,grep(pattern = 'Year',colnames(api4))]=NULL
api4_summ=as.data.frame(smbinning.eda(api4))
class_type_api4=sapply(api4,class)

### Checking for numeric values

char_class=names(class_type_api4[class_type_api4=='character'])

#### Converting character columns to numeric


# change2na=function(x){
#   x=ifelse(x=="#N/A",NA,x)
# }
# nullchange2na=function(x){
#   x=ifelse(x=="null",NA,x)
# }
# #api4[,char_class]=lapply(api4[,char_class],change2na)
# api4[,char_class]=lapply(api4[,char_class],nullchange2na)
#api4[,char_class]=lapply(api4[,char_class],as.numeric)
#api4_summ_v1=as.data.frame(smbinning.eda(api4))

#### Eliminate variables with more than 700 missing values (more than 2 years of missing data)

eliminate_vars=as.character(api4_summ$eda.Field[api4_summ$eda.Miss>=3000])
api4[,eliminate_vars]=NULL


#######

api4_summ_v2=as.data.frame(smbinning.eda(api4))


### Shortlisting variables based on study of SA coal market


#write.csv(api4_summ_v2,'Variables_considered.csv',row.names = F)


#### Selecting shortlisted variables


shortlisted_vars_df=read.csv('Variables_considered.csv',stringsAsFactors = F)
shortlisted_vars=as.character(subset(shortlisted_vars_df,shortlisted_vars_df$Shortlisted==1)$eda.Field)
shortlisted_vars[c(57,59)]=c('Commodity_Quandl_Price_EuropeBrent_Spot_FOB_Crude_Oil_Daily','Commodity_Quandl_HenryHubNaturalGasSpot_Price_Daily')
shortlisted_vars=shortlisted_vars[!grepl(pattern='Year',shortlisted_vars)]
api4=api4[,shortlisted_vars]
#api4[,grep(pattern='electricity_gen',colnames(api4))]=NULL
#api4[,grep(pattern='co2',colnames(api4))]=NULL

#### Removing NA values of API4 - dependent variable

api4=subset(api4,!is.na(API4))
api4_summ_v3=as.data.frame(smbinning.eda(api4))
missing_vars=as.character(api4_summ_v3$eda.Field[api4_summ_v3$eda.Miss>0])
api4=arrange(api4,DATE)


date_range_cols=as.character(api4_summ_v3$eda.Field)
api4_range=data.frame()
for(i in 1:length(date_range_cols))
{
  api4_temp=subset(api4[,c('DATE',date_range_cols[i])],!is.na(get(date_range_cols[i])))
  api4_range[i,'column']=date_range_cols[i]
  api4_range[i,'min_date']=min(api4_temp$DATE,na.rm=T)
  api4_range[i,'max_date']=max(api4_temp$DATE,na.rm=T)
}

#### API4
api4[,c('macro_fred_GDP_Russia_Quarterly','Weather_Ncdc_TempMax_PortElizabeth_SouthAfrica_Daily')]=NULL
api4[,c(4:8)]=NULL
api4_copy=api4
#api4=api4_copy
api4[,missing_vars]=lapply(api4[,missing_vars],na.interp)
api4_summ_v4=as.data.frame(smbinning.eda(api4))
api4_ts=ts(api4,freq=261,start=decimal_date(ymd("2001-08-23")))
api4_ts=api4_ts[,2:40]
summary(api4_ts)
#save(api4_ts,file='api4_ts.Rda')
load(file='api4_ts.Rda')

####################### Bivariate Plots - Dependent Variable

api4=api4[,c(1,26,2:25,27:40)]
setwd('C:\\Users\\Rohit Jagtani\\Desktop\\Coal_forecasting_external_data\\time_series_analysis\\Bivariate_Plots')
library(tidyr)
for(i in 3:40)
{
forex_m <- api4[,c(1,2,i)] %>%  gather(variable, value, -DATE)
a=basicplot <- ggplot(forex_m, aes(x = DATE, y = value, colour = variable)) + labs(x = "", colour = "",y='') + theme_minimal()
a=a + geom_line() + facet_wrap(~variable, ncol = 1,scales='free_y') + theme(legend.position="bottom")
ggsave(a,filename = paste0(colnames(api4)[i],'.jpeg'),height=7.5,width = 10)
print(i)
}

########################

diff_df=data.frame()
for(i in 1:ncol(api4_ts))
{
diff_df[i,'column']=colnames(api4_ts)[i]
diff_df[i,'diff_order_adf']=ndiffs(api4_ts[,i],test='adf',max.d = 2000)
diff_df[i,'diff_order_kpss']=ndiffs(api4_ts[,i],test='kpss',max.d = 2000)
diff_df[i,'diff_order_pp']=ndiffs(api4_ts[,i],test='pp',max.d = 2000)
#diff_df[i,'seasonal_diff_order_ch']=nsdiffs(api4_ts[,i],test='ch')
#diff_df[i,'seasonal_diff_order_hegy']=nsdiffs(api4_ts[,i],test='hegy')
diff_df[i,'seasonal_diff_order_ocsb']=nsdiffs(api4_ts[,i],test='ocsb',max.D = 4500)
diff_df[i,'seasonal_diff_order_seas']=nsdiffs(api4_ts[,i],test='seas',max.D = 4500)
p = periodogram(api4_ts[,i])
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 3)
time = 1/top2$f
diff_df[i,'top_freq_1']=time[1]
diff_df[i,'top_freq_2']=time[2]
diff_df[i,'top_freq_3']=time[3]
print(i)
}


#################### Cross-Correlation Analysis

j=14
max_lag=180
cc_test=ccf(x=diff(api4_ts[,j],1),y=diff(api4_ts[,1],1),lag.max=max_lag)
seq_gen=(-1*max_lag):max_lag
for(i in 1:(2*max_lag+1))
{
cc_test$lag[i,,]=seq_gen[i]
}
plot(cc_test)


################################

cc_test=ccf2(x=diff(api4_ts[,10]),y=diff(api4_ts[,1]),max.lag = 600)

best.arima=auto.arima(y=api4_ts[,22])
summary(best.arima)
sarima(api4_ts[,22],p=3,d=1,q=3)



max_lag=100
acf_test=acf(diff(api4_ts[,22]),lag.max = max_lag)
for(i in 1:max_lag)
{
  acf_test$lag[i,,]=i
}
plot(acf_test)

pacf_test=pacf(diff(api4_ts[,22]),lag.max = max_lag)
for(i in 1:max_lag)
{
  pacf_test$lag[i,,]=i
}
plot(pacf_test)


##### Find correlations that stand out 


### Steps for cross correlation

# 1. Check CCF of original data and see if anything meaningful can be inferred from the ccf plot and values
# 2. Remove any trend or pattern by differencing or detrending - check again
# 3. Run ARIMA on X variable. Remove any signal from X variable by taking residuals of the ARIMA regression.
# Filter y variable using the same regression coeffecients to maintain uniformity. Then calculate ccf of the xres
# yres to get significanct lags. Make inferences from the plot and then proceed with original variables.



####### Granger Causality Test

grangertest(x=as.numeric(diff(api4_ts[,25],1)),y=as.numeric(diff(api4_ts[,1],1)),order=1)


#1) Using Standard Models

#2) Using VAR Models

#3) Using VAR Models with correction

#4) Using Non-Linear VAR models
  
#### ARIMAX Functions 

# DO I need to make the regressors stationary ? 

# Do I need to fit ARIMA to my errors ? 

# What is the role of transfer functions ? 

# How is it different from ADL ? 


#################### VAR Model 

## When is VAR Used ? How does VAR solve the problem faced by other standard methods such as ARIMAX ? 

## How do I identify endogenous and exogenous regressors ? 

## Do I need to make my regressors stationary and non-seasonal ? 

################### BVAR



################## RNN/LSTM/GRU




#################
