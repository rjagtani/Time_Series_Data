
##########################  Seasonality Module ########################################################

p = periodogram(api4_ts[,1])
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

# display the 2 highest "power" frequencies
top2




#######################   Deseasonalize a variable ##############################


ts.stl <- stl(api4_ts[,1],"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl) 
plot(api4_ts[,1])
plot(ts.sa)
stationary.test(ts.sa)


###############

nsdiffs(api4_ts[,1])
ndiffs(api4_ts[,1])


####### 


ccf2(ts.sa,diff(api4_ts[,3]),max.lag = 70)

