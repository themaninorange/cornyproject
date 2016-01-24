#Analyses
#Take a look at the data, and set x_adj, v, and y.  y is irrelevant.
plot(US_corn_yield_by_year$Value)
y = US_corn_yield_by_year$Value
x_adj = weightedquality(USnationalcornconditionbyweek$Data.Item)
v = USnationalcornconditionbyweek$Value
acf(weightedquality(x_adj))

#Sum the values weighted by the field conditions.
vsumweek_adj = vsumweek_adj[length(vsumweek_adj):1]
plot(vsumweek_adj)
plot(diff(vsumweek_adj))
acfdata = acf(diff(vsumweek_adj),plot = FALSE)

#Determine order of MA
plot(acfdata)
#MA is 2nd order

#Determine order of AR
pacf(vsumweek_adj)
#AR is 1st or 2nd order

BIC_stats = armasubsets(newstufftime, nar=12, nma=12, ar.method='ols')
plot(bic_stats)

#Fit 2 arima models.  Compare BIC.
fitarima = list()
bic = 1:1

fitarima[[1]] =Arima(vsumweek_adj, order=c(1,1,2))
res = residuals(fitarima[[1]])
tsdisplay(res)
Box.test(res, lag=16, fitdf=4, type="Ljung")
bic[1] = fitarima[[1]]$bic

fitarima[[2]] =Arima(vsumweek_adj, order=c(2,1,2))
res = residuals(fitarima[[2]])
tsdisplay(res)
Box.test(res, lag=16, fitdf=4, type="Ljung")
bic[2] = fitarima[[2]]$bic

#Fit 16 arima models (one for each year).  Compare BIC.
fitarima_year = list()
qfitarima_year = list()
bic_year = 1:1
qbic_year = list()

for(i in 1:16){
  qfitarima_year[[i]] = list()
  qbic_year[[i]] = 1:1
  fitarima_year[[i]] = Arima(diff(sumbymod(weightedquality(x_year[[i]])*v_year[[i]],5)), order = c(2,1,2))
  bic_year[i] = fitarima_year[[i]]$bic
  
  #  qfitarima_year[[i]][[1]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT VERY POOR"]), order = c(2,2,2))
  qfitarima_year[[i]][[2]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT POOR"]), order = c(2,1,2))
  qfitarima_year[[i]][[3]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT FAIR"]), order = c(2,1,2))
  qfitarima_year[[i]][[4]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT GOOD"]), order = c(2,1,2))
  qfitarima_year[[i]][[5]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT EXCELLENT"]), order = c(2,1,2))
  
  #   qbic_year[[i]][1] = qfitarima_year[[i]][[1]]$bic
  qbic_year[[i]][2] = qfitarima_year[[i]][[2]]$bic
  qbic_year[[i]][3] = qfitarima_year[[i]][[3]]$bic
  qbic_year[[i]][4] = qfitarima_year[[i]][[4]]$bic
  qbic_year[[i]][5] = qfitarima_year[[i]][[5]]$bic
}
bic_year

###
###
###