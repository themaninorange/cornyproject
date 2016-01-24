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

#arumasubsets
fitarmasubset = armasubsets(vsumweek_adj,nar = 5,nma = 5)
plot(fitarmasubse)

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

for(i in (1:16)[vec1>=21]){
  qfitarima_year[[i]] = list()
  qbic_year[[i]] = 1:1
  fitarima_year[[i]] = Arima(diff(sumbymod(weightedquality(x_year[[i]])*v_year[[i]],5)), order = c(2,1,2))
  bic_year[i] = fitarima_year[[i]]$bic
  
  #  qfitarima_year[[i]][[1]] = Arima(diff(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT VERY POOR"]), order = c(2,2,2))
  qfitarima_year[[i]][[2]] = Arima(diff(v_year[[i]][1:(length(v_year)-5)][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT POOR"]), order = c(2,1,2))
  qfitarima_year[[i]][[3]] = Arima(diff(v_year[[i]][1:(length(v_year)-5)][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT FAIR"]), order = c(2,1,2))
  qfitarima_year[[i]][[4]] = Arima(diff(v_year[[i]][1:(length(v_year)-5)][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT GOOD"]), order = c(2,1,2))
  qfitarima_year[[i]][[5]] = Arima(diff(v_year[[i]][1:(length(v_year)-5)][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT EXCELLENT"]), order = c(2,1,2))
  
  #   qbic_year[[i]][1] = qfitarima_year[[i]][[1]]$bic
  qbic_year[[i]][2] = qfitarima_year[[i]][[2]]$bic
  qbic_year[[i]][3] = qfitarima_year[[i]][[3]]$bic
  qbic_year[[i]][4] = qfitarima_year[[i]][[4]]$bic
  qbic_year[[i]][5] = qfitarima_year[[i]][[5]]$bic
}

#####
#After removing discontinuities
#####

plot(vdiff_excellent[-yearend])
plot(vdiff_good[-yearend])
plot(vdiff_fair[-yearend])
plot(vdiff_poor[-yearend])
plot(vdiff_verypoor[-yearend])

#Excellent
acfdata_excellent = acf(vdiff_excellent[-yearend],plot = FALSE)
plot(acfdata_excellent)
pacf(vdiff_excellent[-yearend])
BIC_stats = armasubsets(vdiff_excellent[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Good
acfdata_good = acf(vdiff_good[-yearend],plot = FALSE)
plot(acfdata_good)
pacf(vdiff_good[-yearend])
BIC_stats = armasubsets(vdiff_good[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Fair
acfdata_fair = acf(vdiff_fair[-yearend],plot = FALSE)
plot(acfdata_fair)
pacf(vdiff_fair[-yearend])
BIC_stats = armasubsets(vdiff_fair[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,1)

#Poor
acfdata_poor = acf(vdiff_poor[-yearend],plot = FALSE)
plot(acfdata_poor)
pacf(vdiff_poor[-yearend])
BIC_stats = armasubsets(vdiff_poor[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Verypoor
acfdata_verypoor = acf(vdiff_verypoor[-yearend],plot = FALSE)
plot(acfdata_verypoor)
pacf(vdiff_verypoor[-yearend])
BIC_stats = armasubsets(vdiff_verypoor[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(3,1,2)

#Fit 5 arima models.  Compare BIC.
qfitarima = list()
qbic = 1:1

qfitarima[[1]] =Arima(vdiff_excellent, order=c(2,1,2))
res = residuals(qfitarima[[1]])
tsdisplay(res, main= "Excellent")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[1] = qfitarima[[1]]$bic

qfitarima[[2]] =Arima(vdiff_good, order=c(2,1,2))
res = residuals(qfitarima[[2]])
tsdisplay(res, main = "Good")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[2] = qfitarima[[2]]$bic

qfitarima[[3]] =Arima(vdiff_fair, order=c(2,1,1))
res = residuals(qfitarima[[3]])
tsdisplay(res, main = "Fair")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[3] = qfitarima[[3]]$bic

qfitarima[[4]] =Arima(vdiff_poor, order=c(2,1,2))
res = residuals(qfitarima[[4]])
tsdisplay(res, main = "Poor")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[4] = qfitarima[[4]]$bic

qfitarima[[5]] =Arima(vdiff_verypoor, order=c(3,1,2))
res = residuals(qfitarima[[5]])
tsdisplay(res, main = "Very Poor")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[5] = qfitarima[[5]]$bic

qbic

###
###
###