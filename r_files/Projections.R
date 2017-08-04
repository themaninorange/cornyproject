#Taking everything but the last 2 and projecting them.

newlength = length(vdiff_excellent[-yearend]) - 2
vset_excellent = vdiff_excellent[-yearend][1:newlength]
vset_good = vdiff_good[-yearend][1:newlength]
vset_fair = vdiff_fair[-yearend][1:newlength]
vset_poor = vdiff_poor[-yearend][1:newlength]
vset_verypoor = vdiff_verypoor[-yearend][1:newlength]

#Excellent
acfdata_excellent = acf(vset_excellent[-yearend],plot = FALSE)
plot(acfdata_excellent)
pacf(vdiff_excellent[-yearend])
BIC_stats = armasubsets(vset_excellent[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Good
acfdata_good = acf(vset_good[-yearend],plot = FALSE)
plot(acfdata_good)
pacf(vdiff_good[-yearend])
BIC_stats = armasubsets(vset_good[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Fair
acfdata_fair = acf(vset_fair[-yearend],plot = FALSE)
plot(acfdata_fair)
pacf(vdiff_fair[-yearend])
BIC_stats = armasubsets(vset_fair[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,1)

#Poor
acfdata_poor = acf(vset_poor[-yearend],plot = FALSE)
plot(acfdata_poor)
pacf(vdiff_poor[-yearend])
BIC_stats = armasubsets(vset_poor[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(2,1,2)

#Verypoor
acfdata_verypoor = acf(vset_verypoor[-yearend],plot = FALSE)
plot(acfdata_verypoor)
pacf(vdiff_verypoor[-yearend])
BIC_stats = armasubsets(vset_verypoor[-yearend], nar = 12, nma = 12, ar.method = 'ols')
plot(BIC_stats)
#(3,1,2)

#Fit 5 arima models.  Compare BIC.
qfitarima = list()
qbic = 1:1

qfitarima[[1]] =Arima(vset_excellent, order=c(2,1,2))
res = residuals(qfitarima[[1]])
tsdisplay(res, main= "Excellent")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[1] = qfitarima[[1]]$bic

qfitarima[[2]] =Arima(vset_good, order=c(2,1,2))
res = residuals(qfitarima[[2]])
tsdisplay(res, main = "Good")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[2] = qfitarima[[2]]$bic

qfitarima[[3]] =Arima(vset_fair, order=c(2,1,1))
res = residuals(qfitarima[[3]])
tsdisplay(res, main = "Fair")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[3] = qfitarima[[3]]$bic

qfitarima[[4]] =Arima(vset_poor, order=c(2,1,2))
res = residuals(qfitarima[[4]])
tsdisplay(res, main = "Poor")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[4] = qfitarima[[4]]$bic

qfitarima[[5]] =Arima(vset_verypoor, order=c(3,1,2))
res = residuals(qfitarima[[5]])
tsdisplay(res, main = "Very Poor")
Box.test(res, lag=16, fitdf=4, type="Ljung")
qbic[5] = qfitarima[[5]]$bic




qsbic= 1:5
qsfitarima = list()
qsfitarima[[1]] = sarima(xdata = vset_excellent,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
qsbic[1] = qsfitarima[[1]]$BIC
qsfitarima[[2]] = sarima(xdata = vset_good,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
qsbic[2] = qsfitarima[[2]]$BIC
qsfitarima[[3]] = sarima(xdata = vset_fair,p = 2,d = 1,q = 1,P = 1,D = 0,Q = 0,S = 0)
qsbic[3] = qsfitarima[[3]]$BIC
qsfitarima[[4]] = sarima(xdata = vset_poor,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
qsbic[4] = qsfitarima[[4]]$BIC
qsfitarima[[5]] = sarima(xdata = vset_verypoor,p = 3,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
qsbic[5] = qsfitarima[[5]]$BIC

sarima.for(xdata = vset_excellent,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
sarima.for(xdata = vset_good,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
sarima.for(xdata = vset_fair,n.ahead = 2,p = 2,d = 1,q = 1,P = 1,D = 0,Q = 0,S = 0)
sarima.for(xdata = vset_poor,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)
sarima.for(xdata = vset_verypoor,n.ahead = 2,p = 3,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)

predex = sarima.for(xdata = vset_excellent,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)$pred
predgd = sarima.for(xdata = vset_good,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)$pred
predfr = sarima.for(xdata = vset_fair,n.ahead = 2,p = 2,d = 1,q = 1,P = 1,D = 0,Q = 0,S = 0)$pred
predpr = sarima.for(xdata = vset_poor,n.ahead = 2,p = 2,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)$pred
predvp = sarima.for(xdata = vset_verypoor,n.ahead = 2,p = 3,d = 1,q = 2,P = 1,D = 0,Q = 0,S = 0)$pred


actualex = vdiff_excellent[-yearend][-(1:newlength)]
actualgd = vdiff_good[-yearend][-(1:newlength)]
actualfr = vdiff_fair[-yearend][-(1:newlength)]
actualpr = vdiff_poor[-yearend][-(1:newlength)]
actualvp = vdiff_verypoor[-yearend][-(1:newlength)]

errex = abs(predex - actualex)[1:2]
errgd = abs(predgd - actualgd)[1:2]
errfr = abs(predfr - actualfr)[1:2]
errpr = abs(predpr - actualpr)[1:2]
errvp = abs(predvp - actualvp)[1:2]

rmse_ex = sqrt(mean(errex^2))
rmse_gd = sqrt(mean(errgd^2))
rmse_fr = sqrt(mean(errfr^2))
rmse_pr = sqrt(mean(errpr^2))
rmse_vp = sqrt(mean(errvp^2))


qbic

forecast = predict(object = qfitarima[[1]], n.ahead = 2, newxreg = NULL, se.fit = TRUE)

###
###
###