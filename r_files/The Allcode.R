library(tseries)
library(TSA)
library(rdatamarket)
library(forecast)
library(astsa)
library(e1071)

US_corn_yield_by_year = read.csv("~/Dev/r_projects/Data Mining Final/US_corn_yield_by_year")
USnationalcornconditionbyweek = read.csv("~/Dev/r_projects/Data Mining Final/USnationalcornconditionbyweek")

stverypoor="CORN - CONDITION, MEASURED IN PCT VERY POOR"
stpoor="CORN - CONDITION, MEASURED IN PCT POOR" 
stfair="CORN - CONDITION, MEASURED IN PCT FAIR"
stgood="CORN - CONDITION, MEASURED IN PCT GOOD"
stexcellent="CORN - CONDITION, MEASURED IN PCT EXCELLENT"

excellent = USnationalcornconditionbyweek$Value[USnationalcornconditionbyweek$Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT"]
good = USnationalcornconditionbyweek$Value[USnationalcornconditionbyweek$Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD"]
fair= USnationalcornconditionbyweek$Value[USnationalcornconditionbyweek$Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR"]
poor= USnationalcornconditionbyweek$Value[USnationalcornconditionbyweek$Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR"]
verypoor= USnationalcornconditionbyweek$Value[USnationalcornconditionbyweek$Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR"]

y = US_corn_yield_by_year$Value
x = USnationalcornconditionbyweek$Data.Item
v = USnationalcornconditionbyweek$Value
vsumweek = sumbymod(weightedquality(x)*v,5)

y_adj = US_corn_yield_by_year$Value
x_adj = USnationalcornconditionbyweek$Data.Item
v_adj = USnationalcornconditionbyweek$Value
vsumweek_adj = sumbymod(weightedquality(x)*v,5)



#################

x_adj = USnationalcornconditionbyweek$Data.Item
v_adj = USnationalcornconditionbyweek$Value
plot(v_adj)

x_adj[1:110] = USnationalcornconditionbyweek$Data.Item[110:1]
x_adj[111:220] = USnationalcornconditionbyweek$Data.Item[220:111]
x_adj[221:320] = USnationalcornconditionbyweek$Data.Item[320:221]
x_adj[321:420] = USnationalcornconditionbyweek$Data.Item[420:321]
x_adj[421:530] = USnationalcornconditionbyweek$Data.Item[530:421]
x_adj[531:640] = USnationalcornconditionbyweek$Data.Item[640:531]
x_adj[641:765] = USnationalcornconditionbyweek$Data.Item[765:641]
x_adj[766:880] = USnationalcornconditionbyweek$Data.Item[880:766]
x_adj[881:990] = USnationalcornconditionbyweek$Data.Item[990:881]
x_adj[991:1105] = USnationalcornconditionbyweek$Data.Item[1105:991]
x_adj[1106:1215] = USnationalcornconditionbyweek$Data.Item[1215:1106]
x_adj[1216:1330] = USnationalcornconditionbyweek$Data.Item[1330:1216]
x_adj[1331:1435] = USnationalcornconditionbyweek$Data.Item[1435:1331]
x_adj[1436:1535] = USnationalcornconditionbyweek$Data.Item[1535:1436]
x_adj[1536:1640] = USnationalcornconditionbyweek$Data.Item[1640:1536]
x_adj[1641:1740] = USnationalcornconditionbyweek$Data.Item[1740:1641]

v_adj[1:110] = USnationalcornconditionbyweek$Value[110:1]
v_adj[111:220] = USnationalcornconditionbyweek$Value[220:111]
v_adj[221:320] = USnationalcornconditionbyweek$Value[320:221]
v_adj[321:420] = USnationalcornconditionbyweek$Value[420:321]
v_adj[421:530] = USnationalcornconditionbyweek$Value[530:421]
v_adj[531:640] = USnationalcornconditionbyweek$Value[640:531]
v_adj[641:765] = USnationalcornconditionbyweek$Value[765:641]
v_adj[766:880] = USnationalcornconditionbyweek$Value[880:766]
v_adj[881:990] = USnationalcornconditionbyweek$Value[990:881]
v_adj[991:1105] = USnationalcornconditionbyweek$Value[1105:991]
v_adj[1106:1215] = USnationalcornconditionbyweek$Value[1215:1106]
v_adj[1216:1330] = USnationalcornconditionbyweek$Value[1330:1216]
v_adj[1331:1435] = USnationalcornconditionbyweek$Value[1435:1331]
v_adj[1436:1535] = USnationalcornconditionbyweek$Value[1535:1436]
v_adj[1536:1640] = USnationalcornconditionbyweek$Value[1640:1536]
v_adj[1641:1740] = USnationalcornconditionbyweek$Value[1740:1641]

vsumweek_adj = sumbymod(weightedquality(x_adj)*v_adj,5)
plot(diff(vsumweek_adj))
plot(diff(vsumweek))
sum(diff(vsumweek))
sum(diff(vsumweek_adj))


x_year = list()
x_year[[16]] = USnationalcornconditionbyweek$Data.Item[110:1]
x_year[[15]] = USnationalcornconditionbyweek$Data.Item[220:111]
x_year[[14]] = USnationalcornconditionbyweek$Data.Item[320:221]
x_year[[13]] = USnationalcornconditionbyweek$Data.Item[420:321]
x_year[[12]] = USnationalcornconditionbyweek$Data.Item[530:421]
x_year[[11]] = USnationalcornconditionbyweek$Data.Item[640:531]
x_year[[10]] = USnationalcornconditionbyweek$Data.Item[765:641]
x_year[[9]] = USnationalcornconditionbyweek$Data.Item[880:766]
x_year[[8]] = USnationalcornconditionbyweek$Data.Item[990:881]
x_year[[7]] = USnationalcornconditionbyweek$Data.Item[1105:991]
x_year[[6]] = USnationalcornconditionbyweek$Data.Item[1215:1106]
x_year[[5]] = USnationalcornconditionbyweek$Data.Item[1330:1216]
x_year[[4]] = USnationalcornconditionbyweek$Data.Item[1435:1331]
x_year[[3]] = USnationalcornconditionbyweek$Data.Item[1535:1436]
x_year[[2]] = USnationalcornconditionbyweek$Data.Item[1640:1536]
x_year[[1]] = USnationalcornconditionbyweek$Data.Item[1740:1641]

v_year = list()
v_year[[16]] = USnationalcornconditionbyweek$Value[110:1]
v_year[[15]] = USnationalcornconditionbyweek$Value[220:111]
v_year[[14]] = USnationalcornconditionbyweek$Value[320:221]
v_year[[13]] = USnationalcornconditionbyweek$Value[420:321]
v_year[[12]] = USnationalcornconditionbyweek$Value[530:421]
v_year[[11]] = USnationalcornconditionbyweek$Value[640:531]
v_year[[10]] = USnationalcornconditionbyweek$Value[765:641]
v_year[[9]] = USnationalcornconditionbyweek$Value[880:766]
v_year[[8]] = USnationalcornconditionbyweek$Value[990:881]
v_year[[7]] = USnationalcornconditionbyweek$Value[1105:991]
v_year[[6]] = USnationalcornconditionbyweek$Value[1215:1106]
v_year[[5]] = USnationalcornconditionbyweek$Value[1330:1216]
v_year[[4]] = USnationalcornconditionbyweek$Value[1435:1331]
v_year[[3]] = USnationalcornconditionbyweek$Value[1535:1436]
v_year[[2]] = USnationalcornconditionbyweek$Value[1640:1536]
v_year[[1]] = USnationalcornconditionbyweek$Value[1740:1641]

for(i in 1:16){
  x_year[[i]] = flipvector(x_year[[i]])
  v_year[[i]] = flipvector(v_year[[i]])
}
v_adj = flipvector(v_adj)
x_adj = flipvector(x_adj)

plot(diff(vsumweek_adj)[1:25])
plot(vsumweek_adj[1:25])
plot(v_adj,ylim = c(0,70))

v_excellent = (v_adj[1:length(v_adj)%%5 ==1])
v_fair = (v_adj[1:length(v_adj)%%5 ==2])
v_good = (v_adj[1:length(v_adj)%%5 ==3])
v_poor = (v_adj[1:length(v_adj)%%5 ==4])
v_verypoor = (v_adj[1:length(v_adj)%%5 ==0])

plot(v_excellent,ylim = c(0,70))
plot(v_good,ylim = c(0,70))
plot(v_fair,ylim = c(0,70))
plot(v_poor,ylim = c(0,70))
plot(v_verypoor,ylim = c(0,70))

vdiff_excellent = diff(v_excellent)
vdiff_good = diff(v_good)
vdiff_fair = diff(v_fair)
vdiff_poor = diff(v_poor)
vdiff_verypoor = diff(v_verypoor)

plot(diff(v_excellent))
plot(diff(v_good))
plot(diff(v_fair))
plot(diff(v_poor))
plot(diff(v_verypoor))
weeksperyear = 1:16
for(i in 1:16){
  weeksperyear[i] = (length(x_year[[i]]))/5
}
yearend = 1:16
for(i in 1:16){
  yearend[i] = sum(weeksperyear[1:i])
}

plot(vdiff_excellent[-yearend])
plot(vdiff_good[-yearend])
plot(vdiff_fair[-yearend])
plot(vdiff_poor[-yearend])
plot(vdiff_verypoor[-yearend])
yearend


for(i in 1:16){
  plot(v_year[[i]][x_year[[i]]=="CORN - CONDITION, MEASURED IN PCT VERY POOR"])
}

vec1 = 1:16
for(i in 1:16){
  vec1[i] = length(x_year[[i]])/5
}

###
###
###

flipvector = function(v){
  return(v[length(v):1])
}

weightedquality = function(x, parameters = 0:4){
  return(parameters[1] * (x =="CORN - CONDITION, MEASURED IN PCT VERY POOR") +
           parameters[2] * (x =="CORN - CONDITION, MEASURED IN PCT POOR") + 
           parameters[3] * (x =="CORN - CONDITION, MEASURED IN PCT FAIR") +
           parameters[4] * (x =="CORN - CONDITION, MEASURED IN PCT GOOD") + 
           parameters[5] * (x =="CORN - CONDITION, MEASURED IN PCT EXCELLENT"))
}

weightedvalue = function(x, v, ...){
  return(weightedquality(x,...)*v)
}

sumbymod = function(x, modulus){
  n = length(x)
  modsum = rep(0,length(x)/modulus)
  for(i in 1:length(modsum)){
    for(j in 1:modulus){
      modsum[i] = modsum[i]+x[modulus*(i-1)+j]
    }
  }
  return(modsum)
}

normfun=function(x)  {
  #Standardizing iris
  
  xbar=apply(x,2,mean)
  xbarMatrix=cbind(rep(1,nrow(x)))%*%xbar
  s=apply(x,2,sd)
  sMatrix=cbind(rep(1,nrow(x)))%*%s
  z=(x-xbarMatrix)/sMatrix
  z
}

haarw=function(x) {
  
  n=length(x)
  s=sqrt(2)
  y=rep(0,n)
  
  k=1
  while (k*2 <= n){
    k=k*2
  }
  
  while (1 < k) {
    k=k/2
    
    for (i in 1:k) {
      y[i]=(x[2*i-1]+x[2*i])/s    #averages
      y[i+k]=(x[2*i-1]-x[2*i])/s            #differences
      
    }
    #should be fixed
    for (i in 1:(2*k))  {
      x[i]=y[i]
    }
  }
  return(x)
}

lagmatrix = function(x, lag = 1){
  nm0 = colnames(x)
  nms = c()
  lags = c()
  n = dim(x)[1]
  for(i in 0:lag){
    lags = cbind(lags, x[(lag-i+1):(n-i),])
    nms = c(nms,paste(nm0,i,sep = ""))
  }
  colnames(lags) = nms
  return(lags)
}

rmse = function(x){
  return(sqrt(mean(x^2)))
}

#Splitdata - Joseph Brown
splitdata = function(data, trainfrac=.7){
  length = dim(data)[1]                   #Data frames should be 2-dimensional.
  index = sample(length,length*trainfrac)
  traindata = data[index,]
  testdata = data[-index,]
  
  return(list(traindata = traindata, testdata = testdata))
}

#confusion matrix function - Dr. Crawford
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

plot.evaluate = function(model, testdata,...){
  n = dim(testdata)[1]
  d = 1:n
  for(i in 1:n){
    d[i] = predict(model,newdata = testdata[i,])
  }
  colvec = c("blue","red")
  ers = rmse(d-testdata[,1])
  plot(testdata[,1], type = 'l',...)
  points(d, col = colvec[(abs(d-testdata[,1])>ers) + 1])
}

evaluate = function(model, testdata){
  n = dim(testdata)[1]
  d = 1:n
  for(i in 1:n){
    d[i] = predict(model,newdata = testdata[i,])
  }
  return(d-testdata[,1])
}

createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

plot.evaluate.group = function(which, models, testdata,steps,...){
  n = dim(testdata)[1]
  d = 1:n
  tempdata = testdata[1:steps,]
  stones = 1:5
  for(i in 1:n){
    tempdata[1,7:11] = testdata[i,2:6]
    for(k in 1:5){
      stones[k] = predict(models[[k]],newdata = testdata[i,])
    }
    tempdata[1,2:6] = stones
    if(steps >1){
      for(j in 2:steps){
        tempdata[j,7:11] = tempdata[j-1,2:6]
        for(k in 1:5){
          stones[k] = predict(models[[k]],newdata = tempdata[j,])
        }
        tempdata[j,2:6] = stones
      }
    }
    d[i] = tempdata[steps,which-1]
  }
  colvec = c("blue","red")
  ers = rmse(d-testdata[,1])
  plot(testdata[,1], type = 'l',...)
  points(d, col = colvec[(abs(d-testdata[,1])>ers) + 1])
}

###
###
###

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

plot(good,col = "green",ylim = c(0,70))
points(excellent,col = "blue")
points(fair,col = "yellow")
points(poor,col = "red")
points(verypoor,col = "black")

###
###
###

designmatrix = cbind(v_verypoor,v_poor,v_fair,v_good,v_excellent)
designmatrix.lag1 = lagmatrix(designmatrix)
designmatrix.lag2 = lagmatrix(designmatrix,2)

# framething  V1~.
# test3
# test4
# testy

n = dim(designmatrix)[1]
dm_excellent.1 = data.frame(cbind(v_excellent[3:n],designmatrix.lag1[1:(n-2),]))
dm_good.1 = data.frame(cbind(v_good[3:n],designmatrix.lag1[1:(n-2),]))
dm_fair.1 = data.frame(cbind(v_fair[3:n],designmatrix.lag1[1:(n-2),]))
dm_poor.1 = data.frame(cbind(v_poor[3:n],designmatrix.lag1[1:(n-2),]))
dm_verypoor.1 = data.frame(cbind(v_verypoor[3:n],designmatrix.lag1[1:(n-2),]))

dm_excellent.2 = data.frame(cbind(v_excellent[4:n],designmatrix.lag2[1:(n-3),]))
dm_good.2 = data.frame(cbind(v_good[4:n],designmatrix.lag2[1:(n-3),]))
dm_fair.2 = data.frame(cbind(v_fair[4:n],designmatrix.lag2[1:(n-3),]))
dm_poor.2 = data.frame(cbind(v_poor[4:n],designmatrix.lag2[1:(n-3),]))
dm_verypoor.2 = data.frame(cbind(v_verypoor[4:n],designmatrix.lag2[1:(n-3),]))

errorslist.1 = list()
errorslist.2 = list()
rmsevec.1 = 1:5
rmsevec.2 = 1:5
minvec.1 = 1:5
minvec.2 = 1:5
maxvec.1 = 1:5
maxvec.2 = 1:5
folds.1 = createfolds(dim(designmatrix.lag1)[1],10)
folds.2 = createfolds(dim(designmatrix.lag2)[1],10)
accvector = 1:10
for(i in 1:10){
  temptest=stdwdbc[folds==i,]
  temptrain=stdwdbc[folds!=i,]
  predM_temp=knn(train=temptrain[,2:31],test=temptest[2:31],
                 cl=temptrain$V2,k=5)
  accvector[i]=confmatrix(temptest$V2,predM_temp)$accuracy
}
mean(accvector)
conditions = c("verypoor","poor","fair","good","excellent")
for(i in 1:5){
  eval(parse(text = paste("mylist = splitdata(dm_",conditions[i],".1)",sep = "")))
  eval(parse(text = paste("traindata_",conditions[i],".1 = mylist$traindata",sep = "")))
  eval(parse(text = paste("testdata_",conditions[i],".1 = mylist$testdata",sep = "")))
  eval(parse(text = paste("model_",conditions[i],".1 = svm(V1~., data = traindata_",conditions[i],".1)",sep = "")))
  
  eval(parse(text = paste("mylist = splitdata(dm_",conditions[i],".2)",sep = "")))
  eval(parse(text = paste("traindata_",conditions[i],".2 = mylist$traindata",sep = "")))
  eval(parse(text = paste("testdata_",conditions[i],".2 = mylist$testdata",sep = "")))
  eval(parse(text = paste("model_",conditions[i],".2 = svm(V1~., data = traindata_",conditions[i],".2)",sep = "")))
  
  eval(parse(text = paste("errorslist.1[[i]] = (evaluate(model_",conditions[i],".1, testdata_",conditions[i],".1))",sep = "")))
  eval(parse(text = paste("errorslist.2[[i]] = (evaluate(model_",conditions[i],".2, testdata_",conditions[i],".2))",sep = "")))
  
  rmsevec.1[i] = rmse(errorslist.1[[i]])
  rmsevec.2[i] = rmse(errorslist.2[[i]])
  minvec.1[i] = min(errorslist.1[[i]])
  minvec.2[i] = min(errorslist.2[[i]])
  maxvec.1[i] = max(errorslist.1[[i]])
  maxvec.2[i] = max(errorslist.2[[i]])
  
  eval(parse(text = paste("plot.evaluate(model_",conditions[i],".1, testdata_",conditions[i],
                          ".1, xlab = \"Week\", ylab = \"Percent ",conditions[i],"\",main = \"",
                          conditions[i]," with 1 lag\")",sep = "")))
  eval(parse(text = paste("plot.evaluate(model_",conditions[i],".2, testdata_",conditions[i],
                          ".2, xlab = \"Week\", ylab = \"Percent ",conditions[i],"\",main = \"",
                          conditions[i]," with 2 lags\")",sep = "")))
}


rmsevec.1 = 1:5
rmsevec.2 = 1:5
minvec.1 = 1:5
minvec.2 = 1:5
maxvec.1 = 1:5
maxvec.2 = 1:5
folds.1 = createfolds(dim(designmatrix.lag1)[1]-1,10)
folds.2 = createfolds(dim(designmatrix.lag2)[1]-1,10)
conditions = c("excellent","good","fair","poor","verypoor")
for(i in 1:5){
  vec1 = 1:10
  vec2 = 1:10
  vec3 = 1:10
  vec4 = 1:10
  vec5 = 1:10
  vec6 = 1:10
  for(k in 1:10){
    eval(parse(text = paste("temptest=dm_",conditions[i],".1[folds.1==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".1[folds.1!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain)
    tempvec = evaluate(tempmodel,temptest)
    vec1[k] = rmse(tempvec)
    vec2[k] = min(tempvec)
    vec3[k] = max(tempvec)
    
    eval(parse(text = paste("temptest=dm_",conditions[i],".2[folds.2==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".2[folds.2!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain)
    tempvec = evaluate(tempmodel,temptest)
    vec4[k] = rmse(tempvec)
    vec5[k] = min(tempvec)
    vec6[k] = max(tempvec)
  }
  rmsevec.1[i] = mean(vec1)
  minvec.1[i] = mean(vec2)
  maxvec.1[i] = mean(vec3)
  
  rmsevec.2[i] = mean(vec4)
  minvec.2[i] = mean(vec5)
  maxvec.2[i] = mean(vec6)
}



rmsevec.1 = 1:5
rmsevec.2 = 1:5
minvec.1 = 1:5
minvec.2 = 1:5
maxvec.1 = 1:5
maxvec.2 = 1:5
folds.1 = createfolds(dim(designmatrix.lag1)[1]-1,10)
folds.2 = createfolds(dim(designmatrix.lag2)[1]-1,10)
conditions = c("excellent","good","fair","poor","verypoor")
for(i in 1:5){
  vec1 = 1:10
  vec2 = 1:10
  vec3 = 1:10
  vec4 = 1:10
  vec5 = 1:10
  vec6 = 1:10
  for(k in 1:10){
    eval(parse(text = paste("temptest=dm_",conditions[i],".1[folds.1==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".1[folds.1!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain)
    tempvec = evaluate(tempmodel,temptest)
    vec1[k] = rmse(tempvec)
    vec2[k] = min(tempvec)
    vec3[k] = max(tempvec)
    
    eval(parse(text = paste("temptest=dm_",conditions[i],".2[folds.2==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".2[folds.2!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain)
    tempvec = evaluate(tempmodel,temptest)
    vec4[k] = rmse(tempvec)
    vec5[k] = min(tempvec)
    vec6[k] = max(tempvec)
  }
  rmsevec.1[i] = mean(vec1)
  minvec.1[i] = mean(vec2)
  maxvec.1[i] = mean(vec3)
  
  rmsevec.2[i] = mean(vec4)
  minvec.2[i] = mean(vec5)
  maxvec.2[i] = mean(vec6)
}

###
###
###

#k-fold CV for Neural Net
neuralnetsize=function(data2,netsizemax){
  
  n=nrow(data2)  
  K=10  
  accmatrix2=matrix(nrow=12,ncol=10)  #tenfold with 12 sizes tried
  #createfolds function
  createfolds=function(n,K){
    reps=ceiling(n/K)
    folds=sample(rep(1:K,reps))
    return(folds[1:n])
  }
  
  #Folds for mydata
  set.seed(5364)
  folds=createfolds(nrow(data2),10)
  
  
  
  for(netsize in 1:netsizemax){
    
    for(k in 1:10){
      
      temptest2=data2[folds==k,]
      temptrain2=data2[folds !=k,]
      
      tempnnet=nnet(temptrain2[,82]~.,data=temptrain2,size=netsize, linout=TRUE)
      yhat=predict(tempnnet, newdata=temptest2)
      residuals  = (temptest2[,82]-yhat)
      accmatrix2[netsize,k] =sqrt(mean(residuals^2))
    }
  }
  accmatrix2   
  accvector2=apply(accmatrix2,1,mean) 
  which.min(accvector2)   
  
}

#this is for the field conditions predictions
neuralnetsize2=function(data2,netsizemax){
  
  n=nrow(data2)  
  K=10  
  accmatrix2=matrix(nrow=12,ncol=10)  #tenfold with 12 sizes tried
  #createfolds function
  createfolds=function(n,K){
    reps=ceiling(n/K)
    folds=sample(rep(1:K,reps))
    return(folds[1:n])
  }
  
  #Folds for mydata
  set.seed(5364)
  folds=createfolds(nrow(data2),10)
  
  
  
  for(netsize in 1:netsizemax){
    
    for(k in 1:10){
      
      temptest2=data2[folds==k,]
      temptrain2=data2[folds !=k,]
      
      tempnnet=nnet(temptrain2[,73]~.,data=temptrain2,size=netsize, linout=TRUE)
      yhat=predict(tempnnet, newdata=temptest2)
      residuals  = (temptest2[,73]-yhat)
      accmatrix2[netsize,k] =sqrt(mean(residuals^2))
    }
  }
  accmatrix2   
  accvector2=apply(accmatrix2,1,mean) 
  which.min(accvector2)   
  
}

#######
#######
#####

#This file extracts the weekly data from the U.S. national corn condition for the first 22 weeks
#from 2000-2014 and transforms it into an array that is easier to work with. 
#In addition to that, it performs ten bootstrapped time series for each category for each year.
#This is added onto the 2-d array. The reason these bootstrapped time series are created is that
#many machine learning/prediction techniques require a high number of samples in order to converge 
#to a useful solution. For this application, I used a bootstrapping method known as maximum entropy.
#This method creates time series samples, and does so without assuming the time series being bootstrapped
#is stationary or linear. 

#This page is just the data page. You will need to import that US_corn_yield_by_year and the USnationalcornconditonby week data sets.
#The other file, the one labeled neuralnetcornyield will run the actual machine learning algorithm.

#This code holds:
#             - 'final array', which includes the extracted data and the bootstrapped data
#             -  It also contains the wavelet transformed version of 'final array' called 'dwt final array'
#             -  This also contains a Gaussian normalized version of the 'final array', which is needed for the neural network input and predictor called 'normfinalarray'

#normalization function  #works for each column, data must be in the form of a dataframe
normfun=function(x)  {
  #Standardizing iris
  
  xbar=apply(x,2,mean)
  xbarMatrix=cbind(rep(1,nrow(x)))%*%xbar
  s=apply(x,2,sd)
  sMatrix=cbind(rep(1,nrow(x)))%*%s
  z=(x-xbarMatrix)/sMatrix
  z
}
install.packages(abind)
library(abind)   #combine multidimensional arrays
#Wavelet libraries

#You can use this code, or if you use R studio, import the data as a csv file and the below code
#is generated automatically.



#creation of standardized array, normfinalarray



#Now, I am going to creat five individual time series using
#the excellent, good, fair, poor, very poor numbers, where each week will be a data point



#works use 15 years worth of data. Ensures some training and testing sets.
#now create a matrix object an loop through each row, updating year (does allow numeric)
#use loop to append column to the matrix object.


#has length 22, each array is a dataframe 
#as example, testvector2014 has all the excellent crop conditions for each week for the year 2014
testvector2014=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2014)$Value
testvector2013=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2013)$Value
testvector2012=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2012)$Value
testvector2011=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2011)$Value
testvector2010=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2010)$Value
testvector2009=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2009)$Value
testvector2008=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2008)$Value
testvector2007=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2007)$Value
testvector2006=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2006)$Value
testvector2005=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2005)$Value
testvector2004=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2004)$Value
testvector2003=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2003)$Value
testvector2002=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2002)$Value
testvector2001=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2001)$Value
testvector2000=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==2000)$Value
testvector1999=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1999)$Value
testvector1998=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1998)$Value
testvector1997=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1997)$Value
testvector1996=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1996)$Value
testvector1995=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1995)$Value
testvector1994=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1994)$Value
testvector1993=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1993)$Value
testvector1992=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1992)$Value
testvector1991=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1991)$Value
testvector1990=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1990)$Value
testvector1989=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1989)$Value
testvector1988=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1988)$Value
testvector1987=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT EXCELLENT" & Year==1987)$Value

testexcellent=cbind(testvector1987,testvector1988,testvector1989, testvector1990,
                    testvector1991, testvector1992, testvector1993, testvector1994,
                    testvector1995, testvector1996, testvector1997, testvector1998,
                    testvector1999, testvector2000, testvector2001, testvector2002,
                    testvector2003, testvector2004, testvector2005, testvector2006,
                    testvector2007, testvector2008, testvector2009, testvector2010,
                    testvector2011, testvector2012, testvector2013, testvector2014)[1:20,]

dftestexcellent=as.data.frame(testexcellent)
x1=dftestexcellent
xbar<- apply(x1, 2, mean)
s<- apply(x1,2,sd)
xbarMatrix<- cbind(rep(1,nrow(dftestexcellent)))%*%xbar
sMatrix<- cbind(rep(1,nrow(x1)))%*%s
z=(x1-xbarMatrix)/sMatrix
z  #normalization
waveletz=apply(z, 2, haarw)  #works!

#xbar<- apply(z, 2, mean)  #this works

#normtestexcellent=apply(testexcellent, 2, standfun)



testvectorg2014=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2014)$Value
testvectorg2013=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2013)$Value
testvectorg2012=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2012)$Value
testvectorg2011=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2011)$Value
testvectorg2010=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2010)$Value
testvectorg2009=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2009)$Value
testvectorg2008=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2008)$Value
testvectorg2007=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2007)$Value
testvectorg2006=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2006)$Value
testvectorg2005=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2005)$Value
testvectorg2004=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2004)$Value
testvectorg2003=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2003)$Value
testvectorg2002=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2002)$Value
testvectorg2001=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2001)$Value
testvectorg2000=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==2000)$Value
testvectorg1999=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1999)$Value
testvectorg1998=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1998)$Value
testvectorg1997=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1997)$Value
testvectorg1996=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1996)$Value
testvectorg1995=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1995)$Value
testvectorg1994=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1994)$Value
testvectorg1993=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1993)$Value
testvectorg1992=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1992)$Value
testvectorg1991=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1991)$Value
testvectorg1990=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1990)$Value
testvectorg1989=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1989)$Value
testvectorg1988=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1988)$Value
testvectorg1987=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT GOOD" & Year==1987)$Value


testgood=cbind(testvectorg1987,testvectorg1988,testvectorg1989, testvectorg1990,
               testvectorg1991, testvectorg1992, testvectorg1993, testvectorg1994,
               testvectorg1995, testvectorg1996, testvectorg1997, testvectorg1998,
               testvectorg1999, testvectorg2000, testvectorg2001, testvectorg2002,
               testvectorg2003, testvectorg2004, testvectorg2005, testvectorg2006,
               testvectorg2007, testvectorg2008, testvectorg2009, testvectorg2010,
               testvectorg2011, testvectorg2012, testvectorg2013, testvectorg2014)[1:20,]

dftestgood=as.data.frame(testgood)
x2=dftestgood
xbar2<- apply(x2, 2, mean)
s2<- apply(x2,2,sd)
xbarMatrix2<- cbind(rep(1,nrow(x2)))%*%xbar2
sMatrix2<- cbind(rep(1,nrow(x2)))%*%s2
z2=(x2-xbarMatrix2)/sMatrix2
#z2   #normalized testgood data frame
waveletz2=apply(z2, 2, haarw)

testvectorf2014=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2014)$Value
testvectorf2013=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2013)$Value
testvectorf2012=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2012)$Value
testvectorf2011=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2011)$Value
testvectorf2010=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2010)$Value
testvectorf2009=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2009)$Value
testvectorf2008=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2008)$Value
testvectorf2007=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2007)$Value
testvectorf2006=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2006)$Value
testvectorf2005=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2005)$Value
testvectorf2004=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2004)$Value
testvectorf2003=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2003)$Value
testvectorf2002=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2002)$Value
testvectorf2001=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2001)$Value
testvectorf2000=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==2000)$Value
testvectorf1999=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1999)$Value
testvectorf1998=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1998)$Value
testvectorf1997=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1997)$Value
testvectorf1996=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1996)$Value
testvectorf1995=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1995)$Value
testvectorf1994=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1994)$Value
testvectorf1993=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1993)$Value
testvectorf1992=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1992)$Value
testvectorf1991=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1991)$Value
testvectorf1990=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1990)$Value
testvectorf1989=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1989)$Value
testvectorf1988=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1988)$Value
testvectorf1987=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT FAIR" & Year==1987)$Value



testfair=cbind(testvectorf1987,testvectorf1988,testvectorf1989, testvectorf1990,
               testvectorf1991, testvectorf1992, testvectorf1993, testvectorf1994,
               testvectorf1995, testvectorf1996, testvectorf1997, testvectorf1998,
               testvectorf1999, testvectorf2000, testvectorf2001, testvectorf2002,
               testvectorf2003, testvectorf2004, testvectorf2005, testvectorf2006,
               testvectorf2007, testvectorf2008, testvectorf2009, testvectorf2010,
               testvectorf2011, testvectorf2012, testvectorf2013, testvectorf2014)[1:20,]

dftestfair=as.data.frame(testfair)
x3=dftestfair
xbar3<- apply(x3, 2, mean)
s3<- apply(x3,2,sd)
xbarMatrix3<- cbind(rep(1,nrow(x3)))%*%xbar3
sMatrix3<- cbind(rep(1,nrow(x3)))%*%s3
z3=(x3-xbarMatrix3)/sMatrix3
#z3    #normalized fair data frame
waveletz3=apply(z3, 2, haarw)    #wavelet transformed normalized z3

testvectorp2014=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2014)$Value
testvectorp2013=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2013)$Value
testvectorp2012=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2012)$Value
testvectorp2011=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2011)$Value
testvectorp2010=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2010)$Value
testvectorp2009=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2009)$Value
testvectorp2008=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2008)$Value
testvectorp2007=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2007)$Value
testvectorp2006=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2006)$Value
testvectorp2005=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2005)$Value
testvectorp2004=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2004)$Value
testvectorp2003=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2003)$Value
testvectorp2002=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2002)$Value
testvectorp2001=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2001)$Value
testvectorp2000=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==2000)$Value
testvectorp1999=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1999)$Value
testvectorp1998=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1998)$Value
testvectorp1997=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1997)$Value
testvectorp1996=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1996)$Value
testvectorp1995=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1995)$Value
testvectorp1994=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1994)$Value
testvectorp1993=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1993)$Value
testvectorp1992=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1992)$Value
testvectorp1991=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1991)$Value
testvectorp1990=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1990)$Value
testvectorp1989=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1989)$Value
testvectorp1988=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1988)$Value
testvectorp1987=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT POOR" & Year==1987)$Value




testpoor=cbind(testvectorp1987,testvectorp1988,testvectorp1989, testvectorp1990,
               testvectorp1991, testvectorp1992, testvectorp1993, testvectorp1994,
               testvectorp1995, testvectorp1996, testvectorp1997, testvectorp1998,
               testvectorp1999, testvectorp2000, testvectorp2001, testvectorp2002,
               testvectorp2003, testvectorp2004, testvectorp2005, testvectorp2006,
               testvectorp2007, testvectorp2008, testvectorp2009, testvectorp2010,
               testvectorp2011, testvectorp2012, testvectorp2013, testvectorp2014)[1:20,]

dftestpoor=as.data.frame(testpoor)
x4=dftestpoor
xbar4<- apply(x4, 2, mean)
s4<- apply(x4,2,sd)
xbarMatrix4<- cbind(rep(1,nrow(x4)))%*%xbar4
sMatrix4<- cbind(rep(1,nrow(x4)))%*%s4
z4=(x4-xbarMatrix4)/sMatrix4
#z4
waveletz4=apply(z4, 2, haarw)

testvectorvp2014=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2014)$Value
testvectorvp2013=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2013)$Value
testvectorvp2012=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2012)$Value
testvectorvp2011=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2011)$Value
testvectorvp2010=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2010)$Value
testvectorvp2009=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2009)$Value
testvectorvp2008=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2008)$Value
testvectorvp2007=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2007)$Value
testvectorvp2006=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2006)$Value
testvectorvp2005=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2005)$Value
testvectorvp2004=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT vERY POOR" & Year==2004)$Value
testvectorvp2003=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2003)$Value
testvectorvp2002=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2002)$Value
testvectorvp2001=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2001)$Value
testvectorvp2000=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==2000)$Value
testvectorvp1999=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1999)$Value
testvectorvp1998=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1998)$Value
testvectorvp1997=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1997)$Value
testvectorvp1996=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1996)$Value
testvectorvp1995=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1995)$Value
testvectorvp1994=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1994)$Value
testvectorvp1993=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1993)$Value
testvectorvp1992=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1992)$Value
testvectorvp1991=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1991)$Vlaue
testvectorvp1990=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1990)$Value
testvectorvp1989=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1989)$Value
testvectorvp1988=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1988)$Value
testvectorvp1987=subset(USnationalcornconditionbyweek, Data.Item=="CORN - CONDITION, MEASURED IN PCT VERY POOR" & Year==1987)$Value


testverypoor=cbind(testvectorvp1987,testvector1988,testvectorvp1989, testvectorvp1990,
                   testvectorvp1991, testvectorvp1992, testvectorvp1993, testvectorvp1994,
                   testvectorvp1995, testvectorvp1996, testvectorvp1997, testvectorvp1998,
                   testvectorvp1999, testvectorvp2000, testvectorvp2001, testvectorvp2002,
                   testvectorvp2003, testvectorvp2004, testvectorvp2005, testvectorvp2006,
                   testvectorvp2007, testvectorvp2008, testvectorvp2009, testvectorvp2010,
                   testvectorvp2011, testvectorvp2012, testvectorvp2013, testvectorvp2014)[1:20,]
#do not use test very poor in caclulations, but if we did, here is the normalized version

dftestverypoor=as.data.frame(testverypoor)
x5=dftestverypoor
xbar5<- apply(x5, 2, mean)
s5<- apply(x5,2,sd)
xbarMatrix5<- cbind(rep(1,nrow(x5)))%*%xbar5
sMatrix5<- cbind(rep(1,nrow(x5)))%*%s5
z5=(x5-xbarMatrix5)/sMatrix5
z5   # normalized vector
waveletz5=apply(z5, 2, haarw)


yearvector=(2000:2014)

#standardization of yearvector
x6=yearvector
mean6=mean(x6)
sd6=sd(x6)
z6=(x6-mean6)/sd6
z6
#we do not apply the wavelet transform to this. This is a single valued object for each vector

#resulting data frames with time series data are:

#testexcellent #excellent condition data set
#testgood #good condition data set
#testfair #fair condition data set
#testpoor    #poor condition data set
#testverypoor   #very poor field condition data set.

#We will now stack the arrays on top of one another to create the input vectors. 
#This consists of all of the array categories put on top of one another.
#also add in year as a vector to take into account nonstationary, increasing yields per year.

stackedarray=abind(testexcellent,testgood,testfair,testpoor, yearvector, along=1)
#check=t(stackedarray)
normstackedarray=abind(z, z2, z3, z4, z6, along=1)
waveletnormstackedarray=abind(waveletz, waveletz2, waveletz3, waveletz4, z6, along=1)

#now get dependent variable vector, yearly yield  #import us_corn_yield_by_yearyear

dependentvar=subset(US_corn_yield_by_year, Period == "YEAR" & Year>=1987)$Value
dependentvar  #gives Value time series for final year from 1987 to 2014

x7=dependentvar
mean7=mean(x7)
sd7=sd(x7)
normeddependentvar=(x7-mean7)/sd7
normeddependentvar
#we also do not perform a wavelet transforms on the output vector

#Works, now need to bootstrap time series
#####example bootstrap code with maximum entropy method
##One of the issues we will run into is that we have a small number of sample time series
###luckily, we can generate more, regardless of the statistical properties of our time series (nonstationary, etc)
## by using maximum entropy bootstrapping methods as shown below.
##make by year.
install(meboot)   #you may need to add this manually
library(meboot)
mebE2014=meboot(testvector2014, reps=100)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebE2013=meboot(testvector2013, reps=100)$ensemble
mebE2012=meboot(testvector2012, reps=100)$ensemble
mebE2011=meboot(testvector2011, reps=100)$ensemble
mebE2010=meboot(testvector2010, reps=100)$ensemble
mebE2009=meboot(testvector2009, reps=100)$ensemble
mebE2008=meboot(testvector2008, reps=100)$ensemble
mebE2007=meboot(testvector2007, reps=100)$ensemble
mebE2006=meboot(testvector2006, reps=100)$ensemble
mebE2005=meboot(testvector2005, reps=100)$ensemble
mebE2004=meboot(testvector2004, reps=100)$ensemble
mebE2003=meboot(testvector2003, reps=100)$ensemble
mebE2002=meboot(testvector2002, reps=100)$ensemble
mebE2001=meboot(testvector2001, reps=100)$ensemble
mebE2000=meboot(testvector2000, reps=100)$ensemble

#bind the above by column

mebE=cbind(mebE2014[1:20,],   #creates 10 ME bootstrapped samples of length 20 time series for  2014 excellent
           mebE2013[1:20,],
           mebE2012[1:20,],
           mebE2011[1:20,],
           mebE2010[1:20,],
           mebE2009[1:20,],
           mebE2008[1:20,],
           mebE2007[1:20,],
           mebE2006[1:20,],
           mebE2005[1:20,],
           mebE2004[1:20,],
           mebE2003[1:20,],
           mebE2002[1:20,],
           mebE2001[1:20,],
           mebE2000[1:20,])


dfmebE=as.data.frame(mebE)

xbar8<- apply(dfmebE, 2, mean)
s8<- apply(dfmebE,2,sd)
xbarMatrix8<- cbind(rep(1,nrow(dfmebE)))%*%xbar8
sMatrix8<- cbind(rep(1,nrow(dfmebE)))%*%s8
z8=(dfmebE-xbarMatrix8)/sMatrix8
#z8   # normalized vector
waveletz8=apply(z8, 2, haarw)  #wavelet transforms applied to each bootstrapped time series


mebg2014=meboot(testvectorg2014, reps=100)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebg2013=meboot(testvectorg2013, reps=100)$ensemble
mebg2012=meboot(testvectorg2012, reps=100)$ensemble
mebg2011=meboot(testvectorg2011, reps=100)$ensemble
mebg2010=meboot(testvectorg2010, reps=100)$ensemble
mebg2009=meboot(testvectorg2009, reps=100)$ensemble
mebg2008=meboot(testvectorg2008, reps=100)$ensemble
mebg2007=meboot(testvectorg2007, reps=100)$ensemble
mebg2006=meboot(testvectorg2006, reps=100)$ensemble
mebg2005=meboot(testvectorg2005, reps=100)$ensemble
mebg2004=meboot(testvectorg2004, reps=100)$ensemble
mebg2003=meboot(testvectorg2003, reps=100)$ensemble
mebg2002=meboot(testvectorg2002, reps=100)$ensemble
mebg2001=meboot(testvectorg2001, reps=100)$ensemble
mebg2000=meboot(testvectorg2000, reps=100)$ensemble

mebg=cbind(mebg2014[1:20,],   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
           mebg2013[1:20,],
           mebg2012[1:20,],
           mebg2011[1:20,],
           mebg2010[1:20,],
           mebg2009[1:20,],
           mebg2008[1:20,],
           mebg2007[1:20,],
           mebg2006[1:20,],
           mebg2005[1:20,],
           mebg2004[1:20,],
           mebg2003[1:20,],
           mebg2002[1:20,],
           mebg2001[1:20,],
           mebg2000[1:20,])

dfmebg=as.data.frame(mebg)
xbar9<- apply(dfmebg, 2, mean)
s9<- apply(dfmebg,2,sd)
xbarMatrix9<- cbind(rep(1,nrow(dfmebg)))%*%xbar9
sMatrix9<- cbind(rep(1,nrow(dfmebg)))%*%s9
z9=(dfmebg-xbarMatrix9)/sMatrix9
#z9   # normalized vector
waveletz9=apply(z9, 2, haarw)


mebf2014=meboot(testvectorf2014, reps=100)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebf2013=meboot(testvectorf2013, reps=100)$ensemble
mebf2012=meboot(testvectorf2012, reps=100)$ensemble
mebf2011=meboot(testvectorf2011, reps=100)$ensemble
mebf2010=meboot(testvectorf2010, reps=100)$ensemble
mebf2009=meboot(testvectorf2009, reps=100)$ensemble
mebf2008=meboot(testvectorf2008, reps=100)$ensemble
mebf2007=meboot(testvectorf2007, reps=100)$ensemble
mebf2006=meboot(testvectorf2006, reps=100)$ensemble
mebf2005=meboot(testvectorf2005, reps=100)$ensemble
mebf2004=meboot(testvectorf2004, reps=100)$ensemble
mebf2003=meboot(testvectorf2003, reps=100)$ensemble
mebf2002=meboot(testvectorf2002, reps=100)$ensemble
mebf2001=meboot(testvectorf2001, reps=100)$ensemble
mebf2000=meboot(testvectorf2000, reps=100)$ensemble

mebf=cbind( mebf2014[1:20,],   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
            mebf2013[1:20,],
            mebf2012[1:20,],
            mebf2011[1:20,],
            mebf2010[1:20,],
            mebf2009[1:20,],
            mebf2008[1:20,],
            mebf2007[1:20,],
            mebf2006[1:20,],
            mebf2005[1:20,],
            mebf2004[1:20,],
            mebf2003[1:20,],
            mebf2002[1:20,],
            mebf2001[1:20,],
            mebf2000[1:20,])

dfmebf=as.data.frame(mebf)
xbar10<- apply(dfmebf, 2, mean)
s10<- apply(dfmebf,2,sd)
xbarMatrix10<- cbind(rep(1,nrow(dfmebf)))%*%xbar10
sMatrix10<- cbind(rep(1,nrow(dfmebf)))%*%s10
z10=(dfmebf-xbarMatrix10)/sMatrix10
#z10   # normalized vector
waveletz10=apply(z10, 2, haarw)

mebp2014=meboot(testvectorp2014, reps=100)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebp2013=meboot(testvectorp2013, reps=100)$ensemble
mebp2012=meboot(testvectorp2012, reps=100)$ensemble
mebp2011=meboot(testvectorp2011, reps=100)$ensemble
mebp2010=meboot(testvectorp2010, reps=100)$ensemble
mebp2009=meboot(testvectorp2009, reps=100)$ensemble
mebp2008=meboot(testvectorp2008, reps=100)$ensemble
mebp2007=meboot(testvectorp2007, reps=100)$ensemble
mebp2006=meboot(testvectorp2006, reps=100)$ensemble
mebp2005=meboot(testvectorp2005, reps=100)$ensemble
mebp2004=meboot(testvectorp2004, reps=100)$ensemble
mebp2003=meboot(testvectorp2003, reps=100)$ensemble
mebp2002=meboot(testvectorp2002, reps=100)$ensemble
mebp2001=meboot(testvector2001, reps=100)$ensemble
mebp2000=meboot(testvectorp2000, reps=100)$ensemble

mebp=cbind(mebp2014[1:20,],   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
           mebp2013[1:20,],
           mebp2012[1:20,],
           mebp2011[1:20,],
           mebp2010[1:20,],
           mebp2009[1:20,],
           mebp2008[1:20,],
           mebp2007[1:20,],
           mebp2006[1:20,],
           mebp2005[1:20,],
           mebp2004[1:20,],
           mebp2003[1:20,],
           mebp2002[1:20,],
           mebp2001[1:20,],
           mebp2000[1:20,])

dfmebp=as.data.frame(mebp)
xbar11<- apply(dfmebp, 2, mean)
s11<- apply(dfmebp,2,sd)
xbarMatrix11<- cbind(rep(1,nrow(dfmebp)))%*%xbar11
sMatrix11<- cbind(rep(1,nrow(dfmebp)))%*%s11
z11=(mebp-xbarMatrix11)/sMatrix11
#z11   # normalized vector
waveletz11=apply(z11, 2, haarw)


bootedstackedarray=abind(mebE,mebg,mebf,mebp, along=1)
normedbootedstackedarray=abind(z8, z9, z10, z11, along=1)
wavnormedbootedstackedarray=abind(waveletz8, waveletz9, waveletz10, waveletz11, along=1)

repnum=100
sampleyearvectors=c(rep(2014,repnum),
                    rep(2013,repnum),
                    rep(2012,repnum),
                    rep(2014,repnum),
                    rep(2010,repnum),
                    rep(2009,repnum),
                    rep(2008,repnum),
                    rep(2007,repnum),
                    rep(2006,repnum),
                    rep(2005,repnum),
                    rep(2004,repnum),
                    rep(2003,repnum),
                    rep(2002,repnum),
                    rep(2001,repnum),
                    rep(2000,repnum))

dfsampleyearvectors=as.data.frame(sampleyearvectors)
x12=dfsampleyearvectors
xbar12<- apply(x12, 2, mean)
s12<- apply(x12,2,sd)
xbarMatrix12<- cbind(rep(1,nrow(x12)))%*%xbar12
sMatrix12<- cbind(rep(1,nrow(x12)))%*%s12
z12=(x12-xbarMatrix12)/sMatrix12
#z12   # normalized vector
#This is the year. We do not apply a wavelet transforms to this vector, as it is a length one object for each data set.

bootstackedarray1=abind(bootedstackedarray,sampleyearvectors, along=1)      #length 81, width 150     
normbootstackedarray1=abind(normedbootedstackedarray, unlist(z12), along=1 )
dwtnormedbootstackedarray1=abind(wavnormedbootedstackedarray, unlist(z12), along=1)
#stacked array length 81, width 15, bootedstackedarray1 =length 81, width 15                   

#the transposed versions of above. These are in the format needed for most models
#each column is a single time series with all of the categories stacked (excellent, good, fair, poor)
tdwtnormedfinalarray=t(dwtnormedfinalarray) 

tnormedfinalarray=t(normedfinalarray)
hu=tstackedarray 

#bootstackedarray1 is the final data output. This gives an array with 11 samples for each   
#year from 2000-2014.Each sample consists of 20 weeks of data and the year.
finalarray=abind(stackedarray,bootstackedarray1, along=2)   
normedfinalarray=abind(normstackedarray, normbootstackedarray1, along=2)
dwtnormedfinalarray=abind(waveletnormstackedarray, dwtnormedbootstackedarray1, along=2)

#now get dependent variable vector, yearly yield  #import us_corn_yield_by_yearyear
#this matches each column(including the booted columns) so that each time series has a yield assigned to it.
dependentvar=c(yield1=subset(US_corn_yield_by_year, Period == "YEAR" & Year>=2000)$Value,
               bootyield2014=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2014)$Value,100),
               bootyield2013=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2013)$Value,100),
               bootyield2012=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2012)$Value,100),
               bootyield2011=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2011)$Value,100),
               bootyield2010=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2010)$Value,100),
               bootyield2009=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2009)$Value,100),
               bootyield2008=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2008)$Value,100),
               bootyield2007=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2007)$Value,100),
               bootyield2006=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2006)$Value,100),
               bootyield2005=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2005)$Value,100),
               bootyield2004=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2004)$Value,100),
               bootyield2003=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2003)$Value,100),
               bootyield2002=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2002)$Value,100),
               bootyield2001=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2001)$Value,100),
               bootyield2000=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2000)$Value,100))
normedarray=abind(normedfinalarray, dependentvar, along=1) #final row is dependent
dwtnormedarray=abind(dwtnormedfinalarray, dependentvar, along=1)

tdwtnormedfinalarray=t(dwtnormedarray) 
tnormedarray=t(normedarray)

#tnormedfinalarray=t(normedarray)  #the final row is the dependent variable for each year
#tdwtnormedarray=t(dwtnormedfinalarray) #the final row is the dependent variable for each year (the yield)

############
###########
##########
#Feed Forward Neural network corn yield prediction

#Below is the data set this code works with. This is a normalized and then wavelet transformed version of the data
#set. It includes 100 booted samples for each year.

#needed packages
install.packages("nnet")
library(nnet)
library(ggplot2)
library(adabag)   #need this for boosting algorithm

#code for confirmation matrix
#confmatrix function
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

#This function can be used to evaluate a neural network if given a data set of appropriate size and of size n
nneteval=function(ndata, n)   {  #n is size of net, ndata is data. this assumes 72 variables, row 73 is dep var
  sample=sample(nrow(ndata), nrow(ndata)*.7)
  trains<-ndata[sample,]
  tests<-ndata[-sample,]
  modelf=nnet(V73~., data=trains, size=n, linout=TRUE)
  nnetpredn=predict(modelf,data=tests)
  RSE2 <- sqrt((tests$V73 - nnetpredn)^2)
  mse2=mean((tests$V73 - nnetpredn)^2)
  varerror=var(tests$V73 - nnetpredn)
  maxabserror=max(RSE2) 
  minabserror=min(RSE2) 
  mse=mse2
  info=list("max absolute error"=maxabserror,"min absolute error"= minabserror,"variance or error" = varerror,"mean squared error" = mse)
  return(info)
}


#

#transposed data set so that columns are sample time series

dataset1=tdwtnormedfinalarray
dataset2=tnormedfinalarray #column 82 is yield, column 81 is year

hu=tstackedarray  #this gives a year as a sample time series, with all field conditions represented



#function to predict nnet size
neuralnetsize(dataset1, 11)   #optimal was 9

set.seed(5364)
sample<- sample(nrow(dataset1), nrow(dataset1)*.7)
trainnn<- dataset1[sample,]
testnn<- dataset1[-sample,]
#neuralnetsize2(dataset1)  #finds the optimal net size for the wavelet transformed data

#training
model=nnet(trainnn[,82]~.,data=trainnn,size=9, linout=TRUE)


#testing
nnetpred1=predict(model, testnn, type="raw")
#nnetpred1

RSE <- sqrt((testnn[,82] - nnetpred1)^2)
#RSE #absolute value of the error for each time series
#
min(RSE)    #0.001400698
max(RSE)   #16.57528

plot(RSE, main="ABS residuals plot")

RMSE=sqrt(mean((testnn[,82] - nnetpred1)^2)) #gives a single value
RMSE #5.940134
MSE=mean((testnn[,82] - nnetpred1)^2)
MSE #35.28519
varianceerror=var(testnn[,82] - nnetpred1)
varianceerror #34.39539

###################NON wavelet transformed time series (still normalized)




set.seed(5364)


sample<- sample(nrow(dataset2), nrow(dataset2)*.7)
trainnn2<- dataset2[sample,]
testnn2<- dataset2[-sample,]
neuralnetsize(dataset2, 11)  #optimal was 10

# once it's ready nnetadaboost(train2, test2, train2$V82, 15, 12)



#I used the neuralnetsize2 function instead
#####
#nneteval=function(ndata, n)   {  #n is size of net, ndata is data. this assumes 72 variables, row 73 is dep var
#sample=sample(nrow(ndata), nrow(ndata)*.7)
#trains<-ndata[sample,]
#tests<-ndata[-sample,]
#modelf=nnet(V72~., data=trains, size=n, linout=TRUE)
#RSE2 <- sqrt((ndata[-sample,]$V82 - modelf)^2)
#mse2=mean((ndata[-sample,]$V82 - modelf)^2)
#varerror=var(ndata[-sample,]$V82 - modelf)
#maxabserror=max(RSE2) 
#minabserror=min(RSE2) 
#mse=mse2
#info=list(maxabserror, minabserror, varerror, mse)
#return(info)
# }




#####


#training
model2=nnet(trainnn2[,82]~.,data=trainnn2,size=10, linout=TRUE)


#testing
nnetpred2=predict(model2, testnn2, type="raw")
#nnetpred2

RSE2 <- sqrt((testnn2[,82] - nnetpred2)^2)
#RSE2 #absolute value of the error for each time series
which.max(RSE2)   
max(RSE2)  #25.08274
which.min(RSE2)    
min(RSE2)   #0.2827361
varerror2=var(testnn2[,82] - nnetpred2)
#varerror2     #99
mse2=mean((testnn2[,82] - nnetpred2)^2)
mse2   #153.4402
plot(RSE2, main="ABS residuals plot")




#############field condition nnet prediction

#Excellent nnet samples
#first 20 samples for entire dwt normed data set
#want to see, given previous history, can we predict future field condition ofr a category

dwtexcellentdata=tdwtnormedfinalarray[,1:18] #all of the excellent data
dwtfairdata=tdwtnormedfinalarray[,21:38] #all of the excellent data
dwtgooddata=tdwtnormedfinalarray[,41:58] #all of the excellent data
dwtpoordata=tdwtnormedfinalarray[,61:78] #all of the excellent data

#transpose data sets to get into correct format for model fitting



#extract y's for data, merge to data frame
#looking for prediction of last two y's.

#N+1 prediction series
yexcellent=tdwtnormedfinalarray[,19]
yfair=tdwtnormedfinalarray[,39]
ygood=tdwtnormedfinalarray[,59]
ypoor=tdwtnormedfinalarray[,79]

#


#N+2 step ahead y series
yexcellent2=tdwtnormedfinalarray[,20]
yfair2=tdwtnormedfinalarray[,40]
ygood2=tdwtnormedfinalarray[,60]
ypoor2=tdwtnormedfinalarray[,80]


#one step ahead data sets
fieldsetexcellentn1=abind(dwtexcellentdata, yexcellent, along=2)
fieldsetgoodn1=abind(dwtgooddata, ygood, along=2)
fieldsetfairn1=abind(dwtfairdata, yfair, along=2)
fieldsetpoorn1=abind(dwtpoordata, ypoor, along=2)

fieldsetexcellentn1=as.data.frame(fieldsetexcellentn1)
fieldsetgoodn1=as.data.frame(fieldsetgoodn1)
fieldsetfairn1=as.data.frame(fieldsetfairn1)
fieldsetpoorn1=as.data.frame(fieldsetpoorn1)

#two step ahead data sets
fieldsetexcellentn2=abind(dwtexcellentdata, yexcellent, along=2)
fieldsetgoodn2=abind(dwtgooddata, ygood, along=2)
fieldsetfairn2=abind(dwtfairdata, yfair, along=2)
fieldsetpoorn2=abind(dwtpoordata, ypoor, along=2)

fieldsetexcellentn2=as.data.frame(fieldsetexcellentn2)
fieldsetgoodn2=as.data.frame(fieldsetgoodn2)
fieldsetfairn2=as.data.frame(fieldsetfairn2)
fieldsetpoorn2=as.data.frame(fieldsetpoorn2)

#trying to predict excellent field numbers using all field condition reports
#create data set 

fieldpredict=abind(dwtexcellentdata, dwtfairdata, dwtgooddata, dwtpoordata, along=2)  #150 by 72

#depedentvar for 1 step ahead

yfieldpredexcellentn1=dwtnormedfinalarray[19,]
yfieldpredfairn1=dwtnormedfinalarray[39,]
yfieldpredgoodn1=dwtnormedfinalarray[59,]
yfieldpredpoorn1=dwtnormedfinalarray[79,]

#dependenvar for 2 step ahead
yfieldpredexcellentn2=dwtnormedfinalarray[20,]
yfieldpredfairn2=dwtnormedfinalarray[40,]
yfieldpredgoodn2=dwtnormedfinalarray[60,]
yfieldpredpoorn2=dwtnormedfinalarray[80,]




#creating one step ahead prediction data sets

fieldpredexcellentn1=abind(fieldpredict, yfieldpredexcellentn1)   #150 by 73
fieldpredfairn1=abind(fieldpredict, yfieldpredfairn1)
fieldpredgoodn1=abind(fieldpredict, yfieldpredgoodn1)
fieldpredpoorn1=abind(fieldpredict, yfieldpredpoorn1)

fieldpredexcellentn1=as.data.frame(fieldpredexcellentn1)
fieldpredfairn1=as.data.frame(fieldpredfairn1)
fieldpredgoodn1=as.data.frame(fieldpredgoodn1)
fieldpredpoorn1=as.data.frame(fieldpredpoorn1)
#fitting 1 step ahead models for correct hidden layer size (max 18)


#creating two set ahead prediction data sets

fieldpredexcellentn2=abind(fieldpredict, yfieldpredexcellentn2)  # 150 by 73, column 73 is dependent var for all
fieldpredfairn2=abind(fieldpredict, yfieldpredfairn2)
fieldpredgoodn2=abind(fieldpredict, yfieldpredgoodn2)
fieldpredpoorn2=abind(fieldpredict, yfieldpredpoorn2)

fieldpredexcellentn2=as.data.frame(fieldpredexcellentn2)
fieldpredfairn2=as.data.frame(fieldpredfairn2)
fieldpredgoodn2=as.data.frame(fieldpredgoodn2)
fieldpredpoorn2=as.data.frame(fieldpredpoorn2)
#f
#finding optimal size for 1 step ahead field condition predictors

###The neural net size functions take a very long time to run with the large amount of bootstrapped data. 
##Only run if necessary

# column 73 is y dependent variable, field conditions
neuralnetsize2(fieldpredexcellentn1, 12)   # 12 
neuralnetsize2(fieldpredgoodn1, 11)    #12
neuralnetsize2(fieldpredfairn1, 12)   #12 
neuralnetsize2(fieldpredpoorn1, 12)    #12
dim(fieldpredexcellentn1)
#finding optimal size for 2 step ahead field condition predictors

neuralnetsize2(fieldpredexcellentn2, 12)   # 12 ?
neuralnetsize2(fieldpredgoodn2, 11)    # 11
neuralnetsize2(fieldpredfairn2, 12)   #`12
neuralnetsize2(fieldpredpoorn2, 12)    # 12

##evalutation of time series predictions
nneteval(fieldpredexcellentn1, 12)
# min abs error 0.000372422
# max abs error 2.581392
#error variance 0.7413346
#mean squared error  0.868548

nneteval(fieldpredfairn1, 12)
#max abs error 1.706283
#min abs error 0.001117004
#error variance  
#MSE  0.1672067

nneteval(fieldpredgoodn1, 12)
# min abs error 2.496881e-05
# max abs error 3.241911
#error variance 
#mean squared error   1.407413

nneteval(fieldpredpoorn1, 12)
# min abs error  0.001081896
# max abs error 2.243367
#error variance 0.3643653
#mean squared error   0.3661849

###nneteval for 2 step ahead 

nneteval(fieldpredexcellentn2, 12)
#max abs error 2.613777
#min abs error  0.0005567541
#error variance  
#MSE 1.111419

nneteval(fieldpredfairn2, 12)
#max abs error 1.29004
#min abs error  0.001909455
#error variance  
#MSE  0.1331234

nneteval(fieldpredgoodn2, 12)
#max abs error 2.805023
#min abs error  0.001046019
#error variance 
#MSE  1.362944

nneteval(fieldpredpoorn2, 12)
#max abs error  2.462995
#min abs error  0.000334757
#error variance  0.4052992

#MSE  0.4052716

##for two steps ahead, fair was the best predictor of future field conditions
## for one step ahead, fair was also the best predictor by mse   0.1672067

###########
###########
##########

library(randomForest)


###start by partitioning data into n sets
##this uses tnormedfinalarray

X=tnormedfinalarray  #column 82 is the corn yield  81 is the year, 

##########hold off on all of this
library(dplyr)

df[sample(nrow(df), 3), ]

set.seed(5364)
sample<- sample(nrow(dataset1), nrow(dataset1)*.7)
train<- dataset1[sample,]
test<- dataset1[-sample,]


X=X[sample(nrow(X),size=5,replace=TRUE),]
X=tnormedfinalarray
X=as.data.frame(X)

X1=sample_n(X,80)
X2=sample_n(X,80)
X3=sample_n(X,80)
X4=sample_n(X,80)
X5=sample_n(X,80)
X6=sample_n(X,80)
X7=sample_n(X,80)
X8=sample_n(X,80)
X9=sample_n(X,80)
X10=sample_n(X,80)
X11=sample_n(X,80)
X12=sample_n(X,80)
X13=sample_n(X,80)
X14=sample_n(X,80)
X15=sample_n(X,80)
X16=sample_n(X,80)
X17=sample_n(X,80)
X19=sample_n(X,80)
X20=sample_n(X,80)
X21=sample_n(X,80)
X22=sample_n(X,80)
X23=sample_n(X,80)
X24=sample_n(X,80)
X25=sample_n(X,80)

X1_E=cbind(X1[,1:20],X1[,81])
X1_G=X1[,21:40], X1[,81])
X1_F=X1[,41:60], X1[,81])
X1_P=X1[,61:80], X1[,81])

##########

Xexcellent=abind(X[,1:20],X[,82])

set.seed(5364)
sample1<- sample(nrow(Xexcellent), nrow(Xexcellent)*.7)
train1<- Xexcellent[sample1,]
test1<- Xexcellent[-sample1,]


RFmodelexcellent=randomForest(train1[,21]~.,data=train1)
RFmodelEpred=predict(RFmodelexcellent,newdata=test1)
RFmodelpredtrain=predict(RFmodelexcellent,newdata=train1)


RSERF <- sqrt((test1[,21] - RFmodelEpred)^2)
mseRF=mean((test1[,21] - RFmodelEpred)^2)
varerror=var(test1[,21] - RFmodelEpred)
maxabserror=max(RSERF) #8.429277
maxabserror
minabserror=min(RSERF) #0
minabserror
mseRF   #0.6948928
mseRFtrain<-mean((train2[,21]-RFmodeltrain)^2)


##Good
XGood=abind(X[,21:40],X[,82])

set.seed(5364)
sample2<- sample(nrow(XGood), nrow(XGood)*.7)
train2<- XGood[sample2,]
test2<- XGood[-sample2,]


RFmodelgood=randomForest(train2[,21]~.,data=train2)
RFmodelgpred=predict(RFmodelgood,newdata=test2)
RFmodelgpredtrain=predict(RFmodelgood,newdata=train2)

RSERg2 <- sqrt((test2[,21] - RFmodelgpred)^2)
mseRg2=mean((test2[,21] - RFmodelgpred)^2)
varerror2=var(test2[,21] - RFmodelgpred)
maxabserror2=max(RSERg2) # 12.95672
maxabserror2
minabserror2=min(RSERF) # 0
minabserror2
mseRg2   #1.1044
mseRg2train<-mean((train2[,21]-RFmodelgpredtrain)^2)

##Fair

XFair=abind(X[,41:60],X[,82])

set.seed(5364)
sample3<- sample(nrow(XFair), nrow(XFair)*.7)
train3<- XFair[sample3,]
test3<- XFair[-sample3,]


RFmodelfair=randomForest(train3[,21]~.,data=train3)
RFmodelfpred=predict(RFmodelfair,newdata=test3)
RFmodelfpredtrain=predict(RFmodelfair,newdata=train3)

RSERF3 <- sqrt((test3[,21] - RFmodelfpred)^2)
mseRF3=mean((test3[,21] - RFmodelfpred)^2)
varerror3=var(test3[,21] - RFmodelfpred)
maxabserror3=max(RSERF3)  #9.01859
maxabserror3
minabserror3=min(RSERF3) # 0
minabserror3
mseRF3   # 0.3629177
msetrain3<-mean((train2[,21]-RFmodelfpredtrain)^2)

##Poor

XPoor=abind(X[,61:80],X[,82])

set.seed(5364)
sample4<- sample(nrow(XPoor), nrow(XPoor)*.7)
train4<- XPoor[sample4,]
test4<- XPoor[-sample4,]


RFmodelpoor=randomForest(train4[,21]~.,data=train4)
RFmodelppred=predict(RFmodelfair,newdata=test4)
RFmodelppredtrain=predict(RFmodelfair,newdata=train4)

RSERF4 <- sqrt((test4[,21] - RFmodelppred)^2)
mseRF4=mean((test4[,21] - RFmodelppred)^2)
varerror4=var(test4[,21] - RFmodelppred)
maxabserror4=max(RSERF4) # 22.56853
maxabserror4
minabserror4=min(RSERF) # 0
minabserror4
mseRF4   #51.48644
mseRF4train=mean((train2[,21]-RFmodelppredtrain)^2)
###Combo model non weighted average

unweightedmodelpred=((RFmodelppredtrain)+(RFmodelfpredtrain)+(RFmodelgpredtrain)+(RFmodelpredtrain))/4
RSERF5 <- sqrt((test4[,21] - unweightedmodelpred)^2)
mseRF5=mean((test4[,21] - unweightedmodelpred)^2)
varerror5=var(test4[,21] - unweightedmodelpred)
maxabserror5=max(RSERF5) # 44.50077
maxabserror5
minabserror5=min(RSERF5) # 0.02409333
minabserror5
mseRF5  #280.0846

#models weighted with training mse, otherwise misinterprets ability to model new data
weightedmodelpred=(((RFmodelppredtrain*mseRF4train)+(RFmodelfpredtrain*mseRFtrain)+(RFmodelgpredtrain*mseRg2train)+(RFmodelpredtrain*mseRFtrain))/(mseRFtrain+mseRg2train+RFmodelfpredtrain+mseRF4train))
tweightedmodelpred=as.data.frame(weightedmodelpred)[1:445,]
RSERF6 <- sqrt((test4[,21] - tweightedmodelpred)^2)
mseRF6=mean((test4[,21] - tweightedmodelpred)^2)
varerror6=var(test4[,21] - tweightedmodelpred)
maxabserror6=max(RSERF5) # 44.50077
maxabserror6
minabserror6=min(RSERF5) #  0.02409333
minabserror6
mseRF6    #11918.41 


####Fair gave the best random forest results with an mse of  0.3629177