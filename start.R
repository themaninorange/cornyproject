library(tseries)
library(TSA)
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