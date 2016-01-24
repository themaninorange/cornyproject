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
# for(i in 1:5){
#   eval(parse(text = paste("tune_",conditions[i],".1 = tune.svm(V1~.,data=traindata_",conditions[i],
#                           ".1, gamma = 10^(-6:0), cost = 10^(1:4))",sep = "")))
# }
# for(i in 1:5){
#   eval(parse(text = paste("tune_",conditions[i],".2 = tune.svm(V1~.,data=traindata_",conditions[i],
#                           ".2, gamma = 10^(-6:0), cost = 10^(1:4))",sep = "")))
# }
bestgamma.1 = c(1e-02, 1e-03, 1e-02, 1e-02, 1e-05)
bestcost.1 = c(100, 1000, 10, 10, 1000)
bestgamma.2 = c(0.010, 0.001, 0.010, 0.001, 0.010)
bestcost.2 = c(10, 1000, 10, 100, 10)
for(i in 1:5){
  eval(parse(text = paste("bestgamma.1[i] = tune_",conditions[i],".1$best.parameters[[1]]",sep = "")))
  eval(parse(text = paste("bestcost.1[i] = tune_",conditions[i],".1$best.parameters[[2]]",sep = "")))
  eval(parse(text = paste("bestgamma.2[i] = tune_",conditions[i],".2$best.parameters[[1]]",sep = "")))
  eval(parse(text = paste("bestcost.2[i] = tune_",conditions[i],".2$best.parameters[[2]]",sep = "")))
}

rmsevec.1 = 1:5
rmsevec.2 = 1:5
minvec.1 = 1:5
minvec.2 = 1:5
maxvec.1 = 1:5
maxvec.2 = 1:5
maevec.1 = 1:5
maevec.2 = 1:5
folds.1 = createfolds(dim(designmatrix.lag1)[1]-1,10)
folds.2 = createfolds(dim(designmatrix.lag2)[1]-1,10)
conditions = c("verypoor","poor","fair","good","excellent")
for(i in 1:5){
  vec1 = 1:10
  vec2 = 1:10
  vec3 = 1:10
  vec4 = 1:10
  vec5 = 1:10
  vec6 = 1:10
  vec7 = 1:10
  vec8 = 1:10
  for(k in 1:10){
    eval(parse(text = paste("temptest=dm_",conditions[i],".1[folds.1==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".1[folds.1!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain, gamma = bestgamma.1[i], cost = bestcost.1[i])
    tempvec = evaluate(tempmodel,temptest)
    vec1[k] = rmse(tempvec)
    vec2[k] = min(tempvec)
    vec3[k] = max(tempvec)
    vec7[k] = mae(tempvec)
    
    eval(parse(text = paste("temptest=dm_",conditions[i],".2[folds.2==k,]",sep = "")))
    eval(parse(text = paste("temptrain=dm_",conditions[i],".2[folds.2!=k,]",sep = "")))
    tempmodel = svm(V1~., data = temptrain, gamma = bestgamma.2[i], cost = bestcost.2[i])
    tempvec = evaluate(tempmodel,temptest)
    vec4[k] = rmse(tempvec)
    vec5[k] = min(tempvec)
    vec6[k] = max(tempvec)
    vec8[k] = mae(tempvec)
  }
  rmsevec.1[i] = mean(vec1)
  minvec.1[i] = mean(vec2)
  maxvec.1[i] = mean(vec3)
  
  rmsevec.2[i] = mean(vec4)
  minvec.2[i] = mean(vec5)
  maxvec.2[i] = mean(vec6)
  
  maevec.1[i] = mean(vec7)
  maevec.2[i] = mean(vec8)
}


conditions = c("verypoor","poor","fair","good","excellent")
for(i in 1:5){
  eval(parse(text = paste("trainyear_",conditions[i],".1 = dm_",conditions[i],
                          ".1[1:(dim(dm_",conditions[i],".1)[1]-18),]",sep = "")))
  eval(parse(text = paste("testyear_",conditions[i],".1 = dm_",conditions[i],
                          ".1[(dim(dm_",conditions[i],".1)[1]-17):(dim(dm_",conditions[i],".1)[1]),]",sep = "")))
  eval(parse(text = paste("model.y_",conditions[i],".1 = svm(V1~., data = trainyear_",conditions[i],".1)",sep = "")))
  
  eval(parse(text = paste("errorslist.1[[i]] = (evaluate(model_",conditions[i],".1, testyear_",conditions[i],".1))",sep = "")))
  
  rmsevec.1[i] = rmse(errorslist.1[[i]])
  minvec.1[i] = min(errorslist.1[[i]])
  maxvec.1[i] = max(errorslist.1[[i]])
  maevec.1[i] = mae(errorslist.1[[i]])
  
  eval(parse(text = paste("plot.evaluate.2(model_",conditions[i],".1, testyear_",conditions[i],
                          ".1, xlab = \"Week\", ylab = \"Percent ",conditions[i],"\",main = \"",
                          conditions[i]," with 1 lag\")",sep = "")))
}


###
###
###