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

plot.evaluate.2 = function(model, testdata,...){
  n = dim(testdata)[1]
  d = 1:n
  for(i in 1:n){
    d[i] = predict(model,newdata = testdata[i,])
  }
  colvec = c("blue","red")
  ers = rmse(d-testdata[,1])
  rnge = range(d,testdata[,1])
  plot(testdata[,1], type = 'l',ylim = rnge,...)
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

plot.evaluate.group = function(which, models, testdata, steps,...){
  #Only applies to sequential testdata.
  if(steps > 1){
    n = dim(testdata)[1]
    d = 1:(n-steps+1)
          #This will be the vector of predictions.
          #For the current data, the y-values are assumed to have been
          # shifted down already.  With a model that accounts for the
          # shifts in data alread and does not require a specialized
          # lag matrix, this is subject to change.
    tempdata = testdata[1:steps,]
    stones = 1:5
    for(i in 1:n){
      tempdata[1,7:11] = testdata[i,2:6]
      for(k in 1:5){
        stones[k] = predict(models[[k]],newdata = testdata[i,])
      }
      for(j in 2:steps){
        tempdata[j,7:11] = tempdata[j-1,2:6]
        for(k in 1:5){
          stones[k] = predict(models[[k]],newdata = tempdata[j,])
        }
        tempdata[j,2:6] = stones
      }
      d[i] = tempdata[steps,which-1]
    }
    colvec = c("blue","red")
    ers = rmse(d-testdata[,1])
    plot(testdata[,1], type = 'l',...)
    points(d, col = colvec[(abs(d-testdata[,1])>ers) + 1])
  } else {evaluate(model[[which]],testdata)}
  
}

mae = function(x){
  return(mean(abs(x)))
}

mink.dist.met = function(x,y,p){
  return((x^p + y^p)^(1/p))
}

buildtimeseries = function(length, variables = 1, lags = 1, noise = 0.5, start = 0, volatility = 0.1*mean(start),
                           lagimpact = (a = runif(lags,-1,1))-mean(a),
                           distribution = rnorm, 
                           stability = 0, stabilityfunc = function(s,a,b){return((1-s)*a + s*b)},
                           center = FALSE){

  if(!is.array(lagimpact)){
    n = length(lagimpact)
  }
  
  a = runif(lags,-1,1)
  path = start
  for(i in (lags+1):length){
    path[i] = volatility*noise*distribution(1) + path[i-1]
    for(j in 1:lags){
      path[i] = path[i] + (1-noise)*(lagimpact[j])*path[i-j]
    }
    if(center){
      path[i] = stabilityfunc(stability,path[i],center)
    }
  }
  return(list(path = path,variables = variables,lags = lags,lagimpact = lagimpact,noise = noise))
}

submean = function(x){
  return(x - mean(x))
}
applymean = function(x,margin){
  
  if(is.array(x)){
    x = x - mean(x)
    x = apply()
  }
}

buildtimeseries2.0 = function(length, variables = 1, lags = 1, noise = 0.5, start = array(0,c(variables,lags)), volatility = 0.1*mean(start),
                              lagimpact = apply((a = array(dim = c(lags,2,2),runif(4*lags,-1,1)))-mean(a),c(2,3),
                                                function(x){return(x-mean(x))}),
                              distribution = rnorm, 
                              stability = 0, stabilityfunc = function(s,a,b){return((1-s)*a + s*b)},
                              center = FALSE){
  
  if(!is.array(lagimpact)){
    n = length(lagimpact)
    lagimpact = as.array(lagimpact)
    dim(lagimpact) = c(n,1,1)
  } else if(dim(lagimpact == 1)){
    dim(lagimpact) = c(dim(lagimpact),1,1)
  } else if(dim(lagimpact == 2)){
    dim(lagimpact) = c(dim(lagimpact)[1],1,1)
  }
  
  if(!is.array(start)){
    m = length(start)
    start = as.array(start)
    dim(start) = c(variables,lags)
               
  }
  if(is.array(start)){
    if(dim(start)==c(variables,lags)){
      path = start
    }else{
      path = array(0,c(variables,lags))
      for(i in 1:variables){
        for(i in 1:lags){
          if(!is.na(start[i,j])){
            path[i,j] = start[i,j]
          }
        }
      }
    }
  }
  path[is.na(path)]=0
  
  for(i in (lags+1):length){
    for(r in 1:variables)
      path[r,i] = volatility*noise*distribution(1) + path[r,i-1]
    for(j in 1:lags){
      path[r,i] = path[r,i] + (1-noise)*(lagimpact[j])*path[i-j]
    }
    if(center){
      path[i] = stabilityfunc(stability,path[i],center)
    }
  }
  return(list(path = path,variables = variables,lags = lags,lagimpact = lagimpact,noise = noise))
}




###
###
###