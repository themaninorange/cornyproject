#Feed Forward Neural network corn yield prediction

#Below is the data set this code works with. This is a normalized and then wavelet transformed version of the data
#set. It includes ten booted samples for each year.

#needed packages
install.packages("nnet")
library(nnet)

#code for confirmation matrix
#confmatrix function
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}



#transposed data set so that columns are sample time series

tdwtnormedfinalarray=t(dwtnormedfinalarray) #81 by 165 to 165 by 81


#the dependent vector
 dataset1=cbind(tdwtnormedfinalarray, dependentvar = dependentvar[1:dim(tdwtnormedfinalarray)[1]])   #last column is dependent var
#divide into training and testing sets
train=sample(nrow(dataset1),round(nrow(dataset1)*.7,0))


model=nnet(dependentvar~.,data=dataset1[train,],size=20, linout=TRUE, MaxNWts = 10000)
predcondition=predict(model,newdata=dataset1[-train,])  #regression, not classification
confmatrix(dependentvar[1:dim(tdwtnormedfinalarray)[1]][-train],predcondition)   #this finds the predicted accuracy on the new data that the network was not trained on.
plot.nnet(model)


