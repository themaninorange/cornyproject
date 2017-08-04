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

install.packages(abind)
library(abind)   #combine multidimensional arrays
#Wavelet libraries
install.packages("wavelets")
library(wavelets)

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
install.packages("meboot")
library(meboot)
mebE2014=meboot(testvector2014, reps=10)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebE2013=meboot(testvector2013, reps=10)$ensemble
mebE2012=meboot(testvector2012, reps=10)$ensemble
mebE2011=meboot(testvector2011, reps=10)$ensemble
mebE2010=meboot(testvector2010, reps=10)$ensemble
mebE2009=meboot(testvector2009, reps=10)$ensemble
mebE2008=meboot(testvector2008, reps=10)$ensemble
mebE2007=meboot(testvector2007, reps=10)$ensemble
mebE2006=meboot(testvector2006, reps=10)$ensemble
mebE2005=meboot(testvector2005, reps=10)$ensemble
mebE2004=meboot(testvector2004, reps=10)$ensemble
mebE2003=meboot(testvector2003, reps=10)$ensemble
mebE2002=meboot(testvector2002, reps=10)$ensemble
mebE2001=meboot(testvector2001, reps=10)$ensemble
mebE2000=meboot(testvector2000, reps=10)$ensemble

#bind the above by column

mebE=cbind(mebE2014[1:20,],   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
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


mebg2014=meboot(testvectorg2014, reps=10)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebg2013=meboot(testvectorg2013, reps=10)$ensemble
mebg2012=meboot(testvectorg2012, reps=10)$ensemble
mebg2011=meboot(testvectorg2011, reps=10)$ensemble
mebg2010=meboot(testvectorg2010, reps=10)$ensemble
mebg2009=meboot(testvectorg2009, reps=10)$ensemble
mebg2008=meboot(testvectorg2008, reps=10)$ensemble
mebg2007=meboot(testvectorg2007, reps=10)$ensemble
mebg2006=meboot(testvectorg2006, reps=10)$ensemble
mebg2005=meboot(testvectorg2005, reps=10)$ensemble
mebg2004=meboot(testvectorg2004, reps=10)$ensemble
mebg2003=meboot(testvectorg2003, reps=10)$ensemble
mebg2002=meboot(testvectorg2002, reps=10)$ensemble
mebg2001=meboot(testvectorg2001, reps=10)$ensemble
mebg2000=meboot(testvectorg2000, reps=10)$ensemble

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


mebf2014=meboot(testvectorf2014, reps=10)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebf2013=meboot(testvectorf2013, reps=10)$ensemble
mebf2012=meboot(testvectorf2012, reps=10)$ensemble
mebf2011=meboot(testvectorf2011, reps=10)$ensemble
mebf2010=meboot(testvectorf2010, reps=10)$ensemble
mebf2009=meboot(testvectorf2009, reps=10)$ensemble
mebf2008=meboot(testvectorf2008, reps=10)$ensemble
mebf2007=meboot(testvectorf2007, reps=10)$ensemble
mebf2006=meboot(testvectorf2006, reps=10)$ensemble
mebf2005=meboot(testvectorf2005, reps=10)$ensemble
mebf2004=meboot(testvectorf2004, reps=10)$ensemble
mebf2003=meboot(testvectorf2003, reps=10)$ensemble
mebf2002=meboot(testvectorf2002, reps=10)$ensemble
mebf2001=meboot(testvectorf2001, reps=10)$ensemble
mebf2000=meboot(testvectorf2000, reps=10)$ensemble

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

mebp2014=meboot(testvectorp2014, reps=10)$ensemble   #creates 10 ME bootstrapped samples of length 22 time series for  2014 excellent
mebp2013=meboot(testvectorp2013, reps=10)$ensemble
mebp2012=meboot(testvectorp2012, reps=10)$ensemble
mebp2011=meboot(testvectorp2011, reps=10)$ensemble
mebp2010=meboot(testvectorp2010, reps=10)$ensemble
mebp2009=meboot(testvectorp2009, reps=10)$ensemble
mebp2008=meboot(testvectorp2008, reps=10)$ensemble
mebp2007=meboot(testvectorp2007, reps=10)$ensemble
mebp2006=meboot(testvectorp2006, reps=10)$ensemble
mebp2005=meboot(testvectorp2005, reps=10)$ensemble
mebp2004=meboot(testvectorp2004, reps=10)$ensemble
mebp2003=meboot(testvectorp2003, reps=10)$ensemble
mebp2002=meboot(testvectorp2002, reps=10)$ensemble
mebp2001=meboot(testvector2001, reps=10)$ensemble
mebp2000=meboot(testvectorp2000, reps=10)$ensemble

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

repnum=10
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


#bootstackedarray1 is the final data output. This gives an array with 11 samples for each   
#year from 2000-2014.Each sample consists of 20 weeks of data and the year.
finalarray=abind(stackedarray,bootstackedarray1, along=2)   
normedfinalarray=abind(normstackedarray, normbootstackedarray1, along=2)
dwtnormedfinalarray=abind(waveletnormstackedarray, dwtnormedbootstackedarray1, along=2)

#now get dependent variable vector, yearly yield  #import us_corn_yield_by_yearyear
#this matches each column(including the booted columns) so that each time series has a yield assigned to it.
dependentvar=c(yield1=subset(US_corn_yield_by_year, Period == "YEAR" & Year>=2000)$Value,
                   bootyield2014=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2014)$Value,10),
                   bootyield2013=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2013)$Value,10),
                   bootyield2012=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2012)$Value,10),
                   bootyield2011=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2011)$Value,10),
                   bootyield2010=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2010)$Value,10),
                   bootyield2009=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2009)$Value,10),
                   bootyield2008=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2008)$Value,10),
                   bootyield2007=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2007)$Value,10),
                   bootyield2006=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2006)$Value,10),
                   bootyield2005=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2005)$Value,10),
                   bootyield2004=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2004)$Value,10),
                   bootyield2003=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2003)$Value,10),
                   bootyield2002=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2002)$Value,10),
                   bootyield2001=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2001)$Value,10),
                   bootyield2000=rep(subset(US_corn_yield_by_year, Period == "YEAR" & Year==2000)$Value,10))
                   
          
###############Extra discrete wavelet transform code


#We first want to transform the array 'final array', which contains the collection 
#of time series into a wavelet transformed series. Since the date, which is included in the array,
#is not part of the time series, we will not include that row in the wavelet transform.

#finalarrayw=finalarray[1:80,]

#haar wt
#example1=dwt(finalarrayw[1:20,1]) ,filter="haar")$V1    #This contains the transformed wavelet time series
#example2=dwt(finalarrayw[21:40,1],filter="haar")$V1
#example2=dwt(finalarrayw[21:40,1],filter="haar")$V1

