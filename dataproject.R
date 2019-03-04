rm(list=ls()) 
dev.off()
dev.new()
setwd("~/Desktop/School/ECON/ECON 209")
approval=read.csv("approval_polllist.csv",header = TRUE)
summary(approval)
var1=approval$timestamp
length(var1)
yougovsub=approval[approval$pollster=='YouGov',]
#Only select the "All polls" Catergory of Yougov polls
yougovsub_allpoll=yougovsub[yougovsub$subgroup=='All polls',]
raw.yougov<-data.frame(yougovsub_allpoll)
#Convert Dates to Numerics

#Process the start and end dates of the data
date1 <- as.Date(raw.yougov$startdate , format = "%m/%d/%Y")
date2<-as.Date(raw.yougov$enddate,format="%m/%d/%Y")

#We set date of each poll to the midpoint of its "start date" and "end date".
dates <- data.frame(date1,date2)
dates$mid<-dates$date2+ floor((dates$date2-dates$date1)/2)
mid.num<-as.numeric(as.character(dates$mid, format="%Y%m%d"))
#Add the midpoint dates to the poll data
yougovwithdates<-cbind(raw.yougov,dates$mid, mid.num)

#Useful tips:
# Think of other variables
#The plot does not check assumptions
#Social Science thinking: possible causes, confounding variables.
#A good approach is to use dummie var, mixed with length of shutdown.
#More datapoints are preferable.

#Now we treat government shutdown as a dummie variable x.
#The US government shutdown of 2018-2019 starts on Dec 22th, 2018 and ends on Jan 25th, 2018. So we choose the poll data starting from Dec 22th 2018 and ending on Jan 25th 2018. 
#First, we split the yougov data into 2 catergories:1. dates with government shutdowns and 2. dates
yougovwithdates=yougovwithdates[yougovwithdates$mid.num>=20180630,] # To reduce noise, only take data from the second half of 2018 to now 
n=dim(yougovwithdates)[1] #the total number of dates
NoSD.YG.df=yougovwithdates[yougovwithdates$mid.num<20181222 |yougovwithdates$mid.num>20190125,]
SD.YG.df=yougovwithdates[yougovwithdates$mid.num>=20181222 & yougovwithdates$mid.num<=20190125,]

#Define the dummie variable. When the government shutdown happens, x=1. When the government shutdown does not happen, x=0.Incorporate the new variable into yougovwithdates
dummie=rep(0,n)
yougovwithdates=cbind(yougovwithdates,dummie)
yougovwithdates$dummie[yougovwithdates$mid.num>=20181222 & yougovwithdates$mid.num<=20190125]=1

#The independent variable
x.dummie=yougovwithdates$dummie
#We use the adjusted approval rate as the depedent varaible to measure trump's popularity
approvalrate.dummie=yougovwithdates$adjusted_approve

#Run the dummie variable regression
fit.dummie=lm(approvalrate.dummie~x.dummie)
plot(x.dummie,approvalrate.dummie) #plot the regression
#Plot the fitted line
mu_hat_star.dummie = fit.dummie$fitted.values
lines(x.dummie,mu_hat_star.dummie)
summary(fit.dummie) #All the useful information is here. 

#Conclusion: government shutdown does not affect trump's approval rate
#Run the SLR diagnostics for dummie variable regression
plot(fit.dummie)
#The normal QQ plot shows a step-wise function. Thus the data is heteroskedastic.
#All the other assumptions hold.

#Let's define another independent variable: the number of days since the shutdown starts.
#We calculate the variable by substracting the starting date of each poll data point with the first starting date 12/22/2018.
shutdowndays=SD.YG.df$`dates$mid`- SD.YG.df$`dates$mid`[1]
#Create another column in yougovwithdates to incorporate our new variable.
SDdays=rep(0,n)
yougovwithdates=cbind(yougovwithdates,SDdays)
yougovwithdates$SDdays[yougovwithdates$mid.num>=20181222 & yougovwithdates$mid.num<=20190125]=shutdowndays

#The independent variable is the number of shutdown days
x.sd=yougovwithdates$SDdays

#The dependent variable is the same.
approvalrate.sdd=yougovwithdates$adjusted_approve

#Now we can plot Trump's approval rates with the number of shutdown days.
plot(x.sd,approvalrate.sdd)
#We can use a SLR model to fit our data
fit.sdd=lm(approvalrate.sdd~x.sd)
summary(fit.sdd) #One can find all the useful data about our SLR model here

#Plot the fitted line
mu_hat_star.sdd = fit.sdd$fitted.values
lines(x.sd, mu_hat_star.sdd)

#Let's check out the SLR diagnostics
plot(fit.sdd)
#The normal QQ plot shows a step-wise function. Thus the data is heteroskedastic.
#All the other assumptions hold.

#Let's define our third indepedent variable: the number of days after the shutdown ends
#First, isolate the poll data before and after the end of shutdown
Afend.YG.df=yougovwithdates[yougovwithdates$mid.num>=20190125,]
Bfend.YG.df=yougovwithdates[yougovwithdates$mid.num<20190125,]

#Calculate the dates after the shutdown
Afdates=Afend.YG.df$`dates$mid`-Afend.YG.df$`dates$mid`[1]

#Create another column in yougovwithdates to incorporate our new variable.
Afdays=rep(0,n)
yougovwithdates=cbind(yougovwithdates,Afdays)
yougovwithdates$Afdays[yougovwithdates$mid.num>=20190125]=Afdates

# Number of days after the end of shutdown is the indepdent variable 
x.end=yougovwithdates$Afdays

#The dependent variable is the same
y.end=yougovwithdates$adjusted_approve

#Plot tha adjusted approval rates with regards to the number of days after shutdown ended
plot(x.end,y.end)

#Fit the data into a SLR model
fit.end=lm(y.end~x.end)
mu_hat_star.end=fit.end$fitted.values
lines(x.end,mu_hat_star.end)

#You can find all the useful information about the SLR model here
summary(fit.end)
#the slope beta_1_hat does not have statistical significance.
plot(fit.end)
#The normal QQ plot shows a step-wise function. Thus the data is heteroskedastic.
#All the other assumptions hold.
#We can run a multilinear regression with the three existing variables: 1. dummie variable of gov shutdown, 2. # of days of gov shutdown. 3.# of days after gov shutdown
#Since our dependent variable will be the same,we can use the same depedent variable in our last case.
fit.mlr.yougov1=lm(y.end~x.dummie+x.sd+x.end)
summary(fit.mlr.yougov1) #All the useful information can be found here.
#The result suggests that the length of shutdown days is the only statistically significant indicator of Trump's popularity among all three indep variables.


#We can introduce another variable: financial market performance
dowjones=read.csv('^DJI.csv',header = TRUE)
summary(dowjones) #Identify the key variables: Date, adj.close
date.dj=as.Date(dowjones$Date)
Date.num=as.numeric(as.character(date.dj))
                             