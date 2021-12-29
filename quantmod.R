df<-data.frame()

#install.packages("quantmod")
library(quantmod)
df<-data.frame(getSymbols("AAPL",auto.assign = F))
#head(df)
#tail(df)
#[begin:end,colbegin:colend]
#[,1] first column only
#[1,] first row only
#[,c(1,4)] all rows of 1 and 4 columns
#[,1:4]all rows of 1 through 4 columns

AAPL<-df
colnames(AAPL)<-c("Open","High","Low","Close","Volume","Adjusted")

#AAPL[,c("Open","Volume")]
#write.csv(AAPL,file = "applestock.csv",row.names = TRUE,col.names = TRUE)
#a<-read.csv("applestock.csv",row.names = 1)

#Plotting the data (Adjusted using quantmod)
plot(Ad(AAPL),type = "l")

#simple Daily return using quantmod package; dayto day improvement 
plot(dailyReturn(Ad(as.xts(AAPL)),type = "arithmetic"),type = 'l',main = "Daily Return",xlab="Days",ylab="Return")
#xts is a zoo data type, better than a dataframe to plot


#Compound return from the starting from the first day.
plot(cumprod(1+dailyReturn(Ad(as.xts(AAPL)),type = "arithmetic")),type = 'l',main = "Daily Return",xlab="Days",ylab="Return")

######  TO COMPARE CLOSE TO ADJUSTED  ##########
#Calculating the compound return of the data from difference of the log
#use nested apply for functions log and diff to take the log and differential respectively
#to claculate the compound return in close and adjusted 
#in apply 2 REPRESENTS to column to which the function is to be applied
compRet<-apply(apply(AAPL[,c("Close","Adjusted")],2,log),2,diff)

#plot(compRet[,1],type='l')

#To compare in plots
CompareAdtoCl<-data.frame(compRet)
plot(CompareAdtoCl[,1],type='l')
lines(CompareAdtoCl[,2],type='l',col='red')

#To compare between cumulative returns log and otherwise

plot(cumprod(1+dailyReturn(Ad(as.xts(AAPL)),type = "arithmetic")))
lines(cumsum(dailyReturn(Ad(as.xts(AAPL)),type = "log")),col='red')


#Using performance analytics to chart a summary of the Comparison
#Use zoo objects for charting with dates and such

#install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)
data<-as.xts(CompareAdtoCl)
data<-exp(data)-1
charts.PerformanceSummary(data,main="Compare Close to Adjusted Culumative discreet return")

### Daily returns of of tesla

TSLA<-dailyReturn(Ad(getSymbols("TSLA",auto.assign = F)))
charts.PerformanceSummary(TSLA)


### Daily returns of of Micorsoft

MSFT<-dailyReturn(Ad(getSymbols("MSFT",auto.assign = F)))
charts.PerformanceSummary(MSFT)

#To compare TSLA with MSFT.

colnames(TSLA)<-c("TSLA")
colnames(MSFT)<-c("MSFT")
MSFTvsTSLA<-merge(MSFT,TSLA,all=F) #only select from the date TSLA starts trading, to avoild NANs
charts.PerformanceSummary(MSFTvsTSLA)

#To calculate sharp ratio
#252 trading days , risk free rate 0.5%
#Annualized returns coming from performance Analytics package 
#used to find the return on investment

table.AnnualizedReturns(MSFTvsTSLA,scale=252,Rf=0.005/252)

