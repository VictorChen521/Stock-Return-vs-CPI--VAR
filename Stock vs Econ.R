library(lmtest)
library(tseries)
library(zoo)
library(magrittr)
library(vars)
library(ggplot2)
library(hrbrthemes)



Dow<-read.csv('Dow.csv')
CPI_Data<-read.csv('CPIAUCSL.csv')


Dow$ï..Date<-as.yearmon(Dow$ï..Date,'%b %y')
Dow<-Dow[order(Dow$ï..Date),]
#Change the date variable from character to date, ascending

CPI_Data$DATE<-as.Date(CPI_Data$DATE,"%Y-%m-%d")
#Change the date variable from character to date


Dow[,c(2:5)]<-lapply(Dow[,c(2:5)],gsub,pattern=',',replace='')
Dow[,c(2:5)]<-as.numeric(unlist(Dow[,c(2:5)]))
#Remove "," in numeric value

Dow$Return<-c((Dow$Price-Dow$Open)/Dow$Open)
#Calculate stock return


CPI<-ts(CPI_Data$CPIAUCSL,start = c(2009,1),frequency = 12)
Return<-ts(Dow$Return,start = c(2009,1),frequency = 12)

Data<-cbind(Dow[,c(1,2,8)],CPI_Data) %>%
  dplyr::mutate(CPI_Change = ((CPIAUCSL-dplyr::lag(CPIAUCSL))/dplyr::lag(CPIAUCSL))*100)
#Combine CPI value with stock return, calculate percent change of CPI

Data$DATE<-as.Date(Data$DATE,"%Y-%m-%d")
#Change the Date variable from character to date

ggplot(Data[which(Data$DATE<'2015-12-01'),],aes(x=DATE))+
  geom_bar(aes(y=CPI_Change),stat = 'identity',size=.1,fill = "#69b3a2",color='black')+
  geom_line(aes(y=Return/0.1),color = rgb(0.2, 0.6, 0.9, 1),size=0.8)+
  scale_y_continuous(
    name = 'CPI (%Change)',
    sec.axis = sec_axis(~.*0.1,name='Return'),
    labels = function(x)paste0(x,'%')
  )+
  theme_ipsum()+
  theme(
    axis.title.y = element_text(color = "#69b3a2",size = 10),
    axis.title.y.right  = element_text(color = rgb(0.2, 0.6, 0.9, 1),size = 10 )
  )

ggplot(Data[which(Data$DATE>'2015-12-01'),],aes(x=DATE))+
  geom_bar(aes(y=CPI_Change),stat = 'identity',size=.1,fill = "#69b3a2",color='black')+
  geom_line(aes(y=Return/0.1),color = rgb(0.2, 0.6, 0.9, 1),size=0.8)+
  scale_y_continuous(
    name = 'CPI (%Change)',
    sec.axis = sec_axis(~.*0.1,name='Return'),
    labels = function(x)paste0(x,'%')
  )+
  theme_ipsum()+
  theme(
    axis.title.y = element_text(color = "#69b3a2",size = 10),
    axis.title.y.right  = element_text(color = rgb(0.2, 0.6, 0.9, 1),size = 10)
  )
#plot Stock Return vs %Change of CPI


diffCPI<-diff(CPI)
diffReturn<-diff(Return)
#Differencing time-series data


adf.test(diffCPI)
adf.test(diffReturn)
# ADF test for stationary 

#Granger Causality testing

#Significant
grangertest(diffCPI~diffReturn,order=3)
grangertest(diffReturn~diffCPI,order=3)



#VAR Model (CPI~Return)

CPIReturn<-cbind(diffCPI,diffReturn)
#Combine two time series data together

Lagselect<-VARselect(CPIReturn[c(1:125),],lag.max = 12,type = 'const')
Lagselect$selection
#Determine the lag order of the VAR model

modelCPIReturn<-VAR(CPIReturn[c(1:125),],p=4,type = 'const',season = NULL,exogen = NULL)
summary(modelCPIReturn)
#Implement model

serial.test(modelCPIReturn,lags.pt = 12,type = 'PT.asymptotic')
#test for autocorrelation
arch.test(modelCPIReturn,lags.multi = 12,multivariate.only = T)
#test for heteroskedasticity
normality.test(modelCPIReturn,multivariate.only = T)
#test for normality


forecast<- predict(modelCPIReturn,n.ahead = 7,ci=0.95)
#make forecast 7 months ahead

fanchart(forecast,names ='diffCPI' )


Prediction<-as.data.frame(forecast$fcst$diffCPI)
#export forecasting value

Prediction[1,]<-Prediction[1,]+255.371
for (i in 2:7) {
  Prediction[i,]<- Prediction[i,]+Prediction[c(i-1),]
}
#redo the differencing value

Prediction$DATE<-seq(from = as.Date('2019-06-01'), to = as.Date('2019-12-01'),by='month')
Prediction<-merge(Prediction,Data,by='DATE',all.y = T)
Prediction$Diff<-Prediction$fcst-Prediction$CPIAUCSL
#Combine the forecasting value with real-world number

ggplot(Prediction[which(Prediction$DATE>'2018-01-01'),], aes(x = DATE, y = fcst)) + 
  geom_line(aes(col='red'),size = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  geom_line(aes(x=DATE,y=CPIAUCSL,color = 'blue'),
            size= 0.8)+
  scale_color_identity(name = '',
                     breaks = c('red','blue'),
                       labels = c('Prediction','Actual'),
                     guide = 'legend')+
  ylab('CPI')
#Compare the forecasting value with the real-world data









