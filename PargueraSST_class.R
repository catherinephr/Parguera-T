###################################
## Catherine Hernandez####
#### Have SST been increasing in La Parguera Nature Reserve?##
#### What is the typical seasonal SST in La Parguera Nature Reserve?#
## In what years do you expect corals experienced beanching in La Parguera?
################

#####Load libraries########
library(tidyverse)
library(lubridate)
library(performance)
library(see)


#######Load Data###########
#import data for lat and long closes to Cayo Enrique, La parguera from NOAA coral reef watch data-- only select one data point every 31
### days to save us time for class and only output SST and DHW data##
data=read_csv("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_DHW%5B(1985-04-01T12:00:00Z):31:(2022-03-08T12:00:00Z)%5D%5B(17.975):1:(17.975)%5D%5B(-67.05):1:(-67.05)%5D,CRW_SST%5B(1985-04-01T12:00:00Z):31:(2022-03-08T12:00:00Z)%5D%5B(17.975):1:(17.975)%5D%5B(-67.05):1:(-67.05)%5D")
#export data frae to CSV

write_csv(data,"NOAA_CRW_SST_DHW_La_Parguera.cvs")
View(data)
data=slice(data,-1)
View(data)
data$datetime=as_datetime(data$time, tz="Etc/GMT-4")
# Never forget to view your data
View(data)
#Cleaning data
# so what does our data look like?
data %>% ggplot()+ geom_line(aes(x=datetime,y=CRW_SST))


#### Have SST been increasing in La Parguera Nature Reserve?##
#wooo, very bad, lets look at our summary of the data
summary(data)
# convert our characters to numbers
data$latitude=as.numeric(data$latitude)
data$longitude=as.numeric(data$longitude)
data$CRW_DHW=as.numeric(data$CRW_DHW)
data$CRW_SST=as.numeric(data$CRW_SST)
#so what does our data look like?
data%>% ggplot()+ geom_line(aes(x=datetime,y=CRW_SST))
#Is SST INCRESING THROUGH TIME?
#creates linear regression model of SST vs time
model=lm(CRW_SST~datetime,data=data)
summary(model) #provide summary of lm model
check_model(model)
data %>% ggplot(aes(x=datetime,y=CRW_SST))+ geom_line()+geom_smooth(method="lm")

#### What is the typical seasonal SST in La Parguera Nature Reserve?#
data$month=substr(data$time,6,7) #create mont colum
View(data)
library(Hmisc)

parguera_climatology=data%>%
group_by(month) %>%
  summarise(ci=list(mean_cl_normal(CRW_SST)%>% 
                      rename(mean=y, lwr=ymin, upr=ymax))) %>%
unnest(cols=c(ci))
View(parguera_climatology)

parguera_climatology%>% ggplot()+
  geom_ribbon(aes(x=month,ymin=lwr,ymax=upr,group=1))+
  geom_line (aes(x=month,y=mean,group=1))


data$year=substr(data$time,1,4)

maxDHWannual=data %>%
  group_by(year) %>%
  summarise(maxDHW=max(CRW_DHW))
View(maxDHWannual)

maxDHWannual$bleaching=
  ifelse(maxDHWannual$maxDHW>=8,"severe",
         ifelse(maxDHWannual$maxDHW>=4 "significant", "none"))
View(maxDHWannual)
bleaching_years=filter(maxDHWannual,maxDHW>4) #selects only years with predicted bleaching
bleaching_years

data%>% ggplot(aes(datetime,y=CRW_DHW))+
  geom_line()+
  geom_hline(yintercept = 4)
                
                
                
              





