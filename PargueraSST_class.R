###################################
## Catherine Hernandez####
#### Have SST been increasing in La Parguera Nature Reserve?##
#### What is the typical seasonal SST in La Parguera Nature Reserve?#
## In what years do you expect corals experienced beanching in La Parguera?
################

# in R markdown:have clear figures and visuals answering the three questions and make sure figures are following data visualization practices--includes text to answer questions

#####Load libraries########
library(tidyverse)
library(lubridate)
library(performance) #test asuption of linear regresion and pvalue
library(see)

#######Load data#####
#Import data for lat and long closet to Enrique, La parguera from NOAA Coral reef watch data only select one data point every 31 days to save us time for class and only output SST and DHW data
#data= read_csv("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_DHW%5B(1985-04-01T12:00:00Z):31:(2022-03-08T12:00:00Z)%5D%5B(17.975):1:(17.975)%5D%5B(-67.05):1:(-67.05)%5D,CRW_SST%5B(1985-04-01T12:00:00Z):31:(2022-03-08T12:00:00Z)%5D%5B(17.975):1:(17.975)%5D%5B(-67.05):1:(-67.05)%5D")
#write_csv(data,"NOAA_CRW_SST_DHW_LA_PARGUERA.csv")
data=read_csv("NOAA_CRW_SST_DHW_LA_PARGUERA.csv")
#to save to r markdown 
#view our data
#View(data)
#first row look annoying -- not tidy! remove 
data=slice(data,-1) #REMOVE FIRST ROW
#always view your data
#view(data)
#LOOK great, but what about the time data
data$datetime= as_datetime(data$time, tz="Etc/GMT-4")
#never forget view data
#view(data)
#SO what does our data look like?

#######cleanig  data##########
data %>% ggplot()+ geom_line(aes(x=datetime, y=CRW_SST))

#WOOOOW, very  bad, lets look summary data
summary(data) #lots of charrecter
#convert our character to numbers
data$latitude= as.numeric(data$latitude)
data$longitude= as.numeric(data$longitude)
data$CRW_DHW= as.numeric(data$CRW_DHW)
data$CRW_SST= as.numeric(data$CRW_SST)
#so what does our data look like?
data %>% ggplot()+ geom_line(aes(x=datetime, y=CRW_SST))

#Have SST been increasing in La Parguera Reserve?
#is STT increasing through time?-- maybe?
#creates linear regression model of SST VS Time
model=lm(CRW_SST~datetime, data=data)
summary(model) #provide summary of lm model
#y=mx +b= b=intercept m=slope == CRWSST mDatetime +b m= "datetime" == m= m(+-SE)Datetime + b(+-SE)
check_model(model) #nice visual way to check lm assumptions
#so what does our regression look like?
data %>% ggplot(aes(x=datetime, y=CRW_SST))+ geom_line() + 
  geom_smooth(method = "lm")+
xlab("Year")+
  ylab ("Sea Surface Temperature (°C)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=15))

##############################What are typical seasson SST?
#What is the typical season SST in La Parguera Nature Reserve?

data$month= substr(data$time,6,7) #creat month data
#view always
#view(data)
library(Hmisc) 
#generate monthly mean SST +- 95% uncertanity
parguera_climatology= data %>%  
  group_by(month) %>% 
  summarise(ci=list(mean_cl_normal(CRW_SST) %>% 
                      rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest(cols = c(ci))
view(parguera_climatology) #view data

parguera_climatology %>% ggplot() +
  geom_ribbon(aes(x=month, ymin=lwr, ymax=upr, group=1),fill="blue") +
  geom_line(aes(x=month, y=mean, group=1))+
  scale_y_continuous(limits = c(26,30),breaks = seq(26,30,1))+
  xlab("Months")+
  ylab ("Mean Seasfurface Temperature(°C)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=))

  
  


##What are potential bleaching years? 
#In what year do you expect coral expierence bleaching in La Parguera?

data$year= substr(data$time,1,4)  

maxDHWannual= data %>% 
  group_by(year) %>% 
  summarise(maxDHW= max(CRW_DHW))

View(maxDHWannual)  

maxDHWannual$bleaching=
  ifelse(maxDHWannual$maxDHW>=8, "severe",
         ifelse(maxDHWannual$maxDHW>=4, "significant", "none"))
#view(maxDHWannual)       
bleaching_years= filter(maxDHWannual, maxDHW>4) 
bleaching_years

data %>% ggplot(aes(x=datetime, y=CRW_DHW)) +
  geom_line() +
  geom_hline(yintercept = 4) +
  xlab("Year")+
  ylab ("Degree Heating Week (°C)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=20))


  


                
                
              





