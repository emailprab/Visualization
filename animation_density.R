library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
library(animation)
library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(choroplethrMaps)
library(choroplethr)
library(maps)
library(data.table)
library(dplyr)
library(statebins)
library(SGP)
library(rgdal)
library(rworldmap)
ani.options(convert = 'convert.exe')

GlobalLandTemperaturesByState <- fread("c:/CSC465/project/GlobalLandTemperaturesByState.csv")

#city <- read.csv("../input/GlobalLandTemperaturesByCity.csv")

GlobalLandTemperaturesByState$year <- substr(GlobalLandTemperaturesByState$dt,1,4)
allYears <- unique(GlobalLandTemperaturesByState$year)

USData<-GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Country=='United States']
head(USData)
USData1 <- aggregate(AverageTemperature~year+State,USData,mean)
head(USData1)
library(stringi)
USData1$State<- as.character(USData1$State)
USData1$State[USData1$State=="Georgia (State)"]<-"Georgia"
USData1$State<- as.factor(USData1$State)
USData5<-data.table(USData1)
USData5[, yoy := c(NA, diff(USData5$AverageTemperature)), by = "State"]

typeof(USData5$AverageTemperature)
USData5$yoy=as.numeric(USData5$yoy)
head(USData5)

datm <- melt(USData1, 'year', 
             USData1.name = 'region',
             value.name = 'State'
             # USData1.name = 'AverageTemperature',
             #  value1.name = 'AverageTemperature'
)
head(datm)
#,n=12500)
choropleths = list()
for (i in 2:ncol(datm)) {
  df           = datm[, c(1, i)]
  colnames(df) = c("region", "value")
  title        = paste0("US Temp: ", colnames(USData1)[i])
  choropleths[[i-1]] = state_choropleth(df, title=title)
}

library(choroplethr)
library(choroplethrMaps)
library(sqldf)
library(tcltk)
head(USData)
USData4<-
  sqldf("
select 
        year,State
        ,avg(AverageTemperature) as value
        from USData
        
        group by 
        State,year
        ")
USData6<-
  sqldf("
        select 
        year,State
        ,yoy as value
        from USData5
        where year >1959
        group by 
        State,year
        ")

#USData %>%
#  select(year,AverageTemperature,State) %>%
#  group_by(year,State) %>%
#  summarise(value=mean(AverageTemperature))-> USData4

USData4$State<- as.character(USData4$State)
USData4$State[USData4$State=="Georgia (State)"]<-"Georgia"
USData4$State<- as.factor(USData4$State)

USData6$State<- as.character(USData6$State)
USData6$State[USData6$State=="Georgia (State)"]<-"Georgia"
USData6$State<- as.factor(USData6$State)


colnames(USData4)[2]<- "region"
USData4$region<-tolower(USData4$region)
head(USData)
USData4 <- na.omit(USData4)

colnames(USData6)[2]<- "region"
USData6$region<-tolower(USData6$region)
head(USData)
USData6 <- na.omit(USData6)


head(USData4)
dev.off()
print(state_choropleth(USData4[USData4$year==1900],
                       title="Land Temperature 1900", 
                       num_colors = 8,
                       legend="Degrees"),reference_map=TRUE)



saveGIF({
  for (thisYear in 1960:2013 ) {
    
    thisYearCountry <- USData4[USData4$year == thisYear, ]
    thisYearCountry <- na.omit(thisYearCountry)
    print(state_choropleth(thisYearCountry,
                           title=paste("Avg. Temp in US", thisYear),
                           num_colors = 8,
                           #brewer_pal="YlOrRd",
                           legend="Degrees"),reference_map=TRUE)
  } 
  
}, interval = 0.3, movie.name = "tempDensity3.gif", ani.width = 800, ani.height = 600)


saveGIF({
  for (thisYear in 1960:2013 ) {
    
    thisYearCountry <- USData6[USData6$year == thisYear, ]
    thisYearCountry <- na.omit(thisYearCountry)
    print(state_choropleth(thisYearCountry,
                           title=paste("Year over year Avg Temp change in US", thisYear),
                           num_colors = 8,
                           #brewer_pal="YlOrRd",
                           legend="Degrees"),reference_map=TRUE)
  } 
  
}, interval = 0.3, movie.name = "tempYoy1.gif", ani.width = 800, ani.height = 600)


saveGIF({
  
 for (thisYear in 1960:2013 ) {
  thisYearCountry <- USData4[USData4$year == thisYear, ]
  thisYearCountry <- na.omit(thisYearCountry)
  #stateData$value=thisYearCountry$value[!sapply(thisYearCountry, is.null)]
  choro = StateChoropleth$new(na.omit(thisYearCountry))
  choro$title = paste("Avg. Temp in US", thisYear)
  choro$ggplot_scale = scale_fill_brewer(name="Avg. temp Degrees"~degree~C, palette="Purples", drop=FALSE)
  choro$render()
  } 

}, interval = 0.3, movie.name = "tempDensity2.gif", ani.width = 800, ani.height = 600)

#saveGIF({
setwd('C:/CSC465/Project/images')  
  for (thisYear in 1960:2013 ) {
    thisYearCountry <- USData4[USData4$year == thisYear, ]
    thisYearCountry <- na.omit(thisYearCountry)
    if (thisYear >= 1960) {name = paste('US', thisYear,'plot.png', sep='')}
    #if (i < 10) {name = paste('000',i,'plot.png',sep='')}

    #if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
    choro = StateChoropleth$new(na.omit(thisYearCountry))
    choro$title = paste("Avg. Temp in US", thisYear)
    choro$ggplot_scale = scale_fill_brewer(name="Avg. temp Degrees"~degree~C, palette="Purples", drop=FALSE)
    choro$render()
    #$render()
    if (thisYear >= 1960) {name = paste('US', thisYear,'plot60.png', sep='')}
    png(name)
    
    dev.off()
}


#install.packages('animation', repos = 'http://yihui.name/xran')
#library(animation)

data(thisYearCountry.regions)
head(thisYearCountry)
saveGIF({
  for (thisYear in allYears) {
    
    thisYearCountry <- GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$year == thisYear, ]
    
    m <- ggplot(thisYearCountry, aes(x=AverageTemperature))
    m <- m + ggtitle(paste("Country - Average Temperature Histogram -", thisYear))
    m <- m + geom_density(alpha=.5, fill = "gray")
    m <- m + xlim(c(-40, 50))
    m <- m + ylim(c(0, .05))
    m <- m + geom_vline(aes(xintercept=mean(AverageTemperature, na.rm=T)),   # Ignore NA values for mean
                        color="red", linetype="dashed", size=1)
    m <- m + geom_vline(aes(xintercept=median(AverageTemperature, na.rm=T)),   # Ignore NA values for mean
                        color="blue", linetype="dashed", size=1)
    
    print(m)
    
  } 
  
}, interval = 0.6, movie.name = "tempDensity6.gif", ani.width = 800, ani.height = 600)
