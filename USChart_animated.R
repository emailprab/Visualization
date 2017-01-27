 library(lattice)
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
 library(knitr)
 library(devtools)
 library(choroplethr)
 
 #head(df_president_ts, n=2)
 #install('C:/Users/Anuvaa/Downloads/ramnathv-rCharts-2c368c8.zip/ramnathv-rCharts-2c368c8')
#install('C:/Users/Anuvaa/Downloads/ram') 
GlobalLandTemperaturesByState <- fread("c:/CSC465/Project/GlobalLandTemperaturesByState.csv")
         
GlobalLandTemperaturesByState$dt<-as.Date(GlobalLandTemperaturesByState$dt,"%Y-%m-%d")
GlobalLandTemperaturesByState$Month<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%m"))
GlobalLandTemperaturesByState$Month.String<-format(GlobalLandTemperaturesByState$dt,"%B")
GlobalLandTemperaturesByState$Year<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%Y"))
head(GlobalLandTemperaturesByState)
GT = fread("c:/CSC465/Project/GlobalLandTemperaturesByState.csv")
GT[,dt:=as.Date(GT[,dt])]
GT[,year:=year(GT[,dt])]
GT[,month:=month(GT[,dt])]
GT[,season:=ifelse(month %in% c(6,7,8),"Summer",
                   ifelse(month %in% c(9,10,11),"Fall",
                          ifelse(month %in% c(12,1,2),"Winter","Spring")
                          
                   ))]

Gbl1960[,season:=ifelse(Month %in% c(6,7,8),"Summer",
                   ifelse(Month %in% c(9,10,11),"Fall",
                          ifelse(Month %in% c(12,1,2),"Winter","Spring")
                          
                   ))]

GT<-na.omit(subset(GT, GT$year >= 1960))
head(GT)
GTseaon<-ggplot(GT,aes(x = month, y = AverageTemperature)) +
  geom_violin(fill = "orange") +
  geom_point(aes(size = AverageTemperatureUncertainty), colour = "blue", position = "jitter") +
  ggtitle ("Temperature distribution by month") +
  xlab("Month") +  ylab ("Average temperature ( ºC )")
ggsave("GlobalViolin1960.png", GTseaon, width=7, height=4, units="in")

GT %>% 
  #filter(Year>1850) %>%
  group_by(month,year) %>% 
  summarise(Temp = mean(AverageTemperature), TempUncertainity = mean(AverageTemperature)) ->cData2
head(cData2,999)
head(GT)
cData2[,season:=ifelse(month %in% c(6,7,8),"Summer",
                   ifelse(month %in% c(9,10,11),"Fall",
                          ifelse(month %in% c(12,1,2),"Winter","Spring")
                          
                   ))]
cData2$month = factor(cData2$month,
                       labels = c("Jan","Feb","Mar","Apr",
                                  "May","Jun","Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))
GTseason1<-ggplot(cData2,aes(x = month, y = Temp)) +
  geom_violin(fill = "orange") +
  #geom_point(aes(size = 2), colour = "blue", position = "jitter") +
  ggtitle ("Temperature distribution by month 1960") +
  xlab("Month") +  ylab ("Average temperature ( ºC )")
ggsave("GlobalViolin1960.png", GTseason1, width=7, height=4, units="in")

GTseason<-ggplot(data = cData2,  aes(x=year,y=Temp,colour=month)) +
 #                ,colour=month,group=season)) +
#,colour=month,group=season)) +
   #  geom_line(aes(colour=month,group=season)) 
  #geom_line(aes(colour=month,group=season)) +
  #geom_smooth(aes(colour=month,group=season) )+
  geom_smooth() +
#+ scale_color_manual(values=c("red", "blue", "green",'yellow'))+
  xlab("year") +ggtitle("Average Temperatures by\nSeason Global") +
  ylab("Average Temperature"~degree*C) + labs(colour='Season')
#color.names<-c("blue", "red","green","yellow")
  ggsave("GblSeason1960.png", GTseason, width=7, height=4, units="in")
  
  Globaltemp <- ggplot(Gbl1960, aes(x = dt, y = AverageTemperature, colour = factor(season), reorder(Month.String, -AverageTemperature, mean)))+
    #geom_line(aes(group = season))+
    #scale_colour_identity("season", breaks=Gbl1960$season, labels=Gbl1960$season, guide="legend") +
    #scale_colour_manual(groups(Gbl1960$season), values = color.names) +
    geom_smooth( ) + ggtitle("Average Temperatures by\nMonth Global") +
    xlab("Year") + ylab("Average Temperature"~degree*C) + labs(colour='Month')
  
  ggsave("Global_1960.png", Globaltemp, width=7, height=4, units="in")
  
  #ggtitle("Average temperature by year")
Globaltemp <- ggplot(Gbl1960, aes(x = dt, y = AverageTemperature, colour = factor(season), reorder(Month.String, -AverageTemperature, mean)))+
  #geom_line(aes(group = season))+
  #scale_colour_identity("season", breaks=Gbl1960$season, labels=Gbl1960$season, guide="legend") +
  #scale_colour_manual(groups(Gbl1960$season), values = color.names) +
  geom_smooth( ) + ggtitle("Average Temperatures by\nMonth Global") +
  xlab("Year") + ylab("Average Temperature"~degree*C) + labs(colour='Month')

ggsave("Global1960.png", Globaltemp, width=7, height=4, units="in")


t = kable(head(GlobalLandTemperaturesByState[,1:10]), format = 'html', table.attr = "class=noliquid")   
t
USData<-GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Country=='United States']
head(USData)
USData1 <- aggregate(AverageTemperature~Year+State,USData,mean)
head(USData1)
USData1$State<- as.character(USData1$State)
USData1$State[USData1$State=="Georgia (State)"]<-"Georgia"
USData1$State<- as.factor(USData1$State)
#head(USData2)
library(reshape2)
aql <- melt(USData1, id.vars = c("Year"))
head(aql)
tail(aql)
datm <- melt(USData1, 'Year', 
             USData1.name = 'region',
             value.name = 'AverageTemperature'
           # USData1.name = 'value',
             #value.name = 'AverageTemperature'
)
aql <- melt(USData1, 'Year', 
             USData1.name = 'region',
             value.name = 'State'
             # USData1.name = 'value',
             #value.name = 'AverageTemperature'
)
head(datm)
head(datm2)
datm2=datm[!duplicated(datm$region), ]
choropleths = list()
for (i in 2:ncol(datm2)) {
  df           = datm2[, c(1, i)]
  colnames(df) = c("region", "value")
  title        = paste0("US Temp: ", colnames(USData1)[i])
  choropleths[[i-1]] = state_choropleth(df, title=title)
}
for (i in 2:ncol(aql)) {
  df           = aql[, c(1, i)]
  colnames(df) = c("region", "value")
  title        = paste0("US Temp: ", colnames(USData1)[i])
  choropleths[[i-1]] = state_choropleth(df, title=title)
}
#head(USData1$State='Georgia')
#USData12<-USData2[USData2$State=='GA']
#USData12<-USData1[USData1$State=='Georgia']
#head(USData12)
datm <- subset(na.omit(datm), 
               !(State %in% c("United States", "District of Columbia"))
)
kable(head(USData1), format = 'html', table.attr = "class=nofluid")
#kable(head(datm), format = 'html', table.attr = "class=nofluid")

USData2 <- transform(USData1,
                   State = state.abb[match(as.character(State), state.name)],
                   fillKey = cut(AverageTemperature, quantile(AverageTemperature, seq(0, 1, 1/5)), labels = LETTERS[1:5]),
                   Year = as.numeric(substr(Year, 1, 4))
)
head(USData2)
kable(head(USData2), format = 'html', table.attr = "class=nofluid")
fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
  c(LETTERS[1:5], 'defaultFill')
)

library(plyr); 
#library(rjson)
library(rCharts)
#library(rMaps)
#install_github('rCharts', 'ramnathv')
dat2 <- dlply(na.omit(USData2), "Year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'State')
  return(y)
})
dat2
options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
map


head(USData2,999)

map2 = map$copy()
map2$set(
  bodyattrs = "ng-app ng-controller='rChartsCtrl'"
)
map2$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)

map2$setTemplate(chartDiv = "
  <div class='container'>
                 <input id='slider' type='range' min=1960 max=2010 ng-model='Year' width=200>
                 <span ng-bind='Year'></span>
                 <div id='' class='rChart datamaps'></div>  
                 </div>
                 <script>
                 function rChartsCtrl($scope){
                 $scope.Year = 1960;
                 $scope.$watch('Year', function(newYear){
                 map.updateChoropleth(chartParams.newData[newYear]);
                 })
                 }
                 </script>"
)
  
map2$set(newData = dat2)
map2

source('C:/Users/Anuvaa/Downloads/ichoropleth.R')
ichoropleth(AverageTemperature ~ State,
            data = USData2[,1:3],
            pal = 'PuRd',
            ncuts = 5,
            animate = 'Year',
            legend = TRUE,
            labels = TRUE
)


map3 = map2$copy()
map3$setTemplate(chartDiv = "
                 <div class='container'>
                 <button ng-click='animateMap()'>Play</button>
                 <div id='chart_1' class='rChart datamaps'></div>  
                 </div>
                 <script>
                 function rChartsCtrl($scope, $timeout){
                 $scope.year = 1960;
                 $scope.animateMap = function(){
                 if ($scope.year > 2010){
                 return;
                 }
                 mapchart_1.updateChoropleth(chartParams.newData[$scope.year]);
                 $scope.year += 1
                 $timeout($scope.animateMap, 1000)
                 }
                 }
                 </script>"
)
map3
