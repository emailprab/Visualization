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
typeof(GlobalLandTemperaturesByState$AverageTemperature)
GlobalLandTemperaturesByState <- fread("c:/CSC465/project/GlobalLandTemperaturesByState.csv")
GlobalLandTemperaturesByState1 <- fread("c:/CSC465/project/GlobalLandTemperaturesByState1.csv")

head(GlobalLandTemperaturesByState)
GlobalLandTemperaturesByState$dt<-as.Date(GlobalLandTemperaturesByState$dt,"%Y-%m-%d")
GlobalLandTemperaturesByState$Month<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%m"))
GlobalLandTemperaturesByState$Month.String<-format(GlobalLandTemperaturesByState$dt,"%B")
GlobalLandTemperaturesByState$Year<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%Y"))

GlobalLandTemperaturesByState1$dt<-as.Date(GlobalLandTemperaturesByState1$dt,"%Y-%m-%d")
GlobalLandTemperaturesByState1$Month<-as.numeric(format(GlobalLandTemperaturesByState1$dt,"%m"))
GlobalLandTemperaturesByState1$Month.String<-format(GlobalLandTemperaturesByState1$dt,"%B")
GlobalLandTemperaturesByState1$Year<-as.numeric(format(GlobalLandTemperaturesByState1$dt,"%Y"))
head(GlobalLandTemperaturesByState1)
data11<-GlobalLandTemperaturesByState1[GlobalLandTemperaturesByState1$Year>1960]
head(data11)
GlobalLandTemperaturesByState.recent.1960 <- GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Year==1960,]
GlobalLandTemperaturesByState.recent.1960 <- aggregate(AverageTemperature~Year+Country,GlobalLandTemperaturesByState.recent,mean)

head(GlobalLandTemperaturesByState.recent.1960)
#join data to a map
gtdMap.1960 <- joinCountryData2Map( GlobalLandTemperaturesByState.recent.1960, 
                               nameJoinColumn="Country", 
                               joinCode="NAME" )

mapDevice('x11') #create a world shaped window

#plot the map
mapCountryData( gtdMap.1960, 
                nameColumnToPlot='AverageTemperature', 
                catMethod='fixedWidth', 
                numCats=100,
                mapTitle="Global Avg temperature 1960"~degree*C)


GlobalLandTemperaturesByState.recent1.1960 <- GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Year==1960,]
head(GlobalLandTemperaturesByState.recent1.1960)
GlobalLandTemperaturesByState.recent1.1960 <- aggregate(GlobalLandTemperaturesByState.recent1.1960$AverageTemperatureUncertainty~GlobalLandTemperaturesByState.recent1.1960$Year+GlobalLandTemperaturesByState.recent1.1960$Country,GlobalLandTemperaturesByState.recent1.1960,mean)

head(GlobalLandTemperaturesByState.recent1.1960)
#join data to a map
gtdMap1.1960 <- joinCountryData2Map( GlobalLandTemperaturesByState.recent1.1960, 
                               nameJoinColumn="GlobalLandTemperaturesByState.recent1.1960$Country", 
                               joinCode="NAME" )

mapDevice('x11') #create a world shaped window

#plot the map
mapCountryData( gtdMap1.1960, 
                addLegend = TRUE, borderCol = "grey",
                nameColumnToPlot='GlobalLandTemperaturesByState.recent1.1960$AverageTemperatureUncertainty', 
                catMethod='fixedWidth', 
                numCats=100,
                mapTitle="Global Avg.Uncertainty temperature 1960")

GlobalLandTemperaturesByState.recent1.1980 <- GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Year==1980,]
head(GlobalLandTemperaturesByState.recent1.1980)
GlobalLandTemperaturesByState.recent1.1980 <- aggregate(GlobalLandTemperaturesByState.recent1.1980$AverageTemperature~GlobalLandTemperaturesByState.recent1.1980$Year+GlobalLandTemperaturesByState.recent1.1980$Country,GlobalLandTemperaturesByState.recent1.1980,mean)

head(GlobalLandTemperaturesByState.recent1.1980)
#join data to a map
gtdMap1.1980 <- joinCountryData2Map( GlobalLandTemperaturesByState.recent1.1980, 
                                     nameJoinColumn="GlobalLandTemperaturesByState.recent1.1980$Country", 
                                     joinCode="NAME" )

mapDevice('x11') #create a world shaped window

#plot the map
mapCountryData( gtdMap1.1980, 
                addLegend = TRUE, borderCol = "grey",
                nameColumnToPlot='GlobalLandTemperaturesByState.recent1.1980$AverageTemperature', 
                catMethod='fixedWidth', 
                numCats=100,
                mapTitle="Global Avg. temperature 1980"~degree*C)


library(scatterplot3d)
# create column indicating point color
GlobalLandTemperaturesByState.recent1.1960$pcolor[GlobalLandTemperaturesByState.recent1.1960$Country=="India"] <- "brown"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="Brazil"] <- "yellow"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="United States"] <- "blue"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="Russia"] <- "orange"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="China"] <- "red"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="Australia"] <- "black"
GlobalLandTemperaturesByState.recent$pcolor[GlobalLandTemperaturesByState.recent$Country=="Canada"] <- "green"

with(GlobalLandTemperaturesByState.recent1.1960, {
  s3d <- scatterplot3d(Country,Year,AverageTemperature,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot Example 4",
                       zlab="Avg temperature"~degree*C,
                       ylab="Year",
                       ylab="Country",
                      # ylim=c(1963, 2013),
                       zlim=c(-10, 30),
                       ylim=c("India", "Brazil", "United States","Russia","China","Australia","Canada"), fill=c("brown", "yellow", "blue","orange","red","black","green"))
  s3d.coords <- s3d$xyz.convert(Country,Year,AverageTemperature)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=row.names(GlobalLandTemperaturesByState.recent),       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Global Temperature",
         c("India", "Brazil", "United States","Russia","China","Australia","Canada"), fill=c("brown", "yellow", "blue","orange","red","black","green"))
})
USData<-GlobalLandTemperaturesByState.recent.1960[GlobalLandTemperaturesByState.recent.1960$Country=="United States"]

head(USData)
library(lattice)
library(gclus)
library(car)
scatterplotMatrix(~AverageTemperature+Month,data=USData, 
      main="US Avg temp month Scatter Plot 1960")
GlobalLandTemperaturesByState[,scaled.temperature:=(ScaledTemperature=scale(AverageTemperature)), by=.(Country,Month)]
gc(reset=TRUE)

ggplot(GlobalLandTemperaturesByState,aes(x=dt,y=scaled.temperature))+
  stat_bin_2d(bins=100)+scale_fill_gradient(low="lightblue",high="red")+geom_smooth(colour="purple")+
  ggtitle("Centered and Scaled Temperatures Over Time")

summary(GlobalLandTemperaturesByState.recent.1960$AverageTemperature)
ggplot(GlobalLandTemperaturesByState.recent.1960,aes(x=AverageTemperature))+geom_density()
library(randomForest)
library(fitdistrplus)
descdist(GlobalLandTemperaturesByState.recent.1960$AverageTemperature,graph=TRUE)
ggplot(GlobalLandTemperaturesByState.recent.1960[Month==1],aes(x=AverageTemperature))+geom_density()

ggplot(GlobalLandTemperaturesByState.recent.1960[Month==5],aes(x=AverageTemperature))+geom_density()
len=length(GlobalLandTemperaturesByState.recent.1960$Country)
len
coeff=numeric(len )
coeff

#res=persp(x=GlobalLandTemperaturesByState.recent$Country, y=GlobalLandTemperaturesByState.recent$Year, z=GlobalLandTemperaturesByState.recent$AverageTemperature)
#mypoints = trans3d(GlobalLandTemperaturesByState.recent$Country,GlobalLandTemperaturesByState.recent$Year,GlobalLandTemperaturesByState.recent$AverageTemperature, pmat=res)
#points(mypoints, pch=1, lwd=2, col="red")


install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)

py <- plotly(username="r_user_guide", key="mw5isa4yqp")  # open plotly connection

# Generate data
library(reshape2) # for melt
GLT=data.frame(GlobalLandTemperaturesByState.recent$Year,GlobalLandTemperaturesByState.recent$Country,GlobalLandTemperaturesByState.recent$AverageTemperature)
head(GLT)
volcano3d <- melt(GLT,id=c("GlobalLandTemperaturesByState.recent.Year"))
names(volcano3d) <- c("Year", "AverageTemperature", "Country")
head(volcano3d)
# Basic plot
head(GlobalLandTemperaturesByState.recent)
v <- ggplot(volcano3d, aes(GlobalLandTemperaturesByState.recent$Year, GlobalLandTemperaturesByState.recent$AverageTemperature ))
v   + stat_contour() 
v <- ggplot(volcano3d, aes(Year, AverageTemperature,Country ))
v   + stat_contour() + geom_tile(aes(fill = AverageTemperature))
#+
#geom_tile(aes(fill = GlobalLandTemperaturesByState.recent$Country))
py$ggplotly()

st1=toupper(state.name)
st1
head(GlobalLandTemperaturesByState)
Data1 <- subset(GlobalLandTemperaturesByState1, toupper(State) %in% c(st1))

                
  Data1<- subset( Data1,             Data1$Year == '1960') 
Data1

data3=aggregate(Data1$AverageTemperature, by=list(Data1$Year,Data1$State), FUN=mean)[3]
head(Data1)
Data2 <-na.omit(subset(Data1, Year>1960))
setkey(Data2$Year,Data2$State)
Data2 %>% 
  #group_by(Year) %>% 
  ummarise(Temp = mean(AverageTemperature)) ->Data2
library(sqldf)
head(Data1)

data4=sqldf("
select 
      State,Year
      ,avg(AverageTemperature) as Avgtemp
      from Data1
      
      group by 
      State,Year
      ")

head(data4,99)
typeof(data4$Year)

Data1960 <-subset(data4, Year=='1960')
head(Data1960)
#where Year == '1960'
value=data4$Avgtemp[!sapply(data4$Avgtemp, is.null)]
region=data4$State[!sapply(data4$State, is.null)]
region
value

region=tolower(region)
#region=sapply(region, capwords)
#region1=capwords(region1)
stateData1960=data.frame(region,value)
nclr <- 8 # number of bins
min <- -30 # theoretical minimum
max <- 30 # theoretical maximum
breaks <- (max - min) / nclr
library(RColorBrewer)
library(classInt)

plotclr <- brewer.pal(nclr, "Oranges")
plotvar <- stateData1960$value
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

library(OIdata)

map("state", # base
    col = "gray80",
    fill = TRUE,
    lty = 0)
map("state", # data
    col = colcode,
    fill = TRUE,
    lty = 0,
    add = TRUE)
map("state", # border
    col = "gray",
    lwd = 1.4,
    lty = 1,
    add = TRUE)
legend("bottomright", # position
       legend = names(attr(colcode, "table")), 
       title = "Percent",
       fill = attr(colcode, "palette"),
       cex = 0.56,
       bty = "n") # border


plotclr <- brewer.pal(nclr, "Oranges")
plotvar <- St
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)
NAColor <- "gray80"
plotclr <- c(plotclr, NAColor)
choro = StateChoropleth$new(na.omit(stateData1960))
choro$title = "1960 Average Temp by State"
choro$ggplot_scale = scale_fill_brewer(name="Avg Temp", palette=2, drop=FALSE)
choro$render()

install.packages("ggplot2")
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("mapproj")
#library(mapproj)
#install.packages("spatstat")
library(spatstat)

theme_set(theme_bw(base_size = 8))
options(scipen = 20)

MyPalette <- colorRampPalette(c(hsv(0, 1, 1), hsv(7/12, 1, 1)))

### Map ###
StateMapData <- map_data("state")
head(StateMapData)

### Some Invented Data ###

IndependentVariable1 <- c("1900-1930", "1931-1960", "1961-2013")
IndependentVariable2 <- c("-5-10", "11-15", "16-20", "21+")

# Here is one way to "stack" lots of copies of the shapefile dataframe on top of each other:
# This needs to be done, because (as far as I know) ggplot2 needs to have the state names and polygon coordinates
# for each level of the faceting variables.

TallData <- expand.grid(1:nrow(StateMapData), IndependentVariable1, IndependentVariable2)
TallData <- data.frame(StateMapData[TallData[, 1], ], TallData)
colnames(TallData)[8:9] <- c("IndependentVariable1", "IndependentVariable2")

# Some random dependent variable we want to plot in color:
TallData$State_IV1_IV2 <- paste(TallData$region, TallData$IndependentVariable1, TallData$IndependentVariable2)
RandomVariable <- runif(length(unique(TallData$State_IV1_IV2)))
TallData$DependentVariable <- by(RandomVariable, unique(TallData$State_IV1_IV2), mean)[TallData$State_IV1_IV2]

### Plot ###

MapPlot <- ggplot(TallData,
                  aes(x = long, y = lat, group = group, fill = DependentVariable))
MapPlot <- MapPlot + geom_polygon()
MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.
MapPlot <- MapPlot + scale_x_continuous(breaks = NA, expand.grid = c(0, 0)) +
  scale_y_continuous(breaks = NA) +
  opts(
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.background = theme_blank(),
    panel.border = theme_blank(),
    expand.grid = c(0, 0),
    axis.ticks = theme_blank(),
    legend.position = "none",
    legend.box = "horizontal",
    title = "Here is my title",
    legend.key.size = unit(2/3, "lines"))
MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)
MapPlot <- MapPlot + geom_path(fill = "transparent", colour = "BLACK", alpha = I(2/3), lwd = I(1/10))
MapPlot <- MapPlot + scale_fill_gradientn("Some/nRandom/nVariable", legend = FALSE,
                                          colours = MyPalette(100))

# This does the "faceting":
MapPlot <- MapPlot + facet_grid(IndependentVariable2 ~ IndependentVariable1)

# print(MapPlot)

ggsave(plot = MapPlot, "YOUR DIRECTORY HERE.png", h = 8.5, w = 11)

library(RJSONIO)
library(googleVis)
#data11<-GlobalLandTemperaturesByState1[GlobalLandTemperaturesByState1$Year>1960]
#head(data11)
#GlobalLandTemperaturesByState.YrAvg <- GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Year==1960,]
GlobalLandTemperaturesByState.YrAvg <- aggregate(AverageTemperature~Year+Country,GlobalLandTemperaturesByState,mean)
GlobalLandTemperaturesByState.YrAvgUn <- aggregate(AverageTemperatureUncertainty~Year+Country,GlobalLandTemperaturesByState,mean)
head(GlobalLandTemperaturesByState.YrAvg)
head(GlobalLandTemperaturesByState.YrAvgUn)
YrAvgUn<-merge(GlobalLandTemperaturesByState.YrAvg,GlobalLandTemperaturesByState.YrAvgUn)
head(YrAvgUn)
vis1<-gvisMotionChart(YrAvgUn,idvar='Country',timevar='Year')
plot(vis1)

Geo=gvisGeoChart(YrAvgUn, locationvar="Country", 
                 colorvar="AverageTemperature",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)
countryunique <- unique(GlobalLandTemperaturesByState$Country)
countryuniqueMonth <- unique( GlobalLandTemperaturesByState$Month)
countryunique1<-merge(countryunique,countryuniqueMonth)
head(countryuniqueMonth)
head(GlobalLandTemperaturesByState)
head(countryunique1,999)
colnames(countryunique1)<-c("Country", "Month")
#countryunique1<-countryunique1[order("Month"),]
#sort(countryunique1$Month)

#countrylatlong <- data.frame([unique(GlobalLandTemperaturesByState$Country),unique(GlobalLandTemperaturesByState$Year)
#head(GlobalLandTemperaturesByState)
  data(wrld_simpl)
head(wrld_simpl)
wrld_simpl@data$id <- wrld_simpl@data$NAME
wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica") 
head(wrld,999)
countrylatlong1 <- fread("c:/CSC465/project/countrylatlong.csv")
head(countrylatlong1)


head(countrylatlong1)
countrylatlong<-merge(countryunique1,countrylatlong1,by.x="Country",by.y="Country")
head(countrylatlong,99)
head(meta.country,99)
meta.country<-unique(countrylatlong)
meta.country<-data.table(meta.country)
meta.country<-unique(countrylatlong[,c(1:4),with=FALSE],by=c("Country","Month","Latitude","Longitude"))
setkey(meta.country,Country,Month)

meta.country.length<-length(meta.country$Country)
meta.country$intercept.coef<-numeric(meta.country.length)
meta.country$year.coef<-numeric(meta.country.length)
typeof(meta.country$Country)
typeof(meta.country$Month)
typeof(dt1$Country)
typeof(dt1$Month)
typeof(i)
meta.country$Country <- as.double(meta.country$Country)
dt1$Country <- as.integer(dt1$Country)

#Create a data table for faster subsetting. Data before 1880 is rejected (uncertainty is too high)
dt1 <- as.data.table(na.omit(subset(GlobalLandTemperaturesByState,Year>1960)))
#dt1 <- merge(dt1,countrylatlong,by.x="Country",by.y="Country")
data.table(dt1)
setkey(dt1,Country,Month)
head(dt1.subset)
head(meta.country)
#This loop will fill in the columns of the meta.city table.
for(i in 1:meta.country.length){
  dt1.subset<-dt1[list(meta.country$Country[i],meta.country$Month[i]),]
  lmfit<-with(dt1.subset,lm.fit(x=cbind(1,Year),y=AverageTemperature))
  meta.country$intercept.coef[i]<-lmfit$coefficients[1]
  meta.country$year.coef[i]<-lmfit$coefficients[2]
}
#ggplot()+borders("world",colour="white",fill="grey")+
#  theme(panel.background=element_rect(fill = "gray93"))+
  #geom_map(data=subset(meta.country, Month==1),aes((mapid=Country),map=meta.country$Country,colour=year.coef),size=3)+
  #geom_segment(data=subset(meta.country, Month==1),aes(xend = Longitude      + delta_Longitude,    yend = Latitude + delta_Latitude,colour=year.coef))
#  scale_colour_gradient(low="yellow",high ="red")+
#  ggtitle("Average Annual Increase in Temperature - January")+
#  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")


Jan<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==1),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - January")+theme(legend.position = "bottom")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Jan, "C:/CSC465/Project/images/JanTempIncrease.png", h = 4, w = 6)
Feb<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==2),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - February")+theme(legend.position = "bottom")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Feb, "C:/CSC465/Project/images/FebTempIncrease.png", h = 4, w = 7)

Mar<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==3),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+theme(legend.position = "bottom")+
  ggtitle("Average Annual Increase in Temperature - March")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Mar, "C:/CSC465/Project/images/MarTempIncrease.png", h = 4, w = 7)

Apr<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==4),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - April")+theme(legend.position = "right")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Apr, "C:/CSC465/Project/images/AprTempIncrease.png", h = 4, w = 7)

May<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==5),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - May")+ theme(legend.position = "right")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = May, "C:/CSC465/Project/images/MayTempIncrease.png", h = 4, w = 7)

Jun<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==6),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+theme(legend.position = "right")+
  ggtitle("Average Annual Increase in Temperature - June")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Jun, "C:/CSC465/Project/images/JunTempIncrease.png", h = 4, w = 7)

Jul<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==7),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - July")+theme(legend.position = "right")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Jul, "C:/CSC465/Project/images/JulTempIncrease.png", h = 4, w = 7)

Aug<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==8),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - August")+theme(legend.position = "right")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Aug, "C:/CSC465/Project/images/AugTempIncrease.png", h = 4, w = 7)

Sep<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==9),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - September")+theme(legend.position = "bottom")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Sep, "C:/CSC465/Project/images/SepTempIncrease.png", h = 4, w = 7)

Oct<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==10),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - October")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Oct, "C:/CSC465/Project/images/OctTempIncrease.png", h = 4, w =7)

Nov<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==11),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - November")+ theme(legend.position = "bottom")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Nov, "C:/CSC465/Project/images/NovTempIncrease.png", h = 4, w = 7)

Dec<-ggplot()+borders("world",colour="white",fill="grey")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.country, Month==12),aes(x=Longitude,y=Latitude,colour=year.coef),size=8)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - December")+theme(legend.position = "bottom")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
ggsave(plot = Dec, "C:/CSC465/Project/images/DecTempIncrease.png", h = 4, w = 7)

