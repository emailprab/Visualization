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

head(GlobalLandTemperaturesByState)
GlobalLandTemperaturesByState$dt<-as.Date(GlobalLandTemperaturesByState$dt,"%Y-%m-%d")
GlobalLandTemperaturesByState$Month<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%m"))
GlobalLandTemperaturesByState$Month.String<-format(GlobalLandTemperaturesByState$dt,"%B")
GlobalLandTemperaturesByState$Year<-as.numeric(format(GlobalLandTemperaturesByState$dt,"%Y"))
USData<-GlobalLandTemperaturesByState[GlobalLandTemperaturesByState$Country=='United States']
USData %>% 
  select(Year,AverageTemperature,State) %>%
  group_by(Year,State) %>%
  summarise(value=mean(AverageTemperature))-> USData4

USData4$State<- as.character(USData4$State)
USData4$State[USData4$State=="Georgia (State)"]<-"Georgia"
USData4$State<- as.factor(USData4$State)

colnames(USData4)[2]<- "region"
USData4$region<-tolower(USData4$region)
head(USData)
USData4 <- na.omit(USData4)
head(USData4)

states_map <- map_data("state")
i <- 0
for (thisYear in 1960:2013 )
#for (wk in unique(by_state_and_week$week)) {
  
  # filter by year
  
#  by_state_and_week %>% filter(week==wk) -> this_wk
thisYearCountry <- USData4[USData4$Year == thisYear, ]
thisYearCountry <- na.omit(thisYearCountry)

  # hack to let us color the state labels in white or black depending on
  # the value of the fill
  
  #this_wk %>%
   # filter(brks %in% c("1m-10m", "10m-50m")) %>%
    #.$state_abb %>%
    #unique -> white_states
  
  #centers %>%
  #  mutate(txt_col="black") %>%
  #  mutate(txt_col=ifelse(id %in% white_states, "white", "black")) -> centers
  
  # setup the plot
  
  gg <- ggplot()
  gg <- gg + geom_map(data=USData4, map=states_map,
                      aes(x=long, y=lat, map_id=id),
                      color="white", fill="#dddddd", size=2)
  gg <- gg + geom_map(data=thisYearCountry, map=stats_map,
                      aes(fill=brks, map_id=state_abb),
                      color="white", size=2)
  gg <- gg + geom_text(data=USData4,
                       aes(label=id, x=x, y=y, color=txt_col), size=4)
  gg <- gg + scale_color_identity()
  gg <- gg + scale_fill_brewer(name="US Avg (all states)",
                               palette="RdPu", na.value="#dddddd", drop=FALSE)
  gg <- gg + guides(fill=guide_legend(override.aes=list(colour=NA)))
  gg <- gg + coord_map()
  gg <- gg + labs(x=NULL, y=NULL,
                  title=sprintf("U.S. Avg Total temp of %sn", thisYear))
  gg <- gg + theme_bw()
  gg <- gg + theme(plot.title=element_text(face="bold", hjust=0, size=24))
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(legend.position="bottom")
  gg <- gg + theme(legend.direction="horizontal")
  gg <- gg + theme(legend.title.align=1)
  
  # save the image
  
  # i'm using "quartz" here since I'm on a Mac. Use what works for you system to ensure you
  # get the best looking output png
  
  png(sprintf("t%03d.png", i), width=800, height=500, type="cairo-png")
  print(gg)
  dev.off()
  
  i <- i + 1
  
 }
}
head(USData1)
USData11<-USData1
USData11 %>%
  mutate(yr=USData1$year ) %>%
  arrange(yr) %>%
  group_by(yr, State) %>%
  tally(AverageTemperature) %>%
  group_by(State) %>%
  mutate(cum=cumsum(n)) %>%
  ungroup %>%
  select(yr, State, cum) %>%
  mutate(yr=yr) %>%
  left_join(tidyr::expand(., yr, State), .) %>%
  group_by(State) %>%
  do(na.locf(.)) %>%
  mutate(state_abb=state.abb[match(State, state.name)],
         cum=as.numeric(ifelse(is.na(cum), 0, cum)),
         brks=cut(cum,
                  breaks=c(0, 5, 10, 15, 20, 25),
                  labels=c("0-5", "5-10", "10-15",
                           "15-20", "20-25"))) -> by_state_and_yr


