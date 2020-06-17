library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggforce)
library(lubridate)
library(anytime)
library(forcats)
library(egg)
library(stringr)

setwd("//ahmct-065/teams/PMRF/Amir")
df=fread(file = "./bin/Final Datasets/SWITRS.csv", sep=",", header = TRUE)

wz.df=df[which(df$ROAD_COND_1=="D" | df$ROAD_COND_2=="D"), ]
colnames(wz.df)
wz.df=wz.df[, c("CASE_ID", "ACCIDENT_YEAR", "COLLISION_DATE", "COLLISION_TIME", "DAY_OF_WEEK", 
                "POPULATION", "CNTY_CITY_LOC", "SPECIAL_COND", "BEAT_TYPE", "CHP_BEAT_TYPE", 
                "PRIMARY_RD", "SECONDARY_RD", "DISTANCE", "DIRECTION", "INTERSECTION", "WEATHER_1", 
                "WEATHER_2", "STATE_HWY_IND", "CALTRANS_COUNTY", "CALTRANS_DISTRICT", "STATE_ROUTE", 
                "ROUTE_SUFFIX", "POSTMILE_PREFIX", "POSTMILE", "LOCATION_TYPE", "RAMP_INTERSECTION", 
                "SIDE_OF_HWY", "TOW_AWAY", "COLLISION_SEVERITY", "NUMBER_KILLED", "NUMBER_INJURED", 
                "PARTY_COUNT", "PRIMARY_COLL_FACTOR", "PCF_CODE_OF_VIOL", "PCF_VIOL_CATEGORY", 
                "PCF_VIOLATION", "PCF_VIOL_SUBSECTION", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", 
                "PED_ACTION", "ROAD_SURFACE", "ROAD_COND_1", "ROAD_COND_2", "LIGHTING", 
                "CONTROL_DEVICE", "CHP_ROAD_TYPE", "PEDESTRIAN_ACCIDENT", "BICYCLE_ACCIDENT", 
                "MOTORCYCLE_ACCIDENT", "TRUCK_ACCIDENT", "NOT_PRIVATE_PROPERTY", "ALCOHOL_INVOLVED", 
                "STWD_VEHTYPE_AT_FAULT", "CHP_VEHTYPE_AT_FAULT", "COUNT_SEVERE_INJ", 
                "COUNT_VISIBLE_INJ", "COUNT_COMPLAINT_PAIN", "COUNT_PED_KILLED", "COUNT_PED_INJURED",
                "COUNT_BICYCLIST_KILLED", "COUNT_BICYCLIST_INJURED", "COUNT_MC_KILLED", 
                "COUNT_MC_INJURED", "PRIMARY_RAMP", "SECONDARY_RAMP", "LATITUDE", "LONGITUDE")]
wz.df =wz.df %>% distinct()

wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==0]="PDO"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==1]="Fatal"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==2]="Severe Injury"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==3]="Visible Injury"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==4]="Complaint of Pain"

wz.df$COLLISION_SEVERITY=factor(wz.df$COLLISION_SEVERITY, levels = c("PDO", "Complaint of Pain", 
                                                                     "Visible Injury", "Severe Injury", "Fatal"))
######################################################################################################
#year
temp.df=wz.df[order(ACCIDENT_YEAR, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
              by=.(ACCIDENT_YEAR, COLLISION_SEVERITY)]

ggplot(data=temp.df, aes(x=ACCIDENT_YEAR, y=count, group=COLLISION_SEVERITY, color=COLLISION_SEVERITY)) +
  geom_line(size=1.5)+
  stat_summary(aes(group=1), geom = "line", fun = sum, colour="black", size=1.1, linetype="dashed")+
  stat_summary(aes(group=1, label=c(rep(NA, 35), rep("Total", 5))), geom = "label", fun=sum, 
               show.legend = FALSE)+
  facet_zoom(y=COLLISION_SEVERITY=="Fatal", ylim = c(0, 150), shrink = TRUE, 
             horizontal = FALSE, zoom.size = 0.75)+
  scale_colour_viridis(discrete = TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center')+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14), validate = FALSE,
        strip.background = element_rect(fill = alpha("grey", 0.5),
                                        linetype = 2,
                                        colour="black"))+
  ggtitle("Work zone collisions by year")+
  ylab("Count")+
  xlab("Accident Year")+
  labs(color="Collision Severity")
  
######################################################################################################
#date
temp.df=wz.df
temp.df$COLLISION_DATE=month(anydate(temp.df$COLLISION_DATE), label = TRUE, abbr = TRUE)
temp.df=setDT(temp.df)[order(COLLISION_DATE, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))),
                by=.(COLLISION_DATE, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                  temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                  temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                  temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=COLLISION_DATE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ggtitle("Work zone collisions by month")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=COLLISION_DATE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  #ggtitle("Work zone collisions by month")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, ncol=2, scales = "free")

ggarrange(p.main, p.severity, heights = c(2, 4))
######################################################################################################
#week day
temp.df=setDT(wz.df)[order(DAY_OF_WEEK, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                     by=.(DAY_OF_WEEK, COLLISION_SEVERITY)]
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==1]="Monday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==2]="Tuesday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==3]="Wednesday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==4]="Thursday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==5]="Friday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==6]="Saturday"
temp.df$DAY_OF_WEEK[temp.df$DAY_OF_WEEK==7]="Sunday"
temp.df$DAY_OF_WEEK=factor(temp.df$DAY_OF_WEEK, levels = c("Monday", "Tuesday", "Wednesday", 
                                                           "Thursday", "Friday", "Saturday", "Sunday"))

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                  temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                  temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                  temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=DAY_OF_WEEK, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ggtitle("Work zone collisions by day of week")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=DAY_OF_WEEK, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  #ggtitle("Work zone collisions by month")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, ncol=2, scales = "free")

ggarrange(p.main, p.severity, heights = c(2, 5))
######################################################################################################
#time of day
temp.df=wz.df
temp.df$COLLISION_TIME=temp.df$COLLISION_TIME %/% 100
temp.df=setDT(temp.df)[order(COLLISION_TIME, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(COLLISION_TIME, COLLISION_SEVERITY)]
temp.df$COLLISION_TIME=factor(temp.df$COLLISION_TIME)

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=COLLISION_TIME, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ggtitle("Work zone collisions by time of day")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=COLLISION_TIME, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  #ggtitle("Work zone collisions by month")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, ncol=2, scales = "free")

ggarrange(p.main, p.severity, heights = c(2, 5))
######################################################################################################
#Beat type
temp.df=setDT(wz.df)
temp.df=setDT(wz.df)[order(CHP_BEAT_TYPE, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                     by=.(CHP_BEAT_TYPE, COLLISION_SEVERITY)]

temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==0]="Not CHP"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==1]="Interstate"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==2]="US Highway"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==3]="State Route"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==4]="County Road Line"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE==5]="County Road Area"
temp.df$CHP_BEAT_TYPE[temp.df$CHP_BEAT_TYPE=="S"]="Administrative Beats"
temp.df$CHP_BEAT_TYPE=factor(temp.df$CHP_BEAT_TYPE, levels = c("Interstate", "State Route", "US Highway", 
                                                               "Not CHP", "County Road Line", "County Road Area", 
                                                               "Administrative Beats"))

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=CHP_BEAT_TYPE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by CHP beat type")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=CHP_BEAT_TYPE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)

ggarrange(p.main, p.severity)
######################################################################################################
#populaion
temp.df=setDT(wz.df)
temp.df=setDT(wz.df)[order(POPULATION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                     by=.(POPULATION, COLLISION_SEVERITY)]

temp.df$POPULATION[temp.df$POPULATION==1]="Less than 2500"
temp.df$POPULATION[temp.df$POPULATION==2]="2500-10000"
temp.df$POPULATION[temp.df$POPULATION==3]="10000-25000"
temp.df$POPULATION[temp.df$POPULATION==4]="25000-50000"
temp.df$POPULATION[temp.df$POPULATION==5]="50000-100000"
temp.df$POPULATION[temp.df$POPULATION==6]="100000-250000"
temp.df$POPULATION[temp.df$POPULATION==7]="Over 250000"
temp.df$POPULATION[temp.df$POPULATION==9]="Unincorporated (Rural)"

temp.df$POPULATION=factor(temp.df$POPULATION, levels = c("Less than 2500", "2500-10000", "10000-25000",
                                                         "25000-50000", "50000-100000", "100000-250000",
                                                         "Over 250000", "Unincorporated (Rural)"))

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=POPULATION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by population")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=POPULATION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#weather
temp.df=setDT(wz.df)
temp1.df=setDT(wz.df)[order(WEATHER_1, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                     by=.(WEATHER_1, COLLISION_SEVERITY)]
temp2.df=setDT(wz.df)[order(WEATHER_2, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                      by=.(WEATHER_2, COLLISION_SEVERITY)]
temp.df=merge(temp1.df, temp2.df, by.x=c("WEATHER_1", "COLLISION_SEVERITY"),
              by.y = c("WEATHER_2", "COLLISION_SEVERITY"), all.x = TRUE)
temp.df[is.na(temp.df)]=0
temp.df[temp.df=="-"]="Not stated"
temp.df$count=temp.df$count.x+temp.df$count.y
temp.df$count[temp.df$WEATHER_1=="Not stated"]=temp.df$count.x

temp.df$WEATHER_1[temp.df$WEATHER_1=="A"]="Clear"
temp.df$WEATHER_1[temp.df$WEATHER_1=="B"]="Cloudy"
temp.df$WEATHER_1[temp.df$WEATHER_1=="C"]="Raining"
temp.df$WEATHER_1[temp.df$WEATHER_1=="D"]="Snowing"
temp.df$WEATHER_1[temp.df$WEATHER_1=="E"]="Fog"
temp.df$WEATHER_1[temp.df$WEATHER_1=="F"]="Other"
temp.df$WEATHER_1[temp.df$WEATHER_1=="G"]="Wind"

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=WEATHER_1, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by weather")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=WEATHER_1, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#county
temp.df=setDT(wz.df)
county_abbr=fread(file = "County_Abbr_PeMS.code.csv", sep = ",", header = TRUE)
temp.df$CALTRANS_COUNTY=county_abbr$ABBREV.[match(temp.df$CNTY_CITY_LOC %/% 100, county_abbr$`Actual Code`)]
temp.df=setDT(temp.df)[order(CALTRANS_COUNTY), .(count=length(unique(CASE_ID))), 
                      by=.(CALTRANS_COUNTY)]

ggplot(data=temp.df, aes(x=CALTRANS_COUNTY, y=count, fill=CALTRANS_COUNTY))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"),
        legend.position = "none")+
  ggtitle("Work zone collisions by county")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")
######################################################################################################
#Location type (ramp, intersectiom)
temp.df=setDT(wz.df)
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==1]="Ramp exit"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==2]="Mid ramp"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==3]="Ramp entry"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==4]="within 100 ft. of ramp"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==5]="Intersection"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==6]="within 250 ft. of intersection"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==7]="Highway"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==8]="Not state highway"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION=="-"]="Not stated"
temp.df$RAMP_INTERSECTION[temp.df$RAMP_INTERSECTION==""]="Missing value"
temp.df$RAMP_INTERSECTION=factor(temp.df$RAMP_INTERSECTION, levels = c("Ramp exit", "Mid ramp", "Ramp entry",
                                                                       "within 100 ft. of ramp", "Intersection",
                                                                       "within 250 ft. of intersection",
                                                                       "Highway", "Not state highway",
                                                                       "Not stated", "Missing value"))

temp.df=setDT(temp.df)[order(RAMP_INTERSECTION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(RAMP_INTERSECTION, COLLISION_SEVERITY)]

temp.df$RAMP_INTERSECTION=str_wrap(temp.df$RAMP_INTERSECTION, width = 12)

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"


p.main=ggplot(data=temp.df1, aes(x=RAMP_INTERSECTION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by location type")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

temp.df=temp.df[-c(which(temp.df$RAMP_INTERSECTION=="Not stated" | 
                         temp.df$RAMP_INTERSECTION=="Missing value")),]

p.severity=ggplot(data=temp.df, aes(x=RAMP_INTERSECTION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#pcf_violation_category
temp.df=setDT(wz.df)
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="01" |
                             temp.df$PCF_VIOL_CATEGORY=="1"]="DUI"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="02" |
                             temp.df$PCF_VIOL_CATEGORY=="2"]="Impeding traffic"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="03" |
                             temp.df$PCF_VIOL_CATEGORY=="3"]="Unsafe speed"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="04" |
                             temp.df$PCF_VIOL_CATEGORY=="4"]="Following closely"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="05" |
                             temp.df$PCF_VIOL_CATEGORY=="5"]="Wrong side"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="06" |
                             temp.df$PCF_VIOL_CATEGORY=="6"]="Improper passing"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="07" |
                             temp.df$PCF_VIOL_CATEGORY=="7"]="Unsafe lane change"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="08" |
                             temp.df$PCF_VIOL_CATEGORY=="8"]="Improper turning"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="09" |
                             temp.df$PCF_VIOL_CATEGORY=="9"]="Aut. ROW"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="10"]="Ped. ROW"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="11"]="Ped. violation"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="12"]="Traffic signal"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="13"]="Bad parking"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="14"]="Lights"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="15"]="Brakes"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="16"]="Other equip."
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="17"]="Other hazard"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="18"]="Other than driver"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="19"]="19"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="20"]="20"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="21"]="Unsafe starting"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="22"]="Other improper driving"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="23"]="Other DUI"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="24"]="Sleep"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="00" |
                             temp.df$PCF_VIOL_CATEGORY=="0"]="Unknown"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="-"]="Not stated"

temp.df=setDT(temp.df)[order(PCF_VIOL_CATEGORY, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(PCF_VIOL_CATEGORY, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

ggplot(data=temp.df1, aes(x=PCF_VIOL_CATEGORY, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by PCF violation category")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

ggplot(data=temp.df, aes(x=PCF_VIOL_CATEGORY, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        #axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by PCF violation category")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 2)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())
######################################################################################################
#type_of_collision
temp.df=setDT(wz.df)
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="A"]="Head-On"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="B"]="Sideswipe"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="C"]="Rear End"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="D"]="Broadside"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="E"]="Hit Object"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="F"]="Overturned"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="G"]="Vehicle/Pedestrian"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="H"]="Other"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="-"]="Not Stated"

temp.df=setDT(temp.df)[order(TYPE_OF_COLLISION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(TYPE_OF_COLLISION, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"


p.main=ggplot(data=temp.df1, aes(x=TYPE_OF_COLLISION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by type of collision")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=TYPE_OF_COLLISION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#mviw
temp.df=setDT(wz.df)
temp.df$MVIW[temp.df$MVIW=="A"]="Non-collision"
temp.df$MVIW[temp.df$MVIW=="B"]="Pedestrian"
temp.df$MVIW[temp.df$MVIW=="C"]="Other MV."
temp.df$MVIW[temp.df$MVIW=="D"]="MV. on other roadway"
temp.df$MVIW[temp.df$MVIW=="E"]="Parked MV."
temp.df$MVIW[temp.df$MVIW=="F"]="Train"
temp.df$MVIW[temp.df$MVIW=="G"]="Bicyle"
temp.df$MVIW[temp.df$MVIW=="H"]="Animal"
temp.df$MVIW[temp.df$MVIW=="I"]="Fixed object"
temp.df$MVIW[temp.df$MVIW=="J"]="Other object"
temp.df$MVIW[temp.df$MVIW=="-"]="Not stated"

temp.df=setDT(temp.df)[order(MVIW, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(MVIW, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=MVIW, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 20, hjust =1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by MVIW")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=MVIW, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#pedestrain action
temp.df=setDT(wz.df)
temp.df$PED_ACTION[temp.df$PED_ACTION=="A"]="No pedestrian involved"
temp.df$PED_ACTION[temp.df$PED_ACTION=="B"]="Crosswalk at intersection"
temp.df$PED_ACTION[temp.df$PED_ACTION=="C"]="Crosswalk not at intersection"
temp.df$PED_ACTION[temp.df$PED_ACTION=="D"]="Crossing not in crosswalk"
temp.df$PED_ACTION[temp.df$PED_ACTION=="E"]="In road/shoulder"
temp.df$PED_ACTION[temp.df$PED_ACTION=="F"]="Not in road"
temp.df$PED_ACTION[temp.df$PED_ACTION=="G"]="Approaching/leaving school bus"
temp.df$PED_ACTION[temp.df$PED_ACTION=="-"]="Not stated"

temp.df=setDT(temp.df)[order(PED_ACTION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(PED_ACTION, COLLISION_SEVERITY)]
temp.df=temp.df[-c(which(temp.df$PED_ACTION=="No pedestrian involved")),]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=PED_ACTION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust =0.5, vjust =0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by pedestrain action (excluding no pedestrains")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

temp.df$PED_ACTION=str_wrap(temp.df$PED_ACTION, width = 12)

p.severity=ggplot(data=temp.df, aes(x=PED_ACTION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#road surface
temp.df=wz.df
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="A"]="Dry"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="B"]="Wet"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="C"]="Snowy/Icy"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="D"]="Slippery"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="-"]="Not stated"

temp.df=setDT(temp.df)[order(ROAD_SURFACE, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(ROAD_SURFACE, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=ROAD_SURFACE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by road surface condition")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

p.severity=ggplot(data=temp.df, aes(x=ROAD_SURFACE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#lighting
temp.df=wz.df
temp.df$LIGHTING[temp.df$LIGHTING=="A"]="Daylight"
temp.df$LIGHTING[temp.df$LIGHTING=="B"]="Dusk"
temp.df$LIGHTING[temp.df$LIGHTING=="C"]="Dark - Street light"
temp.df$LIGHTING[temp.df$LIGHTING=="D"]="Dark - No street light"
temp.df$LIGHTING[temp.df$LIGHTING=="E"]="Dark - Not functioning light"
temp.df$LIGHTING[temp.df$LIGHTING=="-"]="Not stated"

temp.df=setDT(temp.df)[order(LIGHTING, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), 
                       by=.(LIGHTING, COLLISION_SEVERITY)]

temp.df1=temp.df
temp.df1$COLLISION_SEVERITY=as.character(temp.df1$COLLISION_SEVERITY)
temp.df1$COLLISION_SEVERITY[which(temp.df1$COLLISION_SEVERITY=="Fatal" | 
                                    temp.df1$COLLISION_SEVERITY=="Severe Injury" | 
                                    temp.df1$COLLISION_SEVERITY=="Visible Injury" |
                                    temp.df1$COLLISION_SEVERITY=="Complaint of Pain")]="Fatality or Symptomatic Injury"

p.main=ggplot(data=temp.df1, aes(x=LIGHTING, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by road lighting condition")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")

temp.df$LIGHTING=str_wrap(temp.df$LIGHTING, width = 15)

p.severity=ggplot(data=temp.df, aes(x=LIGHTING, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  #ggtitle("Work zone collisions by month")+
  #ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3)+
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

ggarrange(p.main, p.severity)
######################################################################################################
#type of collision vs. pcf violation category vs. severity
temp.df=wz.df
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="A"]="Head-On"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="B"]="Sideswipe"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="C"]="Rear End"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="D"]="Broadside"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="E"]="Hit Object"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="F"]="Overturned"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="G"]="Vehicle/Pedestrian"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="H"]="Other"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="-"]="Not Stated"

temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="01" |
                            temp.df$PCF_VIOL_CATEGORY=="1"]="DUI"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="02" |
                            temp.df$PCF_VIOL_CATEGORY=="2"]="Impeding traffic"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="03" |
                            temp.df$PCF_VIOL_CATEGORY=="3"]="Unsafe speed"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="04" |
                            temp.df$PCF_VIOL_CATEGORY=="4"]="Following closely"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="05" |
                            temp.df$PCF_VIOL_CATEGORY=="5"]="Wrong side"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="06" |
                            temp.df$PCF_VIOL_CATEGORY=="6"]="Improper passing"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="07" |
                            temp.df$PCF_VIOL_CATEGORY=="7"]="Unsafe lane change"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="08" |
                            temp.df$PCF_VIOL_CATEGORY=="8"]="Improper turning"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="09" |
                            temp.df$PCF_VIOL_CATEGORY=="9"]="Aut. ROW"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="10"]="Ped. ROW"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="11"]="Ped. violation"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="12"]="Traffic signal"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="13"]="Bad parking"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="14"]="Lights"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="15"]="Brakes"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="16"]="Other equip."
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="17"]="Other hazard"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="18"]="Other than driver"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="19"]="19"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="20"]="20"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="21"]="Unsafe starting"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="22"]="Other improper driving"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="23"]="Other DUI"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="24"]="Sleep"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="00" |
                            temp.df$PCF_VIOL_CATEGORY=="0"]="Unknown"
temp.df$PCF_VIOL_CATEGORY[temp.df$PCF_VIOL_CATEGORY=="-"]="Not stated"

temp.df=setDT(temp.df)[order(TYPE_OF_COLLISION, PCF_VIOL_CATEGORY, COLLISION_SEVERITY), 
                       .(count=length(unique(CASE_ID))), 
                       by=.(TYPE_OF_COLLISION, PCF_VIOL_CATEGORY, COLLISION_SEVERITY)]

temp.df=temp.df%>%group_by(COLLISION_SEVERITY)%>%mutate(pct=count/sum(count))

ggplot(data=temp.df, aes(x=PCF_VIOL_CATEGORY, y=TYPE_OF_COLLISION, size=pct,
                                    fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_point(shape=21, alpha=0.5)+
  scale_size(range = c(2, 20))+
  scale_fill_viridis(discrete=TRUE, option="C") +
  #theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=14),
        #axis.title.x = element_text(size = 20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=20, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ggtitle("Work zone collisions by type of collision and PCF violation category")+
  ylab("Count")+
  #xlab("Month")+
  labs(fill="Collision Severity")+
  guides(size=FALSE, fill=FALSE)+
  facet_wrap(~COLLISION_SEVERITY, scales = "free", ncol = 3, 
             labeller = label_value)