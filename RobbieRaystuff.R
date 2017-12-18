library(xlsx)
library(DT)
library(ggplot2)
library(plotly) 

top50pitchers <- read.xlsx("Top50PitchersB.xlsx",1,header=TRUE)
custom <- read.csv("CustomLeaderboard.csv", header = TRUE)
custom$RS.9 = ( custom$RS / custom$IP ) * 9


A = seq(1,50)

plot1 <- ggplot(top50pitchers, aes(A,ERA..,color =Lg ,text = paste("Name: ", Name))) + geom_point()
ggplotly(plot1)


#Weakness is alot of walks

plot2 <- ggplot(top50pitchers, aes(ERA..,BB,color =Lg ,text = paste("Name: ", Name))) + geom_point()
ggplotly(plot2)


plot3 <- ggplot(top50pitchers, aes(ERA..,WHIP,color =Lg ,text = paste("Name: ", Name))) + geom_point()
ggplotly(plot3)

####################################################################################

PitchersY4 <- read.xlsx("YearFourPitchers.xlsx",1,header=TRUE)
df <- transform(PitchersY4, SO_W = SO/BB)

#These are just the single comps.
df2 <- df[c(8,9,27,29),]

df3 <- df
df3$New = "Other"
df3$New[8] = "Robbie Ray"
df3$New[9] = "David Price"
df3$New[27] = "Tanner Roark"
df3$New[29] = "Shelby Miller"
####################################################################################
#Strike outs per walks

LeagueSO.Wplot <- ggplot(df3, aes(SO,SO_W,color =New ,text = paste("Name: ", Player))) + geom_point() #+
  #geom_point(data=df2[8, ], size=6) + ylim(1,5.5)

ggplotly(LeagueSO.Wplot)

monthly = read.xlsx("RobbieRayMonthly.xlsx",1,header=TRUE)
monthly$Split <- factor(monthly$Split, levels = monthly$Split)
MonthlySO.Wplot <- ggplot(monthly, aes(Split,SO.W,text = paste("Games Played",G))) + geom_point() + ylim(1,5.5)
ggplotly(MonthlySO.Wplot)


p = subplot(
  plot4,
  plot5,
  nrows = 1
)
p


## Despite having only an average SO/W numbers Robbie Ray was able to improve as the season went on
### as seen in the right graph in august-october he played eight games with a SO/W that is competitive
#### with the best of them. Also in the graph to the left you can see players like David Price who 
##### had similar average numbers in his 4th year and got 4.35 million all the way back in 2010.

####################################################################################################

#FIP
CompsFIPplot <- ggplot(df, aes(SO,FIP,color =Lg ,text = paste("Name: ", Player))) + geom_point() +
  geom_point(data=df[8, ], size=6)
ggplotly(CompsFIPplot)

L2017FIPplot <- ggplot(top50pitchers, aes(ERA..,FIP,color =Lg ,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=top50pitchers[5, ], size=6)
ggplotly(L2017FIPplot)

####################################################################################################
##Innings Pitched


CompsIPplot <- ggplot(df, aes(SO,IP,color =Lg ,text = paste("Name: ", Player))) + geom_point() +
  geom_point(data=df[8, ], size=6) + geom_point(data=df[29, ], size=3)
ggplotly(CompsIPplot)

MonthlyIPplot <- ggplot(monthly, aes(Split,IP,text = paste("Games Played",G))) + geom_point()
ggplotly(MonthlyIPplot)

#Despite being somewhat low in IP this can be attributed to getting into an unlikely 
## Injury at the end of July and carried through august.
### His monthly plot shows that his IP were steady for most of the months excluding July
#### which had the all star break and game (Robbie ray was an all star) and august where
##### he was injured.

####################################################################################################
#ERA

CompsERAplot <- ggplot(df, aes(SO,ERA,color =Lg ,text = paste("Name: ", Player))) + geom_point() +
  geom_point(data=df[8, ], size=6) + geom_point(data=df[29, ], size=3)
ggplotly(CompsERAplot)

MonthlyERAplot <- ggplot(monthly, aes(Split,ERA,text = paste("Games Played",G))) + geom_point()
ggplotly(MonthlyERAplot)

L2017ERAplot <- ggplot(top50pitchers, aes(ERA..,ERA..,color =Lg ,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=top50pitchers[5, ], size=6)
ggplotly(L2017ERAplot)

##Robbie Ray comes in at  19th for comparable players in ERA
##Pretty steady ERA throughout the year. If anything it decreased.

####################################################################################################
#BABIP (Batting Average for Balls In Play)
CompsBABIPplot <- ggplot(df, aes(SO,BAbip,color =Lg ,text = paste("Name: ", Player))) + geom_point() +
  geom_point(data=df[8, ], size=6) + geom_point(data=df[29, ], size=3)
ggplotly(CompsBABIPplot)


####################################################################################################
#WHIP
#although robbie gives up a lot of walks he has a very good WHIP 
df = transform(df, WHIP = (BB + H) / IP)

CompsWHIPplot <- ggplot(df, aes(SO,WHIP,color =Lg ,text = paste("Name: ", Player))) + geom_point() +
  geom_point(data=df[8, ], size=6) + geom_point(data=df[29, ], size=3)
ggplotly(CompsWHIPplot)


L2017WHIPplot<- ggplot(top50pitchers, aes(ERA..,WHIP,color =Lg ,text = paste("Name: ", Name))) + geom_point()

L2017WHIPplot <- ggplot(top50pitchers, aes(ERA..,WHIP,color =Lg ,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=top50pitchers[5, ], size=6)
ggplotly(L2017WHIPplot)


MonthlyWHIPplot <- ggplot(monthly, aes(Split,WHIP,text = paste("Games Played",G))) + geom_point()
ggplotly(MonthlyWHIPplot)


####################################################################################################
#Negative Comparables from class.
NegativeComp= df[c(8,11,61,97), ]



####################################################################################################
####################################################################################################
#Positive Comparables
PositiveComp= df[c(8,9,27,29), ]



####################################################################################################
L2017WHIPplot <- ggplot(top50pitchers, aes(ERA..,WHIP,color =Lg ,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=top50pitchers[5, ], size=6)
ggplotly(L2017WHIPplot)

####################################################################################################


samePitchers2016 <- read.csv("2016ArbPitchers.csv", header = TRUE)
sals = c(3.4,2.35,3.25,4.05,4.315,3.85,3.45,3.0,3.55,3.75,3.4,4.1,2.775,5.0,1.79,2.8,3.575,2.25,2.45)
samePitchers2016$Sal = sals
samePitchers2016 = samePitchers2016[c(5,6,9,10,12,13,16,17,18), ] 
samePitchers2015 <- read.csv("2015ArbPitchers2.csv", header = TRUE)
sals2 = c(7.25,4.325,2.8,2.525)
samePitchers2015$Sal = sals2

samePitchers2016$Year = c(rep(2016,9))
samePitchers2015$Year = c(rep(2015,4))

robRay <- read.csv("RobbieRayS.csv", header = TRUE)

robRay$Sal = 3.9

robRay$Year = 2017

samePitchers = rbind(samePitchers2015,samePitchers2016)
samePitchers = rbind(samePitchers,robRay)
samePitchers <- subset( samePitchers, select = -playerid )
samePitchers <- subset( samePitchers, select = -LOB.)
samePitchers <- subset( samePitchers, select = -HR.FB)
samePitchers <- subset( samePitchers, select = -xFIP )
samePitchers <- subset( samePitchers, select = -GB. )
samePitchers <- subset( samePitchers, select = -Team )

samePlot1 <- ggplot(samePitchers, aes(ERA,Sal, text = paste("Name: ", Name))) + geom_point() 
ggplotly(samePlot1)


samePitchers$std.ERA = abs(samePitchers$ERA / mean(samePitchers$ERA) )
samePitchers$std.FIP = abs(samePitchers$FIP / mean(samePitchers$FIP) )
samePitchers$std.BABIP = abs(samePitchers$BABIP / mean(samePitchers$BABIP) )
samePitchers$std.k9 = abs(samePitchers$K.9 / mean(samePitchers$K.9) )

samePitchers$PitcherRTG =  samePitchers$std.ERA +samePitchers$std.FIP + samePitchers$std.BABIP - samePitchers$std.k9 

samePitchers <- subset( samePitchers, select = -std.ERA)
samePitchers <- subset( samePitchers, select = -std.FIP)
samePitchers <- subset( samePitchers, select = -std.BABIP )
samePitchers <- subset( samePitchers, select = -std.k9 )

samePitchers$PitcherRTG = round(samePitchers$PitcherRTG,3)
samePitchers <- samePitchers[order(-samePitchers$Sal),]

write.xlsx(samePitchers, "/Users/davidrobinson/Desktop/School/MLB_Arbitration/samePitchers.xlsx")


datatable(samePitchers,options = list(ordering=F))



####################################################################################################

PitchersIn2016 = read.xlsx("PitchersIn2016.xlsx",1,header=TRUE,stringsAsFactors=FALSE)
robRay15and17 = read.xlsx("RobRay15and17.xlsx",1,header=TRUE,stringsAsFactors=FALSE)
smiller13 = read.xlsx("SMiller13to15.xlsx",1,header=TRUE,stringsAsFactors=FALSE)
TRoark14and15 = read.xlsx("TRoark14and15.xlsx",1,header=TRUE,stringsAsFactors=FALSE)
PitchersIn2016= rbind(PitchersIn2016,robRay15and17)
PitchersIn2016= rbind(PitchersIn2016,smiller13)
PitchersIn2016= rbind(PitchersIn2016,TRoark14and15)
PitchersIn2016$Name[35] = "Robbie Ray 2016"


PitchersIn2016$New = "Other"
PitchersIn2016$New[35] = "Robbie Ray 2016"
PitchersIn2016$New[147] = "Robbie Ray 2015"
PitchersIn2016$New[148] = "Robbie Ray 2017"
PitchersIn2016$New[17] = "Tanner Roark"
PitchersIn2016$New[149] = "Shelby Miller 2013"
PitchersIn2016$New[150] = "Shelby Miller 2014"
PitchersIn2016$New[151] = "Shelby Miller 2015"
PitchersIn2016$New[152] = "Tanner Roark 2014"
PitchersIn2016$New[153] = "Tanner Roark 2015"

PitchersIn2016$Year= 2016
PitchersIn2016$Year[147] = 2015
PitchersIn2016$Year[148] = 2017
PitchersIn2016$Year[149] = 2013
PitchersIn2016$Year[150] = 2014
PitchersIn2016$Year[151] = 2015
PitchersIn2016$Year[152] = 2014
PitchersIn2016$Year[153] = 2015

PitchersIn2016$Display = 0
PitchersIn2016$Display[35] = 1
PitchersIn2016$Display[147] = 1
PitchersIn2016$Display[148] = 1
PitchersIn2016$Display[17] = 1
PitchersIn2016$Display[149] = 1
PitchersIn2016$Display[150] = 1
PitchersIn2016$Display[151] = 1
PitchersIn2016$Display[152] = 1
PitchersIn2016$Display[153] = 1

####Standardize FIp
hist((PitchersIn2016$FIP-mean(PitchersIn2016$FIP))/sd(PitchersIn2016$FIP))
hist((PitchersIn2017$FIP-mean(PitchersIn2017$FIP))/sd(PitchersIn2017$FIP))

PitchersIn2017$adjFIP = (PitchersIn2017$FIP-mean(PitchersIn2017$FIP))/sd(PitchersIn2017$FIP)
PitchersIn2016$adjFIP = (PitchersIn2016$FIP-mean(PitchersIn2016$FIP))/sd(PitchersIn2016$FIP)


PitchersIn2016$idu <- row.names(PitchersIn2016)

LeagueSO.Wplot <- ggplot(PitchersIn2016, aes(idu,SO.W,text = paste("Name: ", Name))) + geom_point(color = "gray72") +
  geom_point(data=PitchersIn2016[35, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[147, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[148, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[17, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[149, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[150, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[151, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[152, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[153, ], size=3,color = "mediumpurple") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
        ) + 
  labs(y = "Strikeouts per Walks")

ggplotly(LeagueSO.Wplot)


#########
#FIP
LeagueFIPplot <- ggplot(PitchersIn2016, aes(idu,FIP,text = paste("Name: ", Name))) + geom_point(color = "gray72") +
  geom_point(data=PitchersIn2016[35, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[147, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[148, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[17, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[149, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[150, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[151, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[152, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[153, ], size=3,color = "mediumpurple") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "FIP")

ggplotly(LeagueFIPplot)

#########
#WHIP
LeagueWHIPplot <- ggplot(PitchersIn2016, aes(idu,WHIP,text = paste("Name: ", Name))) + geom_point(color = "gray72") +
  geom_point(data=PitchersIn2016[35, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[147, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[148, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[17, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[149, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[150, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[151, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[152, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[153, ], size=3,color = "mediumpurple") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "WHIP")

ggplotly(LeagueWHIPplot)

#########
#ERA
LeagueERAplot <- ggplot(PitchersIn2016, aes(idu,ERA,text = paste("Name: ", Name))) + geom_point(color = "gray72") +
  geom_point(data=PitchersIn2016[35, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[147, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[148, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[149, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[150, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[151, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[152, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[153, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[17, ], size=3,color = "mediumpurple") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "ERA")

ggplotly(LeagueERAplot)


#########
#SO/9
LeagueSO9plot <- ggplot(PitchersIn2016, aes(idu,SO9,text = paste("Name: ", Name))) + geom_point(color = "gray72") +
  geom_point(data=PitchersIn2016[35, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[147, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[148, ], size=5,color = "indianred") +
  geom_point(data=PitchersIn2016[149, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[150, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[151, ], size=3,color = "lightseagreen") +
  geom_point(data=PitchersIn2016[152, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[153, ], size=3,color = "mediumpurple") +
  geom_point(data=PitchersIn2016[17, ], size=3,color = "mediumpurple") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "SO/9",title = "Strikeout per 9 innings for 2016 pitchers with comparables")+
  geom_text(aes(label=ifelse(Display>0,Year,'')),hjust=0,vjust=0)

ggplotly(LeagueSO9plot)


#########
#just comps ERA
df5 = PitchersIn2016[which(PitchersIn2016$Display == 1),]
df5$Player = "Other"
df5$Player[2] = "Robbie Ray"
df5$Player[3] = "Robbie Ray"
df5$Player[4] = "Robbie Ray"
df5$Player[1] = "Tanner Roark"
df5$Player[5] = "Shelby Miller"
df5$Player[6] = "Shelby Miller"
df5$Player[7] = "Shelby Miller"
df5$Player[8] = "Tanner Roark"
df5$Player[9] = "Tanner Roark"


compERAplot <- ggplot(df5, aes(idu,ERA,color = Player,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=df5[2, ], size=5) +
  geom_point(data=df5[3, ], size=5) +
  geom_point(data=df5[4, ], size=5) +
  geom_point(data=df5[5, ], size=3) +
  geom_point(data=df5[6, ], size=3) +
  geom_point(data=df5[7, ], size=3) +
  geom_point(data=df5[8, ], size=3) +
  geom_point(data=df5[9, ], size=3) +
  geom_point(data=df5[1, ], size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "ERA",title = "ERA for Robbie Ray and Comparables")+
  geom_text(aes(label=ifelse(Display>0,Year,'')))

ggplotly(compERAplot)

compWHIPplot <- ggplot(df5, aes(idu,WHIP,color = Player,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=df5[2, ], size=1) +
  geom_point(data=df5[3, ], size=1) +
  geom_point(data=df5[4, ], size=1) +
  geom_point(data=df5[5, ], size=1) +
  geom_point(data=df5[6, ], size=1) +
  geom_point(data=df5[7, ], size=1) +
  geom_point(data=df5[8, ], size=1) +
  geom_point(data=df5[9, ], size=1) +
  geom_point(data=df5[1, ], size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "WHIP",title = "WHIP for Robbie Ray and Comparables")+
  geom_text(aes(label=ifelse(Display>0,Year,''),hjust = 1, vjust = 1))

ggplotly(compWHIPplot)


#ERA again

compSOWplot <- ggplot(df5, aes(idu,SO.W,color = Player,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=df5[2, ], size=1) +
  geom_point(data=df5[3, ], size=1) +
  geom_point(data=df5[4, ], size=1) +
  geom_point(data=df5[5, ], size=1) +
  geom_point(data=df5[6, ], size=1) +
  geom_point(data=df5[7, ], size=1) +
  geom_point(data=df5[8, ], size=1) +
  geom_point(data=df5[9, ], size=1) +
  geom_point(data=df5[1, ], size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "SO/W",title = "SO/W for Robbie Ray and Comparables")+
  geom_text(aes(label=ifelse(Display>0,Year,''),hjust = 1, vjust = 1))

ggplotly(compSOWplot)


#BABIP

df5$BABIP = 0
df5$BABIP[2] = 0.269      #16
df5$BABIP[3] = 0.311      #15
df5$BABIP[4] = 0.267      #17
df5$BABIP[1] = 0.352      #16
df5$BABIP[5] = 0.280      #13
df5$BABIP[6] = 0.256      #14
df5$BABIP[7] = 0.285      #15
df5$BABIP[8] = 0.270      #14
df5$BABIP[9] = 0.292      #15

compBABIPplot <- ggplot(df5, aes(idu,BABIP,color = Player,text = paste("Name: ", Name))) + geom_point() +
  geom_point(data=df5[2, ], size=1) +
  geom_point(data=df5[3, ], size=1) +
  geom_point(data=df5[4, ], size=1) +
  geom_point(data=df5[5, ], size=1) +
  geom_point(data=df5[6, ], size=1) +
  geom_point(data=df5[7, ], size=1) +
  geom_point(data=df5[8, ], size=1) +
  geom_point(data=df5[9, ], size=1) +
  geom_point(data=df5[1, ], size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "BABIP",title = "BABIP for Robbie Ray and Comparables")+
  geom_text(aes(label=ifelse(Display>0,Year,''),hjust = 1, vjust = 1))

ggplotly(compBABIPplot)



######################################################################################
PitchersIn2017 = read.xlsx("PitchersIn2017.xlsx",1,header=TRUE,stringsAsFactors=FALSE)
PitchersIn2017$id <- row.names(PitchersIn2017)

PitchersIn2017$Player = "Other"
PitchersIn2017$Player[64] = "Robbie Ray"      

PlotFIP2017 <- ggplot(PitchersIn2017, aes(id,FIP,color = Player,text = paste("Name: ", Name))) + geom_point() +
  #geom_point(data=PitchersIn2017[64, ], size=4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "FIP",title = "FIP for Robbie Ray compared to 2017 pitchers.")+
  ylim(1,7)

ggplotly(PlotFIP2017)

PitchersIn2016$Player = "Other"
PitchersIn2016$Player[17] = "Tanner Roark"      

PlotFIP2016 <- ggplot(PitchersIn2016, aes(idu,FIP,color = Player,text = paste("Name: ", Name))) + geom_point() +
  #geom_point(data=PitchersIn2017[64, ], size=4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  ) + 
  labs(y = "FIP",title = "FIP for Tanner Roark compared to 2016 pitchers.") +
  ylim(1,7)

ggplotly(PlotFIP2016)


p = subplot(
  PlotFIP2017,
  PlotFIP2016,
  nrows = 1
)
p

#Robbie Ray 17 in FIP in 2017
#Tanner Roark was 24th


PitchersIn2015 = read.xlsx("PitchersIn2015.xlsx",1,header=TRUE,stringsAsFactors=FALSE)


mean(PitchersIn2016$WHIP)
mean(PitchersIn2017$WHIP)

mean(PitchersIn2016$SO.W)
mean(PitchersIn2017$SO.W)

mean(PitchersIn2016$SO)
mean(PitchersIn2017$SO)

mean(PitchersIn2016$FIP)
mean(PitchersIn2017$FIP)

hist((PitchersIn2017$FIP-mean(PitchersIn2017$FIP))/sd(PitchersIn2017$FIP))
mean(PitchersIn2017$FIP)
sd(PitchersIn2017$FIP)

PitchersIn2017$adjFIP = (PitchersIn2017$FIP-mean(PitchersIn2017$FIP))/sd(PitchersIn2017$FIP)
PitchersIn2016$adjFIP = (PitchersIn2016$FIP-mean(PitchersIn2016$FIP))/sd(PitchersIn2016$FIP)
PitchersIn2015$adjFIP = (PitchersIn2015$FIP-mean(PitchersIn2015$FIP))/sd(PitchersIn2015$FIP)

mean(PitchersIn2015$FIP)
sd(PitchersIn2015$FIP)

shelby.miller2015 = PitchersIn2015[9,]





firstYE = read.xlsx("firstYearForR.xlsx",1,header=TRUE,stringsAsFactors=FALSE)

firstYE = firstYE[-6,]

library(leaps)
regsubsets(baseball$Win_Perct ~ baseball$Team_Salary + baseball$Perct_paied_to_Pitcher
           + factor(baseball$SizebyMetroSize) + factor(baseball$SizebyPayroll),
           data = baseball)
bestSubRob = regsubsets(firstYE$Sal ~ firstYE$IP + firstYE$K.9 + firstYE$FIP + firstYE$ERA + firstYE$adjFIP + firstYE$OppAvg + firstYE$BABIP,data = firstYE)
plot(bestSubRob,scale="bic")

winning_model <- lm(firstYE$Sal ~ firstYE$IP +firstYE$adjFIP )
coefficients(winning_model)
plot(winning_model)




