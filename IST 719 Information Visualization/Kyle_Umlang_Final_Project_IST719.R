#  
# Author Kyle Umlang
# Purpose: Poster Board
#

library(maps)  
library(mapproj)
library(RColorBrewer)
library(plotrix)
library(vioplot)
library(dplyr)
library(sqldf)

football<- read.csv("FootballPlayers2.csv"
                 , header=TRUE
                 , stringsAsFactors = FALSE)

football2<- read.csv("FootballPlayers.csv"
                    , header=TRUE
                    , stringsAsFactors = FALSE)
football
View(football)

QB<-football2 %>% 
    select(GS, Rnd, Pos, State, PB) %>%
    filter(Pos == "QB")

View(QB)


g <- ggplot(football2, aes(State))
g + coord_flip()+geom_bar(aes(fill=Pos), width = 1)

aes(x = reorder(State,-GS),GS), fill=Pos)

str(football)


display.brewer.all()

hist(football$GS,xlab="Number of Games Started",ylab="Number of Players", col=brewer.pal(20, "Set1"),
     , main= "Distribution of NFL Starts")

barplot(football$GS,xlab="Player",ylab="Number of Games Started"
        , main= "Number of NFL Starts by NFL Drafted Player")

vioplot(football$G, horizontal=TRUE, col=c("palegreen","paleturquoise1"), 
        xlab="Number of Games Played",  ylab="Players" , main= "Distribution of NFL Games Played by Player")
        
boxplot(football2$Rnd~football2$State, pch=20, cex=2, 
        col=brewer.pal(20, "Spectral"),xlab="States",ylab="Rounds Drafted"
        , main= "NFL Draft Picks by State")

ggplot(football2, aes(x=football2$State, y=football2$PB)) + 
    geom_bar(stat = "identity", main= "NFL Draft Picks by State")


#######################################
# Game Starts
#######################################

agg.dat<- aggregate(football$GS,
                    list(football$State)
                    , sum)

colnames(agg.dat)<-c("state", "game starts")


num.cols<-25
my.color.vec<-rev(heat.colors(num.cols))

pie(rep(1, num.cols), col=my.color.vec)

my.color.vec[1]
my.color.vec[4]
agg.dat


agg.dat$index<-round(rescale(x=agg.dat$`game starts`, c(1, num.cols)),0)
agg.dat$color<-my.color.vec[agg.dat$index]

agg.dat

m<-map("state")
m$names

state.order<-match.map(database = "state", regions=agg.dat$state, 
                       exact=FALSE, warn=TRUE)

cbind(m$names, agg.dat$state[state.order])

map("state", col=agg.dat$color[state.order], fill=TRUE
    , resolution = 0, lty=1, projection = "polyconic", 
    border="tan", main= "Game Starts by NFL Players' Home States | 2010 - 2019 NFL Draft Picks")


#######################################
# Pro Bowls
#######################################

agg.dat<- aggregate(football$PB,
                    list(football$State)
                    , sum)

colnames(agg.dat)<-c("state", "pro bowls")


num.cols<-25
my.color.vec<-rev(heat.colors(num.cols))

pie(rep(1, num.cols), col=my.color.vec)
 
my.color.vec[1]
my.color.vec[4]
agg.dat


agg.dat$index<-round(rescale(x=agg.dat$`pro bowls`, c(1, num.cols)),0)
agg.dat$color<-my.color.vec[agg.dat$index]

agg.dat

m<-map("state")
m$names

state.order<-match.map(database = "state", regions=agg.dat$state, 
                       exact=FALSE, warn=TRUE)

cbind(m$names, agg.dat$state[state.order])

map("state", col=agg.dat$color[state.order], fill=TRUE
    , resolution = 0, lty=1, projection = "polyconic", 
    border="tan", main= "Pro Bowls by NFL Players' Home States | 2010 - 2019 NFL Draft Picks")


#######################################
# Game Starts
#######################################

agg.dat<- aggregate(football$G,
                    list(football$State)
                    , sum)

colnames(agg.dat)<-c("state", "total games played")


num.cols<-25
my.color.vec<-rev(heat.colors(num.cols))

pie(rep(1, num.cols), col=my.color.vec)

my.color.vec[1]
my.color.vec[4]
agg.dat


agg.dat$index<-round(rescale(x=agg.dat$`total games played`, c(1, num.cols)),0)
agg.dat$color<-my.color.vec[agg.dat$index]

agg.dat

m<-map("state")
m$names

state.order<-match.map(database = "state", regions=agg.dat$state, 
                       exact=FALSE, warn=TRUE)

cbind(m$names, agg.dat$state[state.order])

map("state", col=agg.dat$color[state.order], fill=TRUE
    , resolution = 0, lty=1, projection = "polyconic", 
    border="tan", main= "Total Games Played by NFL Players' Home States | 2010 - 2019 NFL Draft Picks")


ggplot(data = football, mapping = aes(x = football$State, y = football$Rnd)) +
    geom_boxplot(alpha = 0)



ggplot(football2) + geom_point(aes(x=Rnd, y=PB)) 

