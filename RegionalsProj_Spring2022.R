#Read in the data
Softball2015 <- read.csv("Softball2015.csv")
View(Softball2015)
Softball2016 <- read.csv("Softball2016.csv")
View(Softball2016)
Softball2017 <- read.csv("Softball2017.csv")
View(Softball2017)
Softball2018 <- read.csv("Softball2018.csv")
View(Softball2018)

#this is the google spreadsheet
Regionals <- read.csv("Data/NCAA Tournament Games 2015-2018 - Sheet1.csv")
View(Regionals)

library(tidyverse)
library(ggplot2)

#filters each data set into teams that went to Regionals
NCAA2015 <- filter(Softball2015, NCAA %in% c("At-large","Auto"))
NCAA2016 <- filter(Softball2016, NCAA %in% c("At-large","Auto"))
NCAA2017 <- filter(Softball2017, NCAA %in% c("At-large","Auto"))
NCAA2018 <- filter(Softball2018, NCAA %in% c("At-large","Auto"))

#adds year variable
NCAA2015 <- mutate(NCAA2015, Year=2015)
NCAA2016 <- mutate(NCAA2016, Year=2016)
NCAA2017 <- mutate(NCAA2017, Year=2017)
NCAA2018 <- mutate(NCAA2018, Year=2018)

#cannot row bind unless number of columns are equal; this removes extra columns in 2018 data set
NCAA2018 <- subset(NCAA2018, select=-c(PitcherK, PitcherBB))

#row binds the four datasets
RegionalStats <- data.frame(rbind(NCAA2015,NCAA2016,NCAA2017,NCAA2018))

#reorders columns so year is first
RegionalStats <- RegionalStats[, c(32,1:31)]

#Adds stats: fielding percentage, singles, total bases, double plays per game, slugging percentage, success rate of stolen bases
#second line adds batting average
RegionalStats <- mutate(RegionalStats, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, SlgPct = TB/AB, SuccessRate = SB/(SB+CS))
RegionalStats <- mutate(RegionalStats, BA=H/AB)

#Joins Regionals and stats into one data frame by game
RegionalGames <- Regionals %>% left_join(RegionalStats, by = c("Home.Team" = "Name", "Year"),  suffix = c(".x", ".home")) %>% left_join(RegionalStats, by = c("Visiting.Team" = "Name", "Year"), suffix = c(".home",".visit"))

#Adds column indicating home team win
RegionalGames <- mutate(RegionalGames, Home.Win = ifelse(Home.Score>Visiting.Score, "Yes", "No"))

#Adds column indicating run differental (absolute value of Home-Visiting runs)
RegionalGames <- mutate(RegionalGames, Run.Diff = Home.Score-Visiting.Score)

#Creates new data frame that standardizes important variables
RegionalGames_Std <- mutate(RegionalGames, Run.Diff = Home.Score-Visiting.Score,
                        Singles.home = Singles.home/G.home, Singles.visit=Singles.visit/G.visit,
                        TB.home=TB.home/G.home, TB.visit=TB.visit/G.visit, 
                        Doubles.home=Doubles.home/G.home, Doubles.visit=Doubles.visit/G.visit,
                        Triples.home=Triples.home/G.home, Triples.visit=Triples.visit/G.visit,
                        HR.home=HR.home/G.home, HR.visit=HR.visit/G.visit,
                        SB.home=SB.home/G.home, SB.visit=SB.visit/G.visit,
                        CS.home=CS.home/G.home, CS.visit=CS.visit/G.visit,
                        RunsScored.home=RunsScored.home/G.home, RunsScored.visit=RunsScored.visit/G.visit,
                        PO.home=PO.home/G.home, PO.visit=PO.visit/G.visit,
                        A.home=A.home/G.home, A.visit=A.visit/G.visit,
                        E.home=E.home/G.home, E.visit=E.visit/G.visit,
                        RunsAllowed.home=RunsAllowed.home/G.home, RunsAllowed.visit=RunsAllowed.visit/G.visit)

#Plots double plays per game home vs away
DoublePlays <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=DPPerGame.home, y=DPPerGame.visit, color=Home.Win))
print(DoublePlays)
DoublePlaysRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=DPPerGame.home, y=DPPerGame.visit, color=Run.Diff))
print(DoublePlaysRuns)

#Plots fielding percentage home vs away
FieldingPct <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=FieldingPct.home, y=FieldingPct.visit, color=Home.Win))
print(FieldingPct)
FieldingPctRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=FieldingPct.home, y=FieldingPct.visit, color=Run.Diff))
print(FieldingPctRuns)

#Plots batting average home vs away
BattingAvg <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=BA.home, y=BA.visit, color=Home.Win))
print(BattingAvg)

#Plots home runs
HomeRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=HR.home, y=HR.visit, color=Home.Win))
print(HomeRuns)
HomeRuns2 <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=HR.home, y=HR.visit, color=Run.Diff))
print(HomeRuns2)

#plots stolen bases
StolenBases <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=SB.home, y=SB.visit, color=Home.Win))
print(StolenBases)
StolenBases2 <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=SB.home, y=SB.visit, color=Run.Diff))
print(StolenBases2)




