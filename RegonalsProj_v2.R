library(tidyverse)
#library(ggplot2)
library(lattice)
library(caret)
library(tidymodels)
library(parsnip)

#Read in the data
Softball2015 <- read.csv("Data/Softball2015.csv")

Softball2016 <- read.csv("Data/Softball2016.csv")

Softball2017 <- read.csv("Data/Softball2017.csv")

Softball2018 <- read.csv("Data/Softball2018.csv")


#this is the google spreadsheet
Regionals <- read.csv("Data/NCAA Tournament Games 2015-2018 - Sheet1 new.csv")


library(tidyverse)
#library(ggplot2)
library(lattice)
library(caret)

library(tidymodels)
# send link to Tidymodels with R online book

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
RegionalStats <- mutate(RegionalStats, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                        TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                        SlgPct = TB/AB, SuccessRate = SB/(SB+CS))
RegionalStats <- mutate(RegionalStats, BA=H/AB)

#Joins Regionals and stats into one data frame by game
RegionalGames <- Regionals %>% left_join(RegionalStats, by = c("Home.Team" = "Name", "Year"),  
                                         suffix = c(".x", ".home")) %>% left_join(RegionalStats, 
                                                                                  by = c("Visiting.Team" = "Name", "Year"), suffix = c(".home",".visit"))

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

#Adds column indicating if home team or visiting team (or neither) is regional host
RegionalGames_Std <- mutate(RegionalGames_Std, 
                            Host= case_when(Regional.Host==Home.Team~1, 
                                            Regional.Host==Visiting.Team~-1,
                                            Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

dim(RegionalGames_Std)
set.seed(1)
obs=seq(1,397,1)
x=sample(obs,300,replace=F)
RegionalGames_Std$Home.Win <- as.factor(RegionalGames_Std$Home.Win)
Games.train=RegionalGames_Std[x,]
Games.test=RegionalGames_Std[-x,]

Differences <- mutate(RegionalGames_Std, Doubles = Doubles.home - Doubles.visit, 
                      Triples = Triples.home - Triples.visit,
                      HR = HR.home - HR.visit, 
                      RunsScored = RunsScored.home - RunsScored.visit,
                      SB = SB.home - SB.visit, 
                      CS = CS.home - CS.visit, 
                      IP = IP.home - IP.visit, 
                      RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                      ER = ER.home - ER.visit,
                      ERA = ERA.home - ERA.visit,
                      PO = PO.home - PO.visit,
                      A = A.home - A.visit,
                      E = E.home - E.visit,
                      DP = DPPerGame.home - DPPerGame.visit,
                      FieldingPct = FieldingPct.home - FieldingPct.visit, 
                      Singles = Singles.home - Singles.visit,
                      TB = TB.home - TB.visit, 
                      SlgPct = SlgPct.home - SlgPct.visit,
                      SuccessRate = SuccessRate.home - SuccessRate.visit,
                      BA = BA.home - BA.visit)
keep <- c("HR", "RunsScored", "SB", "CS", "RunsAllowed", "ER", "ERA", "E", "DP", "SlgPct", "FieldingPct", "Singles", "TB", "Doubles", "Triples", "SuccessRate", "BA", "Run.Diff", "Home.Win", "Host")
Differences <- Differences[keep]

Differences$Home.Win <- as.factor(Differences$Home.Win)

Diff.split <- initial_split(Differences)
diff.train <- analysis(Diff.split)
diff.test <- assessment(Diff.split)

test <- glm(Home.Win ~ Host*(E + SlgPct + HR + ERA + Doubles), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi <- test %>% predict(diff.test, type="response")

logi.predicted <- ifelse(fit.logi> 0.5, "Yes", "No")

counter=0
for(i in 1:100)
{
  if(logi.predicted[i] !=diff.test$Home.Win[i])
  {counter=counter+1}
}

counter
misclass=counter/100
misclass
accuracy = (100-counter)/100
accuracy


test2 <- glm(Home.Win ~ Host*(HR), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi2 <- test2 %>% predict(diff.test, type="response")

logi.predicted2 <- ifelse(fit.logi2 > 0.5, "Yes", "No")

counter2=0
for(i in 1:100)
{
  if(logi.predicted2[i] !=diff.test$Home.Win[i])
  {counter2=counter2+1}
}

counter2
misclass2=counter2/100
misclass2
accuracy2 = (100-counter2)/100
accuracy2

test3 <- glm(Home.Win ~ Host*(Triples + FieldingPct + SlgPct + SuccessRate), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi3 <- test3 %>% predict(diff.test, type="response")

logi.predicted3 <- ifelse(fit.logi3 > 0.5, "Yes", "No")

counter3=0
for(i in 1:100)
{
  if(logi.predicted3[i] !=diff.test$Home.Win[i])
  {counter3=counter3+1}
}

counter3
misclass3=counter3/100
misclass3
accuracy3 = (100-counter3)/100
accuracy3
#test3 highest accuracy other than just HR, replacing Triples w/ HR ~0.67, w/ Doubles~0.68, w/Singles ~0.69

test4 <- glm(Home.Win ~ Host*(Triples), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi4 <- test4 %>% predict(diff.test, type="response")

logi.predicted4 <- ifelse(fit.logi4 > 0.5, "Yes", "No")

counter4=0
for(i in 1:100)
{
  if(logi.predicted4[i] !=diff.test$Home.Win[i])
  {counter4=counter4+1}
}

counter4
misclass4=counter4/100
misclass4
accuracy4 = (100-counter4)/100
accuracy4
